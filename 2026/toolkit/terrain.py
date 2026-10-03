"""
terrain — elevation and hillshade in the house style.

    import terrain
    z, tf = terrain.dem((-122.2, 46.55, -121.35, 47.15), crs="EPSG:32610", res=30)
    rgb = terrain.relief(z, res=30)                 # parchment hillshade, water tinted
    ax.imshow(rgb, extent=terrain.extent(tf, z.shape))

Elevation comes from the Copernicus DEM (GLO-30 or GLO-90), free on AWS with no account.
Tiles are cached in 2026/.cache/dem so each is downloaded once.
"""
from __future__ import annotations

import math
import urllib.request
from pathlib import Path

import numpy as np

CACHE = Path(__file__).resolve().parents[1] / ".cache" / "dem"
UA = {"User-Agent": "30DayMapChallenge-2026 (github.com/bdgroves/30DayMapChallenge)"}


def _tile_name(lat: int, lon: int, res: int) -> str:
    ns = f"N{lat:02d}" if lat >= 0 else f"S{-lat:02d}"
    ew = f"E{lon:03d}" if lon >= 0 else f"W{-lon:03d}"
    arc = "10" if res == 30 else "30"
    return f"Copernicus_DSM_COG_{arc}_{ns}_00_{ew}_00_DEM"


def tiles(bounds, res: int = 30) -> list[Path]:
    """Download (once) every 1° Copernicus tile touching bounds = (west, south, east, north)."""
    w, s, e, n = bounds
    bucket = "copernicus-dem-30m" if res == 30 else "copernicus-dem-90m"
    CACHE.mkdir(parents=True, exist_ok=True)
    out = []
    for lat in range(math.floor(s), math.ceil(n)):
        for lon in range(math.floor(w), math.ceil(e)):
            name = _tile_name(lat, lon, res)
            p = CACHE / f"{name}.tif"
            if not p.exists():
                url = f"https://{bucket}.s3.amazonaws.com/{name}/{name}.tif"
                try:
                    with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=300) as r:
                        p.write_bytes(r.read())
                    print(f"  dem tile {name}")
                except urllib.error.HTTPError as err:          # open ocean has no tile
                    if err.code in (403, 404):
                        continue
                    raise
            out.append(p)
    return out


def dem(bounds, crs: str = "EPSG:32610", res: float = 30, src_res: int | None = None):
    """Elevation (metres) for lon/lat bounds, reprojected to crs at res metres. Returns (z, transform).

    Sea and missing tiles come back as 0, which is how the Copernicus DEM stores the ocean.
    """
    import rasterio
    from rasterio.merge import merge
    from rasterio.warp import Resampling, calculate_default_transform, reproject, transform_bounds

    src_res = src_res or (30 if res < 60 else 90)
    paths = tiles(bounds, src_res)
    srcs = [rasterio.open(p) for p in paths]
    mosaic, mtf = merge(srcs, bounds=bounds, nodata=0)
    src_crs = srcs[0].crs
    for s in srcs:
        s.close()
    l, b, r, t = transform_bounds("EPSG:4326", crs, *bounds, densify_pts=21)
    width, height = int((r - l) / res), int((t - b) / res)
    from rasterio.transform import from_origin
    tf = from_origin(l, t, res, res)
    z = np.zeros((height, width), dtype="float32")
    reproject(mosaic[0].astype("float32"), z, src_transform=mtf, src_crs=src_crs, dst_transform=tf,
              dst_crs=crs, resampling=Resampling.bilinear, src_nodata=0, dst_nodata=0)
    return z, tf


def extent(tf, shape):
    """matplotlib imshow extent (left, right, bottom, top) for a north-up transform."""
    h, w = shape
    return (tf.c, tf.c + tf.a * w, tf.f + tf.e * h, tf.f)


def hillshade(z, res: float, azimuth: float = 315, altitude: float = 40, exaggerate: float = 1.5):
    """0–1 shading; 1 is lit, 0 in shadow."""
    dy, dx = np.gradient(z * exaggerate, res)
    slope = np.pi / 2 - np.arctan(np.hypot(dx, dy))
    aspect = np.arctan2(-dx, dy)
    az, alt = np.radians(360 - azimuth + 90), np.radians(altitude)
    hs = np.sin(alt) * np.sin(slope) + np.cos(alt) * np.cos(slope) * np.cos(az - aspect)
    return np.clip(hs, 0, 1)


def relief(z, res: float, paper="#f5f0e8", shadow="#7a7268", water="#dfe6e3", strength: float = 0.75,
           exaggerate: float = 1.5):
    """RGB image: parchment lit by a hillshade, with water (z <= 0) as a flat pale tint."""
    from matplotlib.colors import to_rgb
    hs = hillshade(z, res, exaggerate=exaggerate)
    # flat ground (hs ≈ sin(alt)) should read as paper, not grey
    flat = np.sin(np.radians(40))
    t = np.clip((flat - hs) / flat, -0.4, 1) * strength
    p, s = np.array(to_rgb(paper)), np.array(to_rgb(shadow))
    rgb = p + np.clip(t, 0, 1)[..., None] * (s - p) + np.clip(-t, 0, 1)[..., None] * (1 - p)
    rgb[z <= 0] = to_rgb(water)
    return np.clip(rgb, 0, 1)
