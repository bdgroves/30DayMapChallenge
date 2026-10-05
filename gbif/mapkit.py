"""Shared bits for the GBIF fish/mammal maps: house style, hillshade, OSM lines -> shapes."""
import json
import os
from pathlib import Path

import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib import font_manager
from shapely.geometry import shape, LineString, MultiLineString, Point
from shapely.ops import polygonize, unary_union, linemerge

ROOT = Path(__file__).resolve().parent.parent
GEO = ROOT / "geo" / "data"
PAPER, INK, STONE, MIST, CREAM = "#f5f0e8", "#1c1a16", "#7a7268", "#d6d0c4", "#ede8dc"
BLUE, RUST, NEUTRAL, GOLD, WATER, RIVER = "#2e7aa6", "#c8602e", "#b8b0a4", "#c8922a", "#bcd3de", "#5d93b3"
CREDIT = "Brooks Groves · brooksgroves.com"

def style():
    fonts = os.environ.get("FONTS", "")
    for n in ("Roboto-Regular.ttf", "Roboto-Medium.ttf"):
        if fonts and Path(fonts, n).exists():
            font_manager.fontManager.addfont(str(Path(fonts, n)))
    plt.rcParams.update({"font.family": "Roboto" if fonts else "DejaVu Sans", "font.size": 10,
        "axes.edgecolor": MIST, "axes.labelcolor": STONE, "xtick.color": STONE, "ytick.color": STONE,
        "axes.facecolor": PAPER, "figure.facecolor": PAPER, "savefig.facecolor": PAPER,
        "axes.spines.top": False, "axes.spines.right": False})

def read_dem(name):
    import rasterio
    with rasterio.open(GEO / f"dem-{name}.tif") as s:
        a = s.read(1).astype(float)
        a[a == -32768] = np.nan
        b = s.bounds
    return a, (b.left, b.right, b.bottom, b.top)

def hillshade(z, extent, az=315, alt=40, exag=1.0):
    w, e, s, n = extent
    lat = np.radians((s + n) / 2)
    dy = (n - s) / z.shape[0] * 110540
    dx = (e - w) / z.shape[1] * 111320 * np.cos(lat)
    zz = np.where(np.isnan(z), np.nanmin(z), z) * exag
    gy, gx = np.gradient(zz, dy, dx)
    slope = np.arctan(np.hypot(gx, gy))
    aspect = np.arctan2(-gx, gy)
    a, al = np.radians(az), np.radians(alt)
    hs = np.sin(al) * np.cos(slope) + np.cos(al) * np.sin(slope) * np.cos(a - aspect)
    return np.clip(hs, 0, 1)

def osm(name):
    return json.load(open(GEO / f"osm-{name}.geojson"))["features"]

def lines(feats, names, kinds=("LineString", "MultiLineString")):
    out = [shape(f["geometry"]) for f in feats if f["properties"].get("name") in names and f["geometry"]["type"] in kinds
           and f["geometry"]["coordinates"]]
    return unary_union([g for g in out if not g.is_empty])

def polygon(feats, name, near=None):
    """Polygon(s) named `name` from OSM outer ways; `near` (lon, lat) keeps only parts within 0.5 deg."""
    ls = []
    for f in feats:
        if f["properties"].get("name") == name and f["geometry"]["type"] in ("LineString", "MultiLineString"):
            g = shape(f["geometry"])
            if near and g.centroid.distance(Point(near)) > 0.5:
                continue
            ls += list(g.geoms) if g.geom_type == "MultiLineString" else [g]
    polys = list(polygonize(unary_union(ls)))
    return unary_union(polys) if polys else None

def point(feats, name):
    for f in feats:
        if f["properties"].get("name") == name:
            g = shape(f["geometry"])
            return g if g.geom_type == "Point" else g.centroid
    return None

def draw(ax, geom, k=1.0, **kw):
    """Plot a shapely geometry on lon*k / lat axes."""
    if geom is None or geom.is_empty:
        return
    gs = getattr(geom, "geoms", [geom])
    for g in gs:
        if g.geom_type == "Polygon":
            x, y = g.exterior.xy
            ax.fill(np.array(x) * k, y, **kw)
        elif g.geom_type in ("LineString", "LinearRing"):
            x, y = g.xy
            ax.plot(np.array(x) * k, y, **kw)
        elif g.geom_type.startswith("Multi") or g.geom_type == "GeometryCollection":
            draw(ax, g, k, **kw)
