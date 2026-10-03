"""
basemap — Mapbox basemaps for the 2026 challenge (Mapbox is this year's sponsor).

    import basemap
    fig, ax = dmc.figure("square")
    ax.set_xlim(x0, x1); ax.set_ylim(y0, y1)      # EPSG:3857 metres (Web Mercator)
    basemap.mapbox(ax)                            # fills the axes with the house Mapbox style
    ... plot your data in EPSG:3857 on top ...
    dmc.frame(..., source="... · " + basemap.CREDIT)

The image comes from the Mapbox Static Images API, which renders Web Mercator only, so a map
with a Mapbox basemap is drawn in EPSG:3857. The Mapbox wordmark is drawn into the image by
Mapbox (their attribution rules require it on static maps); the text credit "© Mapbox
© OpenStreetMap" goes in the map's DATA line: use basemap.CREDIT.

Token: set MAPBOX_TOKEN in the environment (a GitHub secret in Actions). It is never written to
any file. Without a token, mapbox() draws nothing and returns False, so maps still render.

Style: MAPBOX_STYLE (env) or STYLE below, e.g. "bdgroves/abc123" once the house style is
uploaded to Mapbox Studio (toolkit/mapbox/brooks-parchment.json); until then "mapbox/light-v11".
"""
from __future__ import annotations

import hashlib
import math
import os
import urllib.request
from pathlib import Path

CACHE = Path(__file__).resolve().parents[1] / ".cache" / "mapbox"
STYLE = "mapbox/light-v11"
CREDIT = "© Mapbox © OpenStreetMap"
EARTH = 40075016.686                       # Web Mercator circumference, m
TILE = 512                                 # Mapbox GL styles use 512 px tiles
MAX_PX = 1280                              # Static Images API limit per side (before @2x)


def _lonlat(x, y):
    lon = x / EARTH * 360
    lat = math.degrees(2 * math.atan(math.exp(y / 6378137.0)) - math.pi / 2)
    return lon, lat


def available() -> bool:
    """True when a Mapbox token is set, so a map can choose Web Mercator up front."""
    return bool(os.environ.get("MAPBOX_TOKEN"))


def mapbox(ax, style: str | None = None, token: str | None = None, alpha: float = 1.0, zorder: int = 0) -> bool:
    """Fill ax (EPSG:3857 limits) with a Mapbox static image. Returns True if drawn."""
    token = token or os.environ.get("MAPBOX_TOKEN")
    if not token:
        print("  basemap: no MAPBOX_TOKEN set, drawing without a Mapbox basemap")
        return False
    style = style or os.environ.get("MAPBOX_STYLE") or STYLE
    fig = ax.figure
    bbox = ax.get_position().transformed(fig.transFigure).transformed(fig.dpi_scale_trans.inverted())
    # widen the shorter side so the map exactly fills its frame (no squeezing, no gaps)
    x0, x1 = ax.get_xlim()
    y0, y1 = ax.get_ylim()
    cx, cy, sw, sh = (x0 + x1) / 2, (y0 + y1) / 2, x1 - x0, y1 - y0
    frame = bbox.width / bbox.height
    sw, sh = (sh * frame, sh) if sw / sh < frame else (sw, sw / frame)
    x0, x1, y0, y1 = cx - sw / 2, cx + sw / 2, cy - sh / 2, cy + sh / 2
    ax.set_aspect("equal", adjustable="box")
    want_w = bbox.width * 300 / 2                  # CSS pixels at 300 dpi output, @2x image
    res = (x1 - x0) / min(want_w, MAX_PX)          # metres per CSS pixel
    if (y1 - y0) / res > MAX_PX:
        res = (y1 - y0) / MAX_PX
    w, h = int(round((x1 - x0) / res)), int(round((y1 - y0) / res))
    zoom = math.log2(EARTH / (TILE * res))
    lon, lat = _lonlat((x0 + x1) / 2, (y0 + y1) / 2)
    path = (f"/styles/v1/{style}/static/{lon:.6f},{lat:.6f},{zoom:.2f},0,0/{w}x{h}@2x"
            f"?attribution=false&logo=true")
    CACHE.mkdir(parents=True, exist_ok=True)
    cached = CACHE / (hashlib.sha1(path.encode()).hexdigest()[:20] + ".png")
    if not cached.exists():
        url = "https://api.mapbox.com" + path + "&access_token=" + token
        req = urllib.request.Request(url, headers={"User-Agent": "30DayMapChallenge-2026"})
        try:
            with urllib.request.urlopen(req, timeout=120) as r:
                cached.write_bytes(r.read())
        except urllib.error.HTTPError as e:            # never print the URL: it holds the token
            print(f"  basemap: Mapbox answered {e.code} for style {style}; drawing without it")
            return False
    import matplotlib.image as mpimg
    img = mpimg.imread(cached)
    # the image covers exactly w × h CSS pixels at res, centred on the axes centre
    # (fractional zoom is rounded to 2 decimals by Mapbox, so recompute res from it)
    res = EARTH / (TILE * 2 ** round(zoom, 2))
    cx, cy = (x0 + x1) / 2, (y0 + y1) / 2
    ext = (cx - w * res / 2, cx + w * res / 2, cy - h * res / 2, cy + h * res / 2)
    ax.imshow(img, extent=ext, interpolation="bilinear", alpha=alpha, zorder=zorder)
    ax.set_xlim(ext[0], ext[1])            # snap to the image (zoom rounding can leave a hairline gap)
    ax.set_ylim(ext[2], ext[3])
    print(f"  basemap: {style} at zoom {zoom:.2f}")
    return True
