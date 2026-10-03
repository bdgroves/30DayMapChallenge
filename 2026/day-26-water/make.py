"""
Day 26 · Water — Lake Tahoe, clear and deep

Lake Tahoe's floor from the USGS multibeam survey of 1998 (10 m grid, UTM 10, WGS84): 501 m
at its deepest, the second-deepest lake in the US. Draft: depth only; the clarity layer from
the SECCHI project comes next.

Downloads (cached in data/): USGS DDS-55 Lake Tahoe bathymetry (ARC/INFO export grid),
Copernicus 30 m DEM for the land around it.
"""
import gzip
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402
import terrain  # noqa: E402

import numpy as np  # noqa: E402
import rasterio  # noqa: E402
from pyproj import Transformer  # noqa: E402

DAY = 26
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
CRS = "EPSG:32610"
PAGES = ["https://pubs.usgs.gov/dds/dds-55/pacmaps/lt_data.htm", "https://cmgds.marine.usgs.gov/data/pacmaps/lt-data.html",
         "https://cmgds.marine.usgs.gov/data/pacmaps/lt-index.html"]
SURFACE = 1897.0                                   # m, approximate natural rim / lake surface

grid = DATA / "lt_bathy.e00"
if not grid.exists():
    url = None
    for page in PAGES:
        try:
            html = fetch._open(page, 60, 2).decode("latin-1")
        except Exception as e:  # noqa: BLE001
            print(f"  {page}: {e}")
            continue
        m = re.search(r'href="([^"]*lt_bathy\.e00\.gz)"', html, re.I)
        if m:
            from urllib.parse import urljoin
            url = urljoin(page, m.group(1))
            break
    if not url:
        raise SystemExit("could not find lt_bathy.e00.gz on the USGS pages")
    print(f"= bathymetry from {url}")
    grid.write_bytes(gzip.decompress(fetch.get(url, DATA / "lt_bathy.e00.gz")))
head = grid.read_bytes()[:400]
print("= e00 starts: " + head[:120].decode("latin-1").replace("\n", " | "))
try:
    with rasterio.open(grid) as src:
        elev = src.read(1, masked=True).astype(float)
        tf = src.transform
except Exception as e:  # noqa: BLE001
    raise SystemExit(f"GDAL can't read the e00 grid ({e}); see the first bytes above")
depth = SURFACE - elev
depth = np.ma.masked_where(elev.mask | (elev > SURFACE + 5) | (elev < 1000), depth)
print(f"= grid {elev.shape}, elevation {elev.min():.0f}-{elev.max():.0f} m; deepest {depth.max():.0f} m")

fig, ax = dmc.figure("portrait", map_box=(0.08, 0.07, 0.84, 0.74))
l, b, r, t = rasterio.transform.array_bounds(elev.shape[0], elev.shape[1], tf)
pad = 3000
bb = Transformer.from_crs(CRS, 4326, always_xy=True).transform_bounds(l - pad, b - pad, r + pad, t + pad)
z, ztf = terrain.dem(bb, crs=CRS, res=30)
ax.imshow(terrain.relief(z, 30, strength=0.6, exaggerate=1.2), extent=terrain.extent(ztf, z.shape), interpolation="bilinear")
ax.imshow(depth, extent=(l, r, b, t), cmap=dmc.SEQ_WATER, vmin=0, vmax=520, interpolation="bilinear")
cs = ax.contour(np.flipud(depth.filled(np.nan)), levels=[100, 200, 300, 400, 500], extent=(l, r, b, t),
                colors=[dmc.PARCHMENT], linewidths=0.4, alpha=0.7)
ax.clabel(cs, fmt=lambda v: f"{v:.0f} m", fontsize=6, colors=dmc.PARCHMENT)
iy, ix = np.unravel_index(np.ma.argmax(depth), depth.shape)
dx, dy = tf * (ix + 0.5, iy + 0.5)
ax.scatter([dx], [dy], s=20, marker="v", color=dmc.LAVA, zorder=5)
dmc.label(ax, dx + 1200, dy, f"{depth.max():.0f} m", size=8, weight="bold", zorder=6)
ax.set_xlim(l - pad, r + pad)
ax.set_ylim(b - pad, t + pad)
ax.set_aspect("equal")
dmc.scalebar(ax, 5, loc=(0.05, 0.04))

dmc.frame(
    fig, DAY,
    subtitle=(f"The floor of Lake Tahoe from the USGS multibeam survey: {depth.max():.0f} m at the deepest point,\n"
              "darker as it gets deeper. Contours every 100 m."),
    source="USGS Digital Data Series 55 (Lake Tahoe multibeam bathymetry, 1998) · Copernicus DEM GLO-30",
    note="Depth below a lake surface of 1,897 m; the real surface moves a metre or two with the seasons.",
)
dmc.save(fig, DAY, alt=f"Map of Lake Tahoe's floor shaded blue by depth to {depth.max():.0f} m, with 100 m contours and the deepest point marked.")
