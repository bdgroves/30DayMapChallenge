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
def read_e00_grid(path):
    """An uncompressed ARC/INFO export (E00) grid: header, then each row as fixed-width numbers."""
    from rasterio.transform import from_origin
    num = re.compile(r"-?\d\.\d+E[+-]\d+")
    lines = path.read_text(encoding="latin-1").splitlines()
    i = next(k for k, ln in enumerate(lines) if ln.startswith("GRD"))
    hdr = lines[i + 1].split()
    ncols, nrows = int(hdr[0]), int(hdr[1])
    kind = hdr[2][0]                                  # 2 = single precision, 3 = double
    nodata = float(num.findall(lines[i + 1])[0])
    cell = [float(v) for v in num.findall(lines[i + 2])]
    lo = [float(v) for v in num.findall(lines[i + 3])]
    hi = [float(v) for v in num.findall(lines[i + 4])]
    width = 14 if kind == "2" else 21
    per = 5 if kind == "2" else 3
    rows_per = -(-ncols // per)
    k = i + 5
    out = np.empty((nrows, ncols), dtype="float32")
    for r in range(nrows):
        vals = []
        for ln in lines[k:k + rows_per]:
            vals += [float(ln[j:j + width]) for j in range(0, len(ln.rstrip()), width)]
        k += rows_per
        out[r] = vals[:ncols]
    out = np.ma.masked_where(out <= nodata * 0.999 if nodata < 0 else out == nodata, out)
    print(f"= e00 grid {ncols}x{nrows}, cell {cell[0]:g} m, x {lo[0]:.0f}-{hi[0]:.0f}, y {lo[1]:.0f}-{hi[1]:.0f}")
    return out, from_origin(lo[0], hi[1], cell[0], cell[1] if len(cell) > 1 else cell[0])


elev, tf = read_e00_grid(grid)
depth = SURFACE - elev
depth = np.ma.masked_where(elev.mask | (elev > SURFACE + 5) | (elev < 1000), depth)
print(f"= grid {elev.shape}, elevation {elev.min():.0f}-{elev.max():.0f} m; deepest {depth.max():.0f} m")

fig, ax = dmc.figure("portrait", map_box=(0.08, 0.10, 0.84, 0.71))
l, b, r, t = rasterio.transform.array_bounds(elev.shape[0], elev.shape[1], tf)
pad = 3000
bb = Transformer.from_crs(CRS, 4326, always_xy=True).transform_bounds(l - pad, b - pad, r + pad, t + pad)
z, ztf = terrain.dem(bb, crs=CRS, res=30)
ax.imshow(terrain.relief(z, 30, strength=0.6, exaggerate=1.2), extent=terrain.extent(ztf, z.shape), interpolation="bilinear")
from matplotlib.colors import LinearSegmentedColormap  # noqa: E402
LAKE = LinearSegmentedColormap.from_list("tahoe", ["#cfe3ea", "#7fb0c4", dmc.LAKE, "#1f3a4a", "#0f2230"])  # shallow water stays blue
ax.imshow(depth, extent=(l, r, b, t), cmap=LAKE, vmin=0, vmax=520, interpolation="antialiased")
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
    source="USGS DDS-55 multibeam bathymetry (1998) · Copernicus DEM GLO-30",
    note="Depth below a lake surface of 1,897 m; the real surface moves a metre or two with the seasons.",
)
dmc.save(fig, DAY, alt=f"Map of Lake Tahoe's floor shaded blue by depth to {depth.max():.0f} m, with 100 m contours and the deepest point marked.")
