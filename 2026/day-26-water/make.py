"""
Day 26 · Water — Lake Tahoe, clear and deep

Lake Tahoe's floor from the USGS multibeam survey of 1998 (10 m grid, UTM 10, WGS84): 501 m
at its deepest, the second-deepest lake in the US. Beside it, the lake's clarity on the same
downward axis: how far a Secchi disk could be seen at UC Davis TERC's index station, each year
since 1968, as bars hanging from the surface. The record comes from my SECCHI project's copy of
TERC's data (EDI edi.1340, CC BY 4.0), averaged the way TERC's published annual figures are
reproduced: each month first, then the year.

Downloads (cached in data/): USGS DDS-55 Lake Tahoe bathymetry (ARC/INFO export grid),
Copernicus 30 m DEM for the land around it, TERC's Secchi readings from the SECCHI repo.
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
import pandas as pd  # noqa: E402
import rasterio  # noqa: E402
from pyproj import Transformer  # noqa: E402

DAY = 26
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
CRS = "EPSG:32610"
PAGES = ["https://pubs.usgs.gov/dds/dds-55/pacmaps/lt_data.htm", "https://cmgds.marine.usgs.gov/data/pacmaps/lt-data.html",
         "https://cmgds.marine.usgs.gov/data/pacmaps/lt-index.html"]
SURFACE = 1897.0                                   # m, approximate natural rim / lake surface
TERC = "https://raw.githubusercontent.com/bdgroves/secchi/main/data/reference/terc_secchi.csv"
STATIONS = {"LTP": ("Index station", -120.155, 39.0972), "MLTP": ("Mid-lake", -120.0153, 39.1417)}   # TERC metadata
TARGET_M = 29.7                                    # TRPA standard: the 1967-71 average

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

# ── clarity: yearly mean Secchi depth at the index station (months first, then the year) ─────
fetch.get(TERC, DATA / "terc_secchi.csv")
sec = pd.read_csv(DATA / "terc_secchi.csv", parse_dates=["date_time_local"])
ltp = sec[sec["station"] == "LTP"].dropna(subset=["secchi_m"])
tt = ltp["date_time_local"]
monthly = ltp.groupby([tt.dt.year.rename("year"), tt.dt.month.rename("month")])["secchi_m"].mean()
yr = monthly.groupby(level="year").agg(["mean", "size"])
yr = yr[yr["size"] >= 9]                           # whole years only (1967 starts in July; this year isn't over)
best, worst = yr["mean"].idxmax(), yr["mean"].idxmin()
last = yr.index.max()
print(f"= Secchi: {len(ltp):,} index-station readings, {len(yr)} whole years {yr.index.min()}-{last}; "
      f"clearest {best} {yr.loc[best, 'mean']:.1f} m, murkiest {worst} {yr.loc[worst, 'mean']:.1f} m, {last} {yr.loc[last, 'mean']:.1f} m")

fig, ax = dmc.figure("portrait", map_box=(0.06, 0.10, 0.60, 0.71))
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
to = Transformer.from_crs(4326, CRS, always_xy=True)
for code, (name, lo, la) in STATIONS.items():
    sx, sy = to.transform(lo, la)
    ax.scatter([sx], [sy], s=28, color="white", edgecolor=dmc.INK, lw=0.8, zorder=7)
    dmc.label(ax, sx + 900, sy + 500, name, size=7.5, style="italic", zorder=8, halo="#e9f1f3")
ax.set_xlim(l - pad, r + pad)
ax.set_ylim(b - pad, t + pad)
ax.set_aspect("equal")
dmc.scalebar(ax, 5, loc=(0.05, 0.04))

# the clarity panel, to the right of the lake, on a downward depth axis like the lake's own
ax.set_anchor("W")
ax.apply_aspect()
mp = ax.get_position()
cx0 = mp.x1 + 0.07
cax = fig.add_axes((cx0, mp.y0 + 0.02, 0.95 - cx0, mp.height * 0.80))
cm = LAKE
for y, row in yr.iterrows():
    cax.bar(y, row["mean"], width=0.78, bottom=0, color=cm(min(row["mean"], 34) / 34 * 0.55 + 0.1), lw=0)
cax.axhline(TARGET_M, color=dmc.LAVA, lw=0.9, ls=(0, (4, 2)))
cax.text(yr.index.min(), TARGET_M + 0.6, f"Target: {TARGET_M:g} m, the 1967–71 average", family=dmc.MONO, size=6.2,
         color=dmc.LAVA, va="top")
cax.set_ylim(36, 0)                                # surface at the top, deeper below
cax.set_xlim(yr.index.min() - 1, yr.index.max() + 1)
cax.axhline(0, color=dmc.LAKE, lw=1.2)
for s in ("top", "right"):
    cax.spines[s].set_visible(False)
cax.spines["left"].set_color(dmc.MIST)
cax.spines["bottom"].set_visible(False)
cax.xaxis.tick_top()
cax.set_xticks([1970, 1990, 2010, int(last)] if last - 2010 > 6 else [1970, 1990, 2010])
cax.set_yticks([0, 10, 20, 30])
cax.yaxis.set_major_formatter(__import__("matplotlib").ticker.FuncFormatter(lambda v, _: f"{v:.0f} m"))
cax.tick_params(labelsize=7, colors=dmc.STONE, length=2)
for tl in cax.get_xticklabels() + cax.get_yticklabels():
    tl.set_fontfamily(dmc.MONO)
fig.text(cx0, mp.y0 + 0.02 + mp.height * 0.80 + 0.045, "HOW FAR DOWN YOU CAN SEE", family=dmc.MONO, size=7.5, color=dmc.INK)
fig.text(cx0, mp.y0 + 0.02 + mp.height * 0.80 + 0.028, "Secchi depth at the index station, yearly mean", size=7,
         color=dmc.STONE, style="italic")
for y, txt in [(best, "clearest"), (worst, "murkiest")]:
    cax.annotate(f"{y}\n{txt}, {yr.loc[y, 'mean']:.1f} m", (y, yr.loc[y, "mean"]), xytext=(0, -6), textcoords="offset points",
                 ha="center", va="top", family=dmc.MONO, size=6, color=dmc.INK)

dmc.frame(
    fig, DAY,
    subtitle=(f"Lake Tahoe is {depth.max():.0f} m deep. In {best} you could see {yr.loc[best, 'mean']:.0f} m down into it; in {last}, "
              f"{yr.loc[last, 'mean']:.0f} m.\nThe floor from the USGS multibeam survey, and the lake's clarity on the same downward axis."),
    source="USGS DDS-55 bathymetry (1998) · UC Davis TERC Secchi record (EDI, CC BY 4.0) · Copernicus DEM",
    note="Depth below a surface of 1,897 m. Clarity averages each year's months first; quote TERC's own annual figures.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Lake Tahoe's floor shaded blue by depth to {depth.max():.0f} m, with 100 m contours, the deepest point and TERC's "
    f"two Secchi stations marked. Beside it, bars hanging down from the surface show how far a Secchi disk could be seen each "
    f"year since {yr.index.min()}: {yr.loc[best, 'mean']:.0f} m at best in {best}, {yr.loc[worst, 'mean']:.0f} m at worst in "
    f"{worst}, and not at the {TARGET_M:g} m target since {int(yr[yr['mean'] >= TARGET_M].index.max())}."))
