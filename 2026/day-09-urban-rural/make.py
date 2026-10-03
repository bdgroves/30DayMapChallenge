"""
Day 9 · Urban-rural — Tacoma to Paradise

One straight line from the Port of Tacoma to Paradise on Mount Rainier, about 80 km, and what
changes along it: how many people live within a kilometre of the line, and how high the ground is.

Downloads (cached in data/): Census 2020 TIGER/Line blocks for Washington (with POP20),
Copernicus 30 m DEM.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402
import terrain  # noqa: E402

import geopandas as gpd  # noqa: E402
import numpy as np  # noqa: E402
from pyproj import Transformer  # noqa: E402
from rasterio.transform import rowcol  # noqa: E402
from shapely.geometry import LineString, box  # noqa: E402

DAY = 9
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
CRS = "EPSG:32610"
A = (-122.4127, 47.2675)          # Port of Tacoma, Blair Waterway
B = (-121.7355, 46.7860)          # Paradise
HALF = 1000                       # m either side of the line
STEP = 500                        # m between samples
BLOCKS = "https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_53_tabblock20.zip"

to = Transformer.from_crs(4326, CRS, always_xy=True)
ax_, ay_ = to.transform(*A)
bx_, by_ = to.transform(*B)
line = LineString([(ax_, ay_), (bx_, by_)])
L = line.length
print(f"= line {L / 1000:.1f} km")

blocks = fetch.shapes(BLOCKS, DATA / "wa_blocks.zip")
blocks = blocks[blocks["COUNTYFP20"] == "053"].to_crs(CRS)         # Pierce County
blocks["POP20"] = blocks["POP20"].astype(int)
blocks["dens"] = blocks["POP20"] / (blocks.area / 1e6)

# people in each 500 m step of a 2 km-wide corridor (area-weighted from blocks)
d = np.arange(0, L, STEP)
pop = np.zeros(len(d))
ux, uy = (bx_ - ax_) / L, (by_ - ay_) / L
nx, ny = -uy, ux
corr = blocks.cx[min(ax_, bx_) - 3000:max(ax_, bx_) + 3000, min(ay_, by_) - 3000:max(ay_, by_) + 3000]
sidx = corr.sindex
for i, s in enumerate(d):
    p0 = (ax_ + ux * s, ay_ + uy * s)
    p1 = (ax_ + ux * (s + STEP), ay_ + uy * (s + STEP))
    cell = gpd.GeoSeries([LineString([p0, p1]).buffer(HALF, cap_style=2)], crs=CRS).iloc[0]
    for j in sidx.query(cell):
        blk = corr.iloc[j]
        inter = blk.geometry.intersection(cell).area
        if inter > 0 and blk.geometry.area > 0:
            pop[i] += blk["POP20"] * inter / blk.geometry.area
dens = pop / (STEP * 2 * HALF / 1e6)                                # people per km²

# elevation along the line
bb = (min(A[0], B[0]) - 0.06, min(A[1], B[1]) - 0.06, max(A[0], B[0]) + 0.06, max(A[1], B[1]) + 0.06)
z, tf = terrain.dem(bb, crs=CRS, res=30)
xs, ys = ax_ + ux * (d + STEP / 2), ay_ + uy * (d + STEP / 2)
r, c = rowcol(tf, xs, ys)
elev = z[np.clip(r, 0, z.shape[0] - 1), np.clip(c, 0, z.shape[1] - 1)] * 3.28084
km = (d + STEP / 2) / 1000
total = pop.sum()
last = km[np.nonzero(dens >= 100)[0].max()] if np.any(dens >= 100) else 0
empty = km[dens < 1]
print(f"= {total:,.0f} people within 1 km; density ≥100/km² until km {last:.1f}; "
      f"{(dens < 1).sum() * STEP / 1000:.1f} km of the line with under 1 person/km²; top {elev.max():,.0f} ft")

# ── figure ───────────────────────────────────────────────────────────────────
fig = dmc.figure("wide", map_box=(0.05, 0.50, 0.90, 0.29))[0]
mx = fig.axes[0]
pad = 6000
mz, mtf = z, tf
mx.imshow(terrain.relief(mz, 30, strength=0.55, exaggerate=1.3), extent=terrain.extent(mtf, mz.shape),
          interpolation="bilinear", zorder=0)
view = box(min(ax_, bx_) - pad, min(ay_, by_) - pad, max(ax_, bx_) + pad, max(ay_, by_) + pad)
vb = blocks[blocks.intersects(view) & (blocks["dens"] > 0)]
vb.plot(ax=mx, column="dens", cmap=dmc.SEQ_HEAT, vmin=0, vmax=4000, alpha=0.55, lw=0, zorder=1)
gpd.GeoSeries([line.buffer(HALF, cap_style=2)], crs=CRS).boundary.plot(ax=mx, color=dmc.INK, lw=0.6, zorder=3)
mx.plot([ax_, bx_], [ay_, by_], color=dmc.INK, lw=0.8, ls=(0, (3, 2)), zorder=3)
for name, (lon, lat), ha in [("Port of Tacoma", A, "right"), ("Paradise", B, "left"), ("Puyallup", (-122.293, 47.185), "left"),
                             ("Orting", (-122.204, 47.098), "left"), ("Eatonville", (-122.266, 46.867), "right"),
                             ("Ashford", (-122.03, 46.758), "right")]:
    x, y = to.transform(lon, lat)
    mx.scatter([x], [y], s=10, color=dmc.INK, zorder=4)
    dmc.label(mx, x + (1500 if ha == "left" else -1500), y, name, size=7.5, ha=ha, va="center", zorder=5)
mx.set_xlim(view.bounds[0], view.bounds[2])
mx.set_ylim(view.bounds[1], view.bounds[3])
mx.set_aspect("equal")

def panel(rect, y, colour, label, fmt, log=False):
    a = fig.add_axes(rect)
    a.fill_between(km, y, color=colour, alpha=0.25, lw=0, step="mid")
    a.step(km, y, where="mid", color=colour, lw=1.2)
    if log:
        a.set_yscale("symlog", linthresh=10)
    a.set_xlim(0, km.max())
    a.tick_params(labelsize=7, colors=dmc.STONE, length=2)
    for t in a.get_xticklabels() + a.get_yticklabels():
        t.set_fontfamily(dmc.MONO)
    for s in ("top", "right"):
        a.spines[s].set_visible(False)
    a.spines["left"].set_color(dmc.MIST)
    a.spines["bottom"].set_color(dmc.MIST)
    a.yaxis.set_major_formatter(__import__("matplotlib").ticker.FuncFormatter(fmt))
    fig.text(rect[0], rect[1] + rect[3] + 0.012, label, family=dmc.MONO, size=6.8, color=dmc.STONE)
    return a


p1 = panel((0.07, 0.29, 0.88, 0.15), dens, dmc.LAVA, "PEOPLE PER KM², WITHIN 1 KM OF THE LINE  (LOG SCALE)",
           lambda v, _: f"{v:,.0f}", log=True)
p2 = panel((0.07, 0.10, 0.88, 0.13), elev, dmc.LAKE, "GROUND ELEVATION, FEET", lambda v, _: f"{v:,.0f}")
p2.set_xlabel("km from the Port of Tacoma", family=dmc.MONO, fontsize=7.5, color=dmc.STONE)

dmc.frame(
    fig, DAY,
    subtitle=(f"A straight line from the Port of Tacoma to Paradise on Rainier: {L / 1000:.0f} km, {total:,.0f} people living within a\n"
              f"kilometre of it, and {elev.max():,.0f} ft of climb. The city thins out about {last:.0f} km in; the last stretch is forest and mountain."),
    source="U.S. Census Bureau, 2020 Census blocks (POP20) · Copernicus DEM GLO-30",
    note="People are spread evenly across each census block, so the density is smoothed where blocks are large.",
)
dmc.save(fig, DAY, alt=(
    f"A map of the straight line from the Port of Tacoma to Paradise on Mount Rainier over shaded relief and census "
    f"population, with two profiles below: people per square kilometre, high in Tacoma and Puyallup and falling to "
    f"almost none after about km {last:.0f}, and elevation rising to {elev.max():,.0f} ft at Paradise."))
