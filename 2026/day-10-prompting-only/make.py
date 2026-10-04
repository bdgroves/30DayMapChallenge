"""
Day 10 · Prompting only — A map made only by asking

Big Creek's watershed above Groveland as a 1960s line-printer map: a FORTRAN program (BIGCRK.f,
fixed form, upper case, column 72 respected) reads a grid of elevations and prints the basin in
eight tones, the dark ones built by overprinting characters on the same line, the way SYMAP made
maps at Harvard's Laboratory for Computer Graphics. Every line of code here, FORTRAN and Python,
was written by Claude from a prompt, with no hand edits; the prompts are in PROMPTS.md.

Steps:
  1. Python fetches the basin (USGS NLDI, gage 11284400), its streams and the Copernicus DEM,
     and averages the DEM into printer cells. A printer character is 1/10 inch wide and 1/6 inch
     tall, so a cell is 5/3 as tall as it is wide on the ground, to keep the map's shape.
  2. gfortran compiles BIGCRK.f and runs it on GRID.DAT; it writes PRINT.TXT with ASA carriage
     control (' ' new line, '+' print over the last line, '1' new page).
  3. Python feeds PRINT.TXT to a pretend line printer: green-bar paper, tractor holes, and a
     slightly translucent ink so every overprint darkens the cell.
"""
import json
import shutil
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402
import terrain  # noqa: E402

import geopandas as gpd  # noqa: E402
import numpy as np  # noqa: E402
import shapely  # noqa: E402
from matplotlib.patches import Circle, Rectangle  # noqa: E402

DAY = 10
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
CRS = "EPSG:32610"
SITE = "linked-data/nwissite/USGS-11284400"
NC = 110                                              # printer columns given to the map


def nldi(path, name):
    p = DATA / name
    if not p.exists():
        for base in ("https://api.water.usgs.gov/nldi", "https://labs.waterdata.usgs.gov/api/nldi"):
            try:
                p.write_text(json.dumps(fetch.json_get(f"{base}/{path}{'&' if '?' in path else '?'}f=json")))
                break
            except Exception as e:  # noqa: BLE001
                print(f"  {base}: {e}")
    return json.loads(p.read_text())


basin = gpd.GeoDataFrame.from_features(nldi(f"{SITE}/basin", "basin.geojson")["features"], crs=4326).to_crs(CRS)
flow = gpd.GeoDataFrame.from_features(nldi(f"{SITE}/navigation/UT/flowlines?distance=100", "flowlines.geojson")["features"],
                                      crs=4326).to_crs(CRS)
site = nldi(SITE, "site.geojson")["features"][0]["geometry"]["coordinates"]
poly = basin.geometry.union_all()
x0, y0, x1, y1 = poly.bounds
dx = (x1 - x0) / NC
dy = dx * 5 / 3                                       # 6 lines and 10 characters to the inch
nr = int(np.ceil((y1 - y0) / dy))
print(f"= basin {poly.area / 1e6:.1f} km²; printer grid {NC} x {nr}, cell {dx:.0f} m x {dy:.0f} m")

# 1. the grid: mean elevation of each printer cell, inside the basin only
bb = gpd.GeoSeries([shapely.box(x0, y0, x1, y1)], crs=CRS).to_crs(4326).total_bounds
z, tf = terrain.dem(tuple(bb + np.array([-0.01, -0.01, 0.01, 0.01])), crs=CRS, res=30)
rows, cols = np.mgrid[0:z.shape[0], 0:z.shape[1]]
zx, zy = tf.c + (cols + 0.5) * tf.a, tf.f + (rows + 0.5) * tf.e
ci = ((zx - x0) // dx).astype(int)
ri = ((y1 - zy) // dy).astype(int)
ok = (ci >= 0) & (ci < NC) & (ri >= 0) & (ri < nr) & (z > 0)
sums = np.zeros((nr, NC))
cnt = np.zeros((nr, NC))
np.add.at(sums, (ri[ok], ci[ok]), z[ok])
np.add.at(cnt, (ri[ok], ci[ok]), 1)
cx = x0 + (np.arange(NC) + 0.5) * dx
cy = y1 - (np.arange(nr) + 0.5) * dy
gx, gy = np.meshgrid(cx, cy)
inside = shapely.contains_xy(poly, gx, gy) & (cnt > 0)
elev_ft = np.where(inside, np.round(sums / np.maximum(cnt, 1) * 3.28084), -9999).astype(int)
flags = np.zeros((nr, NC), int)
for geom in flow.geometry:
    for line in getattr(geom, "geoms", [geom]):
        pts = line.interpolate(np.arange(0, line.length, dx / 3))
        for p in pts:
            c, r = int((p.x - x0) // dx), int((y1 - p.y) // dy)
            if 0 <= c < NC and 0 <= r < nr and inside[r, c]:
                flags[r, c] = 1
sx, sy = gpd.GeoSeries.from_xy([site[0]], [site[1]], crs=4326).to_crs(CRS).iloc[0].coords[0]
sc, sr = min(NC - 1, max(0, int((sx - x0) // dx))), min(nr - 1, max(0, int((y1 - sy) // dy)))
flags[sr, sc] = 2
if not inside[sr, sc]:
    elev_ft[sr, sc] = elev_ft[inside].min()
work = DATA / "run"
work.mkdir(exist_ok=True)
with open(work / "GRID.DAT", "w") as f:
    f.write(f"{NC} {nr} {dx:.1f} {dy:.1f}\n")
    for r in elev_ft:
        f.write(" ".join(map(str, r)) + "\n")
    for r in flags:
        f.write(" ".join(map(str, r)) + "\n")
print(f"= {inside.sum()} cells inside; {(flags == 1).sum()} stream cells; gage at column {sc + 1}, row {sr + 1}")

# 2. FORTRAN
fc = shutil.which("gfortran")
if not fc:
    raise SystemExit("gfortran not found: sudo apt-get install gfortran")
subprocess.run([fc, "-O2", "-std=legacy", "-o", str(work / "bigcrk"), str(HERE / "BIGCRK.f")], check=True)
subprocess.run([str(work / "bigcrk")], cwd=work, check=True)
printout = (work / "PRINT.TXT").read_text()
(HERE / "out").mkdir(exist_ok=True)
(HERE / "out" / "PRINT.TXT").write_text(printout)            # the raw printout, kept with the map
ver = subprocess.run([fc, "--version"], capture_output=True, text=True).stdout.splitlines()[0]
print(f"= {ver}; printout {len(printout.splitlines())} records")

# 3. the line printer
lines = []                                            # (row, text) after carriage control
row = -1
for rec in printout.splitlines():
    cc, text = (rec[:1] or " "), rec[1:]
    if cc == "+":
        lines.append((row, text))
    else:
        row += 1
        lines.append((row, text))
nrows = row + 1
PW = 14.875                                           # green-bar paper, inches
TOP = 0.75
PH = TOP + nrows / 6 + 0.6
fig, ax = dmc.figure("square", map_box=(0.03, 0.09, 0.94, 0.715))
ax.set_xlim(0, PW)
ax.set_ylim(PH, 0)
ax.set_aspect("equal")
ax.add_patch(Rectangle((0, 0), PW, PH, color="#fbfaf3", zorder=0))
band = 0.5                                            # half-inch bands: three lines of type
y = TOP - 0.05
while y < PH:
    ax.add_patch(Rectangle((0.5, y), PW - 1.0, band, color="#dfeedd", lw=0, zorder=1))
    y += 2 * band
for side in (0.25, PW - 0.25):
    yy = 0.25
    while yy < PH:
        ax.add_patch(Circle((side, yy), 0.078, color=dmc.PARCHMENT, ec="#cfc9bb", lw=0.4, zorder=2))
        yy += 0.5
    ax.plot([side + (0.22 if side < 1 else -0.22)] * 2, [0, PH], color="#d9d3c5", lw=0.5, ls=(0, (1, 2)), zorder=2)
fig.canvas.draw()
inch = ax.transData.transform([(1, 0)])[0][0] - ax.transData.transform([(0, 0)])[0][0]  # display px per paper inch
size = 0.1 * inch / 0.6 * 72 / fig.dpi                # 10 characters to the inch, DM Mono's advance is 0.6 em
x_left = (PW - 13.2) / 2
for r, text in lines:
    ax.text(x_left, TOP + r / 6, text, family=dmc.MONO, size=size, color="#1d1f24", alpha=0.78, va="top", ha="left",
            zorder=3)
ax.set_axis_off()

dmc.frame(
    fig, DAY,
    subtitle=("Big Creek's watershed above Groveland, printed the 1960s way: FORTRAN on a line printer,\n"
              "overprinting characters for the dark tones. Every line of code came from a prompt."),
    source="USGS NLDI (gage 11284400) · Copernicus DEM GLO-30 · GNU Fortran " + ver.split()[-1],
    note="The prompts and the FORTRAN are posted with the map. Streams print as gaps; G is the gage.",
)
dmc.save(fig, DAY, alt=(
    "A green-bar line-printer printout of Big Creek's watershed near Groveland, California: the basin outlined in asterisks "
    "and shaded in eight tones of typed characters, darker where higher, with streams left as blank gaps, a G at the gage, "
    "and a typed legend of elevation classes, cells and percent of area below."))
