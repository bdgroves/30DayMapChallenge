"""
Day 24 · Network — Every stream that reaches Modesto

Every NHDPlus flowline upstream of the USGS gage on the Tuolumne at Modesto (11290000), from the
USGS Network-Linked Data Index, drawn by Strahler stream order. The order isn't in the NLDI
response, so it's worked out here from the network itself: NHDPlus lines run downstream, so one
line's last point is the next one's first, and headwater streams are order 1; where two streams
of the same order meet, the order goes up by one.

Downloads (cached in data/): NLDI upstream flowlines, basin and gages; Copernicus 90 m DEM.
"""
import json
import sys
from collections import defaultdict, deque
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402
import terrain  # noqa: E402

import geopandas as gpd  # noqa: E402
from matplotlib.collections import LineCollection  # noqa: E402
from pyproj import Transformer  # noqa: E402

DAY = 24
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
CRS = "EPSG:32610"
SITE = "USGS-11290000"
GAGES = {"11274790": "Grand Canyon of the Tuolumne", "11276500": "Hetch Hetchy", "11284400": "Big Creek",
         "11289650": "La Grange", "11290000": "Modesto"}


def nldi(path, name):
    for base in ("https://api.water.usgs.gov/nldi", "https://labs.waterdata.usgs.gov/api/nldi"):
        try:
            return json.loads(fetch.get(f"{base}/linked-data/nwissite/{path}", DATA / name, timeout=600))
        except Exception as e:  # noqa: BLE001
            print(f"  {base}: {str(e)[:100]}")
    raise SystemExit(f"NLDI did not answer for {path}")


fl = nldi(f"{SITE}/navigation/UT/flowlines?distance=2000", "flowlines.geojson")
basin = gpd.GeoDataFrame.from_features(nldi(f"{SITE}/basin", "basin.geojson")["features"], crs=4326).to_crs(CRS)
gages = gpd.GeoDataFrame.from_features(nldi(f"{SITE}/navigation/UT/nwissite?distance=2000", "gages.geojson")["features"],
                                       crs=4326).to_crs(CRS)

# ── Strahler order from the geometry ─────────────────────────────────────────
lines = []
for f in fl["features"]:
    g = f["geometry"]
    parts = g["coordinates"] if g["type"] == "MultiLineString" else [g["coordinates"]]
    coords = [c for p in parts for c in p]
    if len(coords) >= 2:
        lines.append(coords)
key = lambda c: (round(c[0], 5), round(c[1], 5))  # noqa: E731
starts = defaultdict(list)
for i, c in enumerate(lines):
    starts[key(c[0])].append(i)
down = {i: starts.get(key(c[-1]), []) for i, c in enumerate(lines)}
up = defaultdict(list)
for i, ds in down.items():
    for j in ds:
        up[j].append(i)
# Braids and reservoir paths split and rejoin; counted naively, every rejoin would bump the order.
# So keep one path to the outlet per line: walk up from the outlet, breadth first, and give each
# line the downstream neighbour it was first reached from. A braid's other branch becomes a side
# stream of order 1, which never raises the order of the river it joins.
outlets = [i for i in range(len(lines)) if not down[i]]
outlet = max(outlets, key=lambda i: len(up[i])) if outlets else 0
parent = {outlet: None}
q = deque([outlet])
while q:
    j = q.popleft()
    for i in up[j]:
        if i not in parent:
            parent[i] = j
            q.append(i)
kids = defaultdict(list)
for i, j in parent.items():
    if j is not None:
        kids[j].append(i)
order = {}
stack = [(outlet, False)]
while stack:                                       # post-order: children before parents
    i, done = stack.pop()
    if not done:
        stack.append((i, True))
        stack.extend((k, False) for k in kids[i])
        continue
    ups = [order[k] for k in kids[i]]
    if not ups:
        order[i] = 1
    else:
        m = max(ups)
        order[i] = m + 1 if ups.count(m) >= 2 else m
missing = len(lines) - len(order)
for i in range(len(lines)):
    order.setdefault(i, 1)
maxo = max(order.values())
heads = sum(1 for i in range(len(lines)) if not kids[i])
splits = sum(1 for i in range(len(lines)) if len(down[i]) > 1)
to = Transformer.from_crs(4326, CRS, always_xy=True)
xy = [list(zip(*to.transform(*zip(*c)))) for c in lines]
km = sum(sum(((a[0] - b[0]) ** 2 + (a[1] - b[1]) ** 2) ** 0.5 for a, b in zip(s, s[1:])) for s in xy) / 1000
area = basin.area.sum() / 1e6
print(f"= {len(lines):,} flowlines, {km:,.0f} km of stream, {heads:,} headwaters, order up to {maxo}; "
      f"basin {area:,.0f} km²; {missing} lines not connected to the outlet; {splits} splits; "
      f"lines by order {[sum(1 for v in order.values() if v == o) for o in range(1, maxo + 1)]}")

# ── map ──────────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("wide", map_box=(0.03, 0.08, 0.70, 0.72))
x0, y0, x1, y1 = basin.total_bounds
pad = 8000
bb = gpd.GeoSeries.from_xy([x0 - 4 * pad, x1 + 4 * pad], [y0 - 4 * pad, y1 + 4 * pad], crs=CRS).to_crs(4326).total_bounds
z, tf = terrain.dem(tuple(bb), crs=CRS, res=120, src_res=90)
ax.imshow(terrain.relief(z, 120, strength=0.5, exaggerate=1.4), extent=terrain.extent(tf, z.shape),
          interpolation="bilinear", zorder=0)
outside = gpd.GeoSeries([gpd.GeoSeries.from_xy([x0], [y0], crs=CRS).iloc[0].buffer(5e5).difference(
    basin.geometry.union_all())], crs=CRS)
outside.plot(ax=ax, color=dmc.PARCHMENT, alpha=0.65, zorder=1)
basin.boundary.plot(ax=ax, color=dmc.STONE, lw=0.7, zorder=2)
for o in range(1, maxo + 1):
    segs = [s for i, s in enumerate(xy) if order[i] == o]
    if segs:
        ax.add_collection(LineCollection(segs, colors=[dmc.SEQ_WATER(0.35 + 0.65 * (o - 1) / max(1, maxo - 1))],
                                         linewidths=0.25 + 0.55 * (o - 1) ** 1.25, capstyle="round", zorder=3 + o))
ours = gages[gages["identifier"].str.replace("USGS-", "").isin(GAGES)]
for _, g in ours.iterrows():
    sid = g["identifier"].replace("USGS-", "")
    ax.scatter([g.geometry.x], [g.geometry.y], s=26, marker="s", color=dmc.LAVA, edgecolor=dmc.PARCHMENT, lw=0.8, zorder=20)
    gx, gy, gha = {"11289650": (2500, -4500, "left"), "11276500": (0, -5000, "center")}.get(sid, (2500, 2500, "left"))
    dmc.label(ax, g.geometry.x + gx, g.geometry.y + gy, GAGES[sid] + " gage", size=7, color=dmc.INK, ha=gha, zorder=21)
for name, (lon, lat) in {"Hetch Hetchy Reservoir": (-119.75, 37.985), "Don Pedro Reservoir": (-120.36, 37.75), "Cherry Lake": (-119.91, 38.02),
                          "Groveland": (-120.231, 37.838), "Tuolumne Meadows": (-119.36, 37.87)}.items():
    x, y = to.transform(lon, lat)
    dmc.label(ax, x, y - 3500, name, size=7, style="italic", color=dmc.STONE, ha="center", zorder=21)
ax.set_xlim(x0 - pad, x1 + pad)
ax.set_ylim(y0 - pad, y1 + pad)
ax.set_aspect("equal")
dmc.scalebar(ax, 20, loc=(0.04, 0.05))

px = 0.75
fig.text(px, 0.78, f"{km:,.0f} km", family=dmc.TITLE, weight=900, size=30, color=dmc.LAKE, va="top")
fig.text(px, 0.69, f"of streams in {len(lines):,} pieces, from {heads:,}\nheadwaters, gather into one river at\n"
         f"Modesto: {area:,.0f} km² of the Sierra.", size=8.5, va="top", linespacing=1.55)
fig.text(px, 0.52, "STRAHLER STREAM ORDER", family=dmc.MONO, size=6.8, color=dmc.STONE)
for o in range(1, maxo + 1):
    yy = 0.49 - (o - 1) * 0.032
    fig.add_artist(__import__("matplotlib").lines.Line2D(
        [px, px + 0.05], [yy, yy], transform=fig.transFigure, lw=0.25 + 0.55 * (o - 1) ** 1.25,
        color=dmc.SEQ_WATER(0.35 + 0.65 * (o - 1) / max(1, maxo - 1)), solid_capstyle="round"))
    n_o = sum(1 for v in order.values() if v == o)
    fig.text(px + 0.065, yy, f"{o}", family=dmc.MONO, size=7.5, va="center")
    fig.text(0.965, yy, f"{n_o:,} lines", family=dmc.MONO, size=7, va="center", ha="right", color=dmc.STONE)
gy = 0.49 - maxo * 0.032 - 0.02
fig.add_artist(__import__("matplotlib").lines.Line2D([px + 0.006], [gy], marker="s", ms=5, color=dmc.LAVA, lw=0,
                                                      transform=fig.transFigure))
fig.text(px + 0.02, gy, "SIERRA-FLOW gages", family=dmc.MONO, size=7, color=dmc.STONE, va="center")

dmc.frame(
    fig, DAY,
    subtitle=("Every stream above the Tuolumne gage at Modesto, thicker as streams join. The order is worked out\n"
              "from the network itself: two order-1 streams make an order 2, two 2s make a 3, and so on."),
    source="USGS NLDI (NHDPlus V2 flowlines and basin) · Copernicus DEM GLO-90",
    note="Hetch Hetchy, Cherry and Don Pedro reservoirs sit on the river; NHDPlus runs flowlines through them.",
)
dmc.save(fig, DAY, alt=(
    f"Map of the Tuolumne River basin above Modesto, {area:,.0f} square kilometres of the Sierra Nevada, with "
    f"{km:,.0f} kilometres of streams drawn thicker by stream order up to order {maxo}, and the five SIERRA-FLOW gages marked."))
