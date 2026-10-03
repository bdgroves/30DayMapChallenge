"""
Day 2 · Lines — Under my own power

Unzip the Strava archive (Settings → My Account → Download your data) into data/, so that
data/activities/ holds the .gpx, .tcx(.gz) and .fit(.gz) files. Every activity near home is drawn
as a faint line; where routes repeat, the lines stack up darker.

Privacy: tracks start and end at home. The spot where most activities start and end is found
automatically and every point within PRIVACY_M of it is removed before anything is drawn, the
same idea as a Strava privacy zone. Nothing in out/ shows where that spot is.
"""
import gzip
import io
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import basemap  # noqa: E402
import dmc  # noqa: E402

import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.collections import LineCollection  # noqa: E402
from pyproj import Transformer  # noqa: E402

DAY = 2
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
PRIVACY_M = 800          # nothing drawn within this distance of home
RADIUS_KM = 25           # map covers this far around the busiest area
TYPES = {"Run": dmc.LAVA, "Ride": dmc.LAKE, "Walk": dmc.SAGE, "Hike": dmc.SAGE, "Swim": dmc.GOLD}


def points_gpx(raw: bytes):
    root = ET.fromstring(raw)
    return [(float(p.get("lat")), float(p.get("lon"))) for p in root.iter() if p.tag.endswith("trkpt")]


def points_tcx(raw: bytes):
    root = ET.fromstring(raw.strip())
    out = []
    for pos in root.iter():
        if pos.tag.endswith("Position"):
            v = {c.tag.split("}")[-1]: c.text for c in pos}
            if "LatitudeDegrees" in v:
                out.append((float(v["LatitudeDegrees"]), float(v["LongitudeDegrees"])))
    return out


def points_fit(raw: bytes):
    import fitdecode
    out = []
    with fitdecode.FitReader(io.BytesIO(raw)) as fr:
        for frame in fr:
            if isinstance(frame, fitdecode.FitDataMessage) and frame.name == "record":
                if frame.has_field("position_lat") and frame.has_field("position_long"):
                    la, lo = frame.get_value("position_lat"), frame.get_value("position_long")
                    if la is not None and lo is not None:
                        out.append((la * 180 / 2**31, lo * 180 / 2**31))
    return out


def load(path: Path):
    raw = path.read_bytes()
    name = path.name.lower()
    if name.endswith(".gz"):
        raw, name = gzip.decompress(raw), name[:-3]
    if name.endswith(".gpx"):
        return points_gpx(raw)
    if name.endswith(".tcx"):
        return points_tcx(raw)
    if name.endswith(".fit"):
        return points_fit(raw)
    return []


acts_dir = next((p for p in [DATA / "activities", *DATA.glob("*/activities")] if p.is_dir()), None)
if acts_dir is None:
    sys.exit("No data/activities/ folder. Unzip the Strava archive into day-02-lines/data/.")
meta_csv = acts_dir.parent / "activities.csv"
meta = pd.read_csv(meta_csv) if meta_csv.exists() else pd.DataFrame()
kind = {}
if len(meta) and "Filename" in meta and "Activity Type" in meta:
    kind = {str(f).split("/")[-1]: t for f, t in zip(meta["Filename"], meta["Activity Type"]) if isinstance(f, str)}

tracks = []
for p in sorted(acts_dir.iterdir()):
    try:
        pts = load(p)
    except Exception as e:  # a broken file shouldn't stop the map
        print(f"  skip {p.name}: {e}")
        continue
    if len(pts) > 10:
        tracks.append((kind.get(p.name, "Other"), np.array(pts)))
if not tracks:
    sys.exit("No tracks with GPS points found.")
print(f"  {len(tracks)} activities with GPS")

# work in a local metric projection around the median start
lat0 = float(np.median([t[1][0, 0] for t in tracks]))
lon0 = float(np.median([t[1][0, 1] for t in tracks]))
tr = Transformer.from_crs(4326, f"+proj=aeqd +lat_0={lat0} +lon_0={lon0} +datum=WGS84 +units=m", always_xy=True)
xy = [(k, np.c_[tr.transform(t[:, 1], t[:, 0])]) for k, t in tracks]

# home = the 200 m cell where the most activities start or end
ends = np.vstack([np.r_[a[0:1], a[-1:]] for _, a in xy])
cells = pd.Series(list(map(tuple, np.round(ends / 200).astype(int)))).value_counts()
home = np.array(cells.index[0]) * 200.0
print(f"  {cells.iloc[0]} starts/ends near the most common spot; trimming {PRIVACY_M} m around it")

# busiest area for the map: densest 5 km cell of all points
allpts = np.vstack([a[::20] for _, a in xy])
dense = pd.Series(list(map(tuple, np.round(allpts / 5000).astype(int)))).value_counts().index[0]
centre = np.array(dense) * 5000.0
R = RADIUS_KM * 1000

segs, cols, dist = [], [], {}
for k, a in xy:
    keep = np.hypot(*(a - home).T) > PRIVACY_M
    near = np.hypot(*(a - centre).T) < R * 1.5
    step = np.hypot(*np.diff(a, axis=0).T)
    dist[k] = dist.get(k, 0) + step[step < 500].sum() / 1000
    m = keep & near
    # split where points were removed so the line doesn't jump the gap
    breaks = np.flatnonzero(np.diff(m.astype(int)) != 0) + 1
    for part_idx, part in zip(np.split(np.arange(len(a)), breaks), np.split(a, breaks)):
        if m[part_idx[0]] and len(part) > 1:
            segs.append(part)
            cols.append(TYPES.get(k, dmc.ASH))
print(f"  {len(segs)} line pieces drawn")

fig, ax = dmc.figure("square", map_box=(0.05, 0.14, 0.90, 0.65))
# drawing coordinates: Web Mercator under a Mapbox basemap, otherwise the local metric grid
MAPBOX = basemap.available()
if MAPBOX:
    back = Transformer.from_crs(tr.target_crs, 3857, always_xy=True)
    P = lambda a: np.c_[back.transform(a[:, 0], a[:, 1])]  # noqa: E731
else:
    P = lambda a: a  # noqa: E731
corners = P(np.array([[centre[0] - R, centre[1] - R * 0.65 / 0.90], [centre[0] + R, centre[1] + R * 0.65 / 0.90]]))
ax.set_xlim(corners[0, 0], corners[1, 0])
ax.set_ylim(corners[0, 1], corners[1, 1])
ax.set_aspect("equal")
if MAPBOX and not basemap.mapbox(ax):
    MAPBOX, P = False, (lambda a: a)
    ax.set_xlim(centre[0] - R, centre[0] + R)
    ax.set_ylim(centre[1] - R * 0.65 / 0.90, centre[1] + R * 0.65 / 0.90)
ax.add_collection(LineCollection([P(sg) for sg in segs], colors=cols, linewidths=0.6, alpha=0.26,
                                 capstyle="round", zorder=2))

to_xy = lambda lon, lat: tr.transform(lon, lat)  # noqa: E731
draw_xy = lambda x, y: tuple(P(np.array([[x, y]]))[0])  # noqa: E731
for name, (lon, lat) in {"Lakewood": (-122.518, 47.172), "Tacoma": (-122.444, 47.253),
                         "University Place": (-122.548, 47.236), "Steilacoom": (-122.603, 47.170),
                         "DuPont": (-122.631, 47.097), "Puyallup": (-122.293, 47.185),
                         "Gig Harbor": (-122.580, 47.329)}.items():
    x, y = to_xy(lon, lat)
    if abs(x - centre[0]) < R * 0.95 and abs(y - centre[1]) < R * 0.75:
        dmc.label(ax, *draw_xy(x, y), name, size=8, ha="center", va="center", style="italic", color=dmc.STONE)
dmc.scalebar(ax, 5, loc=(0.86, 0.04), crs_units_per_km=1000 / np.cos(np.radians(lat0)) if MAPBOX else 1000)

legend = [(k, c) for k, c in TYPES.items() if k in dist and k != "Hike"]
for i, (k, c) in enumerate(legend):
    x = 0.06 + i * 0.16
    fig.add_artist(__import__("matplotlib").lines.Line2D([x, x + 0.03], [0.112, 0.112], color=c, lw=2,
                                                          transform=fig.transFigure))
    fig.text(x + 0.037, 0.112, f"{k} · {dist[k]:,.0f} km", family=dmc.MONO, size=7, va="center")

total = sum(dist.values())
dmc.frame(
    fig, DAY,
    subtitle=(f"Every run, ride and swim in my Strava archive, {len(tracks):,} activities and {total:,.0f} km, drawn as faint\n"
              f"lines. Where I go again and again, they stack up darker."),
    source="My Strava archive" + (" · " + basemap.CREDIT if MAPBOX else ""),
    note=f"Tracks are trimmed {PRIVACY_M} m around home, as a Strava privacy zone would.",
)
dmc.save(fig, DAY, alt=(
    f"A tangle of faint coloured lines around the South Sound, one for each of {len(tracks):,} runs, rides and swims; "
    f"red for runs, blue for rides. Routes repeated many times show as darker lines."))
