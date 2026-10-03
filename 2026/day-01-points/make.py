"""
Day 1 · Points — every geocache I've found

Put the Geocaching "My Finds" pocket query in data/ (the .zip or the .gpx inside it). Each cache
is a dot coloured by the year I found it; finds outside the main map are counted in the panel.

The find date comes from my own "Found it" log in the GPX (set FINDER below); if a cache has none,
the log date of the first find-type log is used.

Downloads (cached in data/): Natural Earth states/provinces and countries.
"""
import io
import sys
import urllib.request
import xml.etree.ElementTree as ET
import zipfile
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import basemap  # noqa: E402
import dmc  # noqa: E402

import geopandas as gpd  # noqa: E402
import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.colors import Normalize  # noqa: E402

DAY = 1
FINDER = "Hipparchus"
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
UA = {"User-Agent": "30DayMapChallenge-2026 (github.com/bdgroves/30DayMapChallenge)"}
FIND_TYPES = {"Found it", "Attended", "Webcam Photo Taken"}


def gpx_files():
    for p in sorted(DATA.glob("*.zip")):
        if p.name.startswith("ne_"):
            continue
        with zipfile.ZipFile(p) as z:
            for n in z.namelist():
                if n.endswith(".gpx") and "-wpts" not in n:
                    yield n, z.read(n)
    for p in sorted(DATA.glob("*.gpx")):
        if "-wpts" not in p.name:
            yield p.name, p.read_bytes()


def parse(raw: bytes):
    root = ET.fromstring(raw)
    ns = {"g": root.tag.split("}")[0][1:]} if root.tag.startswith("{") else {"g": ""}
    out = []
    for w in root.iter():
        if not w.tag.endswith("}wpt") and w.tag != "wpt":
            continue
        rec = {"lat": float(w.get("lat")), "lon": float(w.get("lon"))}
        for c in w:
            t = c.tag.split("}")[-1]
            if t == "name":
                rec["code"] = c.text
            elif t == "time":
                rec["placed"] = c.text
            elif t == "cache":
                for cc in c:
                    tt = cc.tag.split("}")[-1]
                    if tt == "name":
                        rec["name"] = cc.text
                    elif tt == "type":
                        rec["type"] = cc.text
                    elif tt == "state":
                        rec["state"] = cc.text
                    elif tt == "country":
                        rec["country"] = cc.text
                    elif tt == "logs":
                        mine, first = None, None
                        for log in cc:
                            f = {x.tag.split("}")[-1]: x for x in log}
                            kind = (f.get("type").text or "") if f.get("type") is not None else ""
                            if kind not in FIND_TYPES:
                                continue
                            date = f["date"].text if f.get("date") is not None else None
                            who = f["finder"].text if f.get("finder") is not None else ""
                            first = first or date
                            if who and who.lower() == FINDER.lower():
                                mine = date
                        rec["found"] = mine or first
        if rec.get("code", "").startswith("GC"):
            out.append(rec)
    return out


rows = []
for name, raw in gpx_files():
    got = parse(raw)
    print(f"  {name}: {len(got)} caches")
    rows += got
if not rows:
    sys.exit("No GPX in day-01-points/data/. Run the Geocaching 'My Finds' pocket query and save the .zip there.")
df = pd.DataFrame(rows).drop_duplicates("code")
df["found"] = pd.to_datetime(df["found"].fillna(df.get("placed")), errors="coerce", utc=True).dt.tz_localize(None)
df = df.dropna(subset=["found"])
df["year"] = df["found"].dt.year
g = gpd.GeoDataFrame(df, geometry=gpd.points_from_xy(df["lon"], df["lat"]), crs=4326)
n = len(g)
print(f"  {n} finds, {g['found'].min():%Y-%m-%d} to {g['found'].max():%Y-%m-%d}")


def ne(url, name):
    p = DATA / name
    if not p.exists():
        with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=300) as r:
            p.write_bytes(r.read())
    folder = DATA / name.replace(".zip", "")
    if not folder.exists():
        zipfile.ZipFile(p).extractall(folder)
    return gpd.read_file(next(folder.glob("*.shp")))


states = ne("https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_1_states_provinces.zip", "ne_states.zip")
countries = ne("https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_0_countries.zip", "ne_countries.zip")

# where the finds are: state/province for the panel
st = states[["name", "admin", "geometry"]].rename(columns={"name": "st_name", "admin": "st_admin"})
j = gpd.sjoin(g, st, how="left", predicate="within")
j = j[~j.index.duplicated()]
j["where"] = np.where(j["st_admin"] == "United States of America", j["st_name"], j["st_admin"])
places = Counter(j["where"].fillna("Elsewhere"))
n_places = len([k for k in places if k != "Elsewhere"])

# main map: the densest region (5th-95th percentile of the finds), padded
lo_lon, hi_lon = np.percentile(g["lon"], [5, 95])
lo_lat, hi_lat = np.percentile(g["lat"], [5, 95])
pad_lon, pad_lat = max(1.0, (hi_lon - lo_lon) * 0.15), max(0.8, (hi_lat - lo_lat) * 0.12)
box = (lo_lon - pad_lon, lo_lat - pad_lat, hi_lon + pad_lon, hi_lat + pad_lat)
lon0, lat0 = (box[0] + box[2]) / 2, (box[1] + box[3]) / 2
MAPBOX = basemap.available()                      # Mapbox basemaps are Web Mercator
CRS = "EPSG:3857" if MAPBOX else f"+proj=aea +lat_1={box[1] + 1} +lat_2={box[3] - 1} +lat_0={lat0} +lon_0={lon0} +datum=WGS84 +units=m"
inside = g.cx[box[0]:box[2], box[1]:box[3]]
outside = j.loc[~j.index.isin(inside.index)]

MAP_BOX = (0.05, 0.25, 0.90, 0.555)
fig, ax = dmc.figure("portrait", map_box=MAP_BOX)
gp = g.to_crs(CRS)
bb = gpd.GeoSeries.from_xy([box[0], box[2]], [box[1], box[3]], crs=4326).to_crs(CRS).total_bounds
norm = Normalize(g["year"].min(), g["year"].max())
order = gp.sort_values("found")
ax.scatter(order.geometry.x, order.geometry.y, s=7, c=[dmc.SEQ_HEAT(0.25 + 0.75 * norm(y)) for y in order["year"]],
           edgecolor=dmc.INK, lw=0.15, alpha=0.9, zorder=3)
# grow the shorter side so the map fills its frame
want = (MAP_BOX[2] * 8) / (MAP_BOX[3] * 10)
cx, cy, bw, bh = (bb[0] + bb[2]) / 2, (bb[1] + bb[3]) / 2, bb[2] - bb[0], bb[3] - bb[1]
bw, bh = (bh * want, bh) if bw / bh < want else (bw, bw / want)
ax.set_xlim(cx - bw / 2, cx + bw / 2)
ax.set_ylim(cy - bh / 2, cy + bh / 2)
ax.set_aspect("equal")
if not (MAPBOX and basemap.mapbox(ax)):
    MAPBOX = False
    countries.to_crs(CRS).plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5, zorder=0)
    states[states["admin"].isin(["United States of America", "Canada", "Mexico"])].to_crs(CRS).boundary.plot(
        ax=ax, color=dmc.MIST, lw=0.5, zorder=1)
    ax.set_facecolor("#e4ebe8")

# panel: finds per year, top places, elsewhere
yax = fig.add_axes((0.06, 0.105, 0.42, 0.1))
per = g.groupby("year").size()
yax.bar(per.index, per.values, color=[dmc.SEQ_HEAT(0.25 + 0.75 * norm(y)) for y in per.index], width=0.8)
for sp in ("top", "right", "left"):
    yax.spines[sp].set_visible(False)
yax.spines["bottom"].set_color(dmc.MIST)
yax.set_yticks([])
yax.tick_params(labelsize=6.5, colors=dmc.STONE, length=2)
for t in yax.get_xticklabels():
    t.set_fontfamily(dmc.MONO)
best = per.idxmax()
yax.set_title(f"FINDS PER YEAR · MOST IN {best}: {per.max()}", loc="left", family=dmc.MONO, size=6.8,
              color=dmc.STONE, pad=4)
fig.text(0.56, 0.215, "MOST FINDS", family=dmc.MONO, size=6.8, color=dmc.STONE)
for i, (k, c) in enumerate(places.most_common(6)):
    y = 0.195 - i * 0.017
    fig.text(0.56, y, k, size=8, va="center")
    fig.text(0.95, y, f"{c:,}", family=dmc.MONO, size=7.5, color=dmc.LAVA, ha="right", va="center")
if len(outside):
    far = Counter(outside["where"].fillna("Elsewhere")).most_common(4)
    fig.text(0.56, 0.085, f"PLUS {len(outside)} OFF THIS MAP: " + ", ".join(f"{k.upper()} {v}" for k, v in far),
             family=dmc.MONO, size=6.3, color=dmc.STONE, wrap=True)

types = Counter(g.get("type", pd.Series(dtype=str)).fillna("Other"))
trad = types.get("Traditional Cache", 0) / n if n else 0
dmc.frame(
    fig, DAY, title=f"{n:,} finds",
    subtitle=(f"Every geocache I've found as {FINDER}, from my first in {g['found'].min():%B %Y} to "
              f"{g['found'].max():%B %Y}:\n{n_places} states and countries, coloured by the year I found it."),
    source="Geocaching.com 'My Finds' pocket query · " + (basemap.CREDIT if MAPBOX else "Natural Earth"),
    note=f"{trad:.0%} traditional caches; the rest multis, mysteries, letterboxes, earthcaches and events.",
)
dmc.save(fig, DAY, alt=(
    f"Map of {n:,} geocaches found by {FINDER}, shown as small dots coloured from pale for early years to dark red "
    f"for recent ones. The most finds are in {', '.join(k for k, _ in places.most_common(3))}. "
    f"A bar chart shows finds per year, with the most in {best}."))
