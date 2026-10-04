"""
Day 16 · Collaborative map — Your favourite Sierra trailheads

Made with whoever answers: I asked on X for everyone's favourite Sierra Nevada trailhead, and
every reply goes on the map, one dot per trailhead, bigger the more people named it.

answers.csv   one row per answer: trailhead (as written), why (optional, a few words).
              Names only; who said it is never stored or shown.
places.csv    trailheads placed by hand (trailhead, lon, lat): for anything OpenStreetMap can't
              match by name, or matches wrongly. These win over OSM.

Everything else is placed by matching the name against OpenStreetMap's trailheads in the Sierra
(highway=trailhead, via Overpass). Unmatched names are printed when the map renders, to add to
places.csv.

Basemap: Mapbox Outdoors (trails and peaks); without a token, Census state outlines.
"""
import csv
import difflib
import json
import re
import sys
import urllib.parse
from collections import Counter
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import basemap  # noqa: E402
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
import numpy as np  # noqa: E402
from shapely.geometry import box  # noqa: E402

DAY = 16
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
SIERRA = (35.4, -121.4, 40.6, -117.6)                   # S, W, N, E for Overpass
OVERPASS = "https://overpass-api.de/api/interpreter"


def norm(s):
    s = s.lower().replace("&", "and")
    s = re.sub(r"\b(trail ?head|th|trail|the|parking|lot)\b", " ", s)
    return re.sub(r"[^a-z0-9]+", " ", s).strip()


answers = [r["trailhead"].strip() for r in csv.DictReader(open(HERE / "answers.csv", encoding="utf-8")) if r["trailhead"].strip()]
if not answers:
    print("= no answers yet: add replies to day-16-collaborative-map/answers.csv and render again")
    sys.exit(0)

manual = {norm(r["trailhead"]): (float(r["lon"]), float(r["lat"]), r["trailhead"])
          for r in csv.DictReader(open(HERE / "places.csv", encoding="utf-8")) if r.get("lon")}
q = f'[out:json][timeout:180];nwr["highway"="trailhead"]["name"]({",".join(map(str, SIERRA))});out center;'
osm = json.loads(fetch.get(OVERPASS + "?" + urllib.parse.urlencode({"data": q}), DATA / "trailheads.json", timeout=300))
heads = {}
for e in osm["elements"]:
    c = e.get("center") or {"lat": e.get("lat"), "lon": e.get("lon")}
    if c.get("lat"):
        heads.setdefault(norm(e["tags"]["name"]), (c["lon"], c["lat"], e["tags"]["name"]))
print(f"= {len(answers)} answers; {len(heads):,} named trailheads in OSM for the Sierra; {len(manual)} placed by hand")

placed, missing = Counter(), []
where = {}
for a in answers:
    k = norm(a)
    hit = manual.get(k) or heads.get(k)
    if not hit:
        close = difflib.get_close_matches(k, list(heads), n=1, cutoff=0.86)
        hit = heads[close[0]] if close else None
    if hit:
        placed[hit[2]] += 1
        where[hit[2]] = hit[:2]
    else:
        missing.append(a)
print(f"= placed {sum(placed.values())} answers at {len(placed)} trailheads; unplaced: {sorted(set(missing))}")

# ── map ──────────────────────────────────────────────────────────────────────
MAPBOX = basemap.available()
CRS = "EPSG:3857" if MAPBOX else "EPSG:32611"
fig, ax = dmc.figure("portrait", map_box=(0.05, 0.10, 0.90, 0.70))
g = gpd.GeoDataFrame({"name": list(placed), "n": list(placed.values())},
                     geometry=gpd.points_from_xy([where[k][0] for k in placed], [where[k][1] for k in placed]), crs=4326).to_crs(CRS)
view = gpd.GeoSeries([box(-121.2, 35.6, -117.8, 40.4)], crs=4326).to_crs(CRS).total_bounds
ax.set_xlim(view[0], view[2])
ax.set_ylim(view[1], view[3])
ax.set_aspect("equal")
drawn = MAPBOX and basemap.mapbox(ax, style="mapbox/outdoors-v12")
if not drawn:
    st = fetch.shapes(fetch.STATES, DATA / "states.zip")
    st[st["STUSPS"].isin(["CA", "NV"])].to_crs(CRS).plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.6)
size = 30 + 60 * np.sqrt(g["n"].to_numpy())
ax.scatter(g.geometry.x, g.geometry.y, s=size, color=dmc.LAVA, alpha=0.85, edgecolor=dmc.WHITE, lw=0.8, zorder=4)
ax.set_xlim(view[0], view[2])
ax.set_ylim(view[1], view[3])
ax.apply_aspect()
g = g.sort_values("n", ascending=False)
dmc.place_labels(ax, [(r.geometry.x, r.geometry.y, r["name"] + (f" · {r['n']}" if r["n"] > 1 else "")) for _, r in g.iterrows()],
                 gap=7, size=7, zorder=5)
top = g.iloc[0]
dmc.frame(
    fig, DAY,
    title="Your favourite Sierra trailheads",
    subtitle=(f"I asked on X for everyone's favourite trailhead in the Sierra Nevada. {len(answers)} answers, {len(placed)} trailheads;\n"
              f"the most named: {top['name']}" + (f", {top['n']} times." if top["n"] > 1 else ".")),
    source="Your replies on X · trailheads placed with OpenStreetMap" + (" · " + basemap.CREDIT if drawn else ""),
    note="Thanks to everyone who answered. Names only: who said what isn't on the map.",
)
dmc.save(fig, DAY, alt=(
    f"Map of the Sierra Nevada with red dots at {len(placed)} trailheads people named as their favourite, bigger where more "
    f"people named it; {top['name']} is named most."))
