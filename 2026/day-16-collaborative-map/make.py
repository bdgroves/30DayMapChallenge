"""
Day 16 · Collaborative map — Washington's favorite outdoors, by vote

Made by other people: every outdoor winner of KING 5 Evening's "Best Northwest Escapes" viewer
poll I could find (2015-2026), one dot per place, bigger the more
categories it has won. The polls publish winners only (no runners-up, no vote counts), so this
is who won, not by how much. Lodging, restaurants and businesses (skydiving, ziplines, glamping)
are left out; places outdoors are in.

winners.csv   one row per category won: year, category, place, lon, lat, kind (water, park, coast
              or view), source (king5: read on KING 5's own winners page; list: from Brooks's
              compiled list, for years whose pages couldn't be read). Placed by hand.

Basemap: Mapbox Light; without a token, Census state outlines.
"""
import csv
import sys
from collections import defaultdict
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
KIND = {"water": ("#3f6f8a", "Falls, lakes and rivers"), "park": ("#4e6b3a", "Parks and trails"),
        "coast": ("#c8922a", "Beaches and coast"), "view": ("#c0392b", "Drives and views")}

rows = list(csv.DictReader(open(HERE / "winners.csv", encoding="utf-8")))
years = sorted({int(r["year"]) for r in rows})
wins = defaultdict(list)
where = {}
for r in rows:
    wins[r["place"]].append((int(r["year"]), r["category"].replace("Best ", "")))
    where[r["place"]] = (float(r["lon"]), float(r["lat"]), r["kind"])
places = sorted(wins, key=lambda p: (-len(wins[p]), p))
print(f"= {len(rows)} wins across {years}; {len(places)} places; most: {places[0]} ({len(wins[places[0]])})")

# ── map ──────────────────────────────────────────────────────────────────────
MAPBOX = basemap.available()
CRS = "EPSG:3857" if MAPBOX else "EPSG:32610"
fig, ax = dmc.figure("wide", map_box=(0.03, 0.08, 0.60, 0.72))
g = gpd.GeoDataFrame({"name": places, "n": [len(wins[p]) for p in places], "kind": [where[p][2] for p in places]},
                     geometry=gpd.points_from_xy([where[p][0] for p in places], [where[p][1] for p in places]),
                     crs=4326).to_crs(CRS)
view = gpd.GeoSeries([box(-124.9, 45.55, -119.9, 49.05)], crs=4326).to_crs(CRS).total_bounds
# widen the window east to fill the map box rather than leave paper at the sides
bw, bh = ax.get_position().width * fig.get_figwidth(), ax.get_position().height * fig.get_figheight()
want = (view[3] - view[1]) * bw / bh
if want > view[2] - view[0]:
    view[2] = view[0] + want
ax.set_xlim(view[0], view[2])
ax.set_ylim(view[1], view[3])
ax.set_aspect("equal")
drawn = MAPBOX and basemap.mapbox(ax, style="mapbox/light-v11")
if not drawn:
    st = fetch.shapes(fetch.STATES, DATA / "states.zip")
    st[st["STUSPS"].isin(["WA", "OR", "ID"])].to_crs(CRS).plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.6)
size = 30 * g["n"].to_numpy() ** 1.6 + 14
ax.scatter(g.geometry.x, g.geometry.y, s=size, color=[KIND[k][0] for k in g["kind"]], alpha=0.9,
           edgecolor=dmc.WHITE, lw=0.9, zorder=4)
ax.set_xlim(view[0], view[2])
ax.set_ylim(view[1], view[3])
ax.apply_aspect()
dmc.place_labels(ax, [(r.geometry.x, r.geometry.y, r["name"] + (f" · {r['n']}" if r["n"] > 1 else ""))
                      for _, r in g.iterrows()], gap=6, size=6.8, zorder=5)
dmc.scalebar(ax, 50, loc=(0.04, 0.05))

# the panel: what the colors mean, then the places that won more than once, then the rest
import textwrap  # noqa: E402
from matplotlib.lines import Line2D  # noqa: E402

px = 0.655
for i, (k, (c, lab)) in enumerate(KIND.items()):
    x, yk = px + (i % 2) * 0.15, 0.775 - (i // 2) * 0.03
    fig.add_artist(Line2D([x + 0.004], [yk], marker="o", ms=5, color=c, mec=dmc.WHITE, transform=fig.transFigure))
    fig.text(x + 0.014, yk, lab, size=6.8, va="center", color=dmc.INK)
y = 0.69
fig.text(px, y, "MOST WINS", family=dmc.MONO, size=7.2, color=dmc.INK)
y -= 0.04
for p in [q for q in places if len(wins[q]) >= 3]:
    n, c = len(wins[p]), KIND[where[p][2]][0]
    fig.text(px, y, p, size=7.6, weight="bold", color=dmc.INK, va="center")
    fig.text(0.965, y, f"{n}", family=dmc.MONO, size=7.4, color=c, ha="right", va="center")
    detail = textwrap.fill(" · ".join(f"{cat} '{yr % 100:02d}" for yr, cat in sorted(wins[p])), 96)
    fig.text(px, y - 0.014, detail, size=5.5, style="italic", color=dmc.STONE, va="top", linespacing=1.3)
    y -= 0.03 + 0.016 * (detail.count("\n") + 1)
for head, group in (("TWICE", [q for q in places if len(wins[q]) == 2]), ("ONCE", [q for q in places if len(wins[q]) == 1])):
    if not group:
        continue
    y -= 0.008
    fig.text(px, y, head, family=dmc.MONO, size=7.2, color=dmc.INK)
    txt = textwrap.fill(", ".join(sorted(group)), 70)
    fig.text(px, y - 0.018, txt, size=6.3, color=dmc.STONE, va="top", linespacing=1.4)
    y -= 0.03 + 0.021 * (txt.count("\n") + 1)

lead = places[0]
listed = sorted({int(r["year"]) for r in rows if r.get("source") == "list"})
lead_cats = sorted({c.split(" (")[0].lower() for _, c in wins[lead]})
dmc.frame(
    fig, DAY,
    title="Washington's favorite outdoors, by vote",
    subtitle=(f"Every outdoor winner of KING 5 Evening's Best Northwest Escapes viewer poll I could find, {years[0]} to {years[-1]}, "
              f"bigger the more it won.\n{len(places)} places, {len(rows)} wins. {lead} has won {len(wins[lead])} times: "
              + ", ".join(lead_cats[:-1]) + f" and {lead_cats[-1]}."),
    source="KING 5 Evening, Best Northwest Escapes winners · places by hand" + (" · " + basemap.CREDIT if drawn else ""),
    note=("Made by the people who voted. Winners only, so size counts wins, not votes; lodging and businesses left out. "
          + (", ".join(map(str, listed)) + " from a compiled list; other years from KING 5's own pages." if listed else "")),
)
dmc.save(fig, DAY, alt=(
    f"Map of Washington with colored dots at {len(places)} outdoor places that won KING 5 Evening's Best Northwest Escapes "
    f"viewer poll from {years[0]} to {years[-1]}, bigger for more wins. {lead} has won most, {len(wins[lead])} times; "
    + ", ".join(f"{p} {len(wins[p])}" for p in places[1:6]) + ". A list beside the map ranks them."))
