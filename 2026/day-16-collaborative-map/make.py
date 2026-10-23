"""
Day 16 · Collaborative map — Washington's favourite outdoors, by vote

Made by other people: every outdoor winner of KING 5 Evening's "Best Northwest Escapes" viewer
poll that I could read (2017, 2018, 2022, 2023, 2024), one dot per place, bigger the more
categories it has won. The polls publish winners only (no runners-up, no vote counts), so this
is who won, not by how much. Lodging, restaurants and businesses (skydiving, ziplines, glamping)
are left out; places outdoors are in.

winners.csv   one row per category won: year, category, place, lon, lat, kind
              (water, park, coast or view). Placed by hand. Add a year's list and re-render.

Basemap: Mapbox Outdoors; without a token, Census state outlines.
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
ax.set_xlim(view[0], view[2])
ax.set_ylim(view[1], view[3])
ax.set_aspect("equal")
drawn = MAPBOX and basemap.mapbox(ax, style="mapbox/outdoors-v12", alpha=0.85)
if not drawn:
    st = fetch.shapes(fetch.STATES, DATA / "states.zip")
    st[st["STUSPS"].isin(["WA", "OR", "ID"])].to_crs(CRS).plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.6)
size = 26 + 70 * (g["n"].to_numpy() - 1) + 20 * (g["n"].to_numpy() > 1)
ax.scatter(g.geometry.x, g.geometry.y, s=size, color=[KIND[k][0] for k in g["kind"]], alpha=0.9,
           edgecolor=dmc.WHITE, lw=0.9, zorder=4)
ax.set_xlim(view[0], view[2])
ax.set_ylim(view[1], view[3])
ax.apply_aspect()
dmc.place_labels(ax, [(r.geometry.x, r.geometry.y, r["name"] + (f" · {r['n']}" if r["n"] > 1 else ""))
                      for _, r in g.iterrows()], gap=6, size=6.8, zorder=5)
dmc.scalebar(ax, 50, loc=(0.04, 0.05))

# the panel: what the colours mean, then the places that won more than once, then the rest
import textwrap  # noqa: E402
from matplotlib.lines import Line2D  # noqa: E402

px = 0.655
for i, (k, (c, lab)) in enumerate(KIND.items()):
    x, yk = px + (i % 2) * 0.15, 0.775 - (i // 2) * 0.03
    fig.add_artist(Line2D([x + 0.004], [yk], marker="o", ms=5, color=c, mec=dmc.WHITE, transform=fig.transFigure))
    fig.text(x + 0.014, yk, lab, size=6.8, va="center", color=dmc.INK)
y = 0.69
fig.text(px, y, "WON MORE THAN ONCE", family=dmc.MONO, size=7.2, color=dmc.INK)
y -= 0.04
for p in [q for q in places if len(wins[q]) > 1]:
    n, c = len(wins[p]), KIND[where[p][2]][0]
    fig.text(px, y, p, size=7.4, weight="bold" if n > 2 else "normal", color=dmc.INK, va="center")
    fig.text(0.965, y, f"{n}", family=dmc.MONO, size=7.2, color=c, ha="right", va="center")
    y -= 0.021
    fig.text(px, y, " · ".join(f"{cat} '{yr % 100:02d}" for yr, cat in sorted(wins[p])), size=5.6, style="italic",
             color=dmc.STONE, va="center")
    y -= 0.033
once = [q for q in places if len(wins[q]) == 1]
if once:
    y -= 0.005
    fig.text(px, y, "ONCE EACH", family=dmc.MONO, size=7.2, color=dmc.INK)
    txt = ", ".join(f"{q} ('{wins[q][0][0] % 100:02d})" for q in sorted(once))
    fig.text(px, y - 0.018, textwrap.fill(txt, 62), size=6.2, color=dmc.STONE, va="top", linespacing=1.45)

lead = places[0]
yrs = ", ".join(str(v) for v in years[:-1]) + f" and {years[-1]}"
dmc.frame(
    fig, DAY,
    title="Washington's favourite outdoors, by vote",
    subtitle=(f"Every outdoor winner of KING 5 Evening's Best Northwest Escapes viewer poll from {yrs}, bigger the more it won.\n"
              f"{len(places)} places, {len(rows)} wins. {lead} has won {len(wins[lead])} times: "
              + ", ".join(f"{c.lower()} {y}" for y, c in sorted(wins[lead])) + "."),
    source="KING 5 Evening, Best Northwest Escapes winners lists · places by hand" + (" · " + basemap.CREDIT if drawn else ""),
    note="Made by the people who voted. The polls publish winners only, so size counts wins, not votes. Lodging and businesses left out.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Washington with coloured dots at {len(places)} outdoor places that won KING 5 Evening's Best Northwest Escapes "
    f"viewer poll in {yrs}, bigger for more wins. {lead} has won most, {len(wins[lead])} times; "
    + ", ".join(f"{p} {len(wins[p])}" for p in places[1:6]) + ". A list beside the map ranks them."))
