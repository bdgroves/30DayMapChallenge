"""
Day 23 · Taste — Every brewery I've checked into

A year of my Untappd beer history, one circle per brewery, sized by how many different beers
of theirs I've had. The history is the copy HopLove keeps (data/untappd/history.yml, from my
public Untappd beer history page). Only breweries are mapped, never where I drank them.

Breweries are placed from Open Brewery DB (no key) by name; any it can't match exactly come
from MANUAL below, and anything still unplaced is listed when the map renders.

Basemap: Mapbox Light, quiet enough for the circles; without a token, Natural Earth outlines.

Downloads (cached in data/): HopLove's history.yml, Open Brewery DB lookups, Natural Earth.
"""
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
import yaml  # noqa: E402
from shapely.geometry import Point, box  # noqa: E402

DAY = 23
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
HISTORY = "https://raw.githubusercontent.com/bdgroves/hoplove/main/data/untappd/history.yml"
OBDB = "https://api.openbrewerydb.org/v1/breweries/search"
VIEW = (-125.2, 41.6, -116.2, 49.6)                    # Cascadia: lon/lat box for the main map
# Breweries Open Brewery DB doesn't hold, or holds under another name: (lon, lat, place)
MANUAL = {
    "Guinness": (-6.2867, 53.3419, "Dublin"),
    "Birrificio Angelo Poretti": (8.8406, 45.8513, "Induno Olona, Italy"),
    "Ichnusa": (9.0035, 39.2905, "Assemini, Sardinia"),
}


def norm(s: str) -> str:
    s = s.lower().replace("&", "and")
    s = re.sub(r"\(.*?\)", "", s)
    s = re.sub(r"\b(brewing|brewery|brewers|brews|beer|company|co|the|family|fermentation|project|and cidery)\b", " ", s)
    return re.sub(r"[^a-z0-9]+", "", s)


hist = yaml.safe_load(fetch.get(HISTORY, DATA / "history.yml"))["beers"]
count = Counter(b["brewery"] for b in hist)
first = min(b["first"] for b in hist)
last = max(b["last"] for b in hist)

cache_p = DATA / "geocode.json"
cache = json.loads(cache_p.read_text()) if cache_p.exists() else {}
for name in count:
    if name in MANUAL or name in cache:
        continue
    hits = fetch.json_get(OBDB + "?" + urllib.parse.urlencode({"query": name, "per_page": 15})) or []
    want = norm(name)
    pick = None
    for h in hits:
        if h.get("latitude") and (norm(h["name"]) == want or (len(want) > 5 and want in norm(h["name"]))):
            pick = h
            break
    cache[name] = None if not pick else {"lon": float(pick["longitude"]), "lat": float(pick["latitude"]),
                                         "place": f"{pick.get('city')}, {pick.get('state_province') or pick.get('country')}",
                                         "matched": pick["name"]}
cache_p.write_text(json.dumps(cache, indent=1, ensure_ascii=False))

rows, missing = [], []
for name, n in count.items():
    if name in MANUAL:
        lo, la, place = MANUAL[name]
    elif cache.get(name):
        lo, la, place = cache[name]["lon"], cache[name]["lat"], cache[name]["place"]
    else:
        missing.append(name)
        continue
    rows.append({"brewery": name, "beers": n, "place": place, "geometry": Point(lo, la)})
g = gpd.GeoDataFrame(rows, crs=4326)
inside = g[g.within(box(*VIEW))]
away = g[~g.within(box(*VIEW))].sort_values("beers", ascending=False)
print(f"= {len(hist)} beers from {len(count)} breweries, {first} to {last}; placed {len(g)} "
      f"({len(inside)} in Cascadia, {len(away)} elsewhere); unplaced {len(missing)}: {missing}")

# ── map ──────────────────────────────────────────────────────────────────────
MAPBOX = basemap.available()
CRS = "EPSG:3857" if MAPBOX else "+proj=lcc +lat_1=43 +lat_2=48 +lat_0=45.5 +lon_0=-120.7 +datum=WGS84 +units=m"
fig, ax = dmc.figure("portrait", map_box=(0.05, 0.20, 0.62, 0.62))
vb = gpd.GeoSeries([box(*VIEW)], crs=4326).to_crs(CRS).total_bounds
ax.set_xlim(vb[0], vb[2])
ax.set_ylim(vb[1], vb[3])
ax.set_aspect("equal")
drawn = MAPBOX and basemap.mapbox(ax, style="mapbox/light-v11")
if not drawn:
    st = fetch.shapes(fetch.STATES, DATA / "states.zip").to_crs(CRS)
    st.plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5)
gi = inside.to_crs(CRS)
size = lambda n: 18 + 34 * np.asarray(n, float)  # noqa: E731
ax.scatter(gi.geometry.x, gi.geometry.y, s=size(gi["beers"]), color=dmc.GOLD, alpha=0.75, edgecolor=dmc.INK, lw=0.5,
           zorder=4)
for _, r in gi[gi["beers"] >= 3].iterrows():
    dmc.label(ax, r.geometry.x + 26000, r.geometry.y, f"{r['brewery']} · {r['beers']}", size=6.8, va="center", zorder=5)
ax.set_xlim(vb[0], vb[2])
ax.set_ylim(vb[1], vb[3])

# the rest of the world, as a list
x0 = 0.71
fig.text(x0, 0.80, "ELSEWHERE", family=dmc.MONO, size=8, color=dmc.INK)
y = 0.775
for _, r in away.iterrows():
    if y < 0.22:
        fig.text(x0, y, f"… and {len(away) - list(away.index).index(_)} more", size=7, color=dmc.STONE, style="italic")
        break
    fig.text(x0, y, f"{r['brewery']}", size=7.2, color=dmc.INK)
    fig.text(x0, y - 0.013, f"{r['place']} · {r['beers']} beer{'s' if r['beers'] > 1 else ''}", family=dmc.MONO, size=5.8,
             color=dmc.STONE)
    y -= 0.034
# size key
kx, ky = 0.07, 0.15
fig.text(kx, ky + 0.03, "DIFFERENT BEERS", family=dmc.MONO, size=7, color=dmc.STONE)
for i, n in enumerate([1, 4, 16]):
    fig.add_artist(__import__("matplotlib").lines.Line2D([kx + 0.03 + i * 0.09], [ky - 0.005], marker="o", ls="",
                   markersize=np.sqrt(size(n)), color=dmc.GOLD, alpha=0.75, mec=dmc.INK, mew=0.5, transform=fig.transFigure))
    fig.text(kx + 0.03 + i * 0.09 + 0.03, ky - 0.005, str(n), family=dmc.MONO, size=7, va="center", color=dmc.INK)

top, topn = count.most_common(1)[0]
dmc.frame(
    fig, DAY,
    subtitle=(f"A year on Untappd: {len(hist)} different beers from {len(count)} breweries, one circle each, sized by how\n"
              f"many of theirs I've had. {top} leads with {topn}. Mostly Cascadia, a few from far away."),
    source="My Untappd beer history (via HopLove) · Open Brewery DB" + (" · " + basemap.CREDIT if drawn else " · U.S. Census Bureau"),
    note="Breweries only, placed at the brewery; where I drank them isn't on the map.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Cascadia with gold circles at {len(inside)} breweries, sized by how many different beers I've had from each "
    f"in a year of Untappd check-ins; {top} is biggest with {topn}. A list beside it names {len(away)} breweries farther away."))
