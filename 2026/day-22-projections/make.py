"""
Day 22 · Projections — The world from Groveland

An azimuthal equidistant projection centred on Groveland, California, where I grew up. On this
map, and only on this map, every straight line from the centre is the shortest route and its
length is the true distance. The edge of the circle is the point on the far side of the Earth, in
the southern Indian Ocean.

Places are in places.csv (name, lat, lon, note, group): every country on the Countries Visited layer
at brooksgroves.com/maps.html (where I found geocaches there, the middle of those finds), plus a few
places closer to home. Places that share a group (Europe, the Caribbean, Africa) get one label.
Downloads (cached in data/): Natural Earth land and countries.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.patches import Circle  # noqa: E402
from pyproj import Geod  # noqa: E402

DAY = 22
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
HOME = (-120.2313, 37.8385)                       # Groveland, California
PROJ = f"+proj=aeqd +lat_0={HOME[1]} +lon_0={HOME[0]} +datum=WGS84 +units=m"
R = 20_015_000                                    # half the Earth's circumference, m
HOME_PLACES = {"Lakewood", "Brenham", "Reno", "Ruby Mountains"}   # rows in places.csv that aren't countries

places = pd.read_csv(HERE / "places.csv").fillna({"group": "", "note": ""})
geod = Geod(ellps="WGS84")
_, _, dist = geod.inv([HOME[0]] * len(places), [HOME[1]] * len(places), places["lon"], places["lat"])
places["km"] = np.array(dist) / 1000
far = places.sort_values("km").iloc[-1]
print("= " + "; ".join(f"{r['name']} {r['km']:,.0f} km" for _, r in places.sort_values("km").iterrows()))

land = fetch.shapes(fetch.LAND.replace("10m", "50m"), DATA / "land.zip")
countries = fetch.shapes(fetch.COUNTRIES.replace("10m", "50m"), DATA / "countries.zip")


def project(gdf):
    """Azimuthal equidistant, clipped just short of the antipode where it blows up. Big polygons
    (Afro-Eurasia is one) are cut away from the antipode first and densified, so they project to
    valid shapes instead of being dropped."""
    from shapely.geometry import Point
    from shapely.validation import make_valid
    anti = Point(HOME[0] + 180 if HOME[0] < 0 else HOME[0] - 180, -HOME[1]).buffer(1.5, 64)
    g = gdf[["geometry"]].copy()
    g["geometry"] = g.geometry.apply(make_valid).difference(anti).segmentize(0.5)
    g = g[~g.geometry.is_empty].to_crs(PROJ)
    g["geometry"] = g.geometry.apply(make_valid)
    disc = Point(0, 0).buffer(R * 0.995, 256)
    g["geometry"] = g.geometry.intersection(disc)
    return g[~g.geometry.is_empty]


landp = project(land)
ctry = project(countries)

fig, ax = dmc.figure("square", map_box=(0.08, 0.075, 0.84, 0.70))
ax.add_patch(Circle((0, 0), R, color="#dde6e8", zorder=0))     # sea
for km in (5000, 10000, 15000):
    ax.add_patch(Circle((0, 0), km * 1000, fill=False, ec=dmc.MIST, lw=0.6, ls=(0, (2, 3)), zorder=1))
    dmc.label(ax, 0, km * 1000 + 250_000, f"{km:,} km", size=6.5, color=dmc.STONE, ha="center", zorder=6)
landp.plot(ax=ax, color=dmc.PARCHMENT, edgecolor=dmc.STONE, lw=0.35, zorder=2)
ctry.boundary.plot(ax=ax, color=dmc.MIST, lw=0.25, zorder=3)
ax.add_patch(Circle((0, 0), R, fill=False, ec=dmc.INK, lw=1.0, zorder=4))
from pyproj import Transformer  # noqa: E402
to = Transformer.from_crs(4326, PROJ, always_xy=True)
places["x"], places["y"] = to.transform(places["lon"].values, places["lat"].values)
for _, p in places.sort_values("km", ascending=False).iterrows():
    ax.plot([0, p["x"]], [0, p["y"]], color=dmc.LAVA, lw=0.7, alpha=0.8, zorder=5)
    ax.scatter([p["x"]], [p["y"]], s=14, color=dmc.LAVA, edgecolor=dmc.PARCHMENT, lw=0.5, zorder=6)


def tag(x, y, text):
    """Label beyond the point; near the rim, tuck it inside the circle instead."""
    ang = np.arctan2(y, x)
    out = np.hypot(x, y) < R * 0.72
    d = 350_000 if out else -450_000
    side = np.cos(ang) >= 0
    dmc.label(ax, x + d * np.cos(ang), y + d * np.sin(ang), text, size=7,
              ha=("left" if side else "right") if out else ("right" if side else "left"), va="center", zorder=7)


for _, p in places[(places["group"] == "") & (places["km"] >= 2500)].iterrows():
    tag(p["x"], p["y"], f"{p['name']}\n{p['km']:,.0f} km")
for g, gp in places[places["group"] != ""].groupby("group"):
    far_one = gp.sort_values("km").iloc[-1]                 # label beyond the group's farthest place
    lo, hi = gp["km"].min(), gp["km"].max()
    tag(far_one["x"], far_one["y"], f"{g} · {len(gp)} countries\n{lo:,.0f}–{hi:,.0f} km")
ax.scatter([0], [0], s=30, color=dmc.INK, zorder=7)
near = places[(places["km"] < 2500) & (places["group"] == "")].sort_values("km")
if len(near):
    fig.text(0.05, 0.20, "CLOSE TO HOME", family=dmc.MONO, size=6.8, color=dmc.STONE)
    for i, (_, p) in enumerate(near.iterrows()):
        fig.text(0.05, 0.175 - i * 0.022, f"{p['name']}", size=8, va="center")
        fig.text(0.235, 0.175 - i * 0.022, f"{p['km']:,.0f} km", family=dmc.MONO, size=7.5, va="center", ha="right", color=dmc.STONE)
dmc.label(ax, 250_000, -350_000, "Groveland", size=8, weight="bold", zorder=7)
ax.set_xlim(-R * 1.03, R * 1.03)
ax.set_ylim(-R * 1.03, R * 1.03)
ax.set_aspect("equal")

n_countries = int((~places["name"].isin(HOME_PLACES)).sum())
dmc.frame(
    fig, DAY,
    subtitle=(f"I grew up in Groveland, in the Sierra foothills. Since then: {n_countries} countries, each line the shortest route\n"
              f"and true to scale. {far['name']} is the farthest, {far['km']:,.0f} km away; the rim is the far side of the planet."),
    source="Natural Earth · azimuthal equidistant projection centred on Groveland, California, 37.84° N, 120.23° W",
    note="Only distances from the centre are true. Shapes stretch more the farther out they are.",
)
dmc.save(fig, DAY, alt=(
    f"A round world map centred on Groveland, California, where I grew up, in an azimuthal equidistant projection, with red "
    f"lines out to the {n_countries} countries I've been to and a few places closer to home, Lakewood among them. "
    + "; ".join(f"{g}: {len(gp)} countries, {gp['km'].min():,.0f} to {gp['km'].max():,.0f} km" for g, gp in places[places["group"] != ""].groupby("group"))
    + ". " + ", ".join(f"{r['name']} ({r['km']:,.0f} km)" for _, r in places[places["group"] == ""].sort_values("km").iterrows())
    + f". The farthest is {far['name']}."))
