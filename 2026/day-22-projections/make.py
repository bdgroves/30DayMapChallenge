"""
Day 22 · Projections — The world from Lakewood

An azimuthal equidistant projection centred on Lakewood, Washington. On this map, and only on
this map, every straight line from the centre is the shortest route and its length is the true
distance. The edge of the circle is the point on the far side of the Earth, in the Indian Ocean.

Places are in places.csv (name, lat, lon, note): add to it and re-render.
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
HOME = (-122.5185, 47.1718)                       # Lakewood
PROJ = f"+proj=aeqd +lat_0={HOME[1]} +lon_0={HOME[0]} +datum=WGS84 +units=m"
R = 20_015_000                                    # half the Earth's circumference, m

places = pd.read_csv(HERE / "places.csv")
geod = Geod(ellps="WGS84")
_, _, dist = geod.inv([HOME[0]] * len(places), [HOME[1]] * len(places), places["lon"], places["lat"])
places["km"] = np.array(dist) / 1000
far = places.sort_values("km").iloc[-1]
print("= " + "; ".join(f"{r['name']} {r['km']:,.0f} km" for _, r in places.sort_values("km").iterrows()))

land = fetch.shapes(fetch.LAND.replace("10m", "50m"), DATA / "land.zip")
countries = fetch.shapes(fetch.COUNTRIES.replace("10m", "50m"), DATA / "countries.zip")


def project(gdf):
    # clip just short of the antipode, where azimuthal equidistant blows up
    from shapely.geometry import Point
    g = gdf.to_crs(PROJ)
    disc = Point(0, 0).buffer(R * 0.995, 256)
    g = g[g.geometry.is_valid & ~g.geometry.is_empty]
    g["geometry"] = g.geometry.intersection(disc)
    return g[~g.geometry.is_empty]


landp = project(land)
ctry = project(countries)

fig, ax = dmc.figure("square", map_box=(0.08, 0.075, 0.84, 0.70))
ax.add_patch(Circle((0, 0), R, color=dmc.CREAM, zorder=0))
for km in (5000, 10000, 15000):
    ax.add_patch(Circle((0, 0), km * 1000, fill=False, ec=dmc.MIST, lw=0.6, ls=(0, (2, 3)), zorder=1))
    dmc.label(ax, 0, km * 1000 + 250_000, f"{km:,} km", size=6.5, color=dmc.STONE, ha="center", zorder=6)
landp.plot(ax=ax, color=dmc.PARCHMENT, edgecolor=dmc.STONE, lw=0.35, zorder=2)
ctry.boundary.plot(ax=ax, color=dmc.MIST, lw=0.25, zorder=3)
ax.add_patch(Circle((0, 0), R, fill=False, ec=dmc.INK, lw=1.0, zorder=4))
from pyproj import Transformer  # noqa: E402
to = Transformer.from_crs(4326, PROJ, always_xy=True)
for _, p in places.iterrows():
    x, y = to.transform(p["lon"], p["lat"])
    ax.plot([0, x], [0, y], color=dmc.LAVA, lw=0.9, zorder=5)
    ax.scatter([x], [y], s=18, color=dmc.LAVA, edgecolor=dmc.PARCHMENT, lw=0.6, zorder=6)
    ang = np.arctan2(y, x)
    if p["km"] < 2500:
        continue
    dmc.label(ax, x + 300_000 * np.cos(ang), y + 300_000 * np.sin(ang), f"{p['name']}\n{p['km']:,.0f} km", size=7,
              ha="left" if np.cos(ang) >= 0 else "right", va="center", zorder=7)
ax.scatter([0], [0], s=30, color=dmc.INK, zorder=7)
near = places[places["km"] < 2500].sort_values("km")
if len(near):
    fig.text(0.08, 0.20, "CLOSE TO HOME", family=dmc.MONO, size=6.8, color=dmc.STONE)
    for i, (_, p) in enumerate(near.iterrows()):
        fig.text(0.08, 0.175 - i * 0.022, f"{p['name']}", size=8, va="center")
        fig.text(0.27, 0.175 - i * 0.022, f"{p['km']:,.0f} km", family=dmc.MONO, size=7.5, va="center", ha="right", color=dmc.STONE)
dmc.label(ax, 250_000, -350_000, "Lakewood", size=8, weight="bold", zorder=7)
ax.set_xlim(-R * 1.03, R * 1.03)
ax.set_ylim(-R * 1.03, R * 1.03)
ax.set_aspect("equal")

dmc.frame(
    fig, DAY,
    subtitle=(f"The Earth as seen from Lakewood: every straight line from the centre is the shortest route, true to scale.\n"
              f"{far['name']} is {far['km']:,.0f} km away. The rim is the far side of the planet."),
    source="Natural Earth · azimuthal equidistant projection centred on 47.17° N, 122.52° W",
    note="Only distances from the centre are true. Shapes stretch more the farther out they are.",
)
dmc.save(fig, DAY, alt=(
    "A round world map centred on Lakewood, Washington, in an azimuthal equidistant projection, with red lines out to "
    + ", ".join(f"{r['name']} ({r['km']:,.0f} km)" for _, r in places.iterrows()) + "."))
