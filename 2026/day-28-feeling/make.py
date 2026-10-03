"""
Day 28 · Feeling — Did you feel it? Nisqually, 2001

The M6.8 Nisqually earthquake of February 28, 2001, as the people who felt it reported it
to the USGS "Did You Feel It?" survey, averaged into 10 km squares.

Downloads (cached in data/): the USGS event and its DYFI product, Census state outlines,
Natural Earth for Canada. Writes out/map.png and out/alt.txt.
"""
import io
import json
import sys
import urllib.request
import zipfile
from datetime import datetime, timezone
from pathlib import Path
from zoneinfo import ZoneInfo

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402

import geopandas as gpd  # noqa: E402
import matplotlib.pyplot as plt  # noqa: E402
import numpy as np  # noqa: E402
from matplotlib.colors import BoundaryNorm, ListedColormap  # noqa: E402
from shapely.geometry import Point, box  # noqa: E402

DAY = 28
EVENT = "uw10530748"
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
UA = {"User-Agent": "30DayMapChallenge-2026 (github.com/bdgroves/30DayMapChallenge)"}
CRS = "+proj=lcc +lat_1=45 +lat_2=49 +lat_0=47 +lon_0=-121.5 +datum=WGS84 +units=m"
EXTENT = (-125.2, 44.6, -116.4, 49.6)                     # lon/lat box shown


def fetch(url: str, name: str) -> bytes:
    p = DATA / name
    if not p.exists():
        print(f"  downloading {name}")
        with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=120) as r:
            p.write_bytes(r.read())
    return p.read_bytes()


def shapes(url: str, name: str) -> gpd.GeoDataFrame:
    z = zipfile.ZipFile(io.BytesIO(fetch(url, name)))
    shp = [n for n in z.namelist() if n.endswith(".shp")][0]
    folder = DATA / name.replace(".zip", "")
    if not folder.exists():
        z.extractall(folder)
    return gpd.read_file(folder / shp)


# ── the earthquake and its DYFI product ──────────────────────────────────────
ev = json.loads(fetch(f"https://earthquake.usgs.gov/fdsnws/event/1/query?eventid={EVENT}&format=geojson",
                      "event.json"))
props = ev["properties"]
lon, lat, depth = ev["geometry"]["coordinates"]
when = datetime.fromtimestamp(props["time"] / 1000, timezone.utc).astimezone(ZoneInfo("America/Los_Angeles"))
dyfi = props["products"]["dyfi"][0]
contents = dyfi["contents"]
geo = next(k for k in ("dyfi_geo_10km.geojson", "dyfi_geo.geojson", "dyfi_geo_1km.geojson") if k in contents)
cells = gpd.read_file(io.BytesIO(fetch(contents[geo]["url"], geo)))
responses = int(dyfi["properties"].get("numResp") or cells["nresp"].sum())
cells = cells[cells["nresp"] > 0].to_crs(CRS)
print(f"  {props['title']}: {len(cells)} cells, {responses} responses ({geo})")

# ── base map ─────────────────────────────────────────────────────────────────
states = shapes("https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_state_500k.zip", "states.zip")
states = states[states["STUSPS"].isin(["WA", "OR", "ID", "MT", "NV", "CA"])].to_crs(CRS)
world = shapes("https://naciscdn.org/naturalearth/10m/cultural/ne_10m_admin_0_countries.zip", "countries.zip")
canada = world[world["ADMIN"] == "Canada"].to_crs(CRS)
frame_box = gpd.GeoSeries([box(*EXTENT)], crs=4326).to_crs(CRS).total_bounds

# intensity classes, II to VIII+, on the house heat ramp
bounds = [1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 10.5]
names = ["II", "III", "IV", "V", "VI", "VII", "VIII+"]
words = ["weak", "weak", "light", "moderate", "strong", "very strong", "severe"]
colors = [dmc.SEQ_HEAT(x) for x in np.linspace(0.12, 1.0, len(names))]
cmap, norm = ListedColormap(colors), BoundaryNorm(bounds, len(names))

fig, ax = dmc.figure("square", map_box=(0.03, 0.10, 0.94, 0.72))
canada.plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5)
states.plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5)
cells.plot(ax=ax, column="cdi", cmap=cmap, norm=norm, edgecolor="none", alpha=0.95)
states.boundary.plot(ax=ax, color=dmc.WHITE, lw=0.6)
ax.set_xlim(frame_box[0], frame_box[2])
ax.set_ylim(frame_box[1], frame_box[3])

epi = gpd.GeoSeries([Point(lon, lat)], crs=4326).to_crs(CRS).iloc[0]
ax.scatter([epi.x], [epi.y], marker="*", s=260, color=dmc.INK, edgecolor=dmc.PARCHMENT, lw=1.2, zorder=5)
dmc.label(ax, epi.x + 9000, epi.y - 16000, f"Epicenter, {depth:.0f} km down", size=7.5, style="italic")

cities = {"Seattle": (-122.332, 47.606), "Tacoma": (-122.444, 47.253), "Olympia": (-122.900, 47.038),
          "Portland": (-122.679, 45.515), "Vancouver": (-123.121, 49.283), "Spokane": (-117.426, 47.658),
          "Yakima": (-120.506, 46.602), "Bellingham": (-122.488, 48.750), "Bend": (-121.315, 44.058),
          "Wenatchee": (-120.311, 47.423), "Lakewood": (-122.518, 47.172)}
for name, (x, y) in cities.items():
    p = gpd.GeoSeries([Point(x, y)], crs=4326).to_crs(CRS).iloc[0]
    if frame_box[0] < p.x < frame_box[2] and frame_box[1] < p.y < frame_box[3]:
        ax.scatter([p.x], [p.y], s=7, color=dmc.INK, zorder=4)
        dx, ha = (-7000, "right") if name in ("Lakewood", "Olympia", "Vancouver") else (7000, "left")
        dmc.label(ax, p.x + dx, p.y, name, size=7.5, ha=ha, va="center")

# legend: intensity scale
lax = fig.add_axes((0.70, 0.13, 0.25, 0.20))
lax.set_axis_off()
lax.text(0, 1.0, "WHAT PEOPLE FELT", family=dmc.MONO, size=7, color=dmc.STONE, va="top")
for i, (n, w, c) in enumerate(zip(names, words, colors)):
    y = 0.84 - i * 0.12
    lax.add_patch(plt.Rectangle((0, y - 0.045), 0.1, 0.09, color=c, transform=lax.transAxes))
    lax.text(0.14, y, f"{n}", family=dmc.MONO, size=7.5, va="center", transform=lax.transAxes)
    lax.text(0.36, y, w, size=7.5, va="center", transform=lax.transAxes)

dmc.frame(
    fig, DAY,
    subtitle=(f"{when:%B} {when.day}, {when.year}, {when.hour % 12 or 12}:{when:%M} {'a.m.' if when.hour < 12 else 'p.m.'}. Magnitude {props['mag']:.1f}, {depth:.0f} km beneath the\n"
              f"south end of Puget Sound. {responses:,} people told the USGS what they felt; each\n"
              f"10 km square shows the average intensity they reported."),
    source=f"USGS Did You Feel It? (event {EVENT}) · U.S. Census Bureau · Natural Earth",
)
dmc.save(fig, DAY, alt=(
    f"Map of the Pacific Northwest showing how strongly people felt the magnitude {props['mag']:.1f} "
    f"Nisqually earthquake of {when:%B} {when.day}, {when.year}. Small squares coloured from pale "
    f"(weak) to dark red (very strong) cover western Washington, strongest around the epicenter at the "
    f"south end of Puget Sound near Olympia, Lakewood and Tacoma, and fading toward Portland, "
    f"Vancouver and Spokane. Based on {responses:,} 'Did You Feel It?' reports."))
