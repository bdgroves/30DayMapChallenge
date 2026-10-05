"""
Day 19 · Smell — What the Yakima Valley smells like

Every hop field in Yakima County, from the Washington State Department of Agriculture's 2024
crop map, and what Washington's hops smell like: the state's 2025 hop acreage (USDA NASS),
variety by variety, sorted by each variety's leading aroma from HopLove, my hop database.
The leading aroma is the family of the first descriptor HopLove lists for the variety (Citra's
is passionfruit, so tropical). Fields don't say which variety grows in them, so the map shows
where the hops are and the panel shows what they smell like.

Basemap: shaded relief of the valley's ridges from the Copernicus DEM (GLO-30) in the house
parchment, with the Yakima River and its tributaries, towns and ridge names from OpenStreetMap.

Downloads (cached in data/): WSDA Agricultural Land Use 2024 for Yakima County (an ArcGIS
layer published from WSDA's data), HopLove's acreage, aroma taxonomy and hop records.
"""
import json
import sys
import urllib.parse
from collections import defaultdict
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402
import terrain  # noqa: E402

import numpy as np  # noqa: E402

import geopandas as gpd  # noqa: E402
import yaml  # noqa: E402
from shapely.geometry import LineString, Point  # noqa: E402

DAY = 19
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
WSDA = ("https://services.arcgis.com/tsh3MgS57Cs86wmq/arcgis/rest/services/"
        "WSDA_Crop_2024_joinCropDetail_Yakima/FeatureServer/127/query")
HOPLOVE = "https://raw.githubusercontent.com/bdgroves/hoplove/main/data"
YEAR = 2025
FAMILY_COLOUR = {"citrus": "#d9a321", "tropical": "#c0392b", "stone-fruit": "#d9774a", "resinous": "#3e5238",
                 "spice": "#8a5a83", "herbal": "#6f8a6a", "floral": "#c77c9e", "earthy": "#7a5c3e",
                 "berry": "#6e1f17", "vegetal": "#8fa36a", "sweet": "#c8922a", "orchard": "#b5a33a"}

# ── hop fields ───────────────────────────────────────────────────────────────
fp = DATA / "wsda_hops_yakima_2024.geojson"
if not fp.exists():
    feats, off = [], 0
    while True:
        js = fetch.json_get(WSDA + "?" + urllib.parse.urlencode({
            "where": "CropType LIKE 'Hop%'", "outFields": "CropType,ExactAcres,Irrigation", "outSR": 4326,
            "f": "geojson", "resultOffset": off, "resultRecordCount": 2000}))
        feats += js.get("features", [])
        if not js.get("properties", {}).get("exceededTransferLimit") and len(js.get("features", [])) < 2000:
            break
        off += 2000
    fp.write_text(json.dumps({"type": "FeatureCollection", "features": feats}))
fields = gpd.read_file(fp)
field_acres = fields["ExactAcres"].sum()
print(f"= {len(fields):,} hop fields in Yakima County (WSDA 2024), {field_acres:,.0f} acres; types {fields['CropType'].unique()}")

# ── acreage by leading aroma ─────────────────────────────────────────────────
ac = yaml.safe_load(fetch.get(f"{HOPLOVE}/acreage/usda-nass.yml", DATA / "usda-nass.yml"))
tax = yaml.safe_load(fetch.get(f"{HOPLOVE}/taxonomy/aroma-tags.yml", DATA / "aroma-tags.yml"))
family = {k: (v.get("parent") or k) for k, v in tax.items()}
state_total = ac["states"]["WA"]["acres"][YEAR]
by_fam, names, unclassed = defaultdict(float), defaultdict(list), 0.0
for v in ac["varieties"]:
    a = v.get("acres", {}).get("WA", {}).get(YEAR)
    if not isinstance(a, (int, float)) or a <= 0:
        continue
    lead = None
    for s in v.get("slugs") or []:
        try:
            hop = yaml.safe_load(fetch.get(f"{HOPLOVE}/hops/{s}.yml", DATA / "hops" / f"{s}.yml"))
        except Exception:  # noqa: BLE001
            continue
        tags = (hop.get("aroma") or {}).get("tags") or []
        if tags and tags[0] in family:
            lead = family[tags[0]]
            break
    if lead:
        by_fam[lead] += a
        names[lead].append((a, v["name"]))
    else:
        unclassed += a
fams = sorted(by_fam, key=by_fam.get, reverse=True)
classed = sum(by_fam.values())
print(f"= WA {YEAR}: {state_total:,} acres; by leading aroma "
      + ", ".join(f"{f} {by_fam[f]:,.0f}" for f in fams) + f"; unclassified or withheld {state_total - classed:,.0f}")

# ── map ──────────────────────────────────────────────────────────────────────
CRS = "EPSG:32610"
OVERPASS = "https://overpass-api.de/api/interpreter"
fig, ax = dmc.figure("wide", map_box=(0.03, 0.08, 0.58, 0.72))
f = fields.to_crs(CRS)
x0, y0, x1, y1 = f.total_bounds
pad = (x1 - x0) * 0.06
x0, y0, x1, y1 = x0 - pad, y0 - pad, x1 + pad, y1 + pad
# the map box is wider than tall: widen the window to fill it rather than leave paper at the sides
bw, bh = ax.get_position().width * fig.get_figwidth(), ax.get_position().height * fig.get_figheight()
want = (y1 - y0) * bw / bh
if want > x1 - x0:
    cx = (x0 + x1) / 2
    x0, x1 = cx - want / 2, cx + want / 2
ax.set_xlim(x0, x1)
ax.set_ylim(y0, y1)
ax.set_aspect("equal")
ax.set_axis_off()
ll = gpd.GeoSeries([Point(x0, y0), Point(x1, y1)], crs=CRS).to_crs(4326)
W, S, E, N = ll.x.min() - 0.02, ll.y.min() - 0.02, ll.x.max() + 0.02, ll.y.max() + 0.02

# shaded relief: the ridges that wall in the valley (Ahtanum, Rattlesnake Hills, Toppenish Ridge...)
z, tf = terrain.dem((W, S, E, N), crs=CRS, res=40)
rgb = terrain.relief(z, res=40, strength=0.9, exaggerate=2.2)
# feather the edges into the page so the map has no hard box
h, w = z.shape
yy, xx = np.mgrid[0:h, 0:w]
edge = np.minimum.reduce([xx / w, (w - xx) / w, yy / h, (h - yy) / h])
alpha = np.clip(edge / 0.06, 0, 1) ** 1.5
ax.imshow(np.dstack([rgb, alpha]), extent=terrain.extent(tf, z.shape), zorder=0, interpolation="bilinear")


def overpass(q, name):
    url = OVERPASS + "?" + urllib.parse.urlencode({"data": f"[out:json][timeout:180];{q}out geom;"})
    return json.loads(fetch.get(url, DATA / name, timeout=300))


bb = f"{S},{W},{N},{E}"
osm = overpass(f'(way["waterway"="river"]({bb});way["waterway"="canal"]["name"]({bb});'
               f'node["place"~"^(city|town)$"]({bb});way["natural"="ridge"]["name"]({bb});'
               f'node["natural"="peak"]["name"~"Ridge|Hills"]({bb}););', "osm_valley.json")
rivers, canals, towns, ridges = [], [], [], []
for el in osm.get("elements", []):
    t = el.get("tags", {})
    if el["type"] == "way" and "geometry" in el:
        line = LineString([(p["lon"], p["lat"]) for p in el["geometry"]])
        if t.get("waterway") == "river":
            rivers.append((t.get("name", ""), line))
        elif t.get("waterway") == "canal":
            canals.append((t.get("name", ""), line))
        elif t.get("natural") == "ridge":
            ridges.append((t["name"], line))
    elif el["type"] == "node" and t.get("place") in ("city", "town"):
        towns.append((t.get("name", ""), t.get("place"), Point(el["lon"], el["lat"])))
print(f"= OSM: {len(rivers)} river ways, {len(canals)} named canals, {len(towns)} towns, {len(ridges)} named ridges")
WATER = "#5b8fa8"
if canals:
    gpd.GeoSeries([g for _, g in canals], crs=4326).to_crs(CRS).plot(ax=ax, color=WATER, lw=0.45, alpha=0.55, zorder=1)
if rivers:
    big = [g for n, g in rivers if n in ("Yakima River", "Naches River")]
    small = [g for n, g in rivers if n not in ("Yakima River", "Naches River")]
    if small:
        gpd.GeoSeries(small, crs=4326).to_crs(CRS).plot(ax=ax, color=WATER, lw=0.6, alpha=0.75, zorder=1)
    if big:
        gpd.GeoSeries(big, crs=4326).to_crs(CRS).plot(ax=ax, color=WATER, lw=1.5, zorder=1)

# the hops, in hop green, on top
f.plot(ax=ax, color="#5f7d2c", edgecolor="#2f4220", lw=0.2, zorder=3)

# ridge names along the high ground, towns as small dots
for name, line in ridges:
    g = gpd.GeoSeries([line], crs=4326).to_crs(CRS).iloc[0]
    mid = g.interpolate(0.5, normalized=True)
    if x0 < mid.x < x1 and y0 < mid.y < y1 and g.length > 4000:
        a, b2 = g.interpolate(0.4, normalized=True), g.interpolate(0.6, normalized=True)
        ang = np.degrees(np.arctan2(b2.y - a.y, b2.x - a.x))
        ang = ang - 180 if ang > 90 else ang + 180 if ang < -90 else ang
        dmc.label(ax, mid.x, mid.y, name, size=6.5, color=dmc.STONE, style="italic", rotation=ang,
                  ha="center", va="center", rotation_mode="anchor", zorder=4)
items = []
for name, kind, pt in sorted(towns, key=lambda t: t[1] != "city"):
    q = gpd.GeoSeries([pt], crs=4326).to_crs(CRS).iloc[0]
    if x0 < q.x < x1 and y0 < q.y < y1:
        ax.plot(q.x, q.y, "o", ms=2.6, color=dmc.INK, mec=dmc.PARCHMENT, mew=0.6, zorder=5)
        items.append((q.x, q.y, name))
dmc.place_labels(ax, items, size=7, zorder=6)
dmc.scalebar(ax, 10, loc=(0.04, 0.08))

# aroma panel
px, top = 0.65, 0.76
fig.text(px, top + 0.02, f"WASHINGTON'S HOPS, {YEAR}, BY THEIR FIRST SMELL", family=dmc.MONO, size=7.5, color=dmc.INK)
row = 0.052
mx = max(by_fam.values())
for i, fam in enumerate(fams):
    y = top - 0.02 - i * row
    w = 0.20 * by_fam[fam] / mx
    fig.add_artist(__import__("matplotlib").patches.Rectangle((px + 0.085, y - 0.008), w, 0.018, color=FAMILY_COLOUR.get(fam, dmc.ASH),
                                                               transform=fig.transFigure))
    fig.text(px + 0.08, y, tax[fam]["label"], size=8, ha="right", va="center", color=dmc.INK)
    fig.text(px + 0.09 + w, y, f"{by_fam[fam]:,.0f} ac", family=dmc.MONO, size=6.5, va="center", color=dmc.STONE)
    tops = ", ".join(n for _, n in sorted(names[fam], reverse=True)[:3])
    fig.text(px + 0.085, y - 0.019, tops, size=5.8, style="italic", va="center", color=dmc.STONE)
fig.text(px, top - 0.02 - len(fams) * row - 0.005,
         f"Plus {state_total - classed:,.0f} acres of varieties NASS withholds, lumps as 'other',\nor HopLove doesn't describe yet.",
         size=6.5, color=dmc.STONE, va="top", linespacing=1.4)

lead = fams[0]
dmc.frame(
    fig, DAY,
    subtitle=(f"Every hop field in Yakima County ({field_acres:,.0f} acres in 2024), and what Washington's hops smell like: "
              f"by acreage, mostly\n{tax[lead]['label'].lower()}, led by {sorted(names[lead], reverse=True)[0][1]}. "
              f"The aroma of each variety comes from HopLove, my hop database."),
    source="WSDA Agricultural Land Use 2024 · USDA NASS National Hop Report 2025 · HopLove · Copernicus DEM · © OpenStreetMap",
    note="Fields don't record their variety, so the map shows where the hops grow and the panel what they smell like.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Yakima County's {len(fields):,} hop fields in green over shaded relief of the valley and its ridges, with the Yakima River, beside bars of Washington's "
    f"{YEAR} hop acreage by each variety's leading aroma: " + ", ".join(f"{tax[f]['label'].lower()} {by_fam[f]:,.0f} acres" for f in fams[:4]) + "."))
