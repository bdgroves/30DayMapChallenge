"""
Day 11 · Sound — Where the tsunami sirens reach

Washington's All Hazard Alert Broadcast (AHAB) sirens, each with the one-mile circle it's
designed to be heard in outdoors, over the tsunami hazard zones they're there to empty.

The plan was Pierce County's lahar sirens, but their locations aren't published as open data
(OpenStreetMap holds only a handful), so this is the state's tsunami network instead, which
Washington Emergency Management publishes. The one-mile range is the state's own figure: "AHABs
are designed to only be heard while outdoors and within a 1 mile radius of a siren" (Grays
Harbor County Emergency Management).

Basemap: Mapbox Light; without a token, Census state outline.

Downloads (cached in data/): WA EMD AHAB points, WA DNR tsunami hazard zones (ArcGIS REST).
"""
import json
import sys
import urllib.parse
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import basemap  # noqa: E402
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
from shapely.geometry import box  # noqa: E402

DAY = 11
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
AHAB = "https://services7.arcgis.com/vUVXhXafpruJFs3l/arcgis/rest/services/AHAB_Points/FeatureServer/0/query"
DNR = "https://gis.dnr.wa.gov/site3/rest/services/Geology/Tsunami_Hazard/FeatureServer"
MILE = 1609.344
UTM = "EPSG:32610"
VIEW = (-124.95, 46.15, -121.85, 49.05)


def arcgis(url, name, **q):
    p = DATA / name
    if not p.exists():
        feats, off = [], 0
        while True:
            js = fetch.json_get(url + "?" + urllib.parse.urlencode({"where": "1=1", "outFields": "*", "outSR": 4326, "f": "geojson",
                                                                    "resultOffset": off, **q}))
            got = js.get("features", [])
            feats += got
            if not js.get("properties", {}).get("exceededTransferLimit") or not got:
                break
            off += len(got)
        p.write_text(json.dumps({"type": "FeatureCollection", "features": feats}))
    return gpd.read_file(p)


sirens = arcgis(AHAB, "ahab.geojson")
sirens = sirens[~sirens["Name"].str.contains("TEST", case=False, na=False)]
print(f"= {len(sirens)} AHAB sirens; counties {sirens['County'].value_counts().to_dict()}")

# tsunami hazard zones: find the polygon layers on the DNR service
zones = None
try:
    meta = fetch.json_get(DNR + "?f=json")
    layers = meta.get("layers", [])
    print("  DNR layers: " + "; ".join(f"{lyr['id']} {lyr['name']}" for lyr in layers))
    keep = []
    for lyr in layers:
        lj = fetch.json_get(f"{DNR}/{lyr['id']}?f=json")
        if lj.get("geometryType") == "esriGeometryPolygon" and "inundation" in lyr["name"].lower():
            g = arcgis(f"{DNR}/{lyr['id']}/query", f"tsunami_{lyr['id']}.geojson", maxAllowableOffset=0.0003, geometryPrecision=5)
            print(f"  layer {lyr['id']} {lyr['name']}: {len(g)} polygons")
            keep.append(g[["geometry"]])
    if keep:
        zones = gpd.GeoDataFrame(gpd.pd.concat(keep), crs=4326)
        zones["geometry"] = zones.make_valid()
        # the zones run out over open water; only the land in them is where people need to hear a siren
        st = fetch.shapes(fetch.STATES, DATA / "states.zip")
        wa = st[st["STUSPS"] == "WA"].to_crs(4326).geometry.union_all()
        zones = gpd.GeoDataFrame(geometry=[zones.geometry.union_all().intersection(wa)], crs=4326)
except Exception as e:  # noqa: BLE001
    print(f"  tsunami zones unavailable: {e}")

reach = gpd.GeoSeries(sirens.to_crs(UTM).buffer(MILE, 48).union_all(), crs=UTM)
reach_km2 = reach.area.iloc[0] / 1e6
if zones is not None and len(zones):
    zu = gpd.GeoSeries(zones.to_crs(UTM).geometry.union_all(), crs=UTM)
    zone_km2 = zu.area.iloc[0] / 1e6
    covered = zu.intersection(reach.iloc[0]).area.iloc[0] / 1e6
    print(f"= reach {reach_km2:,.0f} km²; hazard zone on land {zone_km2:,.0f} km², {covered / zone_km2:.0%} of it within a mile of a siren")
else:
    zone_km2 = covered = None
    print(f"= reach {reach_km2:,.0f} km²")

# ── map ──────────────────────────────────────────────────────────────────────
MAPBOX = basemap.available()
CRS = "EPSG:3857" if MAPBOX else UTM
fig, ax = dmc.figure("portrait", map_box=(0.05, 0.09, 0.90, 0.72))
vb = gpd.GeoSeries([box(*VIEW)], crs=4326).to_crs(CRS).total_bounds
ax.set_xlim(vb[0], vb[2])
ax.set_ylim(vb[1], vb[3])
ax.set_aspect("equal")
drawn = MAPBOX and basemap.mapbox(ax, style="mapbox/light-v11")
if not drawn:
    st = fetch.shapes(fetch.STATES, DATA / "states.zip")
    st[st["STUSPS"].isin(["WA", "OR"])].to_crs(CRS).plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5)
if zones is not None and len(zones):
    zones.to_crs(CRS).plot(ax=ax, color=dmc.LAVA, alpha=0.35, lw=0, zorder=2)
rings = sirens.to_crs(UTM).buffer(MILE, 48).to_crs(CRS)
rings.plot(ax=ax, facecolor=dmc.LAKE, alpha=0.45, edgecolor="none", zorder=3)
rings.boundary.plot(ax=ax, color="#1f3a4a", lw=0.6, zorder=4)
s = sirens.to_crs(CRS)
ax.scatter(s.geometry.x, s.geometry.y, s=1.5, color=dmc.INK, zorder=5)
for name, lon, lat, ha in [("Long Beach", -124.054, 46.352, "left"), ("Ocean Shores", -124.156, 46.973, "left"),
                           ("Westport", -124.104, 46.89, "left"), ("La Push", -124.636, 47.906, "left"),
                           ("Neah Bay", -124.62, 48.368, "left"), ("Port Angeles", -123.43, 48.118, "left"),
                           ("Bellingham", -122.479, 48.75, "left"), ("Seattle", -122.335, 47.608, "left")]:
    x, y = gpd.GeoSeries.from_xy([lon], [lat], crs=4326).to_crs(CRS).iloc[0].coords[0]
    dmc.label(ax, x + 9000, y, name, size=7.5, va="center", zorder=6)
ax.set_xlim(vb[0], vb[2])
ax.set_ylim(vb[1], vb[3])
k_m = 1000 / 0.682 if CRS == "EPSG:3857" else 1000
dmc.scalebar(ax, 25, loc=(0.06, 0.08), crs_units_per_km=k_m)
from matplotlib.patches import Patch  # noqa: E402
hand = [Patch(facecolor=dmc.LAKE, alpha=0.4, edgecolor=dmc.LAKE, label="Within a mile of a siren")]
if zones is not None and len(zones):
    hand.append(Patch(color=dmc.LAVA, alpha=0.35, label="Tsunami hazard zone, on land"))
ax.legend(handles=hand, loc="upper right", fontsize=8, frameon=True, facecolor=dmc.PARCHMENT, edgecolor=dmc.MIST)

cov = f" {covered / zone_km2:.0%} of the land in the tsunami hazard zone is within that reach." if zone_km2 else ""
dmc.frame(
    fig, DAY,
    subtitle=(f"Washington's {len(sirens)} tsunami sirens, each drawn with the one-mile circle it's designed to be heard in,\n"
              f"outdoors.{cov}"),
    source="WA EMD AHAB sirens · WA DNR tsunami zones · Census" + (" · " + basemap.CREDIT if drawn else ""),
    note="Range: AHABs are designed to be heard outdoors within a 1-mile radius (Grays Harbor County Emergency Management).",
)
dmc.save(fig, DAY, alt=(
    f"Map of western Washington's coast and inland waters with {len(sirens)} tsunami siren locations, each ringed by a "
    f"one-mile circle where it can be heard outdoors" + (", over the tsunami hazard zones in red." if zone_km2 else ".")))
