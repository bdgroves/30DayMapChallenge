"""
Day 9 · Urban-rural — Tacoma to Paradise

One straight line from the Port of Tacoma to Paradise on Mount Rainier, about 74 km, laid out flat as
a ribbon of satellite photo, with what changes along it stacked underneath on the same kilometre
scale: people, buildings, trees, and the ground itself, with the stretches that sit in Rainier's
lahar hazard zones marked. It is Patrick Geddes's Valley Section, the sea-to-mountain transect he
drew for teaching how work and settlement change with the land, run for real.

Two stages:
  python make.py           gather (downloads cached in data/), write out/profiles.csv,
                           out/elevation.csv, out/ribbon.jpg, out/lahar_ribbon.geojson, then draw
  python make.py --draw    draw from those out/ files only (no network)

Downloads:
  Census 2020 TIGER/Line blocks for Washington (POP20)
  Microsoft Global ML Building Footprints (the quadkey files that cover the corridor)
  ESA WorldCover 2021 10 m land cover, Sentinel-2 L2A true colour (Microsoft Planetary Computer)
  USGS volcano hazard areas for Rainier, as compiled by WA DNR (2016)
  Copernicus GLO-30 DEM (toolkit/terrain.py)
"""
import csv
import json
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402

import numpy as np  # noqa: E402
from pyproj import Transformer  # noqa: E402

DAY = 9
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
OUT = HERE / "out"
CRS = "EPSG:32610"
A = (-122.4127, 47.2675)          # Port of Tacoma, Blair Waterway
B = (-121.7355, 46.7860)          # Paradise
HALF = 1000                       # m either side of the line: the counting corridor
RIB = 2500                        # m either side of the line: the photo ribbon
STEP = 500                        # m per profile bin
ESTEP = 100                       # m per elevation sample
RES = 10                          # m, ribbon pixels
BLOCKS = "https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_53_tabblock20.zip"
BUILDINGS = "https://minedbuildings.z5.web.core.windows.net/global-buildings/dataset-links.csv"
# USGS volcano hazard areas for Washington's volcanoes, simplified and served by WA DNR (2016)
LAHAR_SERVICE = "https://gis.dnr.wa.gov/site1/rest/services/Public_Geology/Volcanic_Hazards/MapServer/0"
S2_WINDOW = "2025-07-01/2025-09-20"

to = Transformer.from_crs(4326, CRS, always_xy=True)
ax_, ay_ = to.transform(*A)
bx_, by_ = to.transform(*B)
L = float(np.hypot(bx_ - ax_, by_ - ay_))
ux, uy = (bx_ - ax_) / L, (by_ - ay_) / L          # along the line (south-east)
nx, ny = -uy, ux                                   # across it (north-east is +)


def st(x, y):
    """UTM → (s along the line from the port, t across it, + to the north-east), metres."""
    dx, dy = np.asarray(x) - ax_, np.asarray(y) - ay_
    return dx * ux + dy * uy, dx * nx + dy * ny


def xy(s, t):
    return ax_ + s * ux + t * nx, ay_ + s * uy + t * ny


def ribbon_grid(half=RIB, res=RES):
    """A rotated grid lying along the line: column = along, row 0 = the north-east edge."""
    from affine import Affine
    w, h = int(np.ceil(L / res)), int(2 * half / res)
    tf = Affine(res * ux, -res * nx, ax_ + half * nx, res * uy, -res * ny, ay_ + half * ny)
    return tf, w, h


# ── gather ───────────────────────────────────────────────────────────────────
def census(edges):
    import geopandas as gpd
    from shapely.geometry import LineString
    import fetch
    blocks = fetch.shapes(BLOCKS, DATA / "wa_blocks.zip")
    blocks = blocks[blocks["COUNTYFP20"] == "053"].to_crs(CRS)          # Pierce County
    blocks["POP20"] = blocks["POP20"].astype(int)
    corr = blocks.cx[min(ax_, bx_) - 3000:max(ax_, bx_) + 3000, min(ay_, by_) - 3000:max(ay_, by_) + 3000]
    sidx = corr.sindex
    pop = np.zeros(len(edges) - 1)
    for i, (s0, s1) in enumerate(zip(edges[:-1], edges[1:])):
        cell = LineString([xy(s0, 0), xy(s1, 0)]).buffer(HALF, cap_style=2)
        for j in sidx.query(cell):
            g = corr.geometry.iloc[j]
            inter = g.intersection(cell).area
            if inter > 0 and g.area > 0:
                pop[i] += corr["POP20"].iloc[j] * inter / g.area
    return pop


def quadkeys(bounds, z=9):
    import math
    lon0, lat0, lon1, lat1 = bounds

    def tile(lon, lat):
        s = math.sin(math.radians(lat))
        return (int((lon + 180) / 360 * 2 ** z),
                int((0.5 - math.log((1 + s) / (1 - s)) / (4 * math.pi)) * 2 ** z))
    (x0, y0), (x1, y1) = tile(lon0, lat1), tile(lon1, lat0)
    keys = []
    for x in range(x0, x1 + 1):
        for y in range(y0, y1 + 1):
            q = ""
            for i in range(z, 0, -1):
                m = 1 << (i - 1)
                q += str((1 if x & m else 0) + (2 if y & m else 0))
            keys.append(q)
    return keys


def buildings():
    """Centroids (s, t) of every Microsoft footprint within RIB of the line, cached."""
    import gzip
    import fetch
    cache = DATA / "buildings_st.npy"
    if cache.exists():
        return np.load(cache)
    bounds = (min(A[0], B[0]) - 0.05, min(A[1], B[1]) - 0.05, max(A[0], B[0]) + 0.05, max(A[1], B[1]) + 0.05)
    want = {int(q) for q in quadkeys(bounds)}
    links = fetch.get(BUILDINGS, DATA / "ms_buildings_links.csv").decode().splitlines()
    rows = [r for r in csv.DictReader(links) if r["QuadKey"].isdigit() and int(r["QuadKey"]) in want]
    print(f"  buildings: quadkeys {sorted(want)} → {len(rows)} files ({', '.join(r['Location'] for r in rows)})")
    pts = []
    for r in rows:
        raw = fetch.get(r["Url"], DATA / "ms_buildings" / f"{r['Location']}_{r['QuadKey']}.csv.gz", timeout=1200)
        lon, lat = [], []
        for line in gzip.decompress(raw).decode().splitlines():
            if not line.strip():
                continue
            g = json.loads(line)
            g = g.get("geometry", g)
            ring = np.asarray(g["coordinates"][0] if g["type"] == "Polygon" else g["coordinates"][0][0])
            lon.append(ring[:, 0].mean())
            lat.append(ring[:, 1].mean())
        x, y = to.transform(np.array(lon), np.array(lat))
        s, t = st(x, y)
        keep = (s >= 0) & (s <= L) & (np.abs(t) <= RIB)
        pts.append(np.c_[s[keep], t[keep]])
        print(f"  {r['Location']} {r['QuadKey']}: {len(lon):,} footprints, {keep.sum():,} by the line")
    out = np.vstack(pts) if pts else np.zeros((0, 2))
    np.save(cache, out)
    return out


def pc_items(collection, datetime=None, query=None):
    import planetary_computer
    import pystac_client
    cat = pystac_client.Client.open("https://planetarycomputer.microsoft.com/api/stac/v1",
                                    modifier=planetary_computer.sign_inplace)
    bbox = (min(A[0], B[0]) - 0.03, min(A[1], B[1]) - 0.03, max(A[0], B[0]) + 0.03, max(A[1], B[1]) + 0.03)
    return list(cat.search(collections=[collection], bbox=bbox, datetime=datetime, query=query).items())


def warp(href, tf, w, h, count=1, resampling="nearest", dtype="uint8"):
    import rasterio
    from rasterio.warp import Resampling, reproject
    out = np.zeros((count, h, w), dtype=dtype)
    with rasterio.open(href) as src:
        for b in range(count):
            reproject(rasterio.band(src, b + 1), out[b], dst_transform=tf, dst_crs=CRS,
                      resampling=getattr(Resampling, resampling), dst_nodata=0)
    return out


def worldcover():
    """ESA WorldCover 2021 classes on the ribbon grid (±HALF)."""
    cache = DATA / "worldcover_ribbon.npy"
    if cache.exists():
        return np.load(cache)
    tf, w, h = ribbon_grid(HALF)
    lc = np.zeros((h, w), dtype="uint8")
    items = [i for i in pc_items("esa-worldcover") if "2021" in str(i.datetime or i.properties.get("start_datetime"))]
    for it in items:
        part = warp(it.assets["map"].href, tf, w, h)[0]
        lc = np.where(lc == 0, part, lc)
    print(f"  WorldCover: {len(items)} tiles, {(lc > 0).mean():.1%} of the corridor covered")
    np.save(cache, lc)
    return lc


def sentinel():
    """The clearest summer Sentinel-2 day over the ribbon, true colour on the ribbon grid."""
    cache = DATA / "s2_ribbon.npy"
    if cache.exists():
        return np.load(cache)
    tf, w, h = ribbon_grid(RIB)
    tf20, w20, h20 = ribbon_grid(RIB, 50)               # cloud check on a coarse grid
    items = pc_items("sentinel-2-l2a", S2_WINDOW, {"eo:cloud_cover": {"lt": 20}})
    days = {}
    for it in items:
        days.setdefault(it.datetime.date(), []).append(it)
    scores = []
    for day, its in sorted(days.items()):
        scl = np.zeros((h20, w20), dtype="uint8")
        for it in its:
            part = warp(it.assets["SCL"].href, tf20, w20, h20)[0]
            scl = np.where(scl == 0, part, scl)
        covered = (scl > 0).mean()
        cloud = np.isin(scl, (3, 8, 9, 10)).sum() / max(1, (scl > 0).sum())
        scores.append((covered < 0.995, cloud, day))
        print(f"  S2 {day}: {len(its)} tiles, covers {covered:.1%}, cloud+shadow {cloud:.1%}")
    scores.sort()
    best = scores[0][2]
    print(f"= ribbon photo: Sentinel-2 {best} (cloud+shadow {scores[0][1]:.1%})")
    rgb = np.zeros((3, h, w), dtype="uint8")
    for it in days[best]:
        part = warp(it.assets["visual"].href, tf, w, h, count=3, resampling="average")
        have = (part.min(axis=0) > 0) & (rgb.max(axis=0) == 0)
        rgb[:, have] = part[:, have]
    np.save(cache, rgb)
    (DATA / "s2_date.txt").write_text(str(best))
    return rgb


def lahar(es):
    """Per elevation sample: inside each lahar layer? And the layers clipped to the ribbon, in (s, t)."""
    import geopandas as gpd
    import fetch
    from shapely import contains_xy
    from shapely.geometry import box, mapping
    from shapely.ops import transform as stransform
    cache = DATA / "lahar_dnr.geojson"
    if not cache.exists():
        bb = (min(A[0], B[0]) - 0.05, min(A[1], B[1]) - 0.05, max(A[0], B[0]) + 0.05, max(A[1], B[1]) + 0.05)
        js = fetch.json_get(LAHAR_SERVICE + "/query", dict(
            geometry=",".join(map(str, bb)), geometryType="esriGeometryEnvelope", inSR=4326, outSR=4326,
            spatialRel="esriSpatialRelIntersects", outFields="*", returnGeometry="true", f="geojson"))
        cache.write_text(json.dumps(js))
    g = gpd.read_file(cache)
    print(f"  hazard polygons: {len(g)}; columns {[c for c in g.columns if c != 'geometry']}")
    print(f"  {g[['VOLCANO', 'HAZARD_TYPE']].to_dict('records')}")
    col = "HAZARD_TYPE"
    g = g[(g["VOLCANO"] == "Mount Rainier") & g[col].isin(["Lahars", "Near-volcano hazards"])].to_crs(CRS)
    g["geometry"] = g.geometry.make_valid()
    flags, feats = {}, []
    ex, ey = xy(es, 0)
    strip = box(0, -RIB, L, RIB)
    for val, part in g.groupby(col):
        u = part.geometry.union_all()
        name = "".join(ch if ch.isalnum() else "_" for ch in str(val).lower()).strip("_")
        flags[name] = contains_xy(u, ex, ey)
        clipped = stransform(lambda x, y, z=None: st(x, y), u).intersection(strip)
        if not clipped.is_empty:
            feats.append({"type": "Feature", "properties": {"layer": name, "value": str(val)}, "geometry": mapping(clipped)})
        print(f"  {col} = {val!r}: {flags[name].mean() * L / 1000:.1f} km of the line inside")
    (OUT / "lahar_ribbon.geojson").write_text(json.dumps({"type": "FeatureCollection", "features": feats}))
    return flags


def gather():
    import terrain
    from PIL import Image
    from rasterio.transform import rowcol
    DATA.mkdir(exist_ok=True)
    OUT.mkdir(exist_ok=True)
    edges = np.arange(0, L + 1, STEP)
    edges[-1] = L
    km = (edges[:-1] + edges[1:]) / 2000
    area = np.diff(edges) * 2 * HALF / 1e6                              # km² per bin

    pop = census(edges)
    bst = buildings()
    inb = np.abs(bst[:, 1]) <= HALF
    nb = np.histogram(bst[inb, 0], bins=edges)[0]
    lc = worldcover()
    cols = (np.arange(lc.shape[1]) + 0.5) * RES
    frac = {}
    for name, classes in (("tree", (10,)), ("built", (50,)), ("open", (20, 30, 40, 90, 95, 100)),
                          ("snow_rock", (60, 70)), ("water", (80,))):
        hit = np.isin(lc, classes).sum(axis=0)
        val = (lc > 0).sum(axis=0)
        frac[name] = np.histogram(cols, edges, weights=hit)[0] / np.maximum(1, np.histogram(cols, edges, weights=val)[0])

    bb = (min(A[0], B[0]) - 0.06, min(A[1], B[1]) - 0.06, max(A[0], B[0]) + 0.06, max(A[1], B[1]) + 0.06)
    zz, tf = terrain.dem(bb, crs=CRS, res=30)
    es = np.arange(0, L, ESTEP) + ESTEP / 2
    r, c = rowcol(tf, *xy(es, 0))
    elev = zz[np.clip(r, 0, zz.shape[0] - 1), np.clip(c, 0, zz.shape[1] - 1)]
    lz = lahar(es)

    with open(OUT / "profiles.csv", "w", newline="") as f:
        wr = csv.writer(f)
        wr.writerow(["km", "people", "people_per_km2", "buildings", "buildings_per_km2", "tree_pct", "built_pct",
                     "open_pct", "snow_rock_pct", "water_pct"])
        for i in range(len(km)):
            wr.writerow([f"{km[i]:.2f}", f"{pop[i]:.0f}", f"{pop[i] / area[i]:.1f}", nb[i], f"{nb[i] / area[i]:.1f}",
                         *(f"{100 * frac[k][i]:.1f}" for k in ("tree", "built", "open", "snow_rock", "water"))])
    with open(OUT / "elevation.csv", "w", newline="") as f:
        wr = csv.writer(f)
        wr.writerow(["km", "elev_m", *(f"lahar_{k}" for k in lz)])
        for i in range(len(es)):
            wr.writerow([f"{es[i] / 1000:.2f}", f"{elev[i]:.0f}", *(int(v[i]) for v in lz.values())])
    rgb = sentinel()
    Image.fromarray(np.moveaxis(rgb, 0, -1)).save(OUT / "ribbon.jpg", quality=92)
    meta = {"line_km": round(L / 1000, 2), "s2_date": (DATA / "s2_date.txt").read_text().strip(),
            "ribbon_half_m": RIB, "corridor_half_m": HALF, "res_m": RES, "buildings_in_ribbon": int(len(bst))}
    (OUT / "meta.json").write_text(json.dumps(meta, indent=1))

    # places along the line: how far along, how far off
    for name, ll in PLACES:
        s, t = st(*to.transform(*ll))
        print(f"  place {name}: km {s / 1000:.1f}, {t / 1000:+.1f} km off the line")
    tot = pop.sum()
    print(f"= {L / 1000:.1f} km; {tot:,.0f} people and {nb.sum():,} buildings within 1 km; "
          f"top {elev.max() * 3.28084:,.0f} ft at km {es[elev.argmax()] / 1000:.1f}; "
          f"tree cover {100 * frac['tree'].mean():.0f}% overall")


PLACES = [("Port of Tacoma", A), ("Fife", (-122.357, 47.239)), ("Puyallup", (-122.293, 47.185)),
          ("Orting", (-122.204, 47.098)), ("South Prairie", (-122.093, 47.139)), ("Wilkeson", (-122.046, 47.106)),
          ("Carbonado", (-122.051, 47.080)), ("Mowich Lake", (-121.861, 46.933)), ("Mount Rainier summit", (-121.7603, 46.8529)),
          ("Paradise", B)]


# ── draw ─────────────────────────────────────────────────────────────────────
def read_csv(path):
    with open(path) as f:
        rows = list(csv.DictReader(f))
    return {k: np.array([float(r[k]) for r in rows]) for k in rows[0]}


def draw():
    import matplotlib.pyplot as plt
    import matplotlib.ticker as mt
    from matplotlib.patches import Polygon as MPoly, Rectangle
    from PIL import Image

    P = read_csv(OUT / "profiles.csv")
    E = read_csv(OUT / "elevation.csv")
    meta = json.loads((OUT / "meta.json").read_text())
    km, ekm = P["km"], E["km"]
    Lk = meta["line_km"]
    ft = E["elev_m"] * 3.28084

    fig = dmc.figure("wide", map_box=(0.05, 0.585, 0.90, 0.115))[0]
    rax = fig.axes[0]
    X0, X1 = 0.05, 0.95

    # ribbon
    img = np.asarray(Image.open(OUT / "ribbon.jpg")).astype("float32") / 255
    img = np.clip((img - 0.02) / 0.82, 0, 1) ** 0.85
    hk = meta["ribbon_half_m"] / 1000
    rax.imshow(img, extent=(0, Lk, -hk, hk), aspect="auto", interpolation="lanczos", zorder=0)
    rax.set_xlim(0, Lk)
    rax.set_ylim(-hk, hk)
    lz_layers = [k for k in E if k.startswith("lahar_")]
    lahar_geo = json.loads((OUT / "lahar_ribbon.geojson").read_text()) if (OUT / "lahar_ribbon.geojson").exists() else {"features": []}
    show = LAHAR_SHOW or [f["properties"]["layer"] for f in lahar_geo["features"]]
    for f in lahar_geo["features"]:
        if f["properties"]["layer"] not in show:
            continue
        g = f["geometry"]
        polys = [g["coordinates"]] if g["type"] == "Polygon" else g["coordinates"]
        for p in polys:
            ring = np.asarray(p[0]) / 1000
            rax.add_patch(MPoly(ring, closed=True, fc=dmc.LAVA, ec="none", alpha=0.13, zorder=1))
            rax.add_patch(MPoly(ring, closed=True, fc="none", ec="#f3d2c8", lw=0.5, alpha=0.9, zorder=1))
    for y in (-HALF / 1000, HALF / 1000):
        rax.axhline(y, color=dmc.WHITE, lw=0.45, ls=(0, (2, 2)), alpha=0.75, zorder=2)
    rax.axhline(0, color=dmc.WHITE, lw=0.6, alpha=0.9, zorder=2)

    # Geddes zones above the ribbon
    for (k0, k1, name, trade) in ZONES:
        fig_x = lambda k: X0 + (X1 - X0) * k / Lk   # noqa: E731
        fig.add_artist(plt.Line2D([fig_x(k0) + 0.002, fig_x(k1) - 0.002], [0.708] * 2, color=dmc.INK, lw=0.6,
                                  transform=fig.transFigure))
        for k in (k0, k1):
            fig.add_artist(plt.Line2D([fig_x(k)] * 2, [0.703, 0.713], color=dmc.INK, lw=0.6, transform=fig.transFigure))
        cx = fig_x((k0 + k1) / 2)
        fig.text(cx, 0.717, name.upper(), family=dmc.MONO, size=6.4, color=dmc.INK, ha="center", va="bottom")
        fig.text(cx, 0.739, trade, family=dmc.TEXT, style="italic", size=7, color=dmc.STONE, ha="center", va="bottom")

    def panel(y0, h, label, colour, y, log=False, fmt=lambda v, _: f"{v:,.0f}", ylim=None, lx=0.996, ha="right"):
        a = fig.add_axes((X0, y0, X1 - X0, h))
        a.set_facecolor("none")
        a.fill_between(km, y, color=colour, alpha=0.28, lw=0, step="mid")
        a.step(km, y, where="mid", color=colour, lw=1.0)
        if log:
            a.set_yscale("symlog", linthresh=10)
        a.set_xlim(0, Lk)
        if ylim:
            a.set_ylim(*ylim)
        style(a, fmt)
        a.tick_params(labelbottom=False)
        a.text(lx, 0.92, label, transform=a.transAxes, family=dmc.MONO, size=6.3, color=colour, va="top", ha=ha)
        return a

    def style(a, fmt):
        a.tick_params(labelsize=6.2, colors=dmc.STONE, length=2, pad=1.5)
        for t in a.get_xticklabels() + a.get_yticklabels():
            t.set_fontfamily(dmc.MONO)
        for s in ("top", "right"):
            a.spines[s].set_visible(False)
        a.spines["left"].set_color(dmc.MIST)
        a.spines["bottom"].set_color(dmc.MIST)
        a.yaxis.set_major_formatter(mt.FuncFormatter(fmt))
        a.xaxis.set_major_locator(mt.MultipleLocator(10))
        a.xaxis.set_minor_locator(mt.MultipleLocator(5))
        for k0, k1, *_ in ZONES[1:]:
            a.axvline(k0, color=dmc.MIST, lw=0.5, zorder=0)

    panel(0.475, 0.08, "PEOPLE PER KM²  (LOG SCALE)", dmc.LAVA, P["people_per_km2"], log=True, ylim=(0, 6000))
    panel(0.385, 0.07, "BUILDINGS PER KM²", dmc.GOLD, P["buildings_per_km2"])
    panel(0.295, 0.07, "TREE COVER, %", dmc.SAGE, P["tree_pct"], ylim=(0, 100), lx=66 / Lk, ha="center")

    ea = fig.add_axes((X0, 0.115, X1 - X0, 0.155))
    ea.set_facecolor("none")
    ea.fill_between(ekm, ft, color=dmc.ASH, alpha=0.30, lw=0)
    ea.plot(ekm, ft, color=dmc.INK, lw=0.9)
    ea.set_xlim(0, Lk)
    ea.set_ylim(-600, max(ft) * 1.22)
    style(ea, lambda v, _: f"{v:,.0f}")
    ea.text(0.004, 1.0, "GROUND, FEET ABOVE SEA LEVEL", transform=ea.transAxes, family=dmc.MONO, size=6.3,
            color=dmc.INK, va="top")
    ea.set_xlabel("km from the Port of Tacoma", family=dmc.MONO, fontsize=6.5, color=dmc.STONE, labelpad=1)
    # lahar band: under the profile, where the line is inside a hazard zone
    if lz_layers:
        inside = np.zeros(len(ekm), bool)
        for k in lz_layers:
            if k[len("lahar_"):] in show:
                inside |= E[k] > 0
        runs = np.flatnonzero(np.diff(np.r_[0, inside.astype(int), 0]))
        for s0, s1 in zip(runs[::2], runs[1::2]):
            k0, k1 = ekm[s0] - ESTEP / 2000, ekm[s1 - 1] + ESTEP / 2000
            ea.add_patch(Rectangle((k0, -600), k1 - k0, 380, fc=dmc.LAVA, ec="none", alpha=0.85, zorder=3))
            ea.fill_between(ekm[s0:s1], ft[s0:s1], -220, color=dmc.LAVA, alpha=0.16, lw=0, zorder=1)
        lk = inside.sum() * ESTEP / 1000
        dmc.label(ea, 0.6, max(ft) * 0.98, f"RED BAND: IN A USGS LAHAR HAZARD ZONE, {lk:.0f} KM OF THE LINE", family=dmc.MONO,
                  size=6.2, color=dmc.LAVA, va="top", ha="left")
        print(f"= lahar zone: {lk:.1f} km of the line in {len(runs) // 2} stretches")

    import matplotlib.patheffects as pe
    for name, (lon, lat) in RIBBON_PLACES:
        sk, tk = (v / 1000 for v in st(*to.transform(lon, lat)))
        rax.scatter([sk], [tk], s=5, color=dmc.WHITE, edgecolor=dmc.INK, lw=0.4, zorder=5)
        rax.text(sk + 0.35, tk, name, size=6.6, family=dmc.TEXT, color=dmc.WHITE, va="center", ha="left", zorder=5,
                 path_effects=[pe.withStroke(linewidth=2, foreground="#1c1a16")])
    rax.text(Lk - 0.2, hk - 0.25, "north-east side up", size=5.6, family=dmc.MONO, color=dmc.WHITE, ha="right", va="top",
             zorder=5, path_effects=[pe.withStroke(linewidth=1.6, foreground="#1c1a16")])
    top = int(np.argmax(ft))
    for k, text, dx, dy, ha in [(ekm[top], f"{ft[top]:,.0f} ft on Rainier's south-west flank, {SUMMIT_OFF:.0f} km from the summit", -6, 6, "right"),
                                (ekm[-1], f"Paradise, {ft[-1]:,.0f} ft", -4, -30, "right"),
                                (12.9, "Puyallup, on the valley floor", 0, 18, "center")]:
        i = int(np.argmin(np.abs(ekm - k)))
        ea.annotate(text, (ekm[i], ft[i]), xytext=(dx, dy), textcoords="offset points", ha=ha, va="bottom",
                    family=dmc.TEXT, size=6.6, color=dmc.INK, zorder=6,
                    path_effects=[pe.withStroke(linewidth=2.2, foreground=dmc.PARCHMENT)],
                    arrowprops=dict(arrowstyle="-", color=dmc.STONE, lw=0.5, shrinkA=0, shrinkB=1))

    s = SUMMARY
    dmc.frame(
        fig, DAY,
        subtitle=s["subtitle"],
        source=("U.S. Census Bureau 2020 Census blocks · Microsoft US Building Footprints · ESA WorldCover 2021 · "
                f"modified Copernicus Sentinel-2 data ({meta['s2_date']}) · Copernicus DEM GLO-30"),
        note=("Counts are for a 2 km-wide corridor (dashed lines on the photo); people are spread evenly across each census "
              "block. Lahar zones: USGS simplified volcanic hazards, via WA DNR. Zones after Patrick Geddes's Valley Section. Locator © Mapbox © Maxar."),
    )
    locator(fig)
    dmc.save(fig, DAY, alt=s["alt"])
    # the gallery crop: ribbon and profiles, not just the thin ribbon
    im = Image.open(OUT / "map.png")
    W, H = im.size
    c = im.crop((int(0.03 * W), int(0.10 * H), int(0.97 * W), int(0.90 * H))).convert("RGB")
    c.thumbnail((1200, 1200))
    c.save(OUT / "crop.jpg", quality=86, optimize=True)


def locator(fig):
    """A small map of where the line runs, top right."""
    import geopandas as gpd
    from shapely.geometry import LineString, box
    import basemap
    a = fig.add_axes((0.95 - 0.175 * 6 / 10.667, 0.80, 0.175 * 6 / 10.667, 0.175))
    a.set_axis_off()
    pad = 9000
    cx, cy, r = (ax_ + bx_) / 2, (ay_ + by_) / 2, max(abs(bx_ - ax_), abs(by_ - ay_)) / 2 + pad
    view = box(cx - r, cy - r, cx + r, cy + r)
    drawn = False
    MCRS = CRS
    if basemap.available():
        MCRS = "EPSG:3857"
    vx0, vy0, vx1, vy1 = gpd.GeoSeries([view], crs=CRS).to_crs(MCRS).total_bounds
    a.set_xlim(vx0, vx1)
    a.set_ylim(vy0, vy1)
    if MCRS != CRS:
        drawn = basemap.mapbox(a, style="mapbox/satellite-v9")
    if not drawn:
        a.add_patch(__import__("matplotlib").patches.Rectangle((vx0, vy0), vx1 - vx0, vy1 - vy0, fc=dmc.CREAM, ec="none"))
    line = gpd.GeoSeries([LineString([xy(0, 0), xy(L, 0)])], crs=CRS).to_crs(MCRS)
    line.plot(ax=a, color=dmc.LAVA, lw=1.4, zorder=3)
    gpd.GeoSeries([LineString([xy(0, 0), xy(L, 0)]).buffer(RIB, cap_style=2)], crs=CRS).to_crs(MCRS).boundary.plot(
        ax=a, color=dmc.WHITE, lw=0.5, zorder=3)
    tm = Transformer.from_crs(CRS, MCRS, always_xy=True)
    for name, (lon, lat), ha, dx in [("Tacoma", A, "left", 1), ("Paradise", B, "right", -1)]:
        x, y = tm.transform(*to.transform(lon, lat))
        a.scatter([x], [y], s=6, color=dmc.INK, zorder=4)
        dmc.label(a, x + dx * 3500, y, name, size=6.2, ha=ha, va="center", zorder=5, clip_on=False)
    a.set_xlim(vx0, vx1)
    a.set_ylim(vy0, vy1)


# Set from the first gather (out/profiles.csv, km along the line):
#   port      built-up > 60% and under 100 people/km² until km 4
#   city      over 250 people/km² until km 20
#   acreage   10-120 people/km² under 60-90% trees until km 32.5, then under 1 person/km²
#   forest    over 80% trees, nobody, until km 61.5
#   mountain  rock and ice (WorldCover snow/bare > 50%) from km 61.5
ZONES = [  # (km from, km to, zone, Geddes's trade for it)
    (0, 4, "Port", "fisher"),
    (4, 20, "City & suburb", "the city"),
    (20, 32.5, "Acreage & farm", "peasant"),
    (32.5, 61.5, "Forest", "woodman · hunter"),
    (61.5, 74.24, "Rock & ice", "miner"),
]
RIBBON_PLACES = [("Fife", (-122.357, 47.239)), ("Puyallup", (-122.293, 47.185)), ("Orting", (-122.204, 47.098))]
SUMMIT = (-121.7603, 46.8529)          # Columbia Crest
SUMMIT_OFF = abs(st(*to.transform(*SUMMIT))[1]) / 1000
LAHAR_SHOW = ["lahars"]               # the lahar areas, not the near-volcano zone
SUMMARY = {
    "subtitle": ("A straight line from the Port of Tacoma to Paradise on Mount Rainier, 74 km, laid out flat. Within a kilometre\n"
                 "of it live 35,700 people, nearly all in the first 33 km; then 29 km of forest with almost nobody, then rock and ice.\n"
                 "Patrick Geddes drew this sea-to-mountain slice as his Valley Section; the red is where lahars could run."),
    "alt": ("A long horizontal satellite-photo ribbon of the 74 km straight line from the Port of Tacoma to Paradise on Mount "
            "Rainier, with Rainier's lahar hazard zones outlined in red on the valley floors, labelled above with zones after "
            "Patrick Geddes's Valley Section: port, city and suburb, acreage and farm, forest, rock and ice. Below it, stacked "
            "profiles on the same kilometre scale: people per square kilometre, high through Fife and Puyallup and falling to "
            "almost none after km 33; buildings per square kilometre, peaking in Puyallup; tree cover, rising to near 100% "
            "through the forest and dropping to zero on the mountain; and ground elevation, flat near sea level along the "
            "Puyallup valley, climbing to about 9,000 ft on Rainier's south-west flank and ending at Paradise at about 5,400 ft, with "
            "a red band marking the stretches inside a lahar hazard zone."),
}

if __name__ == "__main__":
    try:
        if "--draw" not in sys.argv:
            gather()
        draw()
    except Exception:
        import traceback
        for line in traceback.format_exc().splitlines()[-8:]:
            print("= ERR " + line[:300])
        raise
