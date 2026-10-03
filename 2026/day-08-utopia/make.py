"""
Day 8 · Utopia — Paradise, Eden, Utopia

Every feature in the US Board on Geographic Names database whose name contains Paradise, Eden,
Utopia, Arcadia or Shangri-La as a whole word: towns, valleys, creeks, churches, cemeteries.

Downloads (cached in data/): USGS GNIS Domestic Names (national file), Census state outlines.
"""
import io
import re
import sys
import zipfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
import pandas as pd  # noqa: E402
from matplotlib.lines import Line2D  # noqa: E402

DAY = 8
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
GNIS = "https://prd-tnm.s3.amazonaws.com/StagedProducts/GeographicNames/DomesticNames/DomesticNames_National_Text.zip"
CRS = "EPSG:5070"                                    # CONUS Albers equal-area
WORDS = {                                            # label: (pattern, colour)
    "Paradise": (r"\bparadise\b", dmc.LAVA),
    "Eden": (r"\beden\b", dmc.SAGE),
    "Arcadia": (r"\barcadia\b", dmc.LAKE),
    "Utopia": (r"\butopia\b", dmc.GOLD),
    "Shangri-La": (r"\bshangri[- ]?la\b", "#8a5a83"),
}

# ── names ────────────────────────────────────────────────────────────────────
cache = DATA / "utopias.csv"
if not cache.exists():
    z = zipfile.ZipFile(io.BytesIO(fetch.get(GNIS, DATA / "gnis.zip", timeout=900)))
    name = next(n for n in z.namelist() if n.lower().endswith(".txt"))
    df = pd.read_csv(z.open(name), sep="|", dtype=str, encoding="utf-8", on_bad_lines="skip")
    df.columns = [c.strip().lower() for c in df.columns]
    print(f"  GNIS: {len(df):,} names; columns {list(df.columns)[:8]}…")
    pat = "|".join(p for p, _ in WORDS.values())
    keep = df[df["feature_name"].str.contains(pat, flags=re.I, regex=True, na=False)]
    keep.to_csv(cache, index=False)
names = pd.read_csv(cache, dtype=str)
lat = next(c for c in names.columns if c in ("prim_lat_dec", "primary_latitude_dec", "latitude"))
lon = next(c for c in names.columns if c in ("prim_long_dec", "primary_longitude_dec", "longitude"))
names["lat"] = pd.to_numeric(names[lat], errors="coerce")
names["lon"] = pd.to_numeric(names[lon], errors="coerce")
names = names[names["lat"].notna() & (names["lat"] != 0)]


def word(n):
    for k, (p, _) in WORDS.items():
        if re.search(p, n, re.I):
            return k
    return None


names["word"] = names["feature_name"].map(word)
counts = names["word"].value_counts()
print("= " + ", ".join(f"{k} {counts.get(k, 0):,}" for k in WORDS))
cls = names["feature_class"].value_counts()
print("= classes: " + ", ".join(f"{k} {v}" for k, v in cls.head(8).items()))
towns = names[names["feature_class"].str.lower() == "populated place"]
print(f"= {len(names):,} features, {len(towns):,} of them towns")

# ── map: the lower 48 ────────────────────────────────────────────────────────
states = fetch.shapes(fetch.STATES, DATA / "states.zip")
lower48 = states[~states["STUSPS"].isin(["AK", "HI", "PR", "VI", "GU", "MP", "AS"])].to_crs(CRS)
pts = gpd.GeoDataFrame(names, geometry=gpd.points_from_xy(names["lon"], names["lat"]), crs=4326)
in48 = pts[pts["lon"].between(-125, -66) & pts["lat"].between(24, 50)].to_crs(CRS)
out48 = len(pts) - len(in48)

fig, ax = dmc.figure("wide", map_box=(0.03, 0.12, 0.70, 0.68))
lower48.plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.5)
lower48.dissolve().boundary.plot(ax=ax, color=dmc.STONE, lw=0.6)
for k, (_, colour) in sorted(WORDS.items(), key=lambda kv: -counts.get(kv[0], 0)):
    g = in48[in48["word"] == k]
    ax.scatter(g.geometry.x, g.geometry.y, s=4 if k in ("Paradise", "Eden") else 9, color=colour,
               alpha=0.75, lw=0, zorder=3)
# a few the map should name
for nm, st_, dx, dy, ha in [("Paradise", "Washington", 0, 60000, "center"), ("Utopia", "Texas", 0, -70000, "center"),
                            ("Paradise", "California", 45000, 20000, "left"), ("Arcadia", "California", 45000, -40000, "left"),
                            ("Eden", "Utah", 40000, 30000, "left"), ("Shangri-La", "Tennessee", 0, -60000, "center")]:
    hit = in48[(in48["feature_name"] == nm) & (in48["state_name"] == st_)
               & (in48["feature_class"].str.lower() == "populated place")]
    if len(hit):
        p = hit.geometry.iloc[0]
        ax.scatter([p.x], [p.y], s=26, facecolor="none", edgecolor=dmc.INK, lw=0.8, zorder=4)
        dmc.label(ax, p.x + dx, p.y + dy, f"{nm}, {st_}", size=7.5, ha=ha, va="center", style="italic", zorder=5)
ax.set_xlim(-2.4e6, 2.3e6)
ax.set_ylim(2.6e5, 3.2e6)
ax.set_aspect("equal")

# side panel
px = 0.755
fig.text(px, 0.79, f"{len(names):,}", family=dmc.TITLE, weight=900, size=36, color=dmc.LAVA, va="top")
def plural(w):
    w = {"civil": "township or other civil division", "locale": "locale"}.get(w.lower(), w.lower())
    if w.startswith("township"):
        return "townships and other civil divisions"
    if w.endswith(("ch", "sh", "s", "x")):
        return w + "es"
    if w.endswith("y") and w[-2:-1] not in "aeiou":
        return w[:-1] + "ies"
    return w + "s"


others = [plural(c) for c in cls.index if c.lower() != "populated place"][:3]
fig.text(px, 0.69, "places named for heaven on earth.\nOnly " + f"{len(towns):,}" + " of them are towns. Most\n"
         f"are {others[0]}, {others[1]}\nand {others[2]}.", size=8.5, va="top", linespacing=1.55)
y = 0.50
for k, (_, colour) in WORDS.items():
    fig.add_artist(Line2D([px + 0.006], [y], marker="o", color=colour, ms=6, lw=0, transform=fig.transFigure))
    fig.text(px + 0.02, y, k, size=9.5, va="center")
    fig.text(0.965, y, f"{counts.get(k, 0):,}", family=dmc.MONO, size=8.5, ha="right", va="center", color=dmc.STONE)
    y -= 0.042
fig.text(px, y - 0.02, "MOST COMMON KINDS", family=dmc.MONO, size=6.8, color=dmc.STONE)
y -= 0.055
for k, v in cls.head(5).items():
    fig.text(px, y, {"Civil": "Township, civil division"}.get(k, k), size=8, va="center")
    fig.text(0.965, y, f"{v:,}", family=dmc.MONO, size=7.5, ha="right", va="center", color=dmc.STONE)
    y -= 0.032

dmc.frame(
    fig, DAY,
    subtitle=("Every feature in the national gazetteer named Paradise, Eden, Arcadia, Utopia or Shangri-La.\n"
              f"{counts.index[0]} is the most common, with {counts.iloc[0]:,}; {counts.index[-1]} the rarest, with {counts.iloc[-1]:,}."),
    source="USGS GNIS Domestic Names (US Board on Geographic Names) · U.S. Census Bureau",
    note=f"Whole words only, so Edenton and Paradisea don't count. {out48:,} more are in Alaska, Hawaii and the territories.",
)
dmc.save(fig, DAY, alt=(
    f"Map of the lower 48 states dotted with {len(in48):,} places named Paradise, Eden, Arcadia, Utopia or "
    f"Shangri-La. Paradise ({counts.get('Paradise', 0):,}) is the most common, then Eden ({counts.get('Eden', 0):,}). "
    f"Only {len(towns):,} are towns; the most common kinds are {', '.join(others)}."))
