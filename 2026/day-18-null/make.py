"""
Day 18 · NULL — Nobody lives here

Nevada's 2020 census blocks, coloured by whether anyone lives in them. A block with a
population of zero is NULL; most of the state is.

Downloads (cached in data/): Census TIGER/Line 2020 tabulation blocks for Nevada (POP20 is in
the file), Census state outlines.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import geopandas as gpd  # noqa: E402
import numpy as np  # noqa: E402
from matplotlib.patches import Patch  # noqa: E402
from pyproj import Transformer  # noqa: E402

DAY = 18
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
CRS = "EPSG:32611"
BLOCKS = "https://www2.census.gov/geo/tiger/TIGER2020/TABBLOCK20/tl_2020_32_tabblock20.zip"

b = fetch.shapes(BLOCKS, DATA / "nv_blocks.zip").to_crs(CRS)
b["POP20"] = b["POP20"].astype(int)
b["ALAND20"] = b["ALAND20"].astype(float)
land = b["ALAND20"].sum()
empty = b[b["POP20"] == 0]
lived = b[b["POP20"] > 0]
pct_area = empty["ALAND20"].sum() / land
pct_blocks = len(empty) / len(b)
pop = b["POP20"].sum()
# how little land holds most of the people: smallest blocks by density first
d = b[b["ALAND20"] > 0].assign(den=lambda x: x["POP20"] / x["ALAND20"]).sort_values("den", ascending=False)
d["cpop"] = d["POP20"].cumsum() / pop
d["cland"] = d["ALAND20"].cumsum() / land
cut = int((d["cpop"] < 0.9).sum()) + 1                      # densest blocks holding 90% of the people
dense_ids = set(d.iloc[:cut]["GEOID20"])
half = d.iloc[cut - 1]["cland"]
b["cls"] = np.where(b["POP20"] == 0, 0, np.where(b["GEOID20"].isin(dense_ids), 2, 1))
few = b[b["cls"] == 1]
dense = b[b["cls"] == 2]
few_pop = few["POP20"].sum() / pop
few_land = few["ALAND20"].sum() / land
print(f"= {len(b):,} blocks, {len(empty):,} empty ({pct_blocks:.0%}); empty land {pct_area:.1%}; "
      f"90% of {pop:,} people on {half:.2%} of the land")

fig, ax = dmc.figure("portrait", map_box=(0.05, 0.08, 0.90, 0.74))
empty.plot(ax=ax, color=dmc.CREAM, edgecolor=dmc.MIST, lw=0.08)
few.plot(ax=ax, color="#c9bfae", edgecolor="#c9bfae", lw=0.1)
dense.plot(ax=ax, color=dmc.INK, edgecolor=dmc.INK, lw=0.25)
ax.legend(handles=[Patch(color=dmc.CREAM, ec=dmc.MIST, label=f"Nobody: {pct_area:.0%} of the land"),
                   Patch(color="#c9bfae", label=f"Some people, {few_pop:.0%} of Nevadans: {few_land:.0%} of the land"),
                   Patch(color=dmc.INK, label=f"Nine in ten Nevadans: {half:.1%} of the land")],
          loc="lower left", bbox_to_anchor=(0.0, 0.08), fontsize=8, frameon=False, handlelength=1.4)
b.dissolve().boundary.plot(ax=ax, color=dmc.INK, lw=0.8)
ax.set_aspect("equal")
to = Transformer.from_crs(4326, CRS, always_xy=True)
for name, (lon, lat), dx, ha in [("Las Vegas", (-115.14, 36.17), -12000, "right"), ("Reno", (-119.81, 39.53), 12000, "left"),
                                 ("Elko", (-115.76, 40.83), 12000, "left"), ("Ely", (-114.89, 39.25), 12000, "left"),
                                 ("Tonopah", (-117.23, 38.07), 12000, "left"), ("Winnemucca", (-117.74, 40.97), 12000, "left"),
                                 ("Austin", (-117.07, 39.49), 12000, "left"), ("Pahrump", (-115.98, 36.21), -12000, "right")]:
    x, y = to.transform(lon, lat)
    dmc.label(ax, x + dx, y, name, size=8.5 if name in ("Las Vegas", "Reno") else 7.5, ha=ha, va="center",
              weight="bold" if name in ("Las Vegas", "Reno") else "normal", zorder=5)
dmc.scalebar(ax, 100, loc=(0.06, 0.04))

dmc.frame(
    fig, DAY,
    subtitle=(f"Nevada's {len(b):,} census blocks. In {len(empty):,} of them, {pct_blocks:.0%}, nobody lived on April 1, 2020:\n"
              f"{pct_area:.0%} of the state's land. Nine in ten Nevadans live on {half:.1%} of it, in black."),
    source="U.S. Census Bureau, 2020 Census TIGER/Line tabulation blocks (POP20)",
    note="A block is the smallest area the census counts. In the desert one can be hundreds of square miles.",
)
dmc.save(fig, DAY, alt=(
    f"Map of Nevada's census blocks: {pct_area:.0%} of the land is pale, where nobody lived in 2020; light grey "
    f"blocks hold a few people each; black specks around Las Vegas, Reno and the towns hold nine in ten "
    f"Nevadans on {half:.1%} of the land."))
