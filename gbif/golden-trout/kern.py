"""The Kern Plateau: the golden trout's two native streams, the Little Kern, and the 1876 coffee-can
route to Cottonwood Creek. Writes out/kern.png."""
import sys
from pathlib import Path
import numpy as np
import pandas as pd
from matplotlib.colors import to_rgb
from matplotlib.lines import Line2D
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from mapkit import *   # noqa: F403

HERE = Path(__file__).resolve().parent
OUT = HERE / "out"; OUT.mkdir(exist_ok=True)
GOLDEN = "#d29a1e"

def main():
    style()
    z, ext = read_dem("kern")
    w, e, s, n = ext
    k = np.cos(np.radians(36.25))
    hs = hillshade(z, ext, exag=1.0, alt=38)
    base = np.array(to_rgb(CREAM))
    rgb = np.clip(base * (0.36 + 0.68 * hs[..., None]), 0, 1)
    feats = osm("kern")
    fig, ax = plt.subplots(figsize=(7.6, 9.2))
    ax.imshow(rgb, extent=(w * k, e * k, s, n), interpolation="lanczos", zorder=0)
    wild = polygon(feats, "Golden Trout Wilderness")
    draw(ax, wild, k, color=to_rgb("#4a5c45") + (0.16,), ec="none", zorder=1)
    if wild is not None:
        draw(ax, wild.boundary, k, color="#4a5c45", lw=0.9, ls=(0, (4, 2)), zorder=2)
    draw(ax, lines(feats, {"Kern River", "Rock Creek", "Whitney Creek", "Cottonwood Creek"}), k, color=RIVER, lw=0.9, zorder=3)
    draw(ax, lines(feats, {"Little Kern River"}), k, color=RUST, lw=1.8, zorder=4)
    draw(ax, lines(feats, {"Golden Trout Creek", "South Fork Kern River", "Mulkey Creek"}), k, color=GOLDEN, lw=2.2, zorder=4)
    for i in range(1, 7):
        nm = ["One", "Two", "Three", "Four", "Five", "Six"][i - 1]
        draw(ax, polygon(feats, f"Cottonwood Lake Number {nm}"), k, color=RIVER, ec="none", zorder=4)
    # GBIF records of the two golden trout, if downloaded
    D = HERE.parent / "data"
    for slug, col, lbl in [("oncorhynchus-mykiss-aguabonita", GOLDEN, "California golden trout"),
                           ("oncorhynchus-mykiss-whitei", RUST, "Little Kern golden trout")]:
        f = D / f"{slug}.csv.gz"
        if f.exists():
            d = pd.read_csv(f, low_memory=False)
            d = d[d.lon.between(w, e) & d.lat.between(s, n)]
            ax.scatter(d.lon * k, d.lat, s=10, color=col, ec=INK, lw=0.35, zorder=6)
    pts = [("Mount Whitney", "^", (-6, 0), "right"), ("Cirque Peak", "^", (-6, 0), "right"), ("Olancha Peak", "^", (6, 0), "left"),
           ("Volcano Falls", "v", (6, -2), "left")]
    for name, mk, off, ha in pts:
        p = point(feats, name)
        if p is None:
            continue
        ax.plot(p.x * k, p.y, marker=mk, ms=6 if mk == "^" else 5, color=INK, mec=PAPER, mew=0.6, zorder=7)
        ax.annotate(name, (p.x * k, p.y), xytext=off, textcoords="offset points", ha=ha, va="center", fontsize=8, color=INK, zorder=8,
                    bbox=dict(boxstyle="round,pad=0.12", fc=CREAM, ec="none", alpha=0.7))
    for name, near in [("Big Whitney Meadow", None), ("Mulkey Meadows", None), ("Templeton Meadows", None), ("Monache Meadow", None)]:
        p = point(feats, name)
        if p is not None:
            ax.text(p.x * k, p.y, name.replace(" ", "\n", 1), fontsize=7.4, color="#4a5c45", style="italic", ha="center", va="center", zorder=7)
    for t, lo, la, col, rot in [("Golden Trout Creek", -118.37, 36.43, "#7a5408", 0), ("South Fork\nKern River", -118.07, 36.08, "#7a5408", 0),
                                ("Little Kern\nRiver", -118.55, 36.25, "#7d3417", 0), ("Kern River", -118.475, 36.45, "#1f4f68", 0),
                                ("Golden Trout\nWilderness", -118.29, 36.30, "#4a5c45", 0), ("Cottonwood\nLakes", -118.15, 36.52, "#1f4f68", 0)]:
        ax.text(lo * k, la, t, fontsize=8.6, color=col, style="italic", ha="center", va="center", zorder=7, fontweight="medium",
                bbox=dict(boxstyle="round,pad=0.12", fc=CREAM, ec="none", alpha=0.6))
    hand = [Line2D([], [], color=GOLDEN, lw=2.4, label="Native range: Golden Trout Creek & South Fork Kern"),
            Line2D([], [], color=RUST, lw=2, label="Little Kern golden trout's river"),
            Line2D([], [], color="#4a5c45", lw=1, ls=(0, (4, 2)), label="Golden Trout Wilderness (1978)")]
    ax.legend(handles=hand, loc="lower left", frameon=True, facecolor=PAPER, edgecolor="none", fontsize=8)
    ax.set_xlim((w + 0.01) * k, (e - 0.01) * k); ax.set_ylim(s + 0.1, n - 0.01)
    ax.set_aspect("equal"); ax.axis("off")
    ax.text(0, 1.05, "Where California's state fish comes from", transform=ax.transAxes, fontsize=14, color=INK, fontweight="medium")
    ax.text(0, 1.02, "The Kern Plateau, at the south end of the high Sierra", transform=ax.transAxes, fontsize=9, color=STONE)
    ax.text(1, -0.01, "Elevation: Copernicus GLO-30 · Streams: © OpenStreetMap contributors · " + CREDIT,
            transform=ax.transAxes, fontsize=6.6, color=STONE, ha="right", va="top")
    fig.savefig(OUT / "kern.png", dpi=200, bbox_inches="tight")

if __name__ == "__main__":
    main()
