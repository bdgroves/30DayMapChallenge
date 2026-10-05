"""Pyramid Lake and the lower Truckee River: dams, the dry Winnemucca Lake basin, and (if present)
GBIF records of cui-ui and Lahontan cutthroat trout. Writes out/pyramid.png."""
import sys
from pathlib import Path
import numpy as np
import pandas as pd
from matplotlib.colors import to_rgb
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from mapkit import *   # noqa: F403

HERE = Path(__file__).resolve().parent
OUT = HERE / "out"; OUT.mkdir(exist_ok=True)

def main():
    style()
    z, ext = read_dem("pyramid")
    w, e, s, n = ext
    k = np.cos(np.radians(39.9))
    hs = hillshade(z, ext, exag=1.3)
    base = np.array(to_rgb(CREAM))
    rgb = np.clip(base * (0.38 + 0.66 * hs[..., None]), 0, 1)
    feats = osm("pyramid")
    fig, ax = plt.subplots(figsize=(8.2, 8.8))
    ax.imshow(rgb, extent=(w * k, e * k, s, n), interpolation="lanczos", zorder=0)
    for name, near in [("Pyramid Lake", (-119.55, 40.0)), ("Lahontan Reservoir", (-119.1, 39.4))]:
        draw(ax, polygon(feats, name, near), k, color="#2f78a3", ec="none", zorder=3)
    draw(ax, polygon(feats, "Anaho Island", (-119.51, 39.95)), k, color=CREAM, ec="none", zorder=4)
    draw(ax, lines(feats, {"Truckee River"}), k, color="#2f78a3", lw=1.3, zorder=3)
    # GBIF records, if the downloads are here
    D = HERE.parent / "data"
    for slug, col, lbl in [("chasmistes-cujus", RUST, "Cui-ui"), ("oncorhynchus-clarkii-henshawi", GOLD, "Lahontan cutthroat trout")]:
        f = D / f"{slug}.csv.gz"
        if f.exists():
            d = pd.read_csv(f, low_memory=False)
            d = d[d.lon.between(w, e) & d.lat.between(s, n)]
            ax.scatter(d.lon * k, d.lat, s=9, color=col, ec=PAPER, lw=0.4, zorder=5, label=f"{lbl} ({len(d):,} GBIF records)")
    marks = [("Derby Dam, 1905", -119.4474, 39.5855, "right", (-6, 4)),
             ("Marble Bluff Dam\n& fishway, 1976", -119.3931, 39.8552, "left", (8, -2)),
             ("Numana Dam", -119.3490, 39.7896, "left", (8, -2))]
    for t, lo, la, ha, off in marks:
        ax.plot(lo * k, la, marker="s", ms=5, color=INK, mec=PAPER, mew=0.8, zorder=7)
        ax.annotate(t, (lo * k, la), xytext=off, textcoords="offset points", ha=ha, va="center", fontsize=8.4, color=INK, zorder=8,
                    bbox=dict(boxstyle="round,pad=0.15", fc=CREAM, ec="none", alpha=0.8))
    for t, lo, la, dx, dy, ha in [("Reno", -119.813, 39.529, -4, 4, "right"), ("Sparks", -119.75, 39.535, 4, 4, "left"),
                                  ("Wadsworth", -119.284, 39.633, -5, 3, "right"), ("Nixon", -119.358, 39.828, 6, -3, "left"),
                                  ("Sutcliffe", -119.603, 39.952, -5, 3, "right")]:
        ax.plot(lo * k, la, "o", ms=2.8, color=INK, zorder=6)
        ax.annotate(t, (lo * k, la), xytext=(dx, dy), textcoords="offset points", fontsize=7.8, color=INK, ha=ha,
                    va="bottom" if dy > 0 else "top", zorder=6)
    for t, lo, la, rot in [("Pyramid Lake", -119.60, 40.10, -52), ("Winnemucca Lake\n(dry)", -119.33, 40.22, 0),
                           ("Truckee River", -119.56, 39.62, 0), ("Anaho Island", -119.51, 39.98, 0)]:
        ax.text(lo * k, la, t, fontsize=9 if t == "Pyramid Lake" else 8.2, color="#f5f0e8" if t in ("Pyramid Lake", "Anaho Island") else "#1f4f68",
                style="italic", ha="center", va="center", rotation=rot, zorder=6, fontweight="medium" if t == "Pyramid Lake" else "normal")
    ax.set_xlim((w + 0.02) * k, (e - 0.02) * k); ax.set_ylim(s + 0.08, n - 0.02)
    ax.set_aspect("equal"); ax.axis("off")
    if ax.get_legend_handles_labels()[0]:
        ax.legend(loc="lower right", frameon=True, facecolor=PAPER, edgecolor="none", fontsize=8.2, markerscale=1.6)
    ax.text(0, 1.05, "The Truckee runs to Pyramid Lake", transform=ax.transAxes, fontsize=14, color=INK, fontweight="medium")
    ax.text(0, 1.018, "The lake's only big river, and the three dams that shaped what lives in it",
            transform=ax.transAxes, fontsize=9, color=STONE)
    ax.text(1, -0.012, "Elevation: Copernicus GLO-90 · Water and dams: © OpenStreetMap contributors · " + CREDIT,
            transform=ax.transAxes, fontsize=6.6, color=STONE, ha="right", va="top")
    fig.savefig(OUT / "pyramid.png", dpi=200, bbox_inches="tight")

if __name__ == "__main__":
    main()
