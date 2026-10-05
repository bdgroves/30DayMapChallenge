"""Golden trout records across the West: two native creeks, and a century of stocking.
Writes out/west.png and out/meadows.png; prints counts by state."""
import sys
from pathlib import Path
import numpy as np
import pandas as pd
import geopandas as gpd
from matplotlib.colors import to_rgb
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from mapkit import *   # noqa: F403

HERE = Path(__file__).resolve().parent
OUT = HERE / "out"; OUT.mkdir(exist_ok=True)
GOLDEN = "#d29a1e"
NE = Path(__import__("os").environ.get("NE", "natural-earth-vector"))
KERN = (-118.75, 35.75, -117.95, 36.62)       # the Kern Plateau window used for "home"
ST = {"Ca": "California", "Id": "Idaho", "Mt": "Montana", "Co": "Colorado", "Wy": "Wyoming", "Nv": "Nevada", "Ut": "Utah",
      "Wa": "Washington", "Or": "Oregon", "Nm": "New Mexico", "Az": "Arizona", "Mi": "Michigan"}

def load():
    d = pd.read_csv(HERE.parent / "data" / "oncorhynchus-mykiss-aguabonita.csv.gz", low_memory=False)
    d["state"] = d.state.astype(str).str.replace(r" \(.*", "", regex=True).str.strip().replace(ST)
    w, s, e, n = KERN
    d["home"] = d.lon.between(w, e) & d.lat.between(s, n)
    return d

def west():
    style()
    d = load()
    z, ext = read_dem("west")
    w, e, s, n = ext
    k = np.cos(np.radians(41))
    hs = hillshade(z, ext, exag=6, alt=42)
    base = np.array(to_rgb(CREAM))
    rgb = np.clip(base * (0.55 + 0.5 * hs[..., None]), 0, 1)
    rgb[np.isnan(z)] = to_rgb(PAPER)
    ocean = np.isnan(z) | (z <= 0)
    rgb[ocean] = to_rgb("#dde6e8")
    fig, ax = plt.subplots(figsize=(9, 8.2))
    ax.imshow(rgb, extent=(w * k, e * k, s, n), interpolation="lanczos", zorder=0)
    lines_ = gpd.read_file(NE / "50m_cultural" / "ne_50m_admin_1_states_provinces_lines.shp").cx[w:e, s:n]
    for g in lines_.geometry:
        draw(ax, g, k, color="#9c9384", lw=0.6, zorder=1)
    a = d[~d.home & d.lon.between(w, e) & d.lat.between(s, n)]
    h = d[d.home]
    ax.scatter(a.lon * k, a.lat, s=11, color=GOLDEN, ec=INK, lw=0.3, zorder=4, label=f"Stocked or spread beyond the Kern Plateau ({len(a):,})")
    ax.scatter(h.lon * k, h.lat, s=11, color=RUST, ec=INK, lw=0.3, zorder=5, label=f"On the Kern Plateau, its home ({len(h):,})")
    wk, sk, ek, nk = KERN
    ax.plot(np.array([wk, ek, ek, wk, wk]) * k, [sk, sk, nk, nk, sk], color=INK, lw=0.8, zorder=6)
    ax.annotate("Golden Trout Creek &\nSouth Fork Kern River", (wk * k, (sk + nk) / 2), xytext=(-10, -36), textcoords="offset points",
                ha="right", fontsize=8.4, color=INK, arrowprops=dict(arrowstyle="-", color=INK, lw=0.6))
    for t, lo, la in [("Wind River\nRange", -108.2, 42.75), ("Idaho high\nlakes", -116.6, 43.6), ("Beartooths &\nMontana lakes", -111.0, 46.35),
                      ("High Sierra\nlakes", -121.2, 37.6)]:
        ax.text(lo * k, la, t, fontsize=8, color=INK, ha="center", va="center", style="italic",
                bbox=dict(boxstyle="round,pad=0.12", fc=CREAM, ec="none", alpha=0.7), zorder=7)
    ax.set_xlim((w + 0.3) * k, (e - 0.2) * k); ax.set_ylim(s + 1.5, n - 0.2)
    ax.set_aspect("equal"); ax.axis("off")
    ax.legend(loc="lower left", frameon=True, facecolor=PAPER, edgecolor="none", fontsize=8.4, markerscale=1.4)
    ax.text(0, 1.055, "From two creeks to the whole West", transform=ax.transAxes, fontsize=14, color=INK, fontweight="medium")
    ax.text(0, 1.022, "Every georeferenced California golden trout record in GBIF, 1876 to 2026", transform=ax.transAxes, fontsize=9, color=STONE)
    ax.text(1, -0.01, "Records: GBIF.org (5 October 2026) doi:10.15468/dl.p6g732 · Elevation: Copernicus GLO-90 · Natural Earth · " + CREDIT,
            transform=ax.transAxes, fontsize=6.6, color=STONE, ha="right", va="top")
    fig.savefig(OUT / "west.png", dpi=200, bbox_inches="tight")
    plt.close(fig)
    print("home", len(h), "away", len(d) - len(h), "total", len(d))
    print(d[~d.home].state.value_counts().head(12).to_string())
    print("states/provinces away:", d[~d.home].state.nunique(), "countries", d.country.value_counts().to_dict())
    print("median ground elevation away by state:"); print(d[~d.home].groupby("state").dem_m.agg(["size", "median"]).query("size>=8").round())
    print("earliest year away:", d[~d.home].groupby("state").year.min().dropna().astype(int).sort_values().head(10).to_dict())

def meadows():
    """Summer daily maximum water temperature in three Golden Trout Creek meadows (Nusslé et al. 2015)."""
    style()
    rows = [("Big Whitney Meadow\n2,963 m", 21.2), ("Ramshaw Meadows\n2,640 m", 25.5), ("Mulkey Meadows\n2,838 m", 26.3)]
    fig, ax = plt.subplots(figsize=(8.2, 3.6))
    y = np.arange(len(rows))
    ax.barh(y, [r[1] for r in rows], 0.55, color=[BLUE, RUST, RUST])
    for yi, (_, v) in zip(y, rows):
        ax.text(v + 0.3, yi, f"{v:.1f} °C", va="center", fontsize=9.5, color=INK)
    ax.axvspan(25, 26, color=RUST, alpha=0.10, lw=0)
    ax.text(25.5, 2.55, "25–26 °C: lethal with\nprolonged exposure for\nsome rainbow trout", ha="center", va="bottom", fontsize=7.8, color=INK)
    ax.set_yticks(y, [r[0] for r in rows]); ax.tick_params(axis="y", length=0); ax.spines["left"].set_visible(False)
    ax.set_xlim(0, 30); ax.set_ylim(-0.5, 3.4)
    ax.set_xlabel("Hottest water temperature recorded in the creek (°C)")
    ax.grid(axis="x", color=MIST, lw=0.6); ax.set_axisbelow(True)
    ax.text(0, 1.13, "Hot water in the high country", transform=ax.transAxes, fontsize=13.5, color=INK, fontweight="medium")
    ax.text(0, 1.05, "Daily maximum stream temperatures in golden trout meadows, from Nusslé, Matthews & Carlson (2015)",
            transform=ax.transAxes, fontsize=9, color=STONE)
    fig.savefig(OUT / "meadows.png", dpi=200, bbox_inches="tight")

if __name__ == "__main__":
    west()
    meadows()
