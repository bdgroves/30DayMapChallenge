"""
Day 14 · Borgesian map — the still for the posts

The map itself is index.html, an interactive page. This takes the 1600 × 1000 screenshot the
posts and gallery use (out/map.png) from that page, as it opens, with headless Chrome (installed on
GitHub's runners), so the picture always matches the page's current text. out/alt.txt is kept.
"""
import shutil
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
OUT = HERE / "out"
OUT.mkdir(exist_ok=True)
chrome = next((c for c in ("google-chrome", "google-chrome-stable", "chromium", "chromium-browser") if shutil.which(c)), None)
if not chrome:
    sys.exit("no Chrome or Chromium on this machine for the screenshot")
shot = OUT / "map_new.png"
subprocess.run([chrome, "--headless=new", "--hide-scrollbars", "--window-size=1600,1000", "--force-device-scale-factor=1",
                "--virtual-time-budget=20000", "--run-all-compositor-stages-before-draw",
                f"--screenshot={shot}", (HERE / "index.html").as_uri()], check=True, timeout=180)
if not shot.exists() or shot.stat().st_size < 20_000:
    sys.exit("the screenshot came out empty; keeping the old out/map.png")
shot.replace(OUT / "map.png")
print(f"= screenshot of index.html saved, {(OUT / 'map.png').stat().st_size // 1024} KB")
