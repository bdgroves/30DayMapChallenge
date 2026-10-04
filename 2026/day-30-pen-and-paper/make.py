"""
Day 30 · Pen & paper — If you hit the third cattle guard

The hot-spring napkin from my memoir story "If You Hit the Third Cattle Guard", redrawn by hand
in bar ink on a cocktail napkin: the springs outside Bridgeport, the roads between them, and the
notes around the edge, counted in cattle guards. Drawn from memory, the way the original was, and
left that way: no real map laid over it, no coordinates.

This script only frames the scan. Put the scan (or a good phone photo, shot flat in daylight)
next to this file as napkin.jpg or napkin.png and render. The story's link goes in days.yml
(post:) on the day.
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402

from PIL import Image  # noqa: E402

DAY = 30
HERE = Path(__file__).resolve().parent
scan = next((p for p in (HERE / "napkin.jpg", HERE / "napkin.png") if p.exists()), None)
if scan is None:
    print("= no napkin yet: put napkin.jpg (or .png) in day-30-pen-and-paper/ and render again")
    sys.exit(0)

im = Image.open(scan).convert("RGB")
print(f"= napkin scan {im.size[0]}x{im.size[1]}")
fig, ax = dmc.figure("portrait", map_box=(0.08, 0.10, 0.84, 0.70))
ax.imshow(im)
ax.set_aspect("equal")
dmc.frame(
    fig, DAY,
    title="If you hit the third cattle guard",
    subtitle=("The hot springs outside Bridgeport, drawn from memory on a bar napkin, the way a stranger drew them\n"
              "for us at the Iron Door forty years ago: no miles, just cattle guards. Close the gate. Go around the guns."),
    source="Pen, a cocktail napkin, and memory",
    note="The story behind it: \"If You Hit the Third Cattle Guard\", at memoir.brooksgroves.com",
)
dmc.save(fig, DAY, alt=(
    "A hand-drawn map on a cocktail napkin in ink: hot springs outside Bridgeport, California joined by rough lines for "
    "dirt roads, with notes around the edge counting cattle guards instead of miles."))
