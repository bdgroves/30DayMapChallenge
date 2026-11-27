"""Build the round-up image for the #30DayMapChallenge 2026 wrap-up post: all thirty maps in a grid.

    python social/2026-11-30-dmc-roundup/make.py

Reads 2026/days.yml and each day's out/map.png, writes fallback.jpg (under 950 KB for Bluesky)
and fallback_alt.txt next to this script. Rerun after any map changes.
"""
from pathlib import Path

import yaml
from PIL import Image, ImageDraw, ImageFont

HERE = Path(__file__).resolve().parent
Y = HERE.parents[1] / "2026"
FONTS = Y / "toolkit" / "fonts"
INK, PARCHMENT, STONE = "#1c1a16", "#f5f0e8", "#7a7268"

COLS, ROWS, TILE, GAP, PAD, HEAD, FOOT = 6, 5, 300, 12, 40, 170, 70
W = PAD * 2 + COLS * TILE + (COLS - 1) * GAP
H = HEAD + ROWS * TILE + (ROWS - 1) * GAP + FOOT

days = sorted(yaml.safe_load((Y / "days.yml").read_text(encoding="utf-8"))["days"], key=lambda d: d["day"])
assert len(days) == 30, len(days)


def folder(d):
    return next(Y.glob(f"day-{d['day']:02d}-*"))


def cover(im, size, zoom=0.72, cy=0.56):
    """Square crop from the body of the map (below the title, above the credits), then scale."""
    w, h = im.size
    s = int(min(w, h) * zoom)
    x0 = (w - s) // 2
    y0 = min(max(int(h * cy - s / 2), 0), h - s)
    return im.crop((x0, y0, x0 + s, y0 + s)).resize((size, size), Image.LANCZOS)


canvas = Image.new("RGB", (W, H), PARCHMENT)
dr = ImageDraw.Draw(canvas)
kick = ImageFont.truetype(str(FONTS / "DMMono-Medium.ttf"), 22)
title = ImageFont.truetype(str(FONTS / "PlayfairDisplay-Bold.ttf"), 64)
num = ImageFont.truetype(str(FONTS / "DMMono-Medium.ttf"), 20)
foot = ImageFont.truetype(str(FONTS / "DMMono-Regular.ttf"), 20)

dr.text((PAD, 34), "#30DAYMAPCHALLENGE · NOVEMBER 2026 · BROOKS GROVES", font=kick, fill=STONE)
dr.text((PAD, 66), "Thirty maps in thirty days", font=title, fill=INK)

for i, d in enumerate(days):
    r, c = divmod(i, COLS)
    x, y = PAD + c * (TILE + GAP), HEAD + r * (TILE + GAP)
    im = Image.open(folder(d) / "out" / "map.png").convert("RGB")
    canvas.paste(cover(im, TILE), (x, y))
    dr.rectangle((x, y, x + TILE - 1, y + TILE - 1), outline="#d8d0c2")
    label = f"{d['day']:02d}"
    bw = dr.textlength(label, font=num) + 16
    dr.rectangle((x, y, x + bw, y + 30), fill=INK)
    dr.text((x + 8, y + 4), label, font=num, fill=PARCHMENT)

dr.text((PAD, H - FOOT + 22), "All thirty, with the stories behind them: brooksgroves.com/30DayMapChallenge/2026",
        font=foot, fill=STONE)

out = HERE / "fallback.jpg"
for q in (90, 85, 80, 75, 70):
    canvas.save(out, "JPEG", quality=q, optimize=True, progressive=True)
    if out.stat().st_size < 950_000:
        break
print(f"{out.name}: {canvas.size}, {out.stat().st_size / 1000:.0f} KB, quality {q}")

titles = "; ".join(f"{d['day']}, {d['title']}" for d in days)
alt = f"Thirty small maps in a grid, numbered 1 to 30, headed \"Thirty maps in thirty days\": {titles}."
assert len(alt) <= 1000, len(alt)  # X cuts alt text at 1,000
(HERE / "fallback_alt.txt").write_text(alt + "\n", encoding="utf-8")
print(f"alt text: {len(alt)} characters")
