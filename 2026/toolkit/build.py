"""
Build everything that comes from days.yml:

  day-NN-theme/README.md   the plan block at the top (your own notes below it are kept)
  day-NN-theme/out/        where each day's finished map goes (map.png, alt.txt)
  day-NN-theme/out/thumb.jpg   made from map.png for the gallery
  day-NN-theme/out/card.jpg    a 1200x630 card for link previews on X, LinkedIn and the like
  share/day-NN.html        a tiny page per day with that card in its preview tags; it forwards to the
                           gallery, so a shared link shows the map and lands on it
  card.jpg                 the gallery's own preview card
  days.json                what the gallery page on brooksgroves.com reads
  README.md                the 2026 overview table

Run from anywhere:  python 2026/toolkit/build.py     (or: pixi run build)
Needs only PyYAML and Pillow.
"""
from __future__ import annotations

import json
import re
from datetime import date, timedelta
from pathlib import Path

import yaml

ROOT = Path(__file__).resolve().parent.parent          # 2026/
REPO = "bdgroves/30DayMapChallenge"
RAW = f"https://raw.githubusercontent.com/{REPO}/main/2026"
SITE = "https://brooksgroves.com/30DayMapChallenge/2026"
FONTS = Path(__file__).resolve().parent / "fonts"
PAPER, INK, STONE, LAVA, MIST = (245, 240, 232), (28, 26, 22), (122, 114, 104), (192, 57, 43), (214, 208, 196)
START, END = "<!-- plan:start (generated from days.yml; edit there) -->", "<!-- plan:end -->"
STATUS = {"idea": "💡 idea", "data": "📦 data in hand", "draft": "✏️ draft", "done": "✅ done", "posted": "📣 posted"}
ORDER = list(STATUS)


def slug(theme: str) -> str:
    s = theme.lower().replace("&", "and")
    return re.sub(r"[^a-z0-9]+", "-", s).strip("-")


def plan_block(d: dict, when: date) -> str:
    lines = [START, f"# Day {d['day']} · {d['theme']}", "",
             f"**{when:%A, %B} {when.day}, {when.year}** · status: {STATUS[d['status']]}", "",
             f"## {d['title']}", "", d["pitch"].strip(), ""]
    if d.get("data"):
        lines += ["**Data**", ""]
        for x in d["data"]:
            lines.append(f"- [{x['name']}]({x['url']})" + (f" — {x['note']}" if x.get("note") else ""))
        lines.append("")
    if d.get("tools"):
        lines += [f"**Tools:** {', '.join(d['tools'])}", ""]
    if d.get("prep"):
        lines += [f"**Prep:** {d['prep']}", ""]
    lines += ["Folder: `data/` for downloads (not committed), `out/` for the finished map (`map.png`, `alt.txt`).",
              END]
    return "\n".join(lines)


def write_readme(folder: Path, block: str) -> None:
    p = folder / "README.md"
    if p.exists():
        old = p.read_text(encoding="utf-8")
        if START in old and END in old:
            new = old[:old.index(START)] + block + old[old.index(END) + len(END):]
        else:
            new = block + "\n\n" + old
    else:
        new = block + "\n\n## Notes\n\n"
    if not p.exists() or new != p.read_text(encoding="utf-8"):
        p.write_text(new, encoding="utf-8")


def thumb(png: Path) -> Path | None:
    t = png.parent / "thumb.jpg"
    # always remade: a fresh checkout gives every file the same time, so timestamps can't say
    # whether the map changed. JPEG output is deterministic, so an unchanged map makes no diff.
    try:
        from PIL import Image
    except ImportError:
        return t if t.exists() else None
    src = png.parent / "crop.jpg"                      # just the map, when the render saved one
    im = Image.open(src if src.exists() else png).convert("RGB")
    im.thumbnail((720, 900))
    im.save(t, quality=84, optimize=True)
    return t


def _font(name: str, size: int):
    from PIL import ImageFont
    return ImageFont.truetype(str(FONTS / name), size)


def _wrap(draw, text: str, font, width: int) -> list[str]:
    lines, cur = [], ""
    for w in text.split():
        trial = f"{cur} {w}".strip()
        if draw.textlength(trial, font=font) <= width or not cur:
            cur = trial
        else:
            lines.append(cur)
            cur = w
    return lines + [cur] if cur else lines


def _card_text(im, x: int, kicker: str, title: str, width: int, blurb: str = "") -> None:
    from PIL import ImageDraw
    dr = ImageDraw.Draw(im)
    W, H = im.size
    dr.rectangle((0, 0, W, 8), fill=LAVA)
    dr.text((x, 70), kicker, font=_font("DMMono-Medium.ttf", 22), fill=LAVA)
    size = 64
    while size > 34:
        f = _font("PlayfairDisplay-Black.ttf", size)
        lines = _wrap(dr, title, f, width)
        if len(lines) <= 3:
            break
        size -= 4
    y = 118
    for ln in lines:
        dr.text((x, y), ln, font=f, fill=INK)
        y += int(size * 1.12)
    if blurb:
        f = _font("LibreBaskerville-Regular.ttf", 21)
        y += 22
        lines = _wrap(dr, blurb, f, width)
        for i, ln in enumerate(lines[:7]):
            if i == 6 and len(lines) > 7:
                ln = ln.rstrip(",;:. ") + "…"
            dr.text((x, y), ln, font=f, fill=STONE)
            y += 33
    dr.line((x, H - 92, x + width, H - 92), fill=MIST, width=2)
    dr.text((x, H - 72), "BROOKS GROVES · BROOKSGROVES.COM", font=_font("DMMono-Regular.ttf", 19), fill=STONE)


def card(png: Path, d: dict) -> Path | None:
    """1200x630 link-preview card: the map on the left, the day and title on the right."""
    try:
        from PIL import Image
    except ImportError:
        return None
    c = png.parent / "card.jpg"
    src = png.parent / "crop.jpg"
    m = Image.open(src if src.exists() else png).convert("RGB")
    im = Image.new("RGB", (1200, 630), PAPER)
    box_w, box_h = 560, 566
    m.thumbnail((box_w, box_h), Image.LANCZOS)
    mx, my = 32 + (box_w - m.width) // 2, 32 + (box_h - m.height) // 2
    im.paste(m, (mx, my))
    first = re.split(r"(?<=[.!?])\s", d["pitch"].strip(), maxsplit=1)[0]
    _card_text(im, 640, f"#30DAYMAPCHALLENGE · DAY {d['day']:02d} · {d['theme'].upper()}", d["title"], 510, first)
    im.save(c, quality=86, optimize=True)
    return c


def gallery_card(out: list[dict]) -> None:
    try:
        from PIL import Image
    except ImportError:
        return
    im = Image.new("RGB", (1200, 630), PAPER)
    thumbs = [ROOT / Path(x["folder"]).name / "out" / "thumb.jpg" for x in out if x["thumb"]][:6]
    cw, ch, gap = 172, 268, 12
    for i, t in enumerate(thumbs):
        m = Image.open(t).convert("RGB")
        r = max(cw / m.width, ch / m.height)
        m = m.resize((round(m.width * r), round(m.height * r)), Image.LANCZOS)
        m = m.crop(((m.width - cw) // 2, (m.height - ch) // 2, (m.width - cw) // 2 + cw, (m.height - ch) // 2 + ch))
        im.paste(m, (32 + (i % 3) * (cw + gap), 32 + (i // 3) * (ch + gap)))
    _card_text(im, 640, "#30DAYMAPCHALLENGE · NOV 2026", "Thirty maps in thirty days", 510,
               "One map a day on the official themes, mostly about places I know: the Sierra foothills, "
               "Mount Rainier, Nevada, and wherever geocaching has taken me.")
    im.save(ROOT / "card.jpg", quality=86, optimize=True)


def share_page(x: dict, has_card: bool) -> None:
    """A page whose preview tags carry this day's card, forwarding people to the gallery."""
    from html import escape as e
    n = x["day"]
    name = Path(x["folder"]).name
    img = f"{SITE}/{name}/out/card.jpg" if has_card else f"{SITE}/card.jpg"
    url = f"{SITE}/share/day-{n:02d}.html"
    dest = f"../#day-{n}"
    title = f"Day {n} · {x['theme']}: {x['title']}"
    desc = x["alt"] or x["pitch"]
    if len(desc) > 200:
        desc = desc[:197].rsplit(" ", 1)[0] + "…"
    html = f"""<!DOCTYPE html>
<html lang="en"><head><meta charset="UTF-8">
<title>{e(title)} — #30DayMapChallenge 2026</title>
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta name="description" content="{e(desc)}">
<link rel="canonical" href="{url}">
<meta property="og:type" content="article">
<meta property="og:site_name" content="Brooks Groves">
<meta property="og:title" content="{e(title)}">
<meta property="og:description" content="{e(desc)}">
<meta property="og:url" content="{url}">
<meta property="og:image" content="{img}">
<meta property="og:image:width" content="1200"><meta property="og:image:height" content="630">
<meta property="og:image:alt" content="{e(x['alt'] or title)}">
<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:title" content="{e(title)}">
<meta name="twitter:description" content="{e(desc)}">
<meta name="twitter:image" content="{img}">
<meta http-equiv="refresh" content="0; url={dest}">
<script>location.replace({json.dumps(dest)})</script>
<style>body{{background:#f5f0e8;color:#1c1a16;font:16px Georgia,serif;padding:40px}}a{{color:#c0392b}}</style>
</head><body><p><a href="{dest}">{e(title)} →</a></p></body></html>
"""
    p = ROOT / "share" / f"day-{n:02d}.html"
    if not p.exists() or p.read_text(encoding="utf-8") != html:
        p.write_text(html, encoding="utf-8")


def main() -> None:
    plan = yaml.safe_load((ROOT / "days.yml").read_text(encoding="utf-8"))
    start = date.fromisoformat(str(plan["start"]))
    days = sorted(plan["days"], key=lambda d: d["day"])
    assert [d["day"] for d in days] == list(range(1, 31)), "days.yml needs exactly days 1-30"
    out = []
    for d in days:
        assert d["status"] in STATUS, f"day {d['day']}: status must be one of {ORDER}"
        when = start + timedelta(days=d["day"] - 1)
        (ROOT / "share").mkdir(exist_ok=True)
        folder = ROOT / f"day-{d['day']:02d}-{slug(d['theme'])}"
        (folder / "out").mkdir(parents=True, exist_ok=True)
        (folder / "data").mkdir(exist_ok=True)
        write_readme(folder, plan_block(d, when))
        png = folder / "out" / "map.png"
        alt = folder / "out" / "alt.txt"
        t = thumb(png) if png.exists() else None
        cd = card(png, d) if png.exists() else None
        rel = folder.name
        out.append({
            "day": d["day"], "date": when.isoformat(), "weekday": f"{when:%a}", "theme": d["theme"],
            "title": d["title"], "pitch": d["pitch"].strip(), "status": d["status"],
            "tools": d.get("tools", []), "data": [x["name"] for x in d.get("data", [])],
            "folder": f"https://github.com/{REPO}/tree/main/2026/{rel}",
            "image": f"{RAW}/{rel}/out/map.png" if png.exists() else None,
            "thumb": f"{RAW}/{rel}/out/thumb.jpg" if t else None,
            "alt": alt.read_text(encoding="utf-8").strip() if alt.exists() else "",
            "post": d.get("post"),
            "link": d.get("link"),
            "share": f"{SITE}/share/day-{d['day']:02d}.html",
        })
        share_page(out[-1], cd is not None)
    gallery_card(out)
    counts = {s: sum(1 for x in out if x["status"] == s) for s in ORDER}
    manifest = {"year": plan["year"], "start": start.isoformat(), "hashtag": plan["hashtag"],
                "author": plan["author"], "repo": f"https://github.com/{REPO}/tree/main/2026",
                "counts": counts, "days": out}
    (ROOT / "days.json").write_text(json.dumps(manifest, indent=1, ensure_ascii=False) + "\n", encoding="utf-8")

    rows = ["| Day | Date | Theme | Map | Status |", "|---:|---|---|---|---|"]
    for x in out:
        name = Path(x["folder"]).name
        rows.append(f"| {x['day']} | {x['weekday']} {date.fromisoformat(x['date']):%b} {int(x['date'][-2:])} | "
                    f"{x['theme']} | [{x['title']}]({name}/) | {STATUS[x['status']]} |")
    done = counts["done"] + counts["posted"]
    readme = ROOT / "README.md"
    head = readme.read_text(encoding="utf-8").split("<!-- table:start -->")[0] if readme.exists() else ""
    tail = readme.read_text(encoding="utf-8").split("<!-- table:end -->")[1] if readme.exists() and "<!-- table:end -->" in readme.read_text(encoding="utf-8") else ""
    table = (f"<!-- table:start -->\n**{done} of 30 done** · " +
             " · ".join(f"{STATUS[s]} {counts[s]}" for s in ORDER) + "\n\n" + "\n".join(rows) + "\n<!-- table:end -->")
    readme.write_text(head + table + tail, encoding="utf-8")
    print(f"built 30 days: " + ", ".join(f"{s} {counts[s]}" for s in ORDER))


if __name__ == "__main__":
    main()
