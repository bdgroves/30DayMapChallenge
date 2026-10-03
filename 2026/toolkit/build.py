"""
Build everything that comes from days.yml:

  day-NN-theme/README.md   the plan block at the top (your own notes below it are kept)
  day-NN-theme/out/        where each day's finished map goes (map.png, alt.txt)
  day-NN-theme/out/thumb.jpg   made from map.png for the gallery
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
    if t.exists() and t.stat().st_mtime >= png.stat().st_mtime:
        return t
    try:
        from PIL import Image
    except ImportError:
        return t if t.exists() else None
    im = Image.open(png).convert("RGB")
    im.thumbnail((720, 900))
    im.save(t, quality=84, optimize=True)
    return t


def main() -> None:
    plan = yaml.safe_load((ROOT / "days.yml").read_text(encoding="utf-8"))
    start = date.fromisoformat(str(plan["start"]))
    days = sorted(plan["days"], key=lambda d: d["day"])
    assert [d["day"] for d in days] == list(range(1, 31)), "days.yml needs exactly days 1-30"
    out = []
    for d in days:
        assert d["status"] in STATUS, f"day {d['day']}: status must be one of {ORDER}"
        when = start + timedelta(days=d["day"] - 1)
        folder = ROOT / f"day-{d['day']:02d}-{slug(d['theme'])}"
        (folder / "out").mkdir(parents=True, exist_ok=True)
        (folder / "data").mkdir(exist_ok=True)
        write_readme(folder, plan_block(d, when))
        png = folder / "out" / "map.png"
        alt = folder / "out" / "alt.txt"
        t = thumb(png) if png.exists() else None
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
        })
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
