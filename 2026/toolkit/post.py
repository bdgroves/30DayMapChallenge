"""
Post a day's map to X and Bluesky, so the challenge keeps going while I'm away.

  python 2026/toolkit/post.py --preview all     show what every day would post (no network, no keys)
  python 2026/toolkit/post.py --preview 12      just one day
  python 2026/toolkit/post.py                   post today's day (Pacific time) for real
  python 2026/toolkit/post.py --day 12          post day 12 for real

Rules, so nothing goes out by accident:
  * only a day whose status in days.yml is `done` is posted; anything else is skipped and logged
  * the map must exist at day-NN-theme/out/map.png
  * each platform is posted at most once per day: posted.json remembers what went out, so re-running
    (or the retry run later the same day) only fills in a platform that failed
  * a platform without its keys is skipped, not an error
  * when every platform with keys has posted, the day's status becomes `posted`

Keys (GitHub secrets in Actions, environment variables locally):
  X:        X_API_KEY, X_API_SECRET, X_ACCESS_TOKEN, X_ACCESS_SECRET   (OAuth 1.0a, Read and Write)
  Bluesky:  BSKY_HANDLE, BSKY_APP_PASSWORD                             (an app password, not the real one)

Needs PyYAML and Pillow; posting also needs requests, requests-oauthlib and atproto.
"""
from __future__ import annotations

import argparse
import io
import json
import os
import re
import sys
from datetime import date, datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

import yaml
from PIL import Image

sys.path.insert(0, str(Path(__file__).resolve().parent))
from build import SITE, post_text, slug  # noqa: E402  (same text as the "Post it" block and the gallery)

ROOT = Path(__file__).resolve().parent.parent          # 2026/
POSTED = ROOT / "posted.json"
PACIFIC = ZoneInfo("America/Los_Angeles")
MAX_BYTES = 950_000                                    # Bluesky's image limit is ~976 KB; X allows 5 MB
X_LIMIT, BSKY_LIMIT = 280, 300
X_URL_LEN = 23                                         # X counts every link as 23 characters


def load() -> tuple[dict, list[dict]]:
    plan = yaml.safe_load((ROOT / "days.yml").read_text(encoding="utf-8"))
    return plan, sorted(plan["days"], key=lambda d: d["day"])


def folder(d: dict) -> Path:
    return ROOT / f"day-{d['day']:02d}-{slug(d['theme'])}"


def share_url(d: dict) -> str:
    return f"{SITE}/share/day-{d['day']:02d}.html"


def text_for(d: dict) -> str:
    return post_text(d) + "\n" + share_url(d)


def x_length(text: str) -> int:
    return len(re.sub(r"https?://\S+", "x" * X_URL_LEN, text))


def image_for(d: dict) -> tuple[bytes, str] | None:
    """The map as a JPEG small enough for both platforms, plus its alt text."""
    png = folder(d) / "out" / "map.png"
    if not png.exists():
        return None
    im = Image.open(png).convert("RGB")
    side = 2400
    while True:
        im2 = im.copy()
        im2.thumbnail((side, side), Image.LANCZOS)
        for q in (90, 85, 80, 75):
            buf = io.BytesIO()
            im2.save(buf, "JPEG", quality=q, optimize=True, progressive=True)
            if buf.tell() <= MAX_BYTES:
                alt_file = folder(d) / "out" / "alt.txt"
                alt = alt_file.read_text(encoding="utf-8").strip() if alt_file.exists() else ""
                return buf.getvalue(), alt or f"Map: {d['title']}"
        side = int(side * 0.8)


def day_for_today(plan: dict) -> int | None:
    start = date.fromisoformat(str(plan["start"]))
    n = (datetime.now(PACIFIC).date() - start).days + 1
    return n if 1 <= n <= 30 else None


def readiness(d: dict) -> list[str]:
    """Reasons this day can't post yet (empty list = ready)."""
    problems = []
    if d["status"] not in ("done", "posted"):
        problems.append(f"status is '{d['status']}', needs 'done'")
    if not (folder(d) / "out" / "map.png").exists():
        problems.append("no out/map.png")
    elif not (folder(d) / "out" / "alt.txt").exists():
        problems.append("no out/alt.txt (alt text will be just the title)")
    t = text_for(d)
    if x_length(t) > X_LIMIT:
        problems.append(f"too long for X ({x_length(t)} > {X_LIMIT})")
    if len(t) > BSKY_LIMIT:
        problems.append(f"too long for Bluesky ({len(t)} > {BSKY_LIMIT})")
    return problems


# ---------------------------------------------------------------- X

def post_x(text: str, jpg: bytes, alt: str) -> str:
    import requests
    from requests_oauthlib import OAuth1
    auth = OAuth1(os.environ["X_API_KEY"], os.environ["X_API_SECRET"],
                  os.environ["X_ACCESS_TOKEN"], os.environ["X_ACCESS_SECRET"])
    r = requests.post("https://api.x.com/2/media/upload", auth=auth, timeout=120,
                      files={"media": ("map.jpg", jpg, "image/jpeg")},
                      data={"media_category": "tweet_image"})
    if r.status_code >= 300:
        raise RuntimeError(f"X media upload {r.status_code}: {r.text[:400]}")
    media_id = r.json()["data"]["id"]
    try:                                               # alt text is nice to have, never worth failing over
        m = requests.post("https://api.x.com/2/media/metadata", auth=auth, timeout=60,
                          json={"id": media_id, "metadata": {"alt_text": {"text": alt[:1000]}}})
        if m.status_code >= 300:
            print(f"  x: alt text not set ({m.status_code}: {m.text[:200]})")
    except Exception as e:  # noqa: BLE001
        print(f"  x: alt text not set ({e})")
    r = requests.post("https://api.x.com/2/tweets", auth=auth, timeout=60,
                      json={"text": text, "media": {"media_ids": [media_id]}})
    if r.status_code >= 300:
        raise RuntimeError(f"X post {r.status_code}: {r.text[:400]}")
    return f"https://x.com/i/status/{r.json()['data']['id']}"


# ---------------------------------------------------------------- Bluesky

def bsky_text(text: str):
    """Bluesky needs facets for clickable links and hashtags."""
    from atproto import client_utils
    tb = client_utils.TextBuilder()
    for i, line in enumerate(text.split("\n")):
        if i:
            tb.text("\n")
        for tok in re.split(r"(\s+)", line):
            if re.fullmatch(r"https?://\S+", tok):
                tb.link(tok.replace("https://", ""), tok)
            elif re.fullmatch(r"#\w+", tok):
                tb.tag(tok, tok[1:])
            elif tok:
                tb.text(tok)
    return tb


def post_bluesky(text: str, jpg: bytes, alt: str) -> str:
    from atproto import Client
    c = Client()
    c.login(os.environ["BSKY_HANDLE"], os.environ["BSKY_APP_PASSWORD"])
    res = c.send_image(text=bsky_text(text), image=jpg, image_alt=alt[:2000])
    rkey = res.uri.rsplit("/", 1)[-1]
    return f"https://bsky.app/profile/{os.environ['BSKY_HANDLE']}/post/{rkey}"


PLATFORMS = {
    "x": (post_x, ["X_API_KEY", "X_API_SECRET", "X_ACCESS_TOKEN", "X_ACCESS_SECRET"]),
    "bluesky": (post_bluesky, ["BSKY_HANDLE", "BSKY_APP_PASSWORD"]),
}


# ---------------------------------------------------------------- bookkeeping

def mark_posted(day: int) -> None:
    """Flip `status: done` to `status: posted` for this day only, keeping days.yml's comments and layout."""
    p = ROOT / "days.yml"
    s = p.read_text(encoding="utf-8")
    blocks = re.split(r"(?m)(?=^  - day: )", s)
    for i, b in enumerate(blocks):
        if re.match(rf"  - day: {day}\s*$", b.split("\n", 1)[0]):
            new, n = re.subn(r"(?m)^    status: done\s*$", "    status: posted", b)
            assert n == 1, f"day {day}: expected one 'status: done' line, found {n}"
            blocks[i] = new
            break
    else:
        raise SystemExit(f"day {day} not found in days.yml")
    p.write_text("".join(blocks), encoding="utf-8")


def preview(days: list[dict], which: str) -> None:
    pick = days if which == "all" else [d for d in days if d["day"] == int(which)]
    ready = 0
    for d in pick:
        problems = readiness(d)
        ready += not problems
        img = image_for(d) if (folder(d) / "out" / "map.png").exists() else None
        print(f"\n=== Day {d['day']} · {d['theme']} · {'READY' if not problems else 'NOT READY'}")
        print(text_for(d))
        t = text_for(d)
        print(f"--- X {x_length(t)}/{X_LIMIT} · Bluesky {len(t)}/{BSKY_LIMIT}"
              + (f" · image {len(img[0]) // 1000} KB" if img else ""))
        for p in problems:
            print(f"!!! {p}")
    print(f"\n{ready} of {len(pick)} ready to post.")


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--preview", metavar="DAY|all", help="show what would be posted; no network, no keys")
    ap.add_argument("--day", type=int, help="post this day instead of today's")
    ap.add_argument("--only", choices=list(PLATFORMS), help="post to one platform only")
    a = ap.parse_args()

    plan, days = load()
    if a.preview:
        preview(days, a.preview)
        return 0

    n = a.day or day_for_today(plan)
    if n is None:
        print("Today isn't a challenge day; nothing to post.")
        return 0
    d = next(x for x in days if x["day"] == n)
    problems = [p for p in readiness(d) if not p.startswith("no out/alt.txt")]
    if problems:
        print(f"::warning title=Day {n} not posted::" + "; ".join(problems))
        return 0

    posted = json.loads(POSTED.read_text(encoding="utf-8")) if POSTED.exists() else {}
    mine = posted.setdefault(str(n), {})
    text = text_for(d)
    jpg, alt = image_for(d)
    failed, tried = [], []
    for name, (fn, keys) in PLATFORMS.items():
        if a.only and name != a.only:
            continue
        if mine.get(name):
            print(f"  {name}: already posted ({mine[name]})")
            continue
        if not all(os.environ.get(k) for k in keys):
            print(f"  {name}: no keys, skipped")
            continue
        tried.append(name)
        try:
            url = fn(text, jpg, alt)
            mine[name] = url
            print(f"::notice title=Day {n} on {name}::{url}")
        except Exception as e:  # noqa: BLE001
            failed.append(name)
            print(f"::error title=Day {n} on {name} failed::{e}")
        POSTED.write_text(json.dumps(posted, indent=1, sort_keys=True) + "\n", encoding="utf-8")

    configured = [k for k, (_, keys) in PLATFORMS.items() if all(os.environ.get(x) for x in keys)]
    if configured and all(mine.get(k) for k in configured) and d["status"] == "done":
        mark_posted(n)
        print(f"  day {n}: status -> posted")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
