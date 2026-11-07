"""Post a one-off (non-challenge) item to X and Bluesky: a video, with a still image as fallback.

    python social/post_one_off.py social/<folder> [--only x|bluesky] [--dry-run]

The folder holds text_x.txt, text_bsky.txt (optional; defaults to text_x.txt), film.mp4,
alt.txt, fallback.jpg, fallback_alt.txt. Results go to <folder>/posted.json; a platform
already listed there is skipped, so a rerun never double-posts.
"""
import json, os, re, sys, time, argparse
from pathlib import Path

ap = argparse.ArgumentParser()
ap.add_argument("folder"); ap.add_argument("--only", choices=["x", "bluesky"]); ap.add_argument("--dry-run", action="store_true")
a = ap.parse_args()
F = Path(a.folder)
rd = lambda n, d="": (F / n).read_text(encoding="utf-8").strip() if (F / n).exists() else d
text_x = rd("text_x.txt"); text_b = rd("text_bsky.txt", text_x)
video = (F / "film.mp4").read_bytes() if (F / "film.mp4").exists() else None
alt = rd("alt.txt"); still = (F / "fallback.jpg").read_bytes(); still_alt = rd("fallback_alt.txt", alt)
done_path = F / "posted.json"
done = json.loads(done_path.read_text()) if done_path.exists() else {}
X_URL = 23
xlen = len(re.sub(r"https?://\S+", "x" * X_URL, text_x)) + sum(1 for c in text_x if ord(c) > 0x10FF)
print(f"X length ~{xlen}/280; video {len(video or b'') / 1e6:.1f} MB; still {len(still) / 1e3:.0f} KB")
if xlen > 280:
    sys.exit("X text too long")


def x_auth():
    from requests_oauthlib import OAuth1
    return OAuth1(os.environ["X_API_KEY"], os.environ["X_API_SECRET"],
                  os.environ["X_ACCESS_TOKEN"], os.environ["X_ACCESS_SECRET"])


def x_upload_video(data):
    import requests
    auth, base = x_auth(), "https://api.x.com/2/media/upload"
    r = requests.post(base + "/initialize", auth=auth, timeout=60,
                      json={"media_type": "video/mp4", "total_bytes": len(data), "media_category": "tweet_video"})
    if r.status_code >= 300:
        raise RuntimeError(f"init {r.status_code}: {r.text[:300]}")
    mid = r.json()["data"]["id"]
    step = 4 * 1024 * 1024
    for i in range(0, len(data), step):
        r = requests.post(f"{base}/{mid}/append", auth=auth, timeout=300,
                          files={"media": ("film.mp4", data[i:i + step], "application/octet-stream")},
                          data={"segment_index": str(i // step)})
        if r.status_code >= 300:
            raise RuntimeError(f"append {r.status_code}: {r.text[:300]}")
    r = requests.post(f"{base}/{mid}/finalize", auth=auth, timeout=120)
    if r.status_code >= 300:
        raise RuntimeError(f"finalize {r.status_code}: {r.text[:300]}")
    info = r.json().get("data", {}).get("processing_info")
    while info and info.get("state") in ("pending", "in_progress"):
        time.sleep(max(2, info.get("check_after_secs", 3)))
        s = requests.get(base, auth=auth, timeout=60, params={"command": "STATUS", "media_id": mid})
        info = s.json().get("data", {}).get("processing_info")
        print("  x: processing", info)
    if info and info.get("state") == "failed":
        raise RuntimeError(f"processing failed: {info}")
    return mid


def x_upload_image(data):
    import requests
    r = requests.post("https://api.x.com/2/media/upload", auth=x_auth(), timeout=120,
                      files={"media": ("map.jpg", data, "image/jpeg")}, data={"media_category": "tweet_image"})
    if r.status_code >= 300:
        raise RuntimeError(f"image upload {r.status_code}: {r.text[:300]}")
    return r.json()["data"]["id"]


def post_x():
    import requests
    kind = "video"
    try:
        mid = x_upload_video(video)
        alt_text = alt
    except Exception as e:
        print("  x: video failed, posting the still instead:", e)
        mid, alt_text, kind = x_upload_image(still), still_alt, "image"
    try:
        requests.post("https://api.x.com/2/media/metadata", auth=x_auth(), timeout=60,
                      json={"id": mid, "metadata": {"alt_text": {"text": alt_text[:1000]}}})
    except Exception as e:
        print("  x: alt text not set", e)
    r = requests.post("https://api.x.com/2/tweets", auth=x_auth(), timeout=60,
                      json={"text": text_x, "media": {"media_ids": [mid]}})
    if r.status_code >= 300:
        raise RuntimeError(f"tweet {r.status_code}: {r.text[:400]}")
    return f"https://x.com/i/status/{r.json()['data']['id']}", kind


def bsky_text(text):
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


def post_bsky():
    from atproto import Client
    c = Client(); c.login(os.environ["BSKY_HANDLE"], os.environ["BSKY_APP_PASSWORD"])
    kind = "video"
    try:
        res = c.send_video(text=bsky_text(text_b), video=video, video_alt=alt[:2000])
    except Exception as e:
        print("  bluesky: video failed, posting the still instead:", e)
        res, kind = c.send_image(text=bsky_text(text_b), image=still, image_alt=still_alt[:2000]), "image"
    rkey = res.uri.rsplit("/", 1)[-1]
    return f"https://bsky.app/profile/{os.environ['BSKY_HANDLE']}/post/{rkey}", kind


if a.dry_run:
    print("--- X ---\n" + text_x + "\n--- Bluesky ---\n" + text_b)
    sys.exit(0)
failed = False
for name, fn in (("x", post_x), ("bluesky", post_bsky)):
    if a.only and a.only != name:
        continue
    if name in done:
        print(f"{name}: already posted {done[name]}")
        continue
    try:
        url, kind = fn()
        done[name] = {"url": url, "media": kind}
        print(f"{name}: posted ({kind}) {url}")
    except Exception as e:
        failed = True
        print(f"::error title={name}::{e}")
    done_path.write_text(json.dumps(done, indent=1))
sys.exit(1 if failed else 0)
