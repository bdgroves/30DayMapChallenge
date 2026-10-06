"""
Day 25 · Is this a map? — A day of ground motion

Twenty-four hours of the seismometer at Camp Muir (UW.RCM, 10,100 ft on Mount Rainier), one
line per hour, the way a helicorder drum draws it: July 8, 2025, Pacific time, the day the largest
earthquake swarm ever recorded at Rainier began. Earthquakes the USGS located within 15 km that day are
marked above their line. It's a map of a day at a place. Or a picture of time.

Downloads (cached in data/): EarthScope FDSN station and dataselect, USGS ComCat.
"""
import io
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import numpy as np  # noqa: E402
import pandas as pd  # noqa: E402
from obspy import UTCDateTime, read  # noqa: E402

DAY = 25
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
NET, STA = "UW", "RCM"
LOCAL = datetime(2025, 7, 8)                       # the Pacific day drawn
OFFSET = -7                                         # PDT
START = (LOCAL - timedelta(hours=OFFSET)).replace(tzinfo=timezone.utc)
END = START + timedelta(hours=24)
FDSN = "https://service.earthscope.org/fdsnws"
SUMMIT = (-121.7603, 46.8529)

# ── which vertical channel was running that day ──────────────────────────────
txt = fetch.get(f"{FDSN}/station/1/query?net={NET}&sta={STA}&level=channel&format=text"
                f"&starttime={START:%Y-%m-%dT%H:%M:%S}&endtime={END:%Y-%m-%dT%H:%M:%S}", DATA / "channels.txt").decode()
chans = [ln.split("|") for ln in txt.splitlines() if ln and not ln.startswith("#")]
zs = [(c[2], c[3], float(c[14])) for c in chans if c[3].endswith("Z")]
pick = None
for pref in ("HHZ", "BHZ", "EHZ", "SHZ"):
    pick = next((z for z in zs if z[1] == pref), None)
    if pick:
        break
if not pick:
    raise SystemExit(f"no vertical channel at {NET}.{STA}: {zs}")
loc, cha, sr = pick
print(f"= {NET}.{STA}.{loc or '--'}.{cha} at {sr:g} Hz")

raw = fetch.get(f"{FDSN}/dataselect/1/query?net={NET}&sta={STA}&loc={loc or '--'}&cha={cha}"
                f"&start={START:%Y-%m-%dT%H:%M:%S}&end={END:%Y-%m-%dT%H:%M:%S}", DATA / f"{STA}.mseed", timeout=900)
st = read(io.BytesIO(raw))
st.merge(fill_value=0)
st.detrend("demean")
st.filter("bandpass", freqmin=1, freqmax=10, corners=2)
tr = st[0]
t0 = UTCDateTime(START)
data = tr.data.astype(float)
srate = tr.stats.sampling_rate
offs = int(round((tr.stats.starttime - t0) * srate))
print(f"= {len(data) / srate / 3600:.1f} h of data, starting {tr.stats.starttime}")

# ── the day's earthquakes ────────────────────────────────────────────────────
qcsv = fetch.get("https://earthquake.usgs.gov/fdsnws/event/1/query?format=csv"
                 f"&starttime={START:%Y-%m-%dT%H:%M:%S}&endtime={END:%Y-%m-%dT%H:%M:%S}"
                 f"&latitude={SUMMIT[1]}&longitude={SUMMIT[0]}&maxradiuskm=15&orderby=time-asc", DATA / "quakes.csv")
q = pd.read_csv(io.BytesIO(qcsv)) if qcsv.strip() else pd.DataFrame(columns=["time", "mag"])
print(f"= {len(q)} located earthquakes within 15 km, largest M{q['mag'].max() if len(q) else 0:.1f}")

# ── draw ─────────────────────────────────────────────────────────────────────
fig, ax = dmc.figure("portrait", map_box=(0.10, 0.115, 0.84, 0.70))
per = int(3600 * srate)
scale = np.percentile(np.abs(data[data != 0]), 99.5) * 2.2 if np.any(data) else 1
colours = [dmc.INK, dmc.LAVA, dmc.LAKE, dmc.SAGE]
for h in range(24):
    a = max(0, h * per - offs)
    b = max(0, (h + 1) * per - offs)
    seg = data[a:b]
    if len(seg) == 0:
        continue
    x = (np.arange(len(seg)) + (a + offs - h * per)) / per * 60
    y = -h + np.clip(seg / scale, -1.6, 1.6) * 0.45
    step = max(1, len(seg) // 6000)            # thin for drawing; keep peaks with min/max pairs
    if step > 1:
        n = len(seg) // step * step
        xs = x[:n].reshape(-1, step)[:, [0, -1]].ravel()
        ys = np.c_[y[:n].reshape(-1, step).min(1), y[:n].reshape(-1, step).max(1)].ravel()
    else:
        xs, ys = x, y
    ax.plot(xs, ys, color=colours[h % 4], lw=0.35, solid_joinstyle="round")
for _, e in q.iterrows():
    t = pd.Timestamp(e["time"]).tz_localize(None) if pd.Timestamp(e["time"]).tzinfo is None else pd.Timestamp(e["time"]).tz_convert(None)
    sec = (t - pd.Timestamp(START.replace(tzinfo=None))).total_seconds()
    h, m = int(sec // 3600), (sec % 3600) / 60
    ax.scatter([m], [-h + 0.48], s=6 + 10 * max(0, e["mag"]), color=dmc.GOLD, edgecolor=dmc.INK, lw=0.3, zorder=5)
ax.set_xlim(0, 60)
ax.set_ylim(-23.8, 0.8)
ax.set_axis_on()
for sp in ax.spines.values():
    sp.set_visible(False)
ax.set_yticks([-h for h in range(0, 24, 2)])
ax.set_yticklabels([f"{(h % 12) or 12} {'am' if h < 12 else 'pm'}" for h in range(0, 24, 2)], family=dmc.MONO, fontsize=7.5)
ax.set_xticks([0, 15, 30, 45, 60])
ax.set_xticklabels(["0", "15", "30", "45", "60 min"], family=dmc.MONO, fontsize=7.5)
ax.tick_params(length=0, colors=dmc.STONE)
ax.grid(axis="x", color=dmc.MIST, lw=0.5)

dmc.frame(
    fig, DAY,
    subtitle=(f"Twenty-four hours of the seismometer at Camp Muir, 10,100 ft up Mount Rainier, on July 8, 2025:\n"
              f"one line per hour, like a drum recorder. Gold dots are the {len(q)} earthquakes the USGS located that day."),
    source=f"EarthScope FDSN ({NET}.{STA}.{loc or '--'}.{cha}, 1-10 Hz) · USGS ComCat",
    note="Pacific Daylight Time. The swarm that began early that morning is the largest ever recorded at Rainier.",
)
dmc.save(fig, DAY, alt=(
    f"A helicorder: 24 horizontal wiggly lines, one per hour of July 8, 2025, recorded at Camp Muir on Mount Rainier, "
    f"in alternating ink, red, blue and green. Gold dots mark the {len(q)} earthquakes located nearby that day."))
