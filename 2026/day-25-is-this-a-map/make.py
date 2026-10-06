"""
Day 25 · Is this a map? — The Beast Quake

January 8, 2011: Seahawks 41, Saints 36, NFC wild card. With the game in the fourth quarter,
Marshawn Lynch broke nine tackles on a 67-yard touchdown run, and the crowd's celebration
shook the ground hard enough to show up on KDK, the Pacific Northwest Seismic Network
station across Occidental Avenue from the stadium. The whole afternoon at KDK, drawn like a
helicorder drum, ten minutes a line. A map of a game, or of a place?

If KDK's 2011 record isn't in the public archive, this falls back to the Rainier helicorder
(rainier.py).

Downloads (cached in data/): EarthScope FDSN station and dataselect.
"""
import io
import runpy
import sys
from datetime import datetime, timedelta, timezone
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "toolkit"))
import dmc  # noqa: E402
import fetch  # noqa: E402

import numpy as np  # noqa: E402

DAY = 25
HERE = Path(__file__).resolve().parent
DATA = HERE / "data"
DATA.mkdir(exist_ok=True)
NET, STA = "UW", "KDK"
OFFSET = -8                                         # PST
LOCAL0 = datetime(2011, 1, 8, 12, 50)               # drawn from 12:50 to 5:10 PM, kickoff was mid-afternoon
ROWS, ROW_MIN = 26, 10
START = (LOCAL0 - timedelta(hours=OFFSET)).replace(tzinfo=timezone.utc)
END = START + timedelta(minutes=ROWS * ROW_MIN)
FDSN = "https://service.earthscope.org/fdsnws"


def fallback(why):
    print(f"= Beast Quake not drawn ({why}); drawing the Rainier helicorder instead")
    runpy.run_path(str(HERE / "rainier.py"), run_name="__main__")
    sys.exit(0)


try:
    txt = fetch.get(f"{FDSN}/station/1/query?net={NET}&sta={STA}&level=channel&format=text"
                    f"&starttime={START:%Y-%m-%dT%H:%M:%S}&endtime={END:%Y-%m-%dT%H:%M:%S}",
                    DATA / "kdk_channels.txt", tries=2).decode()
except Exception as e:  # noqa: BLE001
    fallback(f"no station metadata: {e.__class__.__name__}")
chans = [ln.split("|") for ln in txt.splitlines() if ln and not ln.startswith("#")]
print("= KDK channels in Jan 2011: " + ", ".join(f"{c[2] or '--'}.{c[3]}@{c[14]}" for c in chans))
zs = [(c[2], c[3], float(c[14])) for c in chans if c[3].endswith("Z")]
from obspy import UTCDateTime, read  # noqa: E402

tr = None
for pref in ("HHZ", "EHZ", "BHZ", "ENZ", "HNZ", "SHZ"):
    for loc, cha, sr in [z for z in zs if z[1] == pref]:
        try:
            raw = fetch.get(f"{FDSN}/dataselect/1/query?net={NET}&sta={STA}&loc={loc or '--'}&cha={cha}"
                            f"&start={START:%Y-%m-%dT%H:%M:%S}&end={END:%Y-%m-%dT%H:%M:%S}",
                            DATA / f"KDK_{cha}.mseed", timeout=900, tries=2)
        except Exception as e:  # noqa: BLE001
            print(f"= {cha}: no data ({e.__class__.__name__})")
            continue
        if not raw:
            print(f"= {cha}: empty")
            continue
        st = read(io.BytesIO(raw))
        st.merge(fill_value=0)
        tr = st[0]
        print(f"= using {NET}.{STA}.{loc or '--'}.{cha} at {sr:g} Hz, {tr.stats.npts / sr / 60:.0f} min from {tr.stats.starttime}")
        break
    if tr is not None:
        break
if tr is None:
    fallback("no vertical-channel data for that afternoon")

tr.detrend("demean")
tr.filter("bandpass", freqmin=1, freqmax=10, corners=2)
srate = tr.stats.sampling_rate
t0 = UTCDateTime(START)
data = tr.data.astype(float)
offs = int(round((tr.stats.starttime - t0) * srate))

# the loudest stretch: 3-second RMS envelope
win = int(3 * srate)
env = np.sqrt(np.convolve(data ** 2, np.ones(win) / win, mode="same"))
peak_i = int(np.argmax(env))
peak_local = LOCAL0 + timedelta(seconds=(peak_i + offs) / srate)
second = np.sort(env[np.abs(np.arange(len(env)) - peak_i) > 120 * srate])[-1] if len(env) > 240 * srate else 0
ratio = env[peak_i] / max(second, 1e-9)
print(f"= loudest 3 s at {peak_local:%I:%M:%S %p} PST, {ratio:.1f}x the next-loudest moment more than 2 min away")

# ── draw: a helicorder of the afternoon ──────────────────────────────────────
fig, ax = dmc.figure("portrait", map_box=(0.12, 0.115, 0.82, 0.70))
per = int(ROW_MIN * 60 * srate)
scale = np.percentile(np.abs(data[data != 0]), 99.0) * 2.5 if np.any(data) else 1
colours = [dmc.INK, dmc.LAKE]
for r in range(ROWS):
    a = max(0, r * per - offs)
    b = max(0, (r + 1) * per - offs)
    seg = data[a:b]
    if len(seg) == 0:
        continue
    x = (np.arange(len(seg)) + (a + offs - r * per)) / per * ROW_MIN
    y = -r + np.clip(seg / scale, -1.8, 1.8) * 0.42
    step = max(1, len(seg) // 5000)
    if step > 1:
        n = len(seg) // step * step
        xs = x[:n].reshape(-1, step)[:, [0, -1]].ravel()
        ys = np.c_[y[:n].reshape(-1, step).min(1), y[:n].reshape(-1, step).max(1)].ravel()
    else:
        xs, ys = x, y
    ax.plot(xs, ys, color=colours[r % 2], lw=0.4, solid_joinstyle="round")

# the run
pr = int((peak_i + offs) // per)
pm = ((peak_i + offs) % per) / srate / 60
ax.annotate(f"Lynch's 67-yard run\n{peak_local:%-I:%M %p}", xy=(pm, -pr + 0.5), xytext=(pm + (1.5 if pm < 6 else -1.5), -pr + 2.2),
            ha="left" if pm < 6 else "right", va="bottom", size=8, color=dmc.LAVA, family=dmc.TEXT,
            arrowprops=dict(arrowstyle="-", color=dmc.LAVA, lw=0.8))

ax.set_xlim(0, ROW_MIN)
ax.set_ylim(-ROWS + 0.2, 0.9)
ax.set_axis_on()
for sp in ax.spines.values():
    sp.set_visible(False)
ticks = [r for r in range(ROWS) if (LOCAL0 + timedelta(minutes=r * ROW_MIN)).minute == 0]
ax.set_yticks([-r for r in ticks])
ax.set_yticklabels([f"{(LOCAL0 + timedelta(minutes=r * ROW_MIN)):%-I} pm" for r in ticks], family=dmc.MONO, fontsize=7.5)
ax.set_xticks([0, 2, 4, 6, 8, 10])
ax.set_xticklabels(["0", "2", "4", "6", "8", "10 min"], family=dmc.MONO, fontsize=7.5)
ax.tick_params(length=0, colors=dmc.STONE)
ax.grid(axis="x", color=dmc.MIST, lw=0.5)

cha_id = f"{NET}.{STA}.{tr.stats.location or '--'}.{tr.stats.channel}"
dmc.frame(
    fig, DAY, title="The Beast Quake",
    subtitle=("January 8, 2011, the afternoon the Seahawks beat the Saints, on the seismometer across the street\n"
              "from the stadium, ten minutes a line. Marshawn Lynch's touchdown run made the ground shake."),
    source=f"EarthScope FDSN ({cha_id}, 1-10 Hz) · Pacific Northwest Seismic Network",
    note="Pacific Standard Time. Every wiggle is the stadium: the crowd, the plays, the band.",
)
dmc.save(fig, DAY, alt=(
    f"A helicorder of the afternoon of January 8, 2011, at seismic station KDK beside the Seahawks' stadium: "
    f"{ROWS} wiggly lines, ten minutes each, from 12:50 to 5:10 pm. The biggest burst, at {peak_local:%-I:%M %p}, "
    f"is labelled as Marshawn Lynch's 67-yard touchdown run, the Beast Quake."))
