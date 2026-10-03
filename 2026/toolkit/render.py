"""Render one or more days' maps:  python toolkit/render.py 28   or   pixi run render 8 15 18

Runs each day's make.py (or make.R with Rscript) inside its folder, carrying on past a day
that fails, then rebuilds the plan once so thumbnails and days.json pick up the new maps.
Exits non-zero if any day failed, after rendering the rest.
"""
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

days = [int(a) for a in " ".join(sys.argv[1:]).replace(",", " ").split() if a.isdigit()]
if not days:
    sys.exit("usage: render.py DAY [DAY ...]")
failed = []
for day in days:
    folders = sorted(ROOT.glob(f"day-{day:02d}-*"))
    if not folders:
        failed.append(f"{day}: no folder (run toolkit/build.py)")
        continue
    folder = folders[0]
    for script, cmd in (("make.py", [sys.executable, "make.py"]), ("make.R", ["Rscript", "make.R"])):
        if (folder / script).exists():
            print(f"> {folder.name}/{script}", flush=True)
            rc = subprocess.run(cmd, cwd=folder).returncode
            if rc:
                failed.append(f"{day}: {script} failed with exit code {rc}")
                print(f"! day {day} failed (exit {rc})", flush=True)
            break
    else:
        failed.append(f"{day}: no make.py or make.R yet")
subprocess.run([sys.executable, str(ROOT / "toolkit" / "build.py")], check=True)
if failed:
    print("FAILED: " + "; ".join(failed), flush=True)
    sys.exit(1)
