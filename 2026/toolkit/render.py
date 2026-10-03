"""Render one day's map:  python toolkit/render.py 28   (or: pixi run render 28)

Runs the day's make.py (or make.R with Rscript) inside its folder, then rebuilds the plan
so the thumbnail and days.json pick up the new map.
"""
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent

if len(sys.argv) < 2 or not sys.argv[1].isdigit():
    sys.exit("usage: render.py DAY")
day = int(sys.argv[1])
folders = sorted(ROOT.glob(f"day-{day:02d}-*"))
if not folders:
    sys.exit(f"no folder for day {day}; run toolkit/build.py")
folder = folders[0]
for script, cmd in (("make.py", [sys.executable, "make.py"]), ("make.R", ["Rscript", "make.R"])):
    if (folder / script).exists():
        print(f"> {folder.name}/{script}", flush=True)
        rc = subprocess.run(cmd, cwd=folder).returncode
        if rc:
            sys.exit(f"{script} failed with exit code {rc}")
        break
else:
    sys.exit(f"{folder.name} has no make.py or make.R yet")
subprocess.run([sys.executable, str(ROOT / "toolkit" / "build.py")], check=True)
