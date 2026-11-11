# live challengers from the model comparison, run each morning after the R forecast:
#   chronos-2 with inputs (no training) and the plain pytorch network (chosen_mlp.json, refit from scratch each day),
# both exactly as they ran in the backtest, and timestral (mistral, added 11 nov 2026) with the same inputs as chronos-2. appended to forecasts/challengers_py.csv, never overwritten.
#   python bakeoff/live.py DAILY.csv
# DAILY.csv is written by scripts/05_forecast_tomorrow.R; its last row is tomorrow, with no prices yet
import json, os, sys
from datetime import datetime, timezone
from pathlib import Path
from zoneinfo import ZoneInfo
import numpy as np
import pandas as pd

HERE = Path(__file__).parent
sys.path.insert(0, str(HERE))
from common import load, inputs  # noqa: E402

OUT = HERE.parent / "forecasts" / "challengers_py.csv"


def main(path):
    now = datetime.now(ZoneInfo("Europe/Copenhagen"))
    if now.hour * 100 + now.minute >= 1150 and os.environ.get("DK1_PREVIEW") != "true":
        print("after 11:50 danish time, nothing saved"); return
    D = load(path)
    d = len(D["dates"]) - 1
    target = str(D["dates"][d])
    old = pd.read_csv(OUT) if OUT.exists() else None
    if old is not None and target in set(old.date.astype(str)):
        print("challengers for", target, "already saved"); return

    row = {}
    try:
        from foundation import chronos_days
        tmp = Path("tmp_py/chronos"); tmp.mkdir(parents=True, exist_ok=True)
        chronos_days(D, [d], tmp)
        row["chronos2"] = json.loads((tmp / f"{target}.json").read_text())["forecast"]
    except Exception as e:
        print("chronos-2 failed:", e)
    try:
        from foundation import timestral_days
        tmp = Path("tmp_py/timestral"); tmp.mkdir(parents=True, exist_ok=True)
        timestral_days(D, [d], tmp)
        row["timestral"] = json.loads((tmp / f"{target}.json").read_text())["forecast"]
    except Exception as e:
        print("timestral failed:", e)
    try:
        import torch
        torch.set_num_threads(os.cpu_count() or 2)
        from nn import Candidate
        cfg = json.loads((HERE / "chosen_mlp.json").read_text())
        Xtr, Y, xd, back = inputs(D, d)
        z, _ = Candidate("mlp", cfg).day(Xtr, Y, xd, seed=d)
        row["torch_plain"] = np.round(back(z), 2).tolist()
    except Exception as e:
        print("pytorch network failed:", e)
    if not row:
        sys.exit("no challenger made a forecast")

    made = datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M")
    new = pd.DataFrame({"forecast_made_utc": made, "date": target, "hour": range(24),
                        **{k: np.round(v, 2) for k, v in row.items()}})
    out = pd.concat([old, new]) if old is not None else new
    out.to_csv(OUT, index=False)
    print("saved", ", ".join(row), "for", target)


if __name__ == "__main__":
    main(sys.argv[1])
