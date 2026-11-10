# every candidate on the same test days: alone, and as a fourth member next to bmt-x, lear-x and xgboost-x (with gas),
# against the three-model ensemble. diebold-mariano with newey-west (7 lags), holm over the candidates
#   python compare.py DATA OUT     DATA holds existing.csv, OUT one folder of daily json files per candidate
import json, sys
from pathlib import Path
import numpy as np
import pandas as pd
from scipy import stats


def dm(d, lag=7):
    n, u = len(d), d - d.mean()
    v = (u @ u) / n + 2 * sum((1 - k / (lag + 1)) * (u[k:] @ u[:-k]) / n for k in range(1, lag + 1))
    t = d.mean() / np.sqrt(v / n) * np.sqrt((n - 1) / n)
    return 2 * stats.t.cdf(-abs(t), n - 1)


data, out = Path(sys.argv[1]), Path(sys.argv[2])
ex = pd.read_csv(data / "existing.csv", parse_dates=["date"])
P = pd.read_csv(data / "daily.csv", parse_dates=["date"]).set_index("date")[[f"p_h{h}" for h in range(24)]]
ex["actual"] = [P.loc[d, f"p_h{h}"] for d, h in zip(ex.date, ex.hour)]
ex["ens3"] = ex[["bmtx", "learx", "xgbx"]].mean(axis=1)
cands = {"dnn_r": ex.set_index(["date", "hour"])["dnn"]} if "dnn" in ex else {}
for f in sorted(p for p in out.iterdir() if p.is_dir()):
    rows = [json.loads(j.read_text()) for j in f.glob("*.json")]
    cands[f.name] = pd.Series({(pd.Timestamp(r["date"]), h): v for r in rows for h, v in enumerate(r["forecast"])})
df = ex.set_index(["date", "hour"])
for k, s in cands.items():
    df[k] = s
days = df.dropna().index.get_level_values(0).unique()
df = df.loc[days]
print(len(days), "common days,", len(cands), "candidates;", {k: int(s.notna().sum() / 24) for k, s in cands.items()}, "days each")
day = lambda f: (df["actual"] - f).abs().groupby(level=0).mean()
base = day(df.ens3)
res = [("ensemble today (3 models)", base.mean(), np.nan, np.nan)]
for k in cands:
    alone, four = day(df[k]), day(df[["bmtx", "learx", "xgbx", k]].mean(axis=1))
    res.append((k, alone.mean(), four.mean(), dm((four - base).to_numpy())))
r = pd.DataFrame(res, columns=["candidate", "mae_alone", "mae_as_4th", "p_vs_today"])
p = r.p_vs_today.to_numpy()
ok = ~np.isnan(p)
order = np.argsort(p[ok])
holm = np.empty(ok.sum())
holm[order] = np.minimum(1, np.maximum.accumulate((ok.sum() - np.arange(ok.sum())) * p[ok][order]))
r.loc[ok, "p_holm"] = holm
r["passes"] = (r.mae_as_4th < base.mean()) & (r.p_holm < 0.05)
print(r.round(4).to_string(index=False))
r.to_csv(out / "comparison.csv", index=False)
