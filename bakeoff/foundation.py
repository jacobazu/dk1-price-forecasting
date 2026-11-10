# pretrained models on the same test days and rules as nn.py. their weights come from hugging face
#   python foundation.py chronos2 DATA OUT     chronos-2 (apache 2.0), hourly prices with inputs, no training
#   python foundation.py tabpfn DATA OUT       tabpfn v2 (open weights), one table per day with the hour as a column
#   python foundation.py chronos2_plain DATA OUT   chronos-2 on the price series alone; only to see what the inputs add, not a candidate
# chronos-2 sees the price series up to the day before and, for tomorrow, the weather forecasts, holidays,
# weekday and gas price; neighbour prices only for the past. tabpfn sees for each hour the same-hour prices
# 1, 2, 3 and 7 days before, neighbour prices the day before, weather, gas, holiday, weekday, last 365 days
import json, sys
from pathlib import Path
import numpy as np
import pandas as pd
from common import load, inputs

TAUS = [round(t, 2) for t in np.arange(0.05, 0.96, 0.05)]


def hourly(D):
    """one row per day and hour: price, inputs known for that hour, and the same row's neighbour prices of the day"""
    n = len(D["dates"])
    rows = []
    nb_names = sorted(set(D["names"]["nb"]))
    nb = {s: D["ex"]["nb"][:, np.array(D["names"]["nb"]) == s] for s in nb_names}   # *_d1: the day before, by hour
    wx = {s: D["ex"][g][:, np.array(D["names"][g]) == s] for g in ["wx", "wxde"] for s in sorted(set(D["names"][g]))}
    for t in range(n):
        for h in range(24):
            r = {"day": t, "hour": h, "price": D["P"][t, h], "dow": D["dow"][t].argmax() + 1,
                 "hol": D["ex"]["hol"][t, 0], "gas": D["ex"]["fuel"][t, 0]}
            for s, X in wx.items():
                r[s] = X[t, h] if X.shape[1] == 24 else X[t, h % X.shape[1]]
            for s, X in nb.items():
                # the day's own neighbour price sits in the next day's row as d1
                r[s.replace("_d1", "")] = X[t + 1, h] if t + 1 < n else np.nan
            rows.append(r)
    H = pd.DataFrame(rows)
    H["timestamp"] = pd.Timestamp("2019-01-01") + pd.to_timedelta(H.day * 24 + H.hour, unit="h")
    return H, [s.replace("_d1", "") for s in nb_names], list(wx)


def chronos_days(D, days, out, pipe=None, context=24 * 7 * 12, plain=False):
    if pipe is None:
        from chronos import Chronos2Pipeline
        pipe = Chronos2Pipeline.from_pretrained("amazon/chronos-2", device_map="auto")
    H, nbc, wxc = hourly(D)
    fut_cols = [] if plain else wxc + ["hol", "dow", "gas", "hour"]   # plain: the price series alone, to see what the inputs add
    if plain:
        nbc = []
    for d in days:
        f = out / f"{D['dates'][d]}.json"
        if f.exists():
            continue
        past = H[(H.day < d) & (H.day >= d - context // 24)].copy()
        fut = H[H.day == d][["timestamp"] + fut_cols].copy()
        if past.price.isna().any() or fut[fut_cols].isna().any().any():
            continue
        past[nbc] = past[nbc].ffill().fillna(0)
        past["item_id"], fut["item_id"] = "dk1", "dk1"
        pr = pipe.predict_df(past[["item_id", "timestamp", "price"] + fut_cols + nbc], future_df=None if plain else fut,
                             target="price", prediction_length=24, quantile_levels=TAUS)
        q = {str(t): pr[str(t)].round(2).tolist() for t in TAUS}
        f.write_text(json.dumps({"date": str(D["dates"][d]), "forecast": q["0.5"], "q": q}))


def tabpfn_days(D, days, out, model=None, window=365):
    if model is None:
        from tabpfn import TabPFNRegressor
        from tabpfn.constants import ModelVersion
        model = lambda: TabPFNRegressor.create_default_for_version(ModelVersion.V2, ignore_pretraining_limits=True)
    H, nbc, wxc = hourly(D)
    P = D["P"]
    for k in [1, 2, 3, 7]:
        H[f"p_d{k}"] = H.groupby("hour").price.shift(k)
    cols = [f"p_d{k}" for k in [1, 2, 3, 7]] + nbc + wxc + ["gas", "hol", "dow", "hour"]
    for c in nbc:   # neighbour prices of the day before
        H[c] = H.groupby("hour")[c].shift(1)
    for d in days:
        f = out / f"{D['dates'][d]}.json"
        if f.exists():
            continue
        tr = H[(H.day < d) & (H.day >= d - window)].dropna(subset=cols + ["price"])
        te = H[H.day == d]
        if te[cols].isna().any().any() or len(tr) < 24 * 100:
            continue
        # prices on the window's asinh scale, as in common.py
        med = np.nanmedian(P[d - window:d]); s = 1.4826 * np.nanmedian(np.abs(P[d - window:d] - med))
        z = lambda x: np.arcsinh((x - med) / s)
        Xtr, Xte = tr[cols].to_numpy(float), te[cols].to_numpy(float)
        pc = [i for i, c in enumerate(cols) if c.startswith("p_d") or c in nbc]
        Xtr[:, pc], Xte[:, pc] = z(Xtr[:, pc]), z(Xte[:, pc])
        m = model()
        m.fit(Xtr, z(tr.price.to_numpy()))
        pred = med + s * np.sinh(m.predict(Xte))
        f.write_text(json.dumps({"date": str(D["dates"][d]), "forecast": np.round(pred, 2).tolist()}))


if __name__ == "__main__":
    if sys.platform == "win32":   # keep the pc awake while this runs
        import ctypes
        ctypes.windll.kernel32.SetThreadExecutionState(0x80000001)
    kind, data, out = sys.argv[1:4]
    D = load(data)
    out = Path(out) / kind
    out.mkdir(parents=True, exist_ok=True)
    t = D["dates"]
    days = [i for i in range(len(t)) if "2025-10-01" <= str(t[i]) <= "2026-11-09"]
    if kind == "tabpfn":
        tabpfn_days(D, days, out)
    else:
        chronos_days(D, days, out, plain=kind == "chronos2_plain")
