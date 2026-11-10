# inputs for one forecast day, built the way R/xmodels.R builds them: last 730 days, prices on an asinh scale
# with the window's median and mad, neighbour prices the same per series, weather and gas standardised, weekday dummies
import numpy as np
import pandas as pd

GROUPS = ["nb", "hol", "wx", "wxde", "fuel"]


def mad(x):
    x = x[np.isfinite(x)]
    return 1.4826 * np.median(np.abs(x - np.median(x)))


def load(path):
    tab = pd.read_csv(path, parse_dates=["date"])
    assert (tab.date.diff().dropna() == pd.Timedelta(days=1)).all(), "days missing"
    P = tab[[f"p_h{h}" for h in range(24)]].to_numpy(float)
    lag = lambda k: np.vstack([np.full((k, 24), np.nan), P[:-k]])
    lags = np.hstack([lag(1), lag(2), lag(3), lag(7)])
    ex = {g: tab[[c for c in tab.columns if c.startswith(g + ":")]].to_numpy(float) for g in GROUPS}
    names = {g: [c.split(":", 1)[1].rsplit("_h", 1)[0] for c in tab.columns if c.startswith(g + ":")] for g in GROUPS}
    dow = np.eye(7)[tab.dow.to_numpy() - 1]
    return dict(dates=tab.date.dt.date.to_numpy(), P=P, lags=lags, ex=ex, names=names, dow=dow)


def inputs(D, d, window=730, groups=GROUPS):
    tr = np.arange(d - window, d)
    P = D["P"]
    med, s = np.nanmedian(P[tr]), mad(P[tr].ravel())
    blocks = [np.arcsinh((D["lags"] - med) / s)]
    for g in groups:
        X = D["ex"][g].copy()
        if g == "hol":
            blocks.append(X)
            continue
        series = np.array(D["names"][g])
        for name in np.unique(series):
            j = series == name
            t = X[tr][:, j].ravel()
            if g == "nb":
                sc = mad(t)
                if not np.isfinite(sc) or sc == 0:
                    sc = np.nanstd(t, ddof=1)
                X[:, j] = np.arcsinh((X[:, j] - np.nanmedian(t)) / sc)
            else:
                X[:, j] = (X[:, j] - np.nanmean(t)) / np.nanstd(t, ddof=1)
        blocks.append(X)
    X = np.hstack(blocks + [D["dow"]])
    if not np.isfinite(X[d]).all():
        raise ValueError("missing inputs for the forecast day")
    keep = tr[np.isfinite(X[tr]).all(1) & np.isfinite(P[tr]).all(1)]
    to = lambda p: np.arcsinh((p - med) / s)
    back = lambda z: med + s * np.sinh(z)
    return X[keep], to(P[keep]), X[d:d + 1], back
