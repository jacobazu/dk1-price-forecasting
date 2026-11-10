# neural networks for tomorrow's 24 prices, in pytorch. rules, set 11 nov 2026 before any test run:
# - test days: the 377 backtest days in existing.csv (1 oct 2025 to 9 nov 2026), each candidate run once on them
# - settings chosen only on every 7th day from 1 oct 2024 to 30 sep 2025, lowest mae wins, then frozen
# - scored in compare.py: mae alone, and as a fourth member next to bmt-x, lear-x and xgboost-x (all with gas),
#   against the three-model ensemble. diebold-mariano with newey-west, holm over the candidates, p < 0.05
# - a winner runs live as a challenger from the day after it is chosen and is judged on 1 mar 2027
#
#   python nn.py tune mlp DATA            settings for one candidate on the tuning days
#   python nn.py test mlp DATA OUT        the frozen settings on the test days, one file per day
# candidates: mlp (plain network), skip (plus a linear path, warm-started each day), jsu (skip with a
# johnson su distribution per hour instead of one number)
import json, sys, os
from pathlib import Path
import numpy as np
import torch
from torch import nn
from common import load, inputs

torch.set_num_threads(int(os.environ.get("THREADS", 2)))
HERE = Path(__file__).parent
TAUS = np.round(np.arange(0.05, 0.96, 0.05), 2)


class Net(nn.Module):
    def __init__(self, k, hidden, drop, skip, out):
        super().__init__()
        layers, n = [], k
        for h in hidden:
            layers += [nn.Linear(n, h), nn.ReLU(), nn.Dropout(drop)]
            n = h
        self.body = nn.Sequential(*layers, nn.Linear(n, out))
        self.lin = nn.Linear(k, out) if skip else None

    def forward(self, x):
        y = self.body(x)
        return y + self.lin(x) if self.lin is not None else y


def jsu_nll(par, y):
    # johnson su: z = gamma + delta * asinh((y - xi) / lam)
    xi, lam, gam, dl = par[:, :24], nn.functional.softplus(par[:, 24:48]) + 1e-3, par[:, 48:72], nn.functional.softplus(par[:, 72:]) + 1e-3
    u = (y - xi) / lam
    z = gam + dl * torch.asinh(u)
    return (-torch.log(dl) + torch.log(lam) + 0.5 * torch.log1p(u ** 2) + 0.5 * z ** 2).mean()


def jsu_quantiles(par, taus):
    xi, lam, gam, dl = par[:, :24], nn.functional.softplus(par[:, 24:48]) + 1e-3, par[:, 48:72], nn.functional.softplus(par[:, 72:]) + 1e-3
    zq = torch.distributions.Normal(0, 1).icdf(torch.tensor(taus, dtype=torch.float32))[:, None, None]
    return xi + lam * torch.sinh((zq - gam) / dl)   # taus x days x 24


def fit(net, X, Y, cfg, epochs, seed, dist=False):
    g = torch.Generator().manual_seed(seed)
    n = len(X)
    perm = torch.randperm(n, generator=g)
    val, tr = perm[: n // 5], perm[n // 5:]
    opt = torch.optim.AdamW(net.parameters(), lr=cfg["lr"], weight_decay=cfg["wd"])
    loss_f = jsu_nll if dist else (lambda p, y: (p - y).abs().mean())
    best, state, wait = np.inf, None, 0
    for ep in range(epochs):
        net.train()
        for b in tr[torch.randperm(len(tr), generator=g)].split(cfg["batch"]):
            opt.zero_grad()
            loss_f(net(X[b]), Y[b]).backward()
            opt.step()
        net.eval()
        with torch.no_grad():
            v = loss_f(net(X[val]), Y[val]).item()
        if v < best - 1e-5:
            best, state, wait = v, {k: t.clone() for k, t in net.state_dict().items()}, 0
        elif (wait := wait + 1) >= cfg["patience"]:
            break
    net.load_state_dict(state)
    return net


def scale(Xtr, xd):
    mu, sd = Xtr.mean(0), Xtr.std(0)
    ok = sd > 1e-8
    f = lambda x: torch.tensor(np.clip((x[:, ok] - mu[ok]) / sd[ok], -6, 6), dtype=torch.float32)
    return f(Xtr), f(xd)


class Candidate:
    """one candidate; warm keeps the networks between days (skip and jsu), mlp starts fresh each day"""

    def __init__(self, kind, cfg):
        self.kind, self.cfg, self.nets = kind, cfg, None

    def day(self, Xtr, Y, xd, seed):
        X, x = scale(Xtr, xd)
        Yt = torch.tensor(Y, dtype=torch.float32)
        c, dist = self.cfg, self.kind == "jsu"
        warm = self.kind != "mlp" and self.nets is not None and self.nets[0].body[0].in_features == X.shape[1]
        if not warm:
            self.nets = [Net(X.shape[1], c["hidden"], c["drop"], self.kind != "mlp", 96 if dist else 24) for _ in range(c["seeds"])]
        for i, net in enumerate(self.nets):
            torch.manual_seed(seed * 10 + i)
            fit(net, X, Yt, c, c["update_epochs"] if warm else c["epochs"], seed * 10 + i, dist)
        with torch.no_grad():
            if dist:
                q = torch.stack([jsu_quantiles(n(x), TAUS) for n in self.nets]).mean(0)[:, 0, :].numpy()   # taus x 24
                return q[list(TAUS).index(0.5)], q
            return torch.stack([n(x) for n in self.nets]).mean(0)[0].numpy(), None


SPACE = {
    "hidden": [[64], [128], [256], [128, 64], [256, 128], [512, 256], [256, 128, 64]],
    "drop": [0.0, 0.1, 0.2, 0.3],
    "lr": [3e-4, 1e-3, 3e-3],
    "wd": [0.0, 1e-4, 1e-3, 1e-2],
    "batch": [32, 64, 128],
}
BASE = {"epochs": 300, "patience": 20, "update_epochs": 15, "seeds": 3}


def days_between(D, a, b, step=1):
    t = D["dates"]
    return [i for i in range(len(t)) if str(a) <= str(t[i]) <= str(b)][::step]


def run(kind, cfg, D, days, out=None):
    cand, res = Candidate(kind, cfg), {}
    for d in days:
        f = Path(out) / f"{D['dates'][d]}.json" if out else None
        if f and f.exists():
            continue
        try:
            Xtr, Y, xd, back = inputs(D, d)
        except ValueError:
            continue
        z, q = cand.day(Xtr, Y, xd, seed=d)
        res[d] = back(z)
        if f:
            row = {"date": str(D["dates"][d]), "forecast": res[d].round(2).tolist()}
            if q is not None:
                row["q"] = {str(t): back(q[i]).round(2).tolist() for i, t in enumerate(TAUS)}
            f.write_text(json.dumps(row))
    return res


def tune(kind, data, trials=int(os.environ.get("TRIALS", 25))):
    import optuna
    D = load(data)
    days = days_between(D, "2024-10-01", "2025-09-30", 7)
    log = HERE / f"tune_{kind}.csv"

    def objective(tr):
        cfg = {**BASE, **{k: tr.suggest_categorical(k, [json.dumps(v) for v in vals]) for k, vals in SPACE.items()}}
        cfg = {k: json.loads(v) if isinstance(v, str) else v for k, v in cfg.items()}
        if os.environ.get("QUICK"):
            cfg["seeds"] = 1
        res = run(kind, cfg, D, days)
        mae = float(np.mean([np.abs(D["P"][d] - f).mean() for d, f in res.items()]))
        with open(log, "a") as fh:
            fh.write(f"{tr.number},{mae:.3f},{len(res)},{json.dumps(cfg)}\n")
        return mae

    st = optuna.create_study(direction="minimize", sampler=optuna.samplers.TPESampler(seed=1))
    st.optimize(objective, n_trials=trials)
    best = {**BASE, **{k: json.loads(v) for k, v in st.best_params.items()}}
    (HERE / f"chosen_{kind}.json").write_text(json.dumps(best, indent=1))
    print(kind, "best tuning mae", round(st.best_value, 2), best)


if __name__ == "__main__":
    what, kind, data = sys.argv[1:4]
    if what == "tune":
        tune(kind, data)
    else:
        out = Path(sys.argv[4]) / kind
        out.mkdir(parents=True, exist_ok=True)
        D = load(data)
        cfg = json.loads((HERE / f"chosen_{kind}.json").read_text())
        run(kind, cfg, D, days_between(D, "2025-10-01", "2026-11-09"), out)
