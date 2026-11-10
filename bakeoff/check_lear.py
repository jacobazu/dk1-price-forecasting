# the python inputs reproduce lear-x? lasso with bic per hour on 15 test days, against R's lear-x with gas
import sys, numpy as np, pandas as pd
from sklearn.linear_model import LassoLarsIC
from sklearn.preprocessing import StandardScaler
from common import load, inputs
D = load(sys.argv[1] + "/daily.csv"); ex = pd.read_csv(sys.argv[1] + "/existing.csv", parse_dates=["date"])
idx = {t: i for i, t in enumerate(D["dates"])}
days = sorted(ex.date.dt.date.unique())[::25]
for t in days:
    d = idx[t]; Xtr, Y, xd, back = inputs(D, d); sc = StandardScaler().fit(Xtr); sd = sc.scale_ > 1e-9; Xtr = sc.transform(Xtr)[:, sd]; xd = sc.transform(xd)[:, sd]
    f = back(np.array([LassoLarsIC(criterion="bic").fit(Xtr, Y[:, h]).predict(xd)[0] for h in range(24)]))
    r = ex[ex.date.dt.date == t].sort_values("hour").learx.to_numpy()
    print(t, len(Y), "mae py", round(np.mean(np.abs(D["P"][d] - f)), 1), "mae R", round(np.mean(np.abs(D["P"][d] - r)), 1),
          "py vs R", round(np.mean(np.abs(f - r)), 1))
