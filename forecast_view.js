// forecast page: the forecast per hour with its range, and what a desk would do with it: a 1 MW / 2 MWh battery's
// orders for tomorrow, with limit prices. data from R/forecast_view.R
(function () {
  const D = JSON.parse(document.getElementById("fv-data").textContent), root = document.getElementById("fv");
  const taus = Array.from({ length: 19 }, (_, i) => 0.05 + 0.05 * i), n = 24;
  const mute = "#78716c", grid = "#f0ece8", aqua = "#c0800c", amber = "#b32640", ink = "#1c1917";
  const hh = h => String(h).padStart(2, "0"), r0 = v => Math.round(v);
  let cur = D.main;

  // chance the price ends above x (or below x), read off the 19 quantiles, linear in between
  function above(q, x) {
    if (x <= q[0]) return 0.95; if (x >= q[18]) return 0.05;
    let k = 0; while (q[k + 1] < x) k++;
    const t = taus[k] + 0.05 * (x - q[k]) / Math.max(1e-9, q[k + 1] - q[k]);
    return 1 - t;
  }
  const pct = c => c >= 0.95 ? "over 95%" : c <= 0.05 ? "under 5%" : r0(100 * c) + "%";
  const blue = "#1c7fb0";
  const green = "#15803d", red = "#b91c1c";
  const neg = q => q ? 1 - above(q, 0) : 0;   // chance of a negative price

  // a 1 MW / 2 MWh battery, 90% round trip, 20 €/MWh wear on each MWh sold, at most one full cycle: the plan that
  // earns most on this forecast, in quarter-MW steps (dynamic programming over the charge held)
  const E = Math.sqrt(0.9), WEAR = 20, FEE = 0.055;
  function plan(f) {
    const key = (soc, out) => soc.toFixed(3) + "|" + out.toFixed(3);
    let S = new Map([[key(0, 0), { soc: 0, out: 0, v: 0, q: [] }]]);
    for (let h = 0; h < n; h++) {
      const N = new Map();
      for (const st of S.values()) for (const m of [-1, -0.75, -0.5, -0.25, 0, 0.25, 0.5, 0.75, 1]) {   // + sell, - buy, MW
        const soc = st.soc + (m < 0 ? -m * E : -m / E), out = st.out + (m > 0 ? m / E : 0);
        if (soc < -1e-9 || soc > 2 + 1e-9 || out > 2 + 1e-9) continue;
        const v = st.v + m * f[h] - (m > 0 ? m * (WEAR + FEE) : -m * FEE), k = key(soc, out), o = N.get(k);
        if (!o || v > o.v) N.set(k, { soc, out, v, q: st.q.concat(m) });
      }
      S = N;
    }
    let best = { v: 0, q: Array(n).fill(0) };
    for (const st of S.values()) if (st.v > best.v) best = st;
    return best;
  }
  // neighbouring hours with the same order in one window
  function windows(q) {
    const w = [];
    q.forEach((m, h) => {
      const side = m < 0 ? "buy" : m > 0 ? "sell" : null; if (!side) return;
      const l = w[w.length - 1];
      if (l && l.side == side && l.b == h - 1) { l.b = h; l.hours.push(h); } else w.push({ side, a: h, b: h, hours: [h] });
    });
    return w;
  }
  // limit prices: a buy only pays if it is below what the sales fetch after losses and wear, a sale only if it is
  // above what the charge cost. so: buy at most 0.9 x (sale price - wear), sell at least buy price / 0.9 + wear
  function ticket(M) {
    const f = M.mean, P = plan(f), w = windows(P.q);
    if (!w.length) return { P, w };
    const avg = side => { const hs = w.filter(x => x.side == side).flatMap(x => x.hours); return hs.reduce((a, h) => a + f[h], 0) / hs.length; };
    const ps = avg("sell"), pb = avg("buy");
    w.forEach(x => {
      x.limit = x.side == "buy" ? 0.9 * (ps - WEAR) : pb / 0.9 + WEAR;
      x.fill = M.q ? Math.min(...x.hours.map(h => x.side == "buy" ? 1 - above(M.q[h], x.limit) : above(M.q[h], x.limit))) : null;
      x.mw = Math.max(...x.hours.map(h => Math.abs(P.q[h])));
    });
    return { P, w };
  }

  // each hour: the forecast as a dot, the 9-in-10 range as a soft bar. the battery's buy and sell hours shaded, with
  // the limit price as a line across them: the order fills only if the price ends on the right side of it
  function main(M, T0) {
    const f = M.mean, Q = M.q;
    const all = (Q ? Q.flatMap(r => [r[0], r[18]]).concat(f) : f.slice()).concat(T0.w.map(x => x.limit)), span = Math.max(...all) - Math.min(0, ...all);
    const st = [25, 50, 100, 200, 500].find(v => span / v <= 5) || 1000;
    const top = Math.max(st, Math.ceil(Math.max(...all) * 1.08 / st) * st), bot = Math.min(0, Math.floor(Math.min(...all) / st) * st);
    const W = 1190, H = 440, L = 44, R = 20, T = 40, B = 36;
    const y = p => T + (H - T - B) * (1 - (p - bot) / (top - bot)), cw = (W - L - R) / n, xs = h => L + cw * h, x = h => xs(h) + cw / 2;
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Forecast price per hour with the battery's buy and sell hours">`;
    for (const o of T0.w) {
      const c = o.side == "buy" ? green : red;
      s += `<rect x="${xs(o.a)}" width="${cw * (o.b - o.a + 1)}" y="${T - 6}" height="${H - B - T + 6}" fill="${c}" fill-opacity=".06"/>`;
      s += `<text x="${(xs(o.a) + xs(o.b + 1)) / 2}" y="${T - 14}" text-anchor="middle" fill="${c}" class="fv-s" font-weight="600">${o.side == "buy" ? "buy" : "sell"}</text>`;
    }
    for (let g = bot; g <= top + 1e-9; g += st) s += `<line x1="${L}" x2="${W - R}" y1="${y(g)}" y2="${y(g)}" stroke="${g == 0 && bot < 0 ? "#a8a29e" : grid}"/><text x="${L - 10}" y="${y(g) + 4.5}" text-anchor="end" fill="${mute}">${r0(g)}</text>`;
    s += `<text x="4" y="${T - 26}" fill="${mute}" class="fv-s">€/MWh</text>`;
    const bw = Math.min(26, cw * 0.62);
    if (Q) for (let h = 0; h < n; h++)
      s += `<rect x="${x(h) - bw / 2}" width="${bw}" y="${y(Q[h][18])}" height="${Math.max(3, y(Q[h][0]) - y(Q[h][18]))}" rx="${Math.min(8, bw / 2)}" fill="${neg(Q[h]) >= 0.1 ? blue : ink}" fill-opacity="${neg(Q[h]) >= 0.1 ? .2 : .07}"/>`;
    for (const o of T0.w) {
      const c = o.side == "buy" ? green : red;
      s += `<line x1="${xs(o.a) + 3}" x2="${xs(o.b + 1) - 3}" y1="${y(o.limit)}" y2="${y(o.limit)}" stroke="${c}" stroke-width="2.5" stroke-linecap="round"/>`;
    }
    s += `<path d="${f.map((p, h) => `${h ? "L" : "M"}${x(h).toFixed(1)},${y(p).toFixed(1)}`).join("")}" fill="none" stroke="${ink}" stroke-opacity=".35" stroke-width="1.5"/>`;
    for (let h = 0; h < n; h++) {
      s += `<circle cx="${x(h)}" cy="${y(f[h])}" r="4" fill="${ink}" stroke="#fff" stroke-width="1.5"/>`;
      if (h % 3 == 0) s += `<text x="${x(h)}" y="${H - 10}" text-anchor="middle" fill="${mute}">${hh(h)}:00</text>`;
      s += `<rect class="fv-hit" data-h="${h}" x="${xs(h)}" width="${cw}" y="${T}" height="${H - T}" fill="transparent"/>`;
    }
    return s + "</svg>";
  }

  // the order ticket, as it would go to the exchange
  function orders(M, T0) {
    const head = `<h2>Tomorrow's orders</h2><p>A 1 MW / 2 MWh battery planned on this forecast</p>`;
    if (!T0.w.length) return head + `<p class="fv-none">No trade: the gap between the cheap and dear hours doesn't cover the 10% loss and the wear.</p>`;
    return head + `<table class="fv-tab"><tr><th>Hours</th><th>Order</th><th>Limit</th>${M.q ? "<th>Chance it fills</th>" : ""}</tr>` + T0.w.map(o =>
      `<tr><td>${hh(o.a)}:00–${hh(o.b + 1)}:00</td><td style="color:${o.side == "buy" ? green : red};font-weight:600">${o.side == "buy" ? "Buy" : "Sell"} ${o.mw} MW</td>` +
      `<td>${o.side == "buy" ? "at most" : "at least"} ${r0(o.limit)}</td>${M.q ? `<td>${pct(o.fill)}</td>` : ""}</tr>`).join("") +
      `</table><p class="fv-foot">Expected ${r0(T0.P.v)} € after wear. Limits: where the trade stops paying after the 10% loss and 20 €/MWh wear.</p>`;
  }

  function daybars(M) {
    const d = M.days; if (!d) return `<p>No scored days yet</p>`;
    const k = d.inside.length, w = 360, hg = 132, bw = (w - 12) / k, r = Math.min(6, (bw - 3) / 2); let b = `<svg viewBox="0 0 ${w} ${hg}" role="img" aria-label="Hours inside the range per day">`;
    d.inside.forEach((v, i) => {
      const hgt = Math.max(2 * r, (hg - 40) * v / d.n[i]);
      b += `<rect x="${6 + bw * i + 1.5}" width="${bw - 3}" y="14" height="${hg - 40}" rx="${r}" fill="#f5f2ef"/>`;
      b += `<rect x="${6 + bw * i + 1.5}" width="${bw - 3}" y="${hg - 26 - hgt}" height="${hgt}" rx="${r}" fill="${aqua}" fill-opacity="${v / d.n[i] >= 0.8 ? 1 : .4}"><title>${d.dates[i]}: ${v} of ${d.n[i]} hours inside</title></rect>`;
    });
    return b + `<text x="6" y="${hg - 6}" fill="${mute}">${d.dates[0]}</text><text x="${w - 6}" y="${hg - 6}" text-anchor="end" fill="${mute}">${d.dates[k - 1]}</text></svg>`;
  }

  function meter(k) {
    const p = Math.max(0, Math.min(100, 100 * (k.x - k.lo) / (k.hi - k.lo))), a = 100 * (k.aim - k.lo) / (k.hi - k.lo);
    const col = k.light == "green" ? "#15803d" : k.light == "red" ? "#b91c1c" : "#b45309";
    return `<div class="fv-m"><span>${k.name}</span><b>${k.value}</b></div><div class="fv-bar"><i style="left:${a}%"></i><u style="left:calc(${p}% - 5px);background:${col}"></u></div>`;
  }

  function draw() {
    const M = D.models[cur], f = M.mean, pk = f.indexOf(Math.max(...f)), lw = f.indexOf(Math.min(...f)), avg = r0(f.reduce((a, b) => a + b) / n);
    const keys = Object.keys(D.models), T0 = ticket(M);
    root.innerHTML = `<div class="fv-top"><h1>${D.day}<small>Saved ${D.made}, before the auction</small></h1>
      <div class="fv-num"><b>${avg}</b>average, €/MWh</div><div class="fv-num"><b style="color:${amber}">${r0(f[pk])}</b>highest, ${hh(pk)}:00</div>
      <div class="fv-num"><b>${r0(f[lw])}</b>lowest, ${hh(lw)}:00</div><div class="fv-num"><b>${D.neg}</b>chance of a negative price</div></div>
      ${keys.length > 1 ? `<div class="fv-pills">${keys.map(k => `<button data-m="${k}" class="${k == cur ? "on" : ""}">${D.models[k].label}</button>`).join("")}</div>` : ""}
      <div class="fv-chart">${main(M, T0)}<div class="fv-tip" hidden></div></div>
      <div class="fv-key"><span><i style="background:${ink};height:8px;width:8px;border-radius:50%"></i>forecast</span>${M.q ? `<span><i style="background:${ink};opacity:.12"></i>9 times in 10 inside</span>` : ""}<span><i style="background:${green};height:3px"></i>buy, at most this price</span><span><i style="background:${red};height:3px"></i>sell, at least this price</span>${M.q && M.q.some(q => neg(q) >= 0.1) ? `<span><i style="background:${blue};opacity:.4"></i>1 in 10 or more chance below 0</span>` : ""}</div>
      <div class="fv-low"><div class="fv-pane">${orders(M, T0)}</div>
      <div class="fv-pane"><h2>Hours inside the range</h2><p>Per day, out of 24. Pale: fewer than 8 in 10</p>${daybars(M)}</div>
      <div class="fv-pane"><h2>Last 30 days, ${D.models[D.main].label}</h2><p>Dot: where it is. Line: the aim</p>${D.checks.map(meter).join("")}</div></div>`;
    const tip = root.querySelector(".fv-tip"), box = root.querySelector(".fv-chart");
    root.querySelectorAll(".fv-hit").forEach(el => {
      el.addEventListener("mousemove", e => {
        const h = +el.dataset.h, q = M.q && M.q[h], b = box.getBoundingClientRect();
        tip.innerHTML = `<b>${hh(h)}:00</b> ${r0(f[h])} €/MWh` + (q ? `<br>9 in 10: ${r0(q[0])} to ${r0(q[18])}<br>below 0: ${pct(neg(q))}` : "") + (T0.P.q[h] ? `<br>${T0.P.q[h] < 0 ? "buy" : "sell"} ${Math.abs(T0.P.q[h])} MW` : "");
        tip.hidden = false;
        tip.style.left = Math.min(b.width - 190, Math.max(0, e.clientX - b.left + 14)) + "px"; tip.style.top = Math.max(0, e.clientY - b.top - 70) + "px";
      });
      el.addEventListener("mouseleave", () => { tip.hidden = true; });
    });
    root.querySelectorAll(".fv-pills button").forEach(b => b.addEventListener("click", () => { cur = b.dataset.m; draw(); }));
  }
  draw();
})();
