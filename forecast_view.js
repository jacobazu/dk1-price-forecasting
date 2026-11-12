// forecast page: the forecast per hour and its risk, as a fan (half the time inside the dark band, 9 in 10 inside the
// light one) and a few plain risk numbers. data from R/forecast_view.R
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
  const neg = q => q ? 1 - above(q, 0) : 0;   // chance of a negative price
  const SPIKE = 200;   // reached in about 1 hour in 20 over the last two years

  function main(M) {
    const f = M.mean, Q = M.q;
    const all = Q ? Q.flatMap(r => [r[0], r[18]]).concat(f) : f.slice(), span = Math.max(...all) - Math.min(0, ...all);
    const st = [25, 50, 100, 200, 500].find(v => span / v <= 5) || 1000;
    const top = Math.max(st, Math.ceil(Math.max(...all) * 1.05 / st) * st), bot = Math.min(0, Math.floor(Math.min(...all) / st) * st);
    const W = 1190, H = 420, L = 44, R = 20, T = 34, B = 36;
    const y = p => T + (H - T - B) * (1 - (p - bot) / (top - bot)), cw = (W - L - R) / n, xs = h => L + cw * h, x = h => xs(h) + cw / 2;
    // a band from quantile i to j as one smooth shape through the hours
    const band = (i, j) => {
      const up = Q.map((q, h) => `${x(h).toFixed(1)},${y(q[j]).toFixed(1)}`), dn = Q.map((q, h) => `${x(h).toFixed(1)},${y(q[i]).toFixed(1)}`).reverse();
      return `M${up.join("L")}L${dn.join("L")}Z`;
    };
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Forecast price per hour with its 50% and 90% ranges">`;
    for (let g = bot; g <= top + 1e-9; g += st) s += `<line x1="${L}" x2="${W - R}" y1="${y(g)}" y2="${y(g)}" stroke="${g == 0 && bot < 0 ? "#a8a29e" : grid}"/><text x="${L - 10}" y="${y(g) + 4.5}" text-anchor="end" fill="${mute}">${r0(g)}</text>`;
    s += `<text x="4" y="${T - 16}" fill="${mute}" class="fv-s">€/MWh</text>`;
    if (Q) {
      s += `<path d="${band(0, 18)}" fill="${aqua}" fill-opacity=".14"/><path d="${band(4, 14)}" fill="${aqua}" fill-opacity=".28"/>`;
      // hours where a negative price is a real chance
      Q.forEach((q, h) => { if (neg(q) >= 0.1) s += `<rect x="${xs(h) + 2}" width="${cw - 4}" y="${y(0)}" height="${Math.max(0, y(q[0]) - y(0))}" fill="${blue}" fill-opacity=".35" rx="3"/><text x="${x(h)}" y="${y(Math.min(0, q[0])) + 15}" text-anchor="middle" fill="${blue}" class="fv-s" font-weight="600">${pct(neg(q))}</text>`; });
    }
    s += `<path d="${f.map((p, h) => `${h ? "L" : "M"}${x(h).toFixed(1)},${y(p).toFixed(1)}`).join("")}" fill="none" stroke="${ink}" stroke-width="2.5" stroke-linejoin="round"/>`;
    for (let h = 0; h < n; h++) {
      if (h % 3 == 0) s += `<text x="${x(h)}" y="${H - 10}" text-anchor="middle" fill="${mute}">${hh(h)}:00</text>`;
      s += `<rect class="fv-hit" data-h="${h}" x="${xs(h)}" width="${cw}" y="${T}" height="${H - T}" fill="transparent"/>`;
    }
    return s + "</svg>";
  }

  // risk in a few plain numbers
  function risk(M) {
    const head = `<h2>Risk tomorrow</h2><p>From the forecast's range, hour by hour</p>`;
    if (!M.q) return head + `<p class="fv-none">This model has no range.</p>`;
    const Q = M.q, w = Q.map(q => q[18] - q[0]);
    const wi = w.indexOf(Math.max(...w)), ni = w.indexOf(Math.min(...w));
    const ng = Q.map(neg), gi = ng.indexOf(Math.max(...ng));
    const sp = Q.map(q => above(q, SPIKE)), si = sp.indexOf(Math.max(...sp));
    const row = (k, v) => `<tr><td>${k}</td><td>${v}</td></tr>`;
    return head + `<table class="fv-tab">` +
      row("Least certain hour", `${hh(wi)}:00, 9 in 10 between ${r0(Q[wi][0])} and ${r0(Q[wi][18])}`) +
      row("Most certain hour", `${hh(ni)}:00, between ${r0(Q[ni][0])} and ${r0(Q[ni][18])}`) +
      row("Price below 0", ng[gi] < 0.055 ? "under 5% in every hour" : `${pct(ng[gi])} at ${hh(gi)}:00, the most likely hour`) +
      row(`Price above ${SPIKE}`, sp[si] < 0.055 ? "under 5% in every hour" : `${pct(sp[si])} at ${hh(si)}:00, the most likely hour`) +
      `</table><p class="fv-foot">Above ${SPIKE} €/MWh: about 1 hour in 20 over the last two years.</p>`;
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
    const keys = Object.keys(D.models);
    root.innerHTML = `<div class="fv-top"><h1>${D.day}<small>Saved ${D.made}, before the auction</small></h1>
      <div class="fv-num"><b>${avg}</b>average, €/MWh</div><div class="fv-num"><b style="color:${amber}">${r0(f[pk])}</b>highest, ${hh(pk)}:00</div>
      <div class="fv-num"><b>${r0(f[lw])}</b>lowest, ${hh(lw)}:00</div><div class="fv-num"><b>${D.neg}</b>chance of a negative price</div></div>
      ${keys.length > 1 ? `<div class="fv-pills">${keys.map(k => `<button data-m="${k}" class="${k == cur ? "on" : ""}">${D.models[k].label}</button>`).join("")}</div>` : ""}
      <div class="fv-chart">${main(M)}<div class="fv-tip" hidden></div></div>
      <div class="fv-key"><span><i style="background:${ink};height:3px"></i>forecast</span>${M.q ? `<span><i style="background:${aqua};opacity:.45"></i>half the time inside</span><span><i style="background:${aqua};opacity:.2"></i>9 times in 10 inside</span>` : ""}${M.q && M.q.some(q => neg(q) >= 0.1) ? `<span><i style="background:${blue};opacity:.45"></i>chance of a negative price</span>` : ""}</div>
      <div class="fv-low"><div class="fv-pane">${risk(M)}</div>
      <div class="fv-pane"><h2>Hours inside the range</h2><p>Per day, out of 24. Pale: fewer than 8 in 10</p>${daybars(M)}</div>
      <div class="fv-pane"><h2>Last 30 days, ${D.models[D.main].label}</h2><p>Dot: where it is. Line: the aim</p>${D.checks.map(meter).join("")}</div></div>`;
    const tip = root.querySelector(".fv-tip"), box = root.querySelector(".fv-chart");
    root.querySelectorAll(".fv-hit").forEach(el => {
      el.addEventListener("mousemove", e => {
        const h = +el.dataset.h, q = M.q && M.q[h], b = box.getBoundingClientRect();
        tip.innerHTML = `<b>${hh(h)}:00</b> ${r0(f[h])} €/MWh` + (q ? `<br>half the time: ${r0(q[4])} to ${r0(q[14])}<br>9 in 10: ${r0(q[0])} to ${r0(q[18])}<br>below 0: ${pct(neg(q))}` : "");
        tip.hidden = false;
        tip.style.left = Math.min(b.width - 190, Math.max(0, e.clientX - b.left + 14)) + "px"; tip.style.top = Math.max(0, e.clientY - b.top - 70) + "px";
      });
      el.addEventListener("mouseleave", () => { tip.hidden = true; });
    });
    root.querySelectorAll(".fv-pills button").forEach(b => b.addEventListener("click", () => { cur = b.dataset.m; draw(); }));
  }
  draw();
})();
