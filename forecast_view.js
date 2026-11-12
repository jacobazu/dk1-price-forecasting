// forecast page: the forecast per hour with its range, and the hours that may cross a price limit the reader sets. data from R/forecast_view.R
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
  // a price above 200 €/MWh came in about 1 hour in 20 over the last two years, a negative price in about 1 in 27
  const LIM = 200, ODDS = 0.1;
  const flag = q => !q ? null : above(q, LIM) >= ODDS ? "hi" : (1 - above(q, 0)) >= ODDS ? "neg" : null;

  // each hour: the forecast as a dot, the 9-in-10 range as a bar. hours where the chance of crossing the limit
  // (or of a negative price) is at least the chosen odds are coloured, with the chance written above
  function main(M) {
    const f = M.mean, Q = M.q;
    const all = Q ? Q.flatMap(r => [r[0], r[18]]).concat(f) : f.slice(), span = Math.max(...all) - Math.min(0, ...all);
    const st = [25, 50, 100, 200, 500].find(v => span / v <= 5) || 1000;
    const top = Math.max(st, Math.ceil(Math.max(...all) * 1.08 / st) * st), bot = Math.min(0, Math.floor(Math.min(...all) / st) * st);
    const W = 1190, H = 440, L = 44, R = 70, T = 30, B = 36;
    const y = p => T + (H - T - B) * (1 - (p - bot) / (top - bot)), cw = (W - L - R) / n, xs = h => L + cw * h, x = h => xs(h) + cw / 2;
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Forecast price per hour, with the hours that may cross ${LIM} €/MWh marked">`;
    for (let g = bot; g <= top + 1e-9; g += st) s += `<line x1="${L}" x2="${W - R}" y1="${y(g)}" y2="${y(g)}" stroke="${g == 0 && bot < 0 ? "#a8a29e" : grid}"/><text x="${L - 10}" y="${y(g) + 4.5}" text-anchor="end" fill="${mute}">${r0(g)}</text>`;
    // the limit
    s += `<text x="4" y="${T - 12}" fill="${mute}" class="fv-s">€/MWh</text>`;
    // the limit, drawn only when it is inside the chart: a limit far above every range shouldn't squash the chart
    if (LIM <= top && LIM >= bot) {
      s += `<line x1="${L}" x2="${W - R}" y1="${y(LIM)}" y2="${y(LIM)}" stroke="${amber}" stroke-width="1" stroke-opacity=".8" stroke-dasharray="4 5"/>`;
      s += `<text x="${W - R + 8}" y="${y(LIM) - 3}" fill="${amber}" class="fv-s">limit</text><text x="${W - R + 8}" y="${y(LIM) + 12}" fill="${amber}" class="fv-s">${r0(LIM)}</text>`;
    } else s += `<text x="${W - R}" y="${T - 12}" text-anchor="end" fill="${mute}" class="fv-s">No hour is near ${LIM} €/MWh</text>`;
    const bw = Math.min(26, cw * 0.62);
    s += `<path d="${f.map((p, h) => `${h ? "L" : "M"}${x(h).toFixed(1)},${y(p).toFixed(1)}`).join("")}" fill="none" stroke="${ink}" stroke-opacity=".35" stroke-width="1.5"/>`;
    for (let h = 0; h < n; h++) {
      const q = Q && Q[h], fl = flag(q), col = fl == "hi" ? amber : fl == "neg" ? blue : ink;
      if (q) s += `<rect x="${x(h) - bw / 2}" width="${bw}" y="${y(q[18])}" height="${Math.max(3, y(q[0]) - y(q[18]))}" rx="${Math.min(8, bw / 2)}" fill="${col}" fill-opacity="${fl ? .22 : .07}"/>`;
      s += `<circle cx="${x(h)}" cy="${y(f[h])}" r="${fl ? 5 : 4}" fill="${fl ? col : ink}" stroke="#fff" stroke-width="1.5"/>`;
      if (fl) {
        const c = fl == "hi" ? above(q, LIM) : 1 - above(q, 0);
        s += `<text x="${x(h)}" y="${y(q[18]) - 8}" text-anchor="middle" fill="${col}" class="fv-s" font-weight="600">${pct(c)}</text>`;
      }
      if (h % 3 == 0) s += `<text x="${x(h)}" y="${H - 10}" text-anchor="middle" fill="${mute}">${hh(h)}:00</text>`;
      s += `<rect class="fv-hit" data-h="${h}" x="${xs(h)}" width="${cw}" y="${T}" height="${H - T}" fill="transparent"/>`;
    }
    return s + "</svg>";
  }

  // the hours that cross, as a list: neighbouring hours of the same kind in one row
  function risks(M) {
    if (!M.q) return "<h2>No range yet</h2>";
    const head = `<h2>Hours to watch</h2><p>At least a 1 in 10 chance above ${LIM} €/MWh or below 0</p>`;
    const runs = [];
    M.q.forEach((q, h) => {
      const fl = flag(q); if (!fl) return;
      const c = fl == "hi" ? above(q, LIM) : 1 - above(q, 0), last = runs[runs.length - 1];
      if (last && last.fl == fl && last.b == h - 1) { last.b = h; last.c = Math.max(last.c, c); last.f.push(M.mean[h]); }
      else runs.push({ fl, a: h, b: h, c, f: [M.mean[h]] });
    });
    if (!runs.length) return head + `<p class="fv-none">None tomorrow. The highest chance of going above ${r0(LIM)} is ${pct(Math.max(...M.q.map(q => above(q, LIM))))}.</p>`;
    return head + `<table class="fv-tab"><tr><th>Hours</th><th>Forecast</th><th>Chance, at most</th></tr>` + runs.map(r =>
      `<tr><td>${hh(r.a)}:00${r.b > r.a ? `–${hh(r.b + 1)}:00` : ""}</td><td>${r.b > r.a ? `${r0(Math.min(...r.f))} to ${r0(Math.max(...r.f))}` : r0(r.f[0])}</td><td style="color:${r.fl == "hi" ? amber : blue};font-weight:600">${pct(r.c)} ${r.fl == "hi" ? "above " + r0(LIM) : "below 0"}</td></tr>`).join("") + "</table>";
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
      ${M.q ? `<div class="fv-ctl">Marked: hours with at least a 1 in 10 chance of a price above ${LIM} €/MWh, which only about 1 hour in 20 reached over the last two years, or below 0.</div>` : ""}
      <div class="fv-chart">${main(M)}<div class="fv-tip" hidden></div></div>
      ${M.q ? `<div class="fv-key"><span><i style="background:${ink};height:8px;width:8px;border-radius:50%"></i>forecast</span><span><i style="background:${ink};opacity:.12"></i>9 times in 10 inside</span><span><i style="background:${amber};opacity:.5"></i>may go above the limit</span><span><i style="background:${blue};opacity:.5"></i>may go below 0</span></div>` : ""}
      <div class="fv-low"><div class="fv-pane">${risks(M)}</div>
      <div class="fv-pane"><h2>Hours inside the range</h2><p>Per day, out of 24. Pale: fewer than 8 in 10</p>${daybars(M)}</div>
      <div class="fv-pane"><h2>Last 30 days, ${D.models[D.main].label}</h2><p>Dot: where it is. Line: the aim</p>${D.checks.map(meter).join("")}</div></div>`;
    const tip = root.querySelector(".fv-tip"), box = root.querySelector(".fv-chart");
    root.querySelectorAll(".fv-hit").forEach(el => {
      el.addEventListener("mousemove", e => {
        const h = +el.dataset.h, q = M.q && M.q[h], b = box.getBoundingClientRect();
        tip.innerHTML = `<b>${hh(h)}:00</b> ${r0(f[h])} €/MWh` + (q ? `<br>9 in 10: ${r0(q[0])} to ${r0(q[18])}<br>above ${r0(LIM)}: ${pct(above(q, LIM))}<br>below 0: ${pct(1 - above(q, 0))}` : "");
        tip.hidden = false;
        tip.style.left = Math.min(b.width - 190, Math.max(0, e.clientX - b.left + 14)) + "px"; tip.style.top = Math.max(0, e.clientY - b.top - 70) + "px";
      });
      el.addEventListener("mouseleave", () => { tip.hidden = true; });
    });
    root.querySelectorAll(".fv-pills button").forEach(b => b.addEventListener("click", () => { cur = b.dataset.m; draw(); }));
  }
  draw();
})();
