// forecast page: one column per hour, brighter where the price is more likely. data from R/forecast_view.R
(function () {
  const D = JSON.parse(document.getElementById("fv-data").textContent), root = document.getElementById("fv");
  const taus = Array.from({ length: 19 }, (_, i) => 0.05 + 0.05 * i), n = 24;
  const mute = "#78716c", grid = "#f0ece8", aqua = "#c0800c", amber = "#b32640", ink = "#1c1917";
  const hh = h => String(h).padStart(2, "0"), r0 = v => Math.round(v);
  let cur = D.main;

  // density from the 19 quantiles: points spread evenly over 5-95%, gaussian kernel
  function dens(q, m) {
    const dr = [];
    for (let i = 0; i < 200; i++) {
      const t = 0.05 + 0.9 * i / 199, k = Math.min(17, Math.floor((t - 0.05) / 0.05 + 1e-9));
      dr.push(q[k] + (t - taus[k]) / 0.05 * (q[k + 1] - q[k]));
    }
    const mu = dr.reduce((a, b) => a + b) / 200, sd = Math.sqrt(dr.reduce((a, b) => a + (b - mu) ** 2, 0) / 200);
    const bw = Math.max(1, 0.4 * sd), lo = q[0], hi = q[18], out = [];
    for (let j = 0; j < m; j++) {
      const y = lo + (hi - lo) * j / (m - 1); let s = 0;
      for (const x of dr) s += Math.exp(-0.5 * ((y - x) / bw) ** 2);
      out.push([y, s]);
    }
    const mx = Math.max(...out.map(o => o[1]));
    return out.map(o => [o[0], o[1] / mx]);
  }
  function smooth(pts) {
    let p = "M" + pts[0];
    for (let i = 0; i < pts.length - 1; i++) {
      const a = pts[Math.max(0, i - 1)], b = pts[i], e = pts[i + 1], f = pts[Math.min(pts.length - 1, i + 2)];
      p += `C${b[0] + (e[0] - a[0]) / 6},${b[1] + (e[1] - a[1]) / 6} ${e[0] - (f[0] - b[0]) / 6},${e[1] - (f[1] - b[1]) / 6} ${e}`;
    }
    return p;
  }

  function main(M) {
    const f = M.mean, Q = M.q, pk = f.indexOf(Math.max(...f));
    const all = Q ? Q.flat().concat(f) : f, top = Math.max(50, Math.ceil(Math.max(...all) / 100) * 100), bot = Math.min(0, Math.floor(Math.min(...all) / 100) * 100);
    const W = 1190, H = 470, L = 44, R = 8, T = 16, B = 70, y = p => T + (H - T - B) * (1 - (p - bot) / (top - bot)), cw = (W - L - R) / n, x = h => L + cw * (h + .5);
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Forecast price per hour with its range"><defs>`;
    if (Q) for (let h = 0; h < n; h++) s += `<linearGradient id="fvg${h}" x1="0" y1="0" x2="0" y2="1">` + dens(Q[h], 40).reverse().map((o, i) =>
      `<stop offset="${(i / 39 * 100).toFixed(1)}%" stop-color="${h == pk ? amber : aqua}" stop-opacity="${(0.04 + 0.86 * o[1] ** 1.3).toFixed(3)}"/>`).join("") + "</linearGradient>";
    s += "</defs>";
    const step = (top - bot) / 4;
    for (let g = bot; g <= top + 1e-9; g += step) s += `<line x1="${L}" x2="${W - R}" y1="${y(g)}" y2="${y(g)}" stroke="${g == 0 && bot < 0 ? "#a8a29e" : grid}"/><text x="${L - 10}" y="${y(g) + 4.5}" text-anchor="end" fill="${mute}">${r0(g)}</text>`;
    for (let h = 0; h < n; h++) {
      if (Q) s += `<rect x="${x(h) - cw * .4}" width="${cw * .8}" y="${y(Q[h][18])}" height="${Math.max(2, y(Q[h][0]) - y(Q[h][18]))}" rx="${cw * .16}" fill="url(#fvg${h})"/>`;
      s += `<rect x="${x(h) - cw * .4}" width="${cw * .8}" y="${y(f[h]) - 1.5}" height="3" rx="1.5" fill="#1c1917"/>`;
      s += `<text x="${x(h)}" y="${H - 40}" text-anchor="middle" fill="${mute}">${hh(h)}</text><text x="${x(h)}" y="${H - 16}" text-anchor="middle" class="fv-v" fill="${h == pk ? amber : ink}">${r0(f[h])}</text>`;
      s += `<rect class="fv-hit" data-h="${h}" x="${x(h) - cw / 2}" width="${cw}" y="${T}" height="${H - T}" fill="transparent"/>`;
    }
    return s + "</svg>";
  }

  function one(M, h) {
    if (!M.q) return "";
    const q = M.q[h], dd = dens(q, 120), w = 360, hg = 132, pad = (q[18] - q[0]) * 0.04 + 1, lo = q[0] - pad, hi = q[18] + pad;
    const X = p => 6 + (w - 12) * (p - lo) / (hi - lo), Y = u => hg - 26 - (hg - 40) * u;
    const top = smooth(dd.filter((_, i) => i % 4 == 0 || i == 119).map(o => [X(o[0]), Y(o[1])]));
    let t = `<svg viewBox="0 0 ${w} ${hg}" role="img" aria-label="Distribution of the price at ${hh(h)}:00"><defs><linearGradient id="fva" x1="0" y1="0" x2="0" y2="1"><stop offset="0" stop-color="${aqua}" stop-opacity=".55"/><stop offset="1" stop-color="${aqua}" stop-opacity=".04"/></linearGradient></defs>`;
    t += `<path d="${top}L${X(q[18])},${Y(0)}L${X(q[0])},${Y(0)}Z" fill="url(#fva)"/><path d="${top}" fill="none" stroke="${aqua}" stroke-width="1.8"/>`;
    t += `<line x1="${X(M.mean[h])}" x2="${X(M.mean[h])}" y1="${Y(1.04)}" y2="${Y(0)}" stroke="#1c1917" stroke-width="1.6" stroke-dasharray="3 3"/>`;
    for (const k of [0, 9, 18]) t += `<text x="${X(q[k])}" y="${hg - 6}" text-anchor="${k == 0 ? "start" : k == 18 ? "end" : "middle"}" fill="${mute}">${r0(q[k])}</text>`;
    return t + "</svg>";
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
    const unc = M.q ? M.q.map(q => q[18] - q[0]).indexOf(Math.max(...M.q.map(q => q[18] - q[0]))) : null;
    const keys = Object.keys(D.models);
    root.innerHTML = `<div class="fv-top"><h1>${D.day}<small>Saved ${D.made}, before the auction. The darker the colour, the more likely the price.</small></h1>
      <div class="fv-num"><b>${avg}</b>average, €/MWh</div><div class="fv-num"><b style="color:${amber}">${r0(f[pk])}</b>highest, ${hh(pk)}:00</div>
      <div class="fv-num"><b>${r0(f[lw])}</b>lowest, ${hh(lw)}:00</div><div class="fv-num"><b>${D.neg}</b>chance of a negative price</div></div>
      ${keys.length > 1 ? `<div class="fv-pills">${keys.map(k => `<button data-m="${k}" class="${k == cur ? "on" : ""}">${D.models[k].label}</button>`).join("")}</div>` : ""}
      <div class="fv-chart">${main(M)}<div class="fv-tip" hidden></div></div>
      <div class="fv-low"><div class="fv-pane">${unc == null ? "<h2>No range yet</h2>" : `<h2>${hh(unc)}:00, the least certain hour</h2><p>Half the time below ${r0(M.q[unc][9])} €/MWh</p>${one(M, unc)}`}</div>
      <div class="fv-pane"><h2>Hours inside the range</h2><p>Per day, out of 24. Pale: fewer than 8 in 10</p>${daybars(M)}</div>
      <div class="fv-pane"><h2>Last 30 days, ${D.models[D.main].label}</h2><p>Dot: where it is. Line: the aim</p>${D.checks.map(meter).join("")}</div></div>`;
    const tip = root.querySelector(".fv-tip"), box = root.querySelector(".fv-chart");
    root.querySelectorAll(".fv-hit").forEach(el => {
      el.addEventListener("mousemove", e => {
        const h = +el.dataset.h, q = M.q && M.q[h], b = box.getBoundingClientRect();
        tip.innerHTML = `<b>${hh(h)}:00</b> ${r0(f[h])} €/MWh` + (q ? `<br>9 in 10: ${r0(q[0])} to ${r0(q[18])}<br>half the time: ${r0(q[4])} to ${r0(q[14])}` : "");
        tip.hidden = false;
        tip.style.left = Math.min(b.width - 190, Math.max(0, e.clientX - b.left + 14)) + "px"; tip.style.top = Math.max(0, e.clientY - b.top - 70) + "px";
      });
      el.addEventListener("mouseleave", () => { tip.hidden = true; });
    });
    root.querySelectorAll(".fv-pills button").forEach(b => b.addEventListener("click", () => { cur = b.dataset.m; draw(); }));
  }
  draw();
})();
