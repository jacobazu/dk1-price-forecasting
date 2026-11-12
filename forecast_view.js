// forecast page: tomorrow's price per hour and how uncertain it is. a fan (half the time inside the dark band, 9 times
// in 10 inside the light one), the day as a whole, each part of the day, tomorrow against a normal day, and whether
// the ranges have held. no trading. data from R/forecast_view.R
(function () {
  const D = JSON.parse(document.getElementById("fv-data").textContent), root = document.getElementById("fv");
  const taus = Array.from({ length: 19 }, (_, i) => 0.05 + 0.05 * i), n = 24;
  const mute = "#78716c", grid = "#f0ece8", ink = "#1c1917", blue = "#1c7fb0";
  const hh = h => String(h).padStart(2, "0"), r0 = v => Math.round(v), mean = a => a.reduce((x, y) => x + y, 0) / a.length;
  let cur = D.main;

  // chance the price ends above x, read off the 19 quantiles, linear in between
  function above(q, x) {
    if (x <= q[0]) return 0.95; if (x >= q[18]) return 0.05;
    let k = 0; while (q[k + 1] < x) k++;
    return 1 - (taus[k] + 0.05 * (x - q[k]) / Math.max(1e-9, q[k + 1] - q[k]));
  }
  const neg = q => q ? 1 - above(q, 0) : 0;
  const pct = c => c >= 0.945 ? "over 95%" : c < 0.055 ? "under 5%" : r0(100 * c) + "%";
  const rng = (a, b) => `${r0(a)} to ${r0(b)}`;

  function chart(M) {
    const f = M.mean, Q = M.q, A = D.actual, c = M.color;
    const all = (Q ? Q.flatMap(r => [r[0], r[18]]) : []).concat(f, A || []), span = Math.max(...all) - Math.min(0, ...all);
    const st = [25, 50, 100, 200, 500].find(v => span / v <= 5) || 1000;
    const top = Math.max(st, Math.ceil(Math.max(...all) * 1.04 / st) * st), bot = Math.min(0, Math.floor(Math.min(...all) / st) * st);
    const W = 1190, H = 470, L = 46, R = 16, T = 30, B = 34;
    const y = p => T + (H - T - B) * (1 - (p - bot) / (top - bot)), cw = (W - L - R) / n, x = h => L + cw * (h + 0.5);
    const pts = v => v.map((p, h) => `${x(h).toFixed(1)},${y(p).toFixed(1)}`);
    const band = (i, j) => `M${pts(Q.map(q => q[j])).join("L")}L${pts(Q.map(q => q[i])).reverse().join("L")}Z`;
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Forecast price for each hour with the range it should land in">`;
    for (let g = bot; g <= top + 1e-9; g += st) s += `<line x1="${L}" x2="${W - R}" y1="${y(g)}" y2="${y(g)}" stroke="${g == 0 && bot < 0 ? "#a8a29e" : grid}"/><text x="${L - 10}" y="${y(g) + 4.5}" text-anchor="end" fill="${mute}">${r0(g)}</text>`;
    s += `<text x="0" y="${T - 14}" fill="${mute}" class="fv-s">€/MWh</text>`;
    if (Q) {
      s += `<path d="${band(0, 18)}" fill="${c}" fill-opacity=".13"/><path d="${band(4, 14)}" fill="${c}" fill-opacity=".26"/>`;
      // the least certain hour, named on the chart
      const w = Q.map(q => q[18] - q[0]), wi = w.indexOf(Math.max(...w)), ax = Math.min(W - R - 150, Math.max(L + 150, x(wi)));
      s += `<line x1="${x(wi)}" x2="${x(wi)}" y1="${y(Q[wi][18]) - 4}" y2="${y(Q[wi][18]) - 14}" stroke="${mute}"/>`;
      s += `<text x="${ax}" y="${y(Q[wi][18]) - 20}" text-anchor="middle" fill="#44403c" class="fv-s">Least certain: ${hh(wi)}:00, ${rng(Q[wi][0], Q[wi][18])}</text>`;
      Q.forEach((q, h) => { if (neg(q) >= 0.1) s += `<rect x="${x(h) - cw / 2 + 2}" width="${cw - 4}" y="${y(0)}" height="${Math.max(0, y(q[0]) - y(0))}" fill="${blue}" fill-opacity=".3" rx="3"/>`; });
    }
    s += `<path d="M${pts(f).join("L")}" fill="none" stroke="${ink}" stroke-width="2.5" stroke-linejoin="round" stroke-linecap="round"/>`;
    if (A) A.forEach((p, h) => {
      const out = Q && (p < Q[h][0] || p > Q[h][18]);
      s += `<circle cx="${x(h)}" cy="${y(p)}" r="4.5" fill="${out ? "#b91c1c" : "#ffffff"}" stroke="${out ? "#b91c1c" : ink}" stroke-width="1.8"/>`;
    });
    for (let h = 0; h < n; h += 3) s += `<text x="${x(h)}" y="${H - 9}" text-anchor="middle" fill="${mute}">${hh(h)}:00</text>`;
    s += `<g class="fv-ruler" visibility="hidden"><line y1="${T}" y2="${H - B}" stroke="${ink}" stroke-opacity=".35" stroke-dasharray="3 4"/><circle r="5" fill="${ink}" stroke="#fff" stroke-width="2"/></g>`;
    for (let h = 0; h < n; h++) s += `<rect class="fv-hit" data-h="${h}" data-x="${x(h)}" data-y="${y(f[h])}" x="${x(h) - cw / 2}" width="${cw}" y="${T}" height="${H - T - B}" fill="transparent"/>`;
    return s + "</svg>";
  }

  // each part of the day as one range bar on a shared scale
  function blocks(M) {
    const head = `<h2>By part of the day</h2><p>Average price in each part, €/MWh</p>`;
    if (!M.blocks) return head + `<p class="fv-none">No range for this model yet.</p>`;
    const mn = Math.min(...M.blocks.map(b => Math.min(b.q[0], b.f))), mx = Math.max(...M.blocks.map(b => Math.max(b.q[3], b.f)));
    const lo = mn - 0.04 * (mx - mn), hi = mx + 0.04 * (mx - mn), p = v => 100 * (v - lo) / (hi - lo);
    return head + M.blocks.map(b => `<div class="fv-blk"><div class="fv-blk-n"><b>${b.name}</b>${hh(b.from)}–${hh(b.to == 24 ? 0 : b.to)}</div>
      <div class="fv-blk-b"><i style="left:${p(b.q[0])}%;width:${p(b.q[3]) - p(b.q[0])}%;background:${M.color};opacity:.2"></i><i style="left:${p(b.q[1])}%;width:${p(b.q[2]) - p(b.q[1])}%;background:${M.color};opacity:.5"></i><u style="left:${p(b.f)}%"></u></div>
      <div class="fv-blk-v"><b>${r0(b.f)}</b>${rng(b.q[0], b.q[3])}</div></div>`).join("") +
      `<p class="fv-foot">Dot: forecast. Bar: where it lands 9 times in 10, darker for half the time.</p>`;
  }

  // tomorrow's range width against the last 60 days
  function normal(M, wT) {
    const head = `<h2>Against a normal day</h2><p>How far the price can land from the forecast, 9 times in 10</p>`;
    if (!M.widths || wT == null) return head + `<p class="fv-none">Needs more scored days.</p>`;
    const ws = M.widths, med = ws.slice().sort((a, b) => a - b)[Math.floor(ws.length / 2)];
    const hi = Math.max(wT, ...ws) * 1.08, W = 360, H = 96, x = v => 8 + (W - 16) * v / hi;
    let s = `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="Tomorrow's range width against the last ${ws.length} days">`;
    s += `<line x1="8" x2="${W - 8}" y1="62" y2="62" stroke="${grid}" stroke-width="2"/>`;
    ws.forEach(w => { s += `<line x1="${x(w)}" x2="${x(w)}" y1="50" y2="62" stroke="${mute}" stroke-opacity=".45" stroke-width="2" stroke-linecap="round"/>`; });
    s += `<line x1="${x(med)}" x2="${x(med)}" y1="44" y2="68" stroke="${ink}" stroke-width="1.5"/><text x="${x(med)}" y="84" text-anchor="middle" fill="${mute}" class="fv-s">normal, ±${r0(med)}</text>`;
    const tx = Math.min(W - 60, Math.max(60, x(wT)));
    s += `<circle cx="${x(wT)}" cy="56" r="7" fill="${M.color}" stroke="#fff" stroke-width="2"/><text x="${tx}" y="30" text-anchor="middle" fill="${ink}" font-weight="600">tomorrow, ±${r0(wT)}</text><line x1="${x(wT)}" x2="${x(wT)}" y1="36" y2="47" stroke="${ink}"/>`;
    return head + s + `</svg><p class="fv-foot">Each tick is one of the last ${ws.length} days. €/MWh either side of the forecast, average over the 24 hours.</p>`;
  }

  // have the ranges held: hours inside per day
  function held(M) {
    const d = M.days, head = `<h2>Have the ranges held?</h2>`;
    if (!d) return head + `<p class="fv-none">No scored days yet.</p>`;
    const share = d.inside.reduce((a, b) => a + b, 0) / d.n.reduce((a, b) => a + b, 0);
    const k = d.inside.length, w = 360, hg = 112, bw = (w - 12) / k, r = Math.min(5, (bw - 3) / 2);
    let b = `<svg viewBox="0 0 ${w} ${hg}" role="img" aria-label="Hours inside the range on each day">`;
    d.inside.forEach((v, i) => {
      const hgt = Math.max(2 * r, (hg - 34) * v / d.n[i]);
      b += `<rect x="${6 + bw * i + 1.5}" width="${bw - 3}" y="8" height="${hg - 34}" rx="${r}" fill="#f5f2ef"/>`;
      b += `<rect x="${6 + bw * i + 1.5}" width="${bw - 3}" y="${hg - 26 - hgt}" height="${hgt}" rx="${r}" fill="${M.color}" fill-opacity="${v / d.n[i] >= 0.8 ? 1 : .4}"><title>${d.dates[i]}: ${v} of ${d.n[i]} hours inside</title></rect>`;
    });
    b += `<text x="6" y="${hg - 6}" fill="${mute}" class="fv-s">${d.dates[0]}</text><text x="${w - 6}" y="${hg - 6}" text-anchor="end" fill="${mute}" class="fv-s">${d.dates[k - 1]}</text></svg>`;
    const bias = cur == D.main && D.bias != null && Math.abs(D.bias) >= 5 ? ` Lately the forecast has been about ${r0(Math.abs(D.bias))} €/MWh too ${D.bias > 0 ? "low" : "high"} on average.` : "";
    return head + `<p><b class="fv-big">${r0(100 * share)}%</b> of hours landed inside the 9-in-10 range over the last ${k} days. The aim is 90%.</p>` + b +
      `<p class="fv-foot">One bar per day: hours inside, out of 24. Pale: fewer than 8 in 10.${bias}</p>`;
  }

  function draw() {
    const M = D.models[cur], f = M.mean, Q = M.q, A = D.actual, avg = mean(f), keys = Object.keys(D.models);
    const wT = Q ? mean(Q.map(q => q[18] - q[0])) / 2 : null;
    const wider = M.widths && wT != null ? M.widths.filter(w => w < wT).length / M.widths.length : null;
    const level = wider == null ? null : wider >= 0.7 ? "More than usual" : wider <= 0.3 ? "Less than usual" : "About as usual";
    const ng = Q ? Math.max(...Q.map(neg)) : null, gi = Q ? Q.map(neg).indexOf(ng) : 0;
    const hit = A && Q ? A.filter((p, h) => p >= Q[h][0] && p <= Q[h][18]).length : null;
    const stat = (k, v, sub) => `<div class="fv-stat"><span>${k}</span><b>${v}</b><small>${sub}</small></div>`;
    root.innerHTML = `<div class="fv-top"><h1>${D.day}<small>Forecast saved ${D.made}, before the auction${A ? `. The prices are out: ${hit == null ? "" : `<b>${hit} of 24 hours</b> landed inside the 9-in-10 range, `}the day averaged ${r0(mean(A))} €/MWh` : ""}</small></h1>
      ${keys.length > 1 ? `<div class="fv-pills">${keys.map(k => `<button data-m="${k}" class="${k == cur ? "on" : ""}">${D.models[k].label}</button>`).join("")}</div>` : ""}</div>
      <div class="fv-stats">
      ${stat("Day average", `${r0(avg)}<em> €/MWh</em>`, "the forecast")}
      ${stat("9 times in 10", M.day ? `${r0(M.day[0])}<em> to </em>${r0(M.day[3])}` : "–", "where the day average should land")}
      ${stat("Uncertainty", level || "–", level ? `wider range than on ${M.widths.filter(w => w < wT).length} of the last ${M.widths.length} days` : "no range for this model yet")}
      ${stat("Negative price", ng == null ? "–" : pct(ng), ng == null ? "" : ng < 0.055 ? "chance, in every hour" : `chance at ${hh(gi)}:00, the most likely hour`)}
      </div>
      <div class="fv-chart">${chart(M)}<div class="fv-tip" hidden></div></div>
      <div class="fv-key"><span><i style="background:${ink};height:3px"></i>forecast</span>${Q ? `<span><i style="background:${M.color};opacity:.5"></i>half the time inside</span><span><i style="background:${M.color};opacity:.2"></i>9 times in 10 inside</span>` : ""}${Q && Q.some(q => neg(q) >= 0.1) ? `<span><i style="background:${blue};opacity:.45"></i>1 in 10 or more chance of a negative price</span>` : ""}${A ? `<span><i class="fv-dot"></i>what it cost</span>${hit != null && hit < 24 ? `<span><i class="fv-dot" style="background:#b91c1c;border-color:#b91c1c"></i>outside the range</span>` : ""}` : ""}</div>
      <div class="fv-low"><div class="fv-pane">${blocks(M)}</div><div class="fv-pane">${normal(M, wT)}</div><div class="fv-pane">${held(M)}</div></div>`;
    const tip = root.querySelector(".fv-tip"), box = root.querySelector(".fv-chart"), ruler = root.querySelector(".fv-ruler");
    root.querySelectorAll(".fv-hit").forEach(el => {
      el.addEventListener("mousemove", e => {
        const h = +el.dataset.h, q = Q && Q[h], b = box.getBoundingClientRect();
        ruler.setAttribute("visibility", "visible");
        ruler.querySelector("line").setAttribute("x1", el.dataset.x); ruler.querySelector("line").setAttribute("x2", el.dataset.x);
        ruler.querySelector("circle").setAttribute("cx", el.dataset.x); ruler.querySelector("circle").setAttribute("cy", el.dataset.y);
        tip.innerHTML = `<b>${hh(h)}:00 to ${hh((h + 1) % 24)}:00</b><table><tr><td>Forecast</td><td>${r0(f[h])}</td></tr>` +
          (q ? `<tr><td>Half the time</td><td>${rng(q[4], q[14])}</td></tr><tr><td>9 times in 10</td><td>${rng(q[0], q[18])}</td></tr><tr><td>Below 0</td><td>${pct(neg(q))}</td></tr>` : "") +
          (A ? `<tr><td>What it cost</td><td><b>${r0(A[h])}</b></td></tr>` : "") + `</table>`;
        tip.hidden = false;
        tip.style.left = Math.min(b.width - 230, Math.max(0, e.clientX - b.left + 16)) + "px"; tip.style.top = Math.max(0, e.clientY - b.top - 90) + "px";
      });
      el.addEventListener("mouseleave", () => { tip.hidden = true; ruler.setAttribute("visibility", "hidden"); });
    });
    root.querySelectorAll(".fv-pills button").forEach(b => b.addEventListener("click", () => { cur = b.dataset.m; draw(); }));
  }
  draw();
})();
