// Homepage hero panels: four small figures of simulated data, one for each
// research theme, in the order an outbreak analysis runs from data to
// decision (real-time estimation, forecasting, transmission and
// interventions, modelling for decisions). They are static and unlabelled;
// each panel's title and description are in the page and in its SVG's
// accessible label.
// Colours come from CSS custom properties, so the figures follow the
// light/dark theme.
(function () {
  "use strict";

  const NS = "http://www.w3.org/2000/svg";
  const W = 400;
  const H = 300;

  function el(name, attrs, parent) {
    const node = document.createElementNS(NS, name);
    for (const k in attrs) node.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(node);
    return node;
  }

  // Seeded generator, so a figure can be redrawn from the same seed
  function rng(seed) {
    let s = seed >>> 0;
    return function () {
      s = (s + 0x6d2b79f5) >>> 0;
      let t = s;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  }

  function normal(u) {
    return Math.sqrt(-2 * Math.log(u() + 1e-12)) * Math.cos(2 * Math.PI * u());
  }

  // the figure is drawn above the caption the page gives each panel
  function svgIn(container, label) {
    const svg = el("svg", { viewBox: `0 0 ${W} ${H}`, class: "ef-panel-svg", role: "img", "aria-label": label });
    container.insertBefore(svg, container.firstChild);
    return svg;
  }

  // Forecasting: observations up to now, a forecast fan from the recent
  // growth rate, and the later observations inside or outside it

  const N = 56;
  const NOW = 36;

  function simulate(seed) {
    const u = rng(seed);
    const peak = 30 + u() * 22;
    const r0 = 0.11 + u() * 0.06;
    const logI = [Math.log(8 + u() * 6)];
    for (let t = 1; t < N; t++) {
      const r = r0 * Math.cos((Math.PI * t) / (2 * peak)) + 0.025 * normal(u);
      logI.push(logI[t - 1] + r);
    }
    const obs = logI.map((x) => Math.max(0, Math.exp(x) * Math.exp(0.16 * normal(u))));

    // Fit: smoothed log observations up to now
    const fit = [];
    for (let t = 0; t <= NOW; t++) {
      let s = 0;
      let n = 0;
      for (let k = Math.max(0, t - 3); k <= Math.min(NOW, t + 3); k++) {
        s += Math.log(obs[k] + 1);
        n++;
      }
      fit.push(Math.exp(s / n) - 1);
    }

    // Forecast: carry on the recent growth rate with uncertainty that widens
    const r = (Math.log(fit[NOW] + 1) - Math.log(fit[NOW - 6] + 1)) / 6;
    const fc = [];
    for (let h = 0; h < N - NOW; h++) {
      const mu = Math.log(fit[NOW] + 1) + r * h;
      const sd = 0.12 + 0.075 * Math.sqrt(h) + 0.012 * h;
      fc.push({
        med: Math.exp(mu) - 1,
        lo50: Math.exp(mu - 0.674 * sd) - 1,
        hi50: Math.exp(mu + 0.674 * sd) - 1,
        lo90: Math.exp(mu - 1.645 * sd) - 1,
        hi90: Math.exp(mu + 1.645 * sd) - 1,
      });
    }
    return { obs, fit, fc };
  }

  function inside(sim, t, lo, hi) {
    const f = sim.fc[t - NOW];
    return sim.obs[t] >= f[lo] && sim.obs[t] <= f[hi];
  }

  // A random outbreak whose forecast does reasonably and whose observations
  // all fit in the plot, so no visitor's first view is a forecast that
  // missed everything
  function pickSeed() {
    for (let i = 0; i < 200; i++) {
      const seed = 1 + Math.floor(Math.random() * 1e6);
      const sim = simulate(seed);
      let in50 = 0;
      let in90 = 0;
      for (let t = NOW + 1; t < N; t++) {
        if (inside(sim, t, "lo50", "hi50")) in50++;
        if (inside(sim, t, "lo90", "hi90")) in90++;
      }
      const pastMax = Math.max(...sim.obs.slice(0, NOW + 1));
      const fits = Math.max(...sim.obs) <= pastMax * 2.6;
      if (in50 >= 8 && in90 >= 15 && fits) return seed;
    }
    return 4;
  }

  function forecast(container, label) {
    const svg = svgIn(container, label);
    const sim = simulate(pickSeed());
    const { obs, fit, fc } = sim;
    // Scale to the observations so the history fills a good share of the
    // height; a wide forecast band runs off the top and is clipped there
    const pastMax = Math.max(...obs.slice(0, NOW + 1));
    const futureMax = Math.max(...obs.slice(NOW + 1));
    const ymax = Math.max(pastMax * 1.6, Math.min(futureMax, pastMax * 2.6)) * 1.05;
    const L = 14, R = W - 14, T = 18, B = H - 18;
    const x = (t) => L + ((R - L) * t) / (N - 1);
    const y = (v) => B - ((B - T) * v) / ymax;

    const clip = el("clipPath", { id: "ef-forecast-area" }, el("defs", {}, svg));
    el("rect", { x: 0, y: T - 6, width: W, height: B - T + 12 }, clip);
    el("line", { x1: L, x2: R, y1: B, y2: B, class: "ef-axis" }, svg);
    const plot = el("g", { "clip-path": "url(#ef-forecast-area)" }, svg);

    const area = (lo, hi) =>
      "M" + fc.map((d, h) => `${x(NOW + h)},${y(d[hi])}`).join("L") +
      "L" + fc.map((d, h) => `${x(NOW + h)},${y(d[lo])}`).reverse().join("L") + "Z";
    el("path", { d: area("lo90", "hi90"), class: "ef-band-outer" }, plot);
    el("path", { d: area("lo50", "hi50"), class: "ef-band-inner" }, plot);
    el("path", { d: "M" + fc.map((d, h) => `${x(NOW + h)},${y(d.med)}`).join("L"), class: "ef-median" }, plot);
    el("line", { x1: x(NOW), x2: x(NOW), y1: T - 6, y2: B, class: "ef-now" }, svg);
    el("path", { d: "M" + fit.map((v, t) => `${x(t)},${y(v)}`).join("L"), class: "ef-fit" }, plot);
    for (let t = 0; t < N; t++) {
      const cls = t <= NOW ? "ef-obs past" : inside(sim, t, "lo90", "hi90") ? "ef-obs inside" : "ef-obs outside";
      el("circle", { cx: x(t), cy: y(obs[t]), r: 3.4, class: cls }, plot);
    }
  }

  // Nowcasting: the most recent days are not fully reported yet; the
  // nowcast estimates their eventual counts, and Rt is tracked underneath

  function nowcast(container, label) {
    const svg = svgIn(container, label);
    const u = rng(11);
    const T = 48;
    const Rt = [];
    for (let t = 0; t < T; t++) {
      Rt.push(1.45 - 0.75 / (1 + Math.exp(-(t - 20) / 3)) + 0.35 / (1 + Math.exp(-(t - 36) / 2.5)));
    }
    const inc = [6];
    for (let t = 1; t < T; t++) inc.push(inc[t - 1] * Math.exp((Rt[t] - 1) / 4));
    const obs = inc.map((v) => Math.max(1, v * Math.exp(0.15 * normal(u))));
    // the share of each day's cases reported so far, from a reporting delay
    const rep = obs.map((v, t) => v * (1 - Math.exp(-(T - 1 - t + 0.5) / 3)));
    const ymax = Math.max(...obs) * 1.25;
    const L = 14, R = W - 14, top = 18, base = 190;
    const x = (t) => L + ((R - L) * t) / (T - 1);
    const y = (v) => base - ((base - top) * v) / ymax;
    const bw = ((R - L) / T) * 0.72;
    const recent = 10;

    el("line", { x1: L, x2: R, y1: base, y2: base, class: "ef-axis" }, svg);
    for (let t = 0; t < T; t++) {
      const late = T - 1 - t < recent;
      if (late) {
        const sd = 0.06 + 0.04 * (recent - 1 - (T - 1 - t));
        const hi = obs[t] * Math.exp(1.64 * sd);
        const lo = Math.max(rep[t], obs[t] * Math.exp(-1.64 * sd));
        el("rect", { x: x(t) - bw / 2, y: y(hi), width: bw, height: y(lo) - y(hi), class: "ef-band-inner" }, svg);
      }
      const shown = late ? rep[t] : obs[t];
      el("rect", { x: x(t) - bw / 2, y: y(shown), width: bw, height: base - y(shown), class: "ef-bar" }, svg);
    }
    el("line", { x1: x(T - recent) - bw, x2: x(T - recent) - bw, y1: top - 6, y2: base, class: "ef-now" }, svg);

    // Rt underneath, with its uncertainty growing towards today
    const rtop = 214, rbot = 286, rmin = 0.5, rmax = 1.8;
    const ry = (v) => rbot - ((rbot - rtop) * (v - rmin)) / (rmax - rmin);
    const sd = (t) => 0.05 + 0.25 * Math.max(0, (t - (T - 12)) / 12);
    const upper = Rt.map((v, t) => `${x(t)},${ry(v + 1.64 * sd(t))}`);
    const lower = Rt.map((v, t) => `${x(t)},${ry(v - 1.64 * sd(t))}`).reverse();
    el("path", { d: "M" + upper.join("L") + "L" + lower.join("L") + "Z", class: "ef-band-inner" }, svg);
    el("line", { x1: L, x2: R, y1: ry(1), y2: ry(1), class: "ef-ref" }, svg);
    el("path", { d: "M" + Rt.map((v, t) => `${x(t)},${ry(v)}`).join("L"), class: "ef-fit" }, svg);
  }

  // Transmission and control: a branching-process outbreak in which contact
  // tracing isolates some cases, ending their chains

  function transmission(container, label) {
    const svg = svgIn(container, label);
    let nodes;
    for (let seed = 7; ; seed++) {
      const u = rng(seed);
      // offspring: Poisson with a log-normally varying rate (mean 1.8), so a
      // few cases infect many and most infect few
      const offspring = () => {
        const lam = 1.8 * Math.exp(0.9 * normal(u) - 0.405);
        let n = 0;
        let p = Math.exp(-lam);
        let s = p;
        const r = u();
        while (r > s && n < 8) {
          n++;
          p *= lam / n;
          s += p;
        }
        return n;
      };
      nodes = [{ id: 0, parent: -1, gen: 0, t: 0, traced: false }];
      for (let i = 0; i < nodes.length && nodes.length < 60; i++) {
        const nd = nodes[i];
        if (nd.traced || nd.gen >= 6) continue;
        const kids = offspring();
        for (let j = 0; j < kids && nodes.length < 60; j++) {
          nodes.push({ id: nodes.length, parent: nd.id, gen: nd.gen + 1,
            t: nd.t + 5 + 4 * u(), traced: nd.gen >= 1 && u() < 0.45 });
        }
      }
      // keep an outbreak big enough to read and small enough to fit
      if (nodes.length >= 30 && nodes.length < 60) break;
    }
    // leaves top to bottom in depth-first order, parents centred on children
    const children = nodes.map(() => []);
    nodes.forEach((n) => { if (n.parent >= 0) children[n.parent].push(n.id); });
    const rows = [];
    let next = 0;
    (function place(id) {
      if (children[id].length === 0) {
        rows[id] = next++;
        return;
      }
      children[id].forEach(place);
      const rs = children[id].map((c) => rows[c]);
      rows[id] = (Math.min(...rs) + Math.max(...rs)) / 2;
    })(0);
    const tmax = Math.max(...nodes.map((n) => n.t));
    const x = (t) => 18 + ((W - 36) * t) / tmax;
    const y = (r) => 18 + ((H - 36) * r) / Math.max(1, next - 1);
    nodes.forEach((n) => {
      if (n.parent < 0) return;
      const p = nodes[n.parent];
      const mx = (x(p.t) + x(n.t)) / 2;
      el("path", { d: `M${x(p.t)},${y(rows[p.id])} C${mx},${y(rows[p.id])} ${mx},${y(rows[n.id])} ${x(n.t)},${y(rows[n.id])}`, class: "ef-edge" }, svg);
    });
    nodes.forEach((n) => {
      el("circle", { cx: x(n.t), cy: y(rows[n.id]), r: n.id === 0 ? 5 : 3.6, class: n.traced ? "ef-traced" : "ef-case" }, svg);
    });
  }

  // Modelling in decision making: analyses reach decision makers through
  // many forms of communication (advisory groups, individual advisers,
  // reports), and decisions draw on other influences besides; widths show
  // how much passes along each route

  function policy(container, label) {
    const svg = svgIn(container, label);
    const cols = [64, 200, 336];
    const stages = [
      ["analyses", [78, 122, 166], "ef-analysis"],
      ["communication", [88, 140, 192], "ef-bar"],
      ["decisions", [104, 168, 232], "ef-decision"],
    ];
    const other = { x: cols[0], y: 252 };
    // [stage, from, to, width]
    const flows = [
      [0, 0, 0, 7], [0, 0, 1, 3], [0, 1, 1, 6], [0, 2, 2, 5], [0, 2, 1, 2],
      [1, 0, 0, 6], [1, 1, 0, 3], [1, 1, 1, 5], [1, 2, 2, 4], [1, 2, 1, 2],
    ];
    const flow = (x1, y1, x2, y2, w, cls) => {
      const mx = (x1 + x2) / 2;
      el("path", { d: `M${x1},${y1} C${mx},${y1} ${mx},${y2} ${x2},${y2}`, class: cls, "stroke-width": w * 1.8 }, svg);
    };
    flows.forEach(([c, i, j, w]) => {
      flow(cols[c] + 9, stages[c][1][i], cols[c + 1] - 9, stages[c + 1][1][j], w, "ef-flow");
    });
    // other influences go straight to the decisions
    [[0, 4], [1, 6], [2, 8]].forEach(([j, w]) => {
      flow(other.x + 9, other.y, cols[2] - 9, stages[2][1][j], w, "ef-flow other");
    });
    stages.forEach(([name, ys, cls], c) => {
      el("text", { x: cols[c], y: 44, class: "ef-row-label", "text-anchor": "middle" }, svg).textContent = name;
      ys.forEach((y) => el("rect", { x: cols[c] - 9, y: y - 16, width: 18, height: 32, rx: 3, class: cls }, svg));
    });
    el("rect", { x: other.x - 9, y: other.y - 16, width: 18, height: 32, rx: 3, class: "ef-other" }, svg);
    el("text", { x: other.x, y: other.y - 26, class: "ef-row-label", "text-anchor": "middle" }, svg).textContent = "other influences";
  }

  const panels = { nowcast, forecast, transmission, policy };

  document.addEventListener("DOMContentLoaded", () => {
    document.querySelectorAll("[data-hero-panel]").forEach((node) => {
      const draw = panels[node.dataset.heroPanel];
      if (draw) draw(node, node.getAttribute("aria-label") || "");
      node.removeAttribute("aria-label");
    });
  });
})();
