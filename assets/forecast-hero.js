// Animated forecast for the homepage hero.
//
// It simulates an outbreak, reveals the observations up to "now", opens a
// forecast fan from the recent growth rate, then lets the later observations
// land and counts how many fell inside the 50% and 90% intervals. It plays
// once and stays on the finished figure. Colours come from CSS custom
// properties, so the figure follows the light/dark theme.
(function () {
  "use strict";

  const NS = "http://www.w3.org/2000/svg";
  const W = 800;
  const H = 380;
  const PAD = { l: 18, r: 18, t: 34, b: 34 };
  const N = 56;
  const NOW = 36;

  function el(name, attrs, parent) {
    const node = document.createElementNS(NS, name);
    for (const k in attrs) node.setAttribute(k, attrs[k]);
    if (parent) parent.appendChild(node);
    return node;
  }

  // Seeded generator, so the finished figure can be drawn from the same seed
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

  function simulate(seed) {
    const u = rng(seed);
    const peak = 30 + u() * 22;
    const r0 = 0.11 + u() * 0.06;
    const logI = [Math.log(8 + u() * 6)];
    for (let t = 1; t < N; t++) {
      const r = r0 * Math.cos((Math.PI * t) / (2 * peak)) + 0.025 * normal(u);
      logI.push(logI[t - 1] + r);
    }
    const truth = logI.map(Math.exp);
    const obs = truth.map((x) => Math.max(0, x * Math.exp(0.16 * normal(u))));

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
    const recent = Math.log(fit[NOW] + 1) - Math.log(fit[NOW - 6] + 1);
    const r = recent / 6;
    const fc = [];
    for (let h = 0; h < N - NOW; h++) {
      const mu = Math.log(fit[NOW] + 1) + r * h;
      const sd = 0.12 + 0.075 * Math.sqrt(h) + 0.012 * h;
      fc.push({
        t: NOW + h,
        med: Math.exp(mu) - 1,
        lo50: Math.exp(mu - 0.674 * sd) - 1,
        hi50: Math.exp(mu + 0.674 * sd) - 1,
        lo90: Math.exp(mu - 1.645 * sd) - 1,
        hi90: Math.exp(mu + 1.645 * sd) - 1,
      });
    }
    return { obs, fit, fc };
  }

  function build(container) {
    const reduce = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    const svg = el("svg", {
      viewBox: `0 0 ${W} ${H}`,
      class: "ef-hero-svg",
      role: "img",
      "aria-label":
        "Animated forecast: case counts are observed up to the present, a forecast with uncertainty is made, and later observations are checked against it.",
    });
    container.appendChild(svg);

    // SVG text is sized in figure units and scales with the figure, so set
    // the label size from the drawn width to keep it the same on screen
    const LABEL_PX = 12.5;
    function sizeLabels() {
      const drawn = svg.getBoundingClientRect().width;
      if (drawn > 0) svg.style.setProperty("--ef-label-size", `${(LABEL_PX * W) / drawn}px`);
    }
    sizeLabels();
    if (window.ResizeObserver) new ResizeObserver(sizeLabels).observe(svg);

    // The score is HTML rather than SVG text so it stays legible when the
    // figure is scaled down on a phone
    const score = document.createElement("p");
    score.className = "ef-score";
    container.appendChild(score);

    let timers = [];

    function later(fn, ms) {
      timers.push(setTimeout(fn, ms));
    }

    let finished = false;

    // finish = true draws the completed figure at once, without animating
    function draw(seed, finish) {
      timers.forEach(clearTimeout);
      timers = [];
      finished = false;
      while (svg.firstChild) svg.removeChild(svg.firstChild);

      const { obs, fit, fc } = simulate(seed);
      // Scale to the observations, so the history fills a good share of the
      // height; a wide forecast band may run off the top, as it would in a
      // figure with a fixed axis, and is clipped there rather than flattened
      const pastMax = Math.max(...obs.slice(0, NOW + 1));
      const futureMax = Math.max(...obs.slice(NOW + 1));
      const ymax = Math.max(pastMax * 1.6, Math.min(futureMax, pastMax * 2.6)) * 1.05;
      const x = (t) => PAD.l + ((W - PAD.l - PAD.r) * t) / (N - 1);
      const y = (v) => H - PAD.b - ((H - PAD.t - PAD.b) * v) / ymax;

      el("line", { x1: PAD.l, x2: W - PAD.r, y1: H - PAD.b, y2: H - PAD.b, class: "ef-axis" }, svg);

      // The bands animate their own CSS clip-path, so the plot area is
      // clipped on an enclosing group instead
      const clip = el("clipPath", { id: "ef-plot-area" }, el("defs", {}, svg));
      el("rect", { x: 0, y: PAD.t - 6, width: W, height: H - PAD.t - PAD.b + 12 }, clip);
      const plot = el("g", { "clip-path": "url(#ef-plot-area)" }, svg);
      const bands = el("g", { class: "ef-bands" }, plot);
      const pts = el("g", {}, plot);
      const fitPath = el("path", { class: "ef-fit", d: "" }, plot);

      const nowLine = el(
        "line",
        { x1: x(NOW), x2: x(NOW), y1: PAD.t - 10, y2: H - PAD.b, class: "ef-now" },
        svg
      );
      const nowLabel = el("text", { x: x(NOW) + 8, y: PAD.t - 14, class: "ef-label" }, svg);
      nowLabel.textContent = "now";
      score.textContent = "";

      const area = (lo, hi) =>
        "M" +
        fc.map((d) => `${x(d.t)},${y(d[hi])}`).join("L") +
        "L" +
        fc
          .slice()
          .reverse()
          .map((d) => `${x(d.t)},${y(d[lo])}`)
          .join("L") +
        "Z";
      const outer = el("path", { d: area("lo90", "hi90"), class: "ef-band-outer" }, bands);
      const inner = el("path", { d: area("lo50", "hi50"), class: "ef-band-inner" }, bands);
      const median = el(
        "path",
        { d: "M" + fc.map((d) => `${x(d.t)},${y(d.med)}`).join("L"), class: "ef-median" },
        bands
      );

      const dot = (t, cls) =>
        el("circle", { cx: x(t), cy: y(obs[t]), r: 4.2, class: "ef-obs " + cls }, pts);

      const fitD = (upto) =>
        "M" + fit.slice(0, upto + 1).map((v, t) => `${x(t)},${y(v)}`).join("L");

      // Coverage of the later observations by both intervals, up to t
      const within = (t, lo, hi) => obs[t] >= fc[t - NOW][lo] && obs[t] <= fc[t - NOW][hi];
      const showScore = (upto) => {
        let in50 = 0;
        let in90 = 0;
        for (let t = NOW + 1; t <= upto; t++) {
          if (within(t, "lo50", "hi50")) in50++;
          if (within(t, "lo90", "hi90")) in90++;
        }
        const n = upto - NOW;
        score.textContent = `${in50}/${n} inside 50% · ${in90}/${n} inside 90%`;
      };

      if (finish) {
        for (let t = 0; t < N; t++) {
          dot(t, t <= NOW ? "past" : within(t, "lo90", "hi90") ? "inside" : "outside");
        }
        fitPath.setAttribute("d", fitD(NOW));
        showScore(N - 1);
        finished = true;
        return;
      }

      [outer, inner, median, nowLine, nowLabel].forEach((n) => n.classList.add("ef-hidden"));

      const step = 55;
      for (let t = 0; t <= NOW; t++) {
        later(() => {
          dot(t, "past").classList.add("ef-pop");
          fitPath.setAttribute("d", fitD(t));
        }, t * step);
      }

      const tFc = (NOW + 1) * step + 250;
      later(() => {
        nowLine.classList.remove("ef-hidden");
        nowLabel.classList.remove("ef-hidden");
      }, tFc);
      later(() => {
        [outer, inner, median].forEach((n) => n.classList.remove("ef-hidden"));
        bands.classList.add("ef-reveal");
      }, tFc + 300);

      const tObs = tFc + 1500;
      for (let t = NOW + 1; t < N; t++) {
        later(() => {
          dot(t, within(t, "lo90", "hi90") ? "inside" : "outside").classList.add("ef-drop");
          showScore(t);
          if (t === N - 1) finished = true;
        }, tObs + (t - NOW - 1) * 170);
      }
    }

    // The figure plays once and then stays on the finished forecast
    const seed = 20141 + Math.floor(Math.random() * 1000);
    draw(seed, reduce);

    // A tab hidden mid-animation comes back to the finished figure
    document.addEventListener("visibilitychange", () => {
      if (document.hidden && !finished) draw(seed, true);
    });
  }

  document.addEventListener("DOMContentLoaded", () => {
    document.querySelectorAll("[data-forecast-hero]").forEach(build);
  });
})();
