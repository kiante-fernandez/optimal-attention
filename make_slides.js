const pptxgen = require("pptxgenjs");
const path = require("path");

const pres = new pptxgen();
pres.layout = "LAYOUT_16x9";
pres.author = "Kianté A. Fernandez";
pres.title = "Optimal Information Sampling in Multi-Response Settings";

const C = {
  black: "000000",
  dark: "333333",
  gray: "666666",
  light: "999999",
  white: "FFFFFF",
  rule: "CCCCCC",
};

const FONT = "Calibri";

// ============================================================
// SLIDE 1 — Title
// ============================================================
let s1 = pres.addSlide();
s1.background = { color: C.white };

s1.addText("Optimal Information Sampling\nin Multi-Response Settings", {
  x: 0.8, y: 1.2, w: 8.4, h: 2.0,
  fontFace: FONT, fontSize: 36, color: C.black, bold: true,
  lineSpacingMultiple: 1.2, align: "left",
});

s1.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 3.4, w: 2.0, h: 0.03, fill: { color: C.rule },
});

s1.addText("Kianté A. Fernandez, Frederick Callaway, Ian Krajbich", {
  x: 0.8, y: 3.7, w: 8, h: 0.5,
  fontFace: FONT, fontSize: 16, color: C.dark,
});

s1.addText("UCLA  ·  NYU  ·  Harvard", {
  x: 0.8, y: 4.2, w: 8, h: 0.4,
  fontFace: FONT, fontSize: 13, color: C.light, italic: true,
});


// ============================================================
// SLIDE 2 — Research Question
// ============================================================
let s2 = pres.addSlide();
s2.background = { color: C.white };

s2.addText("The Problem", {
  x: 0.8, y: 0.4, w: 8.4, h: 0.7,
  fontFace: FONT, fontSize: 32, color: C.black, bold: true,
});

s2.addText("How should a decision-maker allocate attention when selecting multiple items from a set of alternatives?", {
  x: 0.8, y: 1.3, w: 8.4, h: 0.8,
  fontFace: FONT, fontSize: 18, color: C.dark, italic: true,
});

s2.addText([
  { text: "Prior work (k = 1): ", options: { bold: true } },
  { text: "Callaway et al. (2021) showed optimal agents attend based on value estimates and uncertainty.", options: {} },
], {
  x: 0.8, y: 2.5, w: 8.4, h: 0.5,
  fontFace: FONT, fontSize: 14, color: C.dark,
});

s2.addText([
  { text: "This work (k > 1): ", options: { bold: true } },
  { text: "Extend the metalevel MDP framework to multi-response settings and validate with eye-tracking data.", options: {} },
], {
  x: 0.8, y: 3.1, w: 8.4, h: 0.5,
  fontFace: FONT, fontSize: 14, color: C.dark,
});


// ============================================================
// SLIDE 3 — Meta-MDP Formulation
// ============================================================
let s3 = pres.addSlide();
s3.background = { color: C.white };

s3.addText("Metalevel MDP Formulation", {
  x: 0.8, y: 0.4, w: 8.4, h: 0.7,
  fontFace: FONT, fontSize: 32, color: C.black, bold: true,
});

s3.addText("4-tuple (B, C, T, R) parameterized by n items, k selections, σ_obs, γ_sample, γ_switch", {
  x: 0.8, y: 1.2, w: 8.4, h: 0.5,
  fontFace: FONT, fontSize: 14, color: C.gray,
});

// Left: Belief State
s3.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 1.9, w: 4.0, h: 0.03, fill: { color: C.rule },
});
s3.addText("Belief State", {
  x: 0.8, y: 2.0, w: 4.0, h: 0.4,
  fontFace: FONT, fontSize: 16, color: C.black, bold: true,
});
s3.addText([
  { text: "b = (μ, λ, focused)", options: { fontFace: "Consolas", fontSize: 14, bold: true, breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "μ ∈ ℝⁿ  — posterior means", options: { fontSize: 13, breakLine: true } },
  { text: "λ ∈ ℝⁿ₊ — precisions", options: { fontSize: 13, breakLine: true } },
  { text: "focused ∈ {0,1,…,n} — current item", options: { fontSize: 13, breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "Initial: μ₀ = 0, λ₀ = 1, focused₀ = 0", options: { fontSize: 12, italic: true, color: C.gray } },
], {
  x: 0.8, y: 2.4, w: 4.0, h: 2.5,
  fontFace: FONT, color: C.dark, valign: "top",
});

// Right: Computations & Cost
s3.addShape(pres.shapes.RECTANGLE, {
  x: 5.4, y: 1.9, w: 4.0, h: 0.03, fill: { color: C.rule },
});
s3.addText("Computations & Cost", {
  x: 5.4, y: 2.0, w: 4.0, h: 0.4,
  fontFace: FONT, fontSize: 16, color: C.black, bold: true,
});
s3.addText([
  { text: "C = {1,…,n} ∪ {⊥}", options: { fontFace: "Consolas", fontSize: 14, bold: true, breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "Sample item c → signal:", options: { fontSize: 13, breakLine: true } },
  { text: "  xₜ ~ N(u⁽ᶜ⁾, σ²_obs)", options: { fontFace: "Consolas", fontSize: 13, breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "Cost = γ_sample + γ_switch  (if switching)", options: { fontSize: 13, breakLine: true } },
  { text: "Cost = γ_sample  (otherwise)", options: { fontSize: 13 } },
], {
  x: 5.4, y: 2.4, w: 4.0, h: 2.5,
  fontFace: FONT, color: C.dark, valign: "top",
});


// ============================================================
// SLIDE 4 — Bayesian Updating & Reward
// ============================================================
let s4 = pres.addSlide();
s4.background = { color: C.white };

s4.addText("Transitions & Rewards", {
  x: 0.8, y: 0.4, w: 8.4, h: 0.7,
  fontFace: FONT, fontSize: 32, color: C.black, bold: true,
});

// Left: Update
s4.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 1.3, w: 4.0, h: 0.03, fill: { color: C.rule },
});
s4.addText("Bayesian Belief Update", {
  x: 0.8, y: 1.4, w: 4.0, h: 0.4,
  fontFace: FONT, fontSize: 16, color: C.black, bold: true,
});
s4.addText([
  { text: "After sampling item c:", options: { fontSize: 13, breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "focused_{t+1} = c", options: { fontFace: "Consolas", fontSize: 13, breakLine: true } },
  { text: "λ⁽ᶜ⁾_{t+1} = λ⁽ᶜ⁾_t + σ⁻²_obs", options: { fontFace: "Consolas", fontSize: 13, breakLine: true } },
  { text: "μ⁽ᶜ⁾_{t+1} = (σ⁻²_obs · xₜ + λ⁽ᶜ⁾_t · μ⁽ᶜ⁾_t) / λ⁽ᶜ⁾_{t+1}", options: { fontFace: "Consolas", fontSize: 12, breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "All other items i ≠ c unchanged.", options: { fontSize: 12, italic: true, color: C.gray } },
], {
  x: 0.8, y: 1.85, w: 4.0, h: 3.0,
  fontFace: FONT, color: C.dark, valign: "top",
});

// Right: Reward
s4.addShape(pres.shapes.RECTANGLE, {
  x: 5.4, y: 1.3, w: 4.0, h: 0.03, fill: { color: C.rule },
});
s4.addText("Reward Function", {
  x: 5.4, y: 1.4, w: 4.0, h: 0.4,
  fontFace: FONT, fontSize: 16, color: C.black, bold: true,
});
s4.addText([
  { text: "During sampling:", options: { fontSize: 13, bold: true, breakLine: true } },
  { text: "R(bₜ, c) = −cost(bₜ, c)", options: { fontFace: "Consolas", fontSize: 13, breakLine: true } },
  { text: "", options: { fontSize: 10, breakLine: true } },
  { text: "Upon termination:", options: { fontSize: 13, bold: true, breakLine: true } },
  { text: "R(bₜ, ⊥) = Σᵢ₌₁ᵏ μ₍ᵢ₎", options: { fontFace: "Consolas", fontSize: 13, breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "μ₍ᵢ₎ = i-th largest posterior mean", options: { fontSize: 12, italic: true, color: C.gray, breakLine: true } },
  { text: "", options: { fontSize: 10, breakLine: true } },
  { text: "Agent selects the top-k items.", options: { fontSize: 13, bold: true } },
], {
  x: 5.4, y: 1.85, w: 4.0, h: 3.0,
  fontFace: FONT, color: C.dark, valign: "top",
});


// ============================================================
// SLIDE 5 — Directed Cognition
// ============================================================
let s5 = pres.addSlide();
s5.background = { color: C.white };

s5.addText("Directed Cognition Approximation", {
  x: 0.8, y: 0.4, w: 8.4, h: 0.7,
  fontFace: FONT, fontSize: 32, color: C.black, bold: true,
});

s5.addText("The optimal policy is intractable. Following Gabaix & Laibson (2006), we use Directed Cognition.", {
  x: 0.8, y: 1.15, w: 8.4, h: 0.5,
  fontFace: FONT, fontSize: 14, color: C.gray, italic: true,
});

s5.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 1.8, w: 8.4, h: 0.03, fill: { color: C.rule },
});

s5.addText("VOC_DC(b, c) = max_{N≥1} [ VOI_N(b, c) − cost_N(b, c) ]", {
  x: 0.8, y: 2.0, w: 8.4, h: 0.6,
  fontFace: "Consolas", fontSize: 16, color: C.black, bold: true, align: "center",
});

s5.addText("\"What if we committed to N samples of item c, then terminated?\"", {
  x: 0.8, y: 2.6, w: 8.4, h: 0.4,
  fontFace: FONT, fontSize: 13, color: C.gray, italic: true, align: "center",
});

// Left: Competing Value
s5.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 3.3, w: 4.0, h: 0.03, fill: { color: C.rule },
});
s5.addText("Competing Value", {
  x: 0.8, y: 3.4, w: 4.0, h: 0.4,
  fontFace: FONT, fontSize: 16, color: C.black, bold: true,
});
s5.addText([
  { text: "cv(c) = μ₍ₙ₋ₖ₊₁₎  if c not in top-k", options: { fontFace: "Consolas", fontSize: 12, breakLine: true } },
  { text: "cv(c) = μ₍ₙ₋ₖ₎    if c in top-k", options: { fontFace: "Consolas", fontSize: 12, breakLine: true } },
  { text: "", options: { fontSize: 4, breakLine: true } },
  { text: "The threshold item c must beat.", options: { fontSize: 12, italic: true, color: C.gray } },
], {
  x: 0.8, y: 3.85, w: 4.0, h: 1.2,
  fontFace: FONT, color: C.dark, valign: "top",
});

// Right: VOI
s5.addShape(pres.shapes.RECTANGLE, {
  x: 5.4, y: 3.3, w: 4.0, h: 0.03, fill: { color: C.rule },
});
s5.addText("Value of Information", {
  x: 5.4, y: 3.4, w: 4.0, h: 0.4,
  fontFace: FONT, fontSize: 16, color: C.black, bold: true,
});
s5.addText([
  { text: "VOI_N(b, c) = E[max(μ̃⁽ᶜ⁾, cv(c))] − baseline(c)", options: { fontFace: "Consolas", fontSize: 12, breakLine: true } },
  { text: "", options: { fontSize: 4, breakLine: true } },
  { text: "Closed-form via standard normal CDF Φ(z).", options: { fontSize: 12, italic: true, color: C.gray } },
], {
  x: 5.4, y: 3.85, w: 4.0, h: 1.2,
  fontFace: FONT, color: C.dark, valign: "top",
});


// ============================================================
// SLIDE 6 — Action Selection
// ============================================================
let s6 = pres.addSlide();
s6.background = { color: C.white };

s6.addText("Action Selection & Termination", {
  x: 0.8, y: 0.4, w: 8.4, h: 0.7,
  fontFace: FONT, fontSize: 32, color: C.black, bold: true,
});

s6.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 1.2, w: 8.4, h: 0.03, fill: { color: C.rule },
});

s6.addText("P(c | b) ∝ exp( β · VOC_DC(b, c) )        β = 1000", {
  x: 0.8, y: 1.5, w: 8.4, h: 0.6,
  fontFace: "Consolas", fontSize: 18, color: C.black, bold: true, align: "center",
});

s6.addText([
  { text: "1.  ", options: { bold: true } },
  { text: "Evaluate VOC_DC(b, c) for each item c", options: { breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "2.  ", options: { bold: true } },
  { text: "Select action via softmax over VOC values", options: { breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "3.  ", options: { bold: true } },
  { text: "Terminate when max VOC ≤ 0", options: { breakLine: true } },
  { text: "", options: { fontSize: 6, breakLine: true } },
  { text: "4.  ", options: { bold: true } },
  { text: "Choose top-k items by posterior mean", options: {} },
], {
  x: 2.5, y: 2.5, w: 5.0, h: 2.5,
  fontFace: FONT, fontSize: 15, color: C.dark, valign: "top",
});


// ============================================================
// SLIDE 7 — Experiment
// ============================================================
let s7 = pres.addSlide();
s7.background = { color: C.white };

s7.addText("Empirical Study", {
  x: 0.8, y: 0.4, w: 8.4, h: 0.7,
  fontFace: FONT, fontSize: 32, color: C.black, bold: true,
});

s7.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 1.2, w: 8.4, h: 0.03, fill: { color: C.rule },
});

s7.addText([
  { text: "N = 75 participants  ·  4 items per trial  ·  k ∈ {1, 2, 3}  ·  100 trials per block", options: { fontSize: 15, bold: true, breakLine: true } },
], {
  x: 0.8, y: 1.5, w: 8.4, h: 0.5,
  fontFace: FONT, color: C.dark,
});

s7.addText([
  { text: "Phase 1 — Rating Task", options: { bold: true, breakLine: true } },
  { text: "Passively viewed 60 snack food images, then rated desirability (1–100).", options: { breakLine: true } },
  { text: "", options: { fontSize: 8, breakLine: true } },
  { text: "Phase 2 — Multi-Response Choice", options: { bold: true, breakLine: true } },
  { text: "Selected k items from sets of 4, blocked by selection size. Webcam-based eye-tracking (WebGazer) recorded fixation patterns.", options: { breakLine: true } },
  { text: "", options: { fontSize: 8, breakLine: true } },
  { text: "Analysis focused on correct trials (selections matching stated preferences).", options: { italic: true, color: C.gray } },
], {
  x: 0.8, y: 2.3, w: 8.4, h: 2.8,
  fontFace: FONT, fontSize: 14, color: C.dark, valign: "top",
});


// ============================================================
// SLIDE 8 — Figure: First Fixation Duration
// ============================================================
let s8 = pres.addSlide();
s8.background = { color: C.white };

s8.addText("First Fixation Duration by Item Value", {
  x: 0.8, y: 0.3, w: 8.4, h: 0.6,
  fontFace: FONT, fontSize: 28, color: C.black, bold: true,
});

s8.addText("Solid = model  ·  Dashed = data  ·  Faint = individual subjects", {
  x: 0.8, y: 0.85, w: 8.4, h: 0.35,
  fontFace: FONT, fontSize: 12, color: C.gray, italic: true,
});

s8.addImage({
  path: path.resolve(__dirname, "figures/model_data_first_duration_2025-12-17.png"),
  x: 0.5, y: 1.3, w: 9.0, h: 4.1,
  sizing: { type: "contain", w: 9.0, h: 4.1 },
});


// ============================================================
// SLIDE 9 — Figure: Proportion of Fixation Time
// ============================================================
let s9 = pres.addSlide();
s9.background = { color: C.white };

s9.addText("Proportion of Total Fixation Time", {
  x: 0.8, y: 0.3, w: 8.4, h: 0.6,
  fontFace: FONT, fontSize: 28, color: C.black, bold: true,
});

s9.addText("Attention concentrates on items near the decision boundary — broadening as k increases.", {
  x: 0.8, y: 0.85, w: 8.4, h: 0.35,
  fontFace: FONT, fontSize: 12, color: C.gray, italic: true,
});

s9.addImage({
  path: path.resolve(__dirname, "figures/model_data_proportion_2025-12-17.png"),
  x: 0.5, y: 1.3, w: 9.0, h: 4.1,
  sizing: { type: "contain", w: 9.0, h: 4.1 },
});


// ============================================================
// SLIDE 10 — Key Findings
// ============================================================
let s10 = pres.addSlide();
s10.background = { color: C.white };

s10.addText("Key Findings", {
  x: 0.8, y: 0.4, w: 8.4, h: 0.7,
  fontFace: FONT, fontSize: 32, color: C.black, bold: true,
});

s10.addShape(pres.shapes.RECTANGLE, {
  x: 0.8, y: 1.2, w: 8.4, h: 0.03, fill: { color: C.rule },
});

s10.addText([
  { text: "1.  ", options: { bold: true } },
  { text: "Selection size (k) determines which items are most informative — VOC depends on position relative to the selection boundary.", options: { breakLine: true } },
  { text: "", options: { fontSize: 10, breakLine: true } },
  { text: "2.  ", options: { bold: true } },
  { text: "As k increases, attention distributes more broadly across items.", options: { breakLine: true } },
  { text: "", options: { fontSize: 10, breakLine: true } },
  { text: "3.  ", options: { bold: true } },
  { text: "When choosing all but one item (k = n−1), identifying the worst item first is computationally efficient — yet participants rarely do so.", options: { breakLine: true } },
], {
  x: 0.8, y: 1.5, w: 8.4, h: 2.5,
  fontFace: FONT, fontSize: 15, color: C.dark, valign: "top",
});

s10.addText("Human attention in multi-response decisions shows qualitative signatures of optimal information sampling.", {
  x: 0.8, y: 4.2, w: 8.4, h: 0.5,
  fontFace: FONT, fontSize: 14, color: C.gray, italic: true,
});


// Write file
const outPath = path.resolve(__dirname, "optimal_attention_slides.pptx");
pres.writeFile({ fileName: outPath }).then(() => {
  console.log("Saved to: " + outPath);
}).catch(err => {
  console.error("Error:", err);
});
