# Optimal information sampling in multi-response choice: status and plan

Updated: 2026-10-22 (night). Authors on the draft: Fernandez, Callaway, Krajbich.

## Where things stand

- Full PLOS Computational Biology draft: `paper/manuscript.md` (pandoc Markdown source, ~9,700 words, 12 numbered
  equations in Callaway et al.'s notation, 9 figures, 3 tables, 76 verified references) builds with `paper/plos/build.sh` into `paper/plos/manuscript.pdf` (PLOS template,
  line numbers, draft watermark), `paper/plos/Fig1-9.tif`, and the working copies `paper/manuscript.docx` / `.pdf`.
- Pipeline (README): `analysis/build_data.R` -> `descriptives.R`, `tests.R` -> `simulations/run_fits.sh` ->
  `simulations/run_best.sh` -> `analysis/figures.R`, `analysis/fig1_fig2.R` -> `paper/plos/build.sh`.
  Figures use the house theme (`R/theme_pub.R`, copied from struc-avg).
- Three response policies (`src/sequential.jl`): simultaneous, best-first, commit-when-certain; each fitted by random
  search on even trials (`results/fits/`), simulated on every trial at its best parameters (`results/model_*`), and
  shown on odd trials. Policy costs: `simulations/policy_cost.jl` (`results/tables/policy_cost_summary.csv`).
- Main results: value-directed attention for k = 1 but no mirror image for k = 3; identical pre-first-response
  attention across k; attention moves down the ranking after each keypress; the best-first policy accounts for these
  and for the longer three-item trials; the subset policies do not. Best-first costs nothing for k = 2 and 3-49% of
  net reward for k = 3 depending on the switching cost.

## Gaze reporting checklist (verified 2026-10-22 against Callaway et al. 2021 and Krajbich papers)

Reported: tracker and sampling interval; ROI definition (50 px buffer) and on-item sample rate (49%, median 51%,
range 20-69%, all 73 gaze participants via the private ID map); trial exclusions; fixation definition, short-fixation share (20% < 100 ms) and robustness to excluding
them; the stricter 34 px pipeline as a robustness check; fixations per trial, first/middle/last durations, items
fixated, refixations, all-four-in-first-four; spatial biases (bottom item under-fixated and under-chosen); first
fixation to the best item; last fixation to a chosen item; dwell share by rank and by phase; time course and
time-resolved fixation probability; gaze entropy; choice by dwell share, last-fixation bias, first-fixation bias;
tracking-quality split. Not reported: individual-participant fits (Callaway's S2).

## Open items before submission

Decisions taken by Kianté on 2026-10-22 (night): title "Fixation patterns in multi-response choice do not reflect
optimal information sampling" (short title unchanged); no fit of the prior bias alpha (a construct from other papers;
alpha = 1 stays fixed and the Fig 9 paragraph discusses it); Fig 5 stays; Table 1 trimmed to the nine rows the
argument needs (full descriptives in S1 Appendix, Table I1); cluster-based permutation tests added
(`analysis/cluster_tests.R`, reported in the Fig 8 paragraph and Methods); companion paper cited with its title
("Rank-order preferences are not quickly accessible", in preparation); Editorial Manager fields drafted in
`paper/plos/submission_fields.md` (CRediT, funding, competing interests, data availability); no individual fits;
the Discussion no longer attributes the shallow gradients to webcam gaze misassignment (the tracking-quality split
stays in the Methods as a robustness check); Uma R. Karmarkar is not an author (acknowledgment suggested in the
submission fields); the data needed for the paper are tracked in this repository (`data/choosek_R.csv`,
`data/choosekEYE_R.csv.gz`); commits carry no Claude attribution lines.

1. Done: directed cognition validated against the exact optimum in a Bernoulli version of the task
   (`simulations/bernoulli_validation.jl`; 96.7-99.6% of optimal reward; symmetry exact), and parameter and policy
   recovery (`simulations/recovery.sh`, `analysis/recovery_table.R`); both in S1 Appendix (sections G, H).
2. Co-author pass on the text (Word file ready); humanizer check after their edits; dissertation chapter 2 docx
   regenerated from `paper/manuscript.md` after the co-author pass.
3. Still to prepare for submission: cover letter, suggested reviewers, Zenodo DOI for the repository release,
   the Acknowledgments line in the manuscript if Kianté wants it.

## Stock-take of 2026-10-22 (morning), kept for reference

## Summary

- Three defects were found today. The ROI-to-item mapping in this repo's R scripts was
  wrong (fixed; the figures changed completely). The manuscript's definition of the
  value of computation was wrong (fixed). The switching cost is ten times the value in
  Callaway et al. (2021) (flagged; needs a decision).
- With the corrected mapping, the data show value-directed attention when choosing one
  item, as in Callaway et al., but not the mirror image the model predicts when choosing
  three. Before the first keypress, attention by item rank is the same in all three
  conditions. After each keypress, attention moves down the ranking.
- Proposed paper: (1) a normative theory of attention for subset selection, including a
  symmetry result (choosing k is rejecting n−k); (2) the empirical finding that people
  instead decompose the task into successive "choose the best remaining" decisions;
  (3) a resource-rational analysis of that decomposition (what it costs and when it is
  near-optimal). Recommended target: PLOS Computational Biology.

## 1. Inventory

### 1.1 Model code (`src/`, Julia)

| File | Content | Status |
|---|---|---|
| `meta_mdp.jl` | belief (μ, λ, focused), Gaussian updating, cost with switch cost, terminal reward = sum of top-k posterior means, rollout | correct |
| `voi.jl` | VOI of N samples of item c via competing value and baseline | correct (derivation in 2.2) |
| `directed_cognition.jl` | VOC_DC = max over N of VOI_N − cost_N, golden-section search over continuous N in [1, 100] | correct; a lower bound on the true VOC |
| `simulations/run_simulations.jl` | n = 4, k = 1..3, 1000 trials each, values and prior N(0, 1) | runs in under a minute |
| `archive/bmps*.jl` | earlier Bayesian metalevel policy search attempts | unused |

Parameters in `simulations/config.jl`: σ_obs = 2.6, γ_sample = 0.0037, γ_switch = 0.0995,
β = 1000.

### 1.2 Data (`data/choosekEYE_R.csv`, built in `choose_k`)

| Fact | Value |
|---|---|
| Subjects in the eye-tracking file | 74 (75 recruited; `P05` is dropped in the choose_k scripts for a condition-assignment bug) |
| Trials | 20,646 (6,800 / 6,803 / 7,025 for k = 1 / 2 / 3; 18 trials have k = 4 from the same bug and must be excluded) |
| Gaze samples | 526,763, all inside an item ROI (off-ROI samples were removed upstream) |
| Inter-sample interval (WebGazer) | median 53 ms, mean 98 ms, 95th percentile 349 ms |
| Fixations per trial (mean) | 3.0 / 3.9 / 4.3 |
| Fixation duration | median 215 / 241 / 263 ms; mean 377 / 437 / 467 ms |
| Median total RT | 1.79 / 2.40 / 2.77 s |
| Trials in which the chosen set is exactly the top-k by rating | 58% / 47% / 57% |
| First fixation lands on the best item | 25% in every condition (no first-fixation value bias) |
| k = 3: first response is the best-rated item | 45% (chance 25%); all three in rank order 29% |

Columns: `t` (ms), `button_ROI` (i, j, k, l), `fixation_id` (ordinal within trial),
`fixation_category` (First, Middle, Last), `item_value_0..3`, `item_rank_0..3`,
`choice_1..3`, `choice_rank_1..3`, `rt_1..3` (cumulative), `dwell_0..3`.

### 1.3 Analyses

In this repo: `analysis/model_data_comparison.R` (two figures: first-fixation duration
and share of fixation time by item value, model vs data), `analysis/quantitative_fit_metrics.R`
(bin correlations), `analysis/plotting.R` (model only).

In `choose_k/archive/analysis/experiment-1/` (moved to the archive on 2026-10-21, nothing
deleted): `exp_1_callaway_fixation_analysis.R` reproduces the analogs of Callaway et al.
Figs 3–7 on this dataset with mixed-model tests, plus four choose-k analyses (fixation
duration resets between responses, gaze cascade restarts per response, people stop
looking at already-chosen items, fixation counts per phase). Figures are in
`choose_k/archive/figures/callaway_fig3..7_*.png` and `callaway_novel_choosek.png`.
Also there: `exp_1_et_data_quality.R` (ROI coverage, screen geometry audit),
`exp_1_eye-movement_psychometrics.R`, `exp_1_roi_sensitivity.R`,
`exp_1_screen_robustness.R`, `exp_1_plot_gaze_trials.R`, `regenerate_et_csv.R`.
These scripts use the correct ROI mapping and are the base for the paper's Results.

### 1.4 Manuscript

About 2,500 words in CogSci format with `apacite` and seven references. The Discussion
contains placeholders ("normatively XX", "people tend to do YY"). The abstract claims that
people rarely identify the worst item first when choosing three of four; no analysis in
the draft supports or tests that claim.

## 2. Findings today

### 2.1 ROI-to-item mapping (fixed)

`model_data_comparison.R` and `quantitative_fit_metrics.R` mapped gaze ROIs to items
alphabetically (i→0, j→1, k→2, l→3). Items are indexed by screen position (left, top,
right, bottom), which the keys j, i, l, k select. Evidence: in k = 1 trials the key
pressed maps perfectly onto `choice_1` as j→0, i→1, l→2, k→3; the last fixation of a trial
lands on the chosen item 38% of the time under the positional mapping and 21% under the
alphabetical one (chance 25%). Every fixation in the old figures was therefore assigned
another item's value, which is why the data lines were flat.

Both scripts now use the positional mapping. The regenerated figures
(`figures/*_2026-10-22.png`) differ qualitatively from the old ones (section 2.6).

Related: the `dwell_0..3` columns accumulate in the order i, j, l, k, so `dwell_0` and
`dwell_1` are swapped relative to item indices 0 and 1. Do not use them; recompute
cumulative dwell from the samples. The archived Fig 5 analog (fixation advantage) used
them and must be redone.

### 2.2 Math check against Lieder, Callaway & Griffiths (2026) and Callaway et al. (2021)

- Value of computation. The draft defined VOC(b, c) = E[V*(b′)] − V*(b) − cost, which is
  at most zero for every computation. The correct definition subtracts the reward for
  terminating now: VOC(b, c) = E[V*(b′) | b, c] − cost(b, c) − R(b, ⊥) = Q*(b, c) − Q*(b, ⊥)
  (book Eq. 10.39; PLOS paper, Methods). Fixed in the tex.
- Preposterior standard deviation of the posterior mean after N samples:
  σ² = w² (1/λ + 1/λ_obs) with λ_obs = N/σ_obs² and w = λ_obs/(λ + λ_obs); equivalently
  1/λ − 1/(λ + λ_obs). Correct in the tex and the code.
- VOI_N. For an unselected item the terminal reward changes only if its new mean exceeds
  the lowest selected mean μ_(n−k+1); for a selected item only if its new mean falls
  below the highest unselected mean μ_(n−k). Because the posterior mean is a martingale,
  both cases are expected-improvement integrals: VOI = E[(μ̃ − cv)⁺] for an unselected
  item and E[(cv − μ̃)⁺] for a selected one, with closed forms σ[φ(z) − z(1 − Φ(z))] and
  σ[φ(z) + zΦ(z)], z = (cv − μ)/σ. The tex now states these instead of the earlier
  conditional-expectation form. The code (`expect_max_dist` minus `comparison_value`)
  computes the same quantity.
- Directed cognition. max over N of the value of "sample c N times, then stop" is a lower
  bound on the true VOC. The book notes this approximation assumes less future control
  than the agent has and so is biased toward stopping, though less than the myopic policy.
  Say this in the paper and validate the approximation (roadmap B3).
- New result added to the tex: selecting k is rejecting n−k. The terminal reward equals the
  sum of all means minus the sum of the n−k lowest, and the sum of all means is a
  martingale that no policy can change, so the optimal policy for (n, k) at belief
  (μ, λ) is the optimal policy for (n, n−k) at (−μ, λ). The DC approximation inherits the
  symmetry exactly. Hence choosing one of four should concentrate sampling on the two
  highest estimates and choosing three of four on the two lowest; choosing two of four is
  symmetric in value. The simulations show exactly this (section 2.6).

### 2.3 Switching cost (decision needed)

The repo uses γ_switch = 0.0995. Callaway et al. (2021) report 0.00995 ± 0.001 (and
γ_sample = 0.00373, σ_x = 2.60), so the repo value is ten times theirs. The manuscript
calls the parameters "similar to those validated in previous research". Simulation with
both values (1000 trials per k, seed 1):

| γ_switch | k | fixations per trial | single-fixation trials | samples per trial | accuracy (top-k chosen) |
|---|---|---|---|---|---|
| 0.0995 (repo) | 1 | 2.9 | 26% | 10.4 | 0.48 |
| 0.0995 (repo) | 2 | 4.1 | 0% | 13.8 | 0.41 |
| 0.0995 (repo) | 3 | 3.0 | 24% | 10.4 | 0.52 |
| 0.00995 (Callaway) | 1 | 8.8 | 0% | 14.6 | 0.57 |
| 0.00995 (Callaway) | 2 | 11.2 | 0% | 17.7 | 0.49 |
| 0.00995 (Callaway) | 3 | 8.9 | 1% | 14.8 | 0.60 |
| data | 1 / 2 / 3 | 3.0 / 3.9 / 4.3 | 0% | — | 0.58 / 0.47 / 0.57 |

The repo value matches the fixation counts; the Callaway value matches accuracy almost
exactly. Neither matches both, so the parameters must be fitted to this dataset
(roadmap B1). Nothing in the code or the tex was changed.

### 2.4 Time units (partly fixed)

Model durations are in samples; the old figures compared them to WebGazer sample counts
(median interval 53 ms, irregular). `model_data_comparison.R` now converts model samples
to milliseconds at 100 ms per sample (the Callaway et al. convention) and measures
empirical fixation duration as last minus first sample time plus one median interval.
Mean fixation duration is then 361 / 326 / 342 ms (model) against 416 / 457 / 491 ms
(data). The 100 ms convention should be stated in Methods or treated as a free scale.

### 2.5 Trial selection

The comparison uses only trials in which the chosen set is exactly the top-k by rating
(about 55% of trials). Conditioning on the outcome inflates value-directed attention:
with all trials the share of dwell on the best item in k = 1 is 0.270, with correct
trials only it is 0.326. Callaway et al. fit and plot all trials and let the model
predict choices. Drop the filter.

### 2.6 What the corrected data show

Share of total dwell by item rank (1 = best), mean over subjects:

| Subset | k | rank 1 | rank 2 | rank 3 | rank 4 |
|---|---|---|---|---|---|
| Model, repo parameters | 1 | 0.34 | 0.28 | 0.22 | 0.16 |
| Model, repo parameters | 2 | 0.25 | 0.25 | 0.26 | 0.24 |
| Model, repo parameters | 3 | 0.16 | 0.21 | 0.28 | 0.35 |
| Data, all trials, whole trial | 1 | 0.270 | 0.250 | 0.245 | 0.235 |
| Data, all trials, whole trial | 2 | 0.264 | 0.253 | 0.250 | 0.234 |
| Data, all trials, whole trial | 3 | 0.243 | 0.251 | 0.258 | 0.248 |
| Data, before the first response | 1 | 0.268 | 0.251 | 0.245 | 0.236 |
| Data, before the first response | 2 | 0.268 | 0.253 | 0.246 | 0.233 |
| Data, before the first response | 3 | 0.265 | 0.251 | 0.245 | 0.238 |
| Data, between responses 1 and 2 | 2 | 0.271 | 0.255 | 0.249 | 0.224 |
| Data, between responses 1 and 2 | 3 | 0.248 | 0.257 | 0.255 | 0.240 |
| Data, between responses 2 and 3 | 3 | 0.221 | 0.247 | 0.277 | 0.255 |

Three things stand out. The k = 1 pattern matches the model's direction but is far weaker
(webcam gaze misassignment attenuates it, and the parameters are not fitted). The k = 3
pattern is nearly flat rather than mirrored. Before the first response the pattern is the
same in all conditions and top-weighted, and after each response it shifts toward the
items still undecided. The archived Fig 6C analog shows the same thing over time: the
probability of fixating the worst item falls over the trial for k = 1 and rises for k = 3,
which is the mirror prediction, but it rises only late, after the first responses.

## 3. Proposed story

1. Normative theory. Extend the metalevel MDP of Callaway et al. (2021) to selecting k of
   n. State the symmetry result. Show the optimal policy's value-and-uncertainty map for
   k = 1, 2, 3 (Fig 2 analog), which flips from the top two items to the bottom two.
2. Empirical dissociation. People show the k = 1 signature in every condition before the
   first response, then move down the ranking. They solve "choose k" as k successive
   "choose the best of what is left" problems. Response order and inter-response times
   from the rank-order paper fit the same picture.
3. Resource-rational analysis of the decomposition. Define the sequential policy (solve
   choose-1 among the remaining items with the current beliefs, commit, repeat) and
   compute its expected reward against the simultaneous policy under the fitted
   parameters. If the loss is small, reusing the single-choice routine is near-optimal
   and the symmetry is a strategy people have no reason to discover; if it is large, the
   paper identifies a real inefficiency and its cost. Either answer is publishable. A
   "commit when no remaining computation can displace the item" rule predicts response
   order and inter-response times.

## 4. Gap analysis against Callaway et al. (2021)

| Analysis in the PLOS paper | Status here |
|---|---|
| Choice psychometric curve by value difference, per k | archived choose_k Fig 3A (data only); model prediction missing |
| Total fixation time distribution, by difficulty, by mean value | archived Fig 3B–D (data only; B needs log axis); model missing |
| Number of fixations: histogram and by difficulty | archived Fig 4A–B (data only) |
| Fixation duration by fixation number (final shorter, later longer) | archived Fig 4C (data; final fixations are the longest here, unlike Callaway, needs a look at how the keypress truncates the last fixation) |
| Fixation advantage (uncertainty-directed attention) | archived Fig 5; built on the swapped dwell columns; panel C is degenerate; redo |
| Share of dwell by value, first-fixation duration by value | this repo (two figures, corrected today) and archived Fig 6A–B |
| P(fixate worst) over time, refixation patterns | archived Fig 6C–E (data only) |
| Choice biases: last fixation, dwell share, first fixation | archived Fig 7 (data only) |
| Parameter fitting (approximate likelihood on summary statistics, Sobol search, even/odd split) | missing |
| Prior bias α and simulation on the empirical rating sets | missing (values are z-scored per subject; model is simulated on N(0, 1) draws) |
| Validation of the policy approximation | missing |
| Individual fits (S2) | missing |
| New here: attention by response phase, sequential policy | data analysis exists in archive; model missing |

## 5. Roadmap

Phase A, data pipeline (this repo, R).
- A1. One script that builds trial- and fixation-level tables from `choosekEYE_R.csv`
  with the positional mapping, excludes the k = 4 trials and the bugged subject, applies
  the pre-registered RT exclusion, computes durations in ms and cumulative dwell from the
  samples, labels response phases from `rt_1..3`, and keeps all trials.
- A2. Port the archived Callaway-style analyses into `analysis/` as functions that take
  either data or model simulations, so every panel shows both.
- A3. Data-quality supplement from `exp_1_et_data_quality.R` (sampling interval, ROI
  coverage from the full gaze file in the choose_k archive, screen geometry).
- A4. Decide how to handle gaze misassignment (an ROI-noise mixture when simulating the
  model, or report the attenuation).

Phase B, modeling (Julia).
- B1. Fit σ_obs, γ_sample, γ_switch, α, β to this dataset with the Callaway et al.
  approximate likelihood (chosen set, number of fixations, total fixation time, dwell share
  per item), Sobol search, even trials for fitting and odd for plotting. Simulate on the
  empirical rating sets. Decide the 100 ms per sample convention.
- B2. Implement the sequential policy and the commit rule; simulate phase-resolved
  predictions for k = 2, 3; compute the expected-reward gap to the simultaneous policy.
- B3. Validate directed cognition against BMPS (`archive/bmps*.jl`) or exact dynamic
  programming on a small discretized case; report the fraction of optimal reward (S1).
- B4. VOC maps by k (Fig 2 analog) and the symmetry figure.

Phase C, figures (PLOS style: data in black with bootstrap CIs, model in color, one row
per k). Fig 1 task and belief updating. Fig 2 optimal policy and symmetry. Fig 3
psychometrics. Fig 4 basic fixation patterns. Fig 5 uncertainty-directed attention.
Fig 6 value-directed attention with both policies. Fig 7 attention across response
phases (the decisive test). Fig 8 choice biases.

Phase D, writing. Move to the PLOS LaTeX template (numbered references, Methods after
Discussion, Author Summary). Rewrite the empirical sections; fill the Discussion
placeholders with the decomposition result; remove the worst-first claim from the
abstract or back it with the phase analysis. Expand the bibliography (section 6). Re-export
the dissertation docx from the tex.

Phase E, submission. Code and data on GitHub and OSF; S1 (methods, validation, fitting)
and S2 (individual fits).

## 6. Venue and style

Recommendation: PLOS Computational Biology, Research Article. The paper is the direct
successor of Callaway et al. (2021) in that venue, the format fits (model, one dataset,
approximate fitting, long Methods), there is no length limit, and webcam eye-tracking is
easier to defend there than at Psychological Review. Psychological Review would need a
broader theory section and most likely a second experiment (a lab eye-tracker, or k of 6
to separate k from n−k); keep it as the stretch target if the sequential-decomposition
theory grows into a general account of multi-response choice.

Style, from the PLOS paper: numbered citations in square brackets in order of appearance;
sections Abstract, Author summary, Introduction, Model, Results, Discussion, Methods,
Supporting information; figures compare human data and model with 95% bootstrap CIs and
show the spread of near-optimal parameter configurations. Switch the tex from `apacite`
to the PLOS bibliography style when moving to the template.

References to add (the draft has seven): Russell & Wefald 1991 (metareasoning); Hay et al.
2012; Callaway et al. 2018 (BMPS); Lieder & Griffiths 2020 (resource-rational analysis);
Lieder, Callaway & Griffiths 2026 (the book); Gabaix & Laibson 2005; Krajbich, Armel &
Rangel 2010 and Krajbich & Rangel 2011 (aDDM, binary and trinary); Shimojo et al. 2003
(gaze cascade); Jang, Sharma & Drugowitsch 2021 (optimal binary attention policy);
Tajima et al. 2016 and 2019 (optimal value-based and multi-alternative policies); Hébert &
Woodford 2023 (rational inattention with sequential sampling); Fudenberg, Strack &
Strzalecki 2018; Thomas et al. 2019 (GLAM); Gluth et al. 2020; Smith & Krajbich 2019;
Towal, Mormann & Koch 2013; Semmelmann & Weigelt 2018 (webcam eye tracking); Fernandez,
Callaway, Karmarkar & Krajbich (rank-order paper, in prep); and the multi-item choice
literature from that paper's introduction.

## 7. Decisions for the three of us

1. Fit the parameters to this dataset rather than reuse Callaway's (and settle the
   switch-cost value).
2. Drop the correct-trials filter.
3. Make the sequential policy the second model in the paper, with the phase-resolved
   attention analysis as the main test.
4. Keep the 100 ms per sample convention or let the time scale float.
5. Target PLOS Computational Biology.
