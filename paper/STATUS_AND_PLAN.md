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
2. Done 2026-10-22 (night): humanizer pass on `paper/manuscript.md` (abstract cut to 279 words for PLOS's 300-word
   limit, two non-locative "where" clauses, "yields", one uniform-rhythm paragraph, table placeholders without em
   dashes, an Ethics statement subsection in the Methods) and a norms check against the 19 PLOS Computational
   Biology papers in ~/Downloads (abstract median 268 words, author summary median 225, figures median 6-7,
   references median about 70, "Materials and methods" heading, ethics heading in about half): the manuscript is
   within all of them (abstract 279, author summary 196, 9 figures, 3 tables, 62 references).
3. Kianté does his own pass first on the dissertation chapter version, rendered 2026-10-22 as
   `dissertation-papers/chapter-2/3_metalevel_MDP_multi_response_v3.docx` by `paper/build_chapter.py` (APA 7
   manuscript: title page with author note, abstract and keywords, author-date citations, Figure/Table blocks,
   S1 as the Appendix; converted source kept in `chapter-2/3_source/`). Re-run the script after edits to
   `paper/manuscript.md`; edits made directly in the Word file must be carried back to the markdown by hand.
   Then the co-author pass (Word file `paper/manuscript.docx`, equation numbers now preserved), the humanizer check
   after their edits.
4. PLOS compliance fixes 2026-10-22: funding removed from the manuscript's Acknowledgments (it lives in the
   submission form); data availability names the GitHub repository; the S1 subtitle carries the new title.
5. Still to prepare for submission: cover letter, suggested reviewers,
   the Acknowledgments line in the manuscript if Kianté wants it (the chapter's author note thanks Uma R. Karmarkar
   for the experiment design; the PLOS manuscript currently has no Acknowledgments section).
