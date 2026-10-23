# refs.bib verification report (2026-10-22)

File: `/private/tmp/claude-501/-Users-kiante-Documents-optimal-attention/8587a2d7-e981-484b-ba0a-900a742db8a6/scratchpad/refs.bib` — 76 entries (75 requested + R).

Method: 70 journal articles were matched by title on the Crossref API, then each DOI's full Crossref record was pulled and used for authors, journal, year (print year preferred), volume, issue, pages, DOI. Fields Crossref lacked were filled from OpenAlex, Europe PMC, Semantic Scholar or RePEc. The six non-Crossref items (two UAI papers, IJCAI WebGazer, Gabaix–Laibson working paper, PUP book, R) were checked on publisher pages (PMLR, ijcai.org, press.princeton.edu, scholar.harvard.edu, CRAN). The file parses with BibTeX 0.99d (all 76 entries, no warnings).

## Entries not fully verified, or where a judgement call was made

1. **hay2012** — Pages 344–353 come from the PMLR reissue of UAI 2012 (R10, verified on proceedings.mlr.press). The original AUAI Press proceedings are indexed on dblp as 346–355 (dblp blocked direct access; this comes from a search summary). Entry cites the PMLR version (publisher = PMLR, url given).
2. **callaway2018** — Same situation: PMLR R16 gives 775–784 (verified); dblp reportedly 776–785. Entry cites the PMLR version.
3. **papoutsaki2016** — ijcai.org abstract page confirms authors, booktitle and first page 3839; the last page (3845) is from the WebGazer GitHub BibTeX / your note, not shown on the IJCAI page. No publisher field (not shown on the page).
4. **gabaix2005** — Working paper, no formal series number found. PDF dated 1 May 2005 on Gabaix's Harvard site (affiliations on the paper: Gabaix MIT & NBER, Laibson Harvard & NBER). Cited as @techreport, type = Working paper, institution = Harvard University, with the PDF url. Adjust institution if you prefer "MIT and Harvard University".
5. **lieder2026** — Princeton University Press page: published 17 Feb 2026, 280 pp., ISBN 9780691259949 (paperback; hardcover appears to be 9780691259956). No DOI. A PsyArXiv version of the book also exists (10.31234/osf.io/wgpr3_v1) but is not cited.
6. **fromer2026** — Crossref/Wiley list the first author as "Romy Froemer" (not Frömer); kept as published. Early-view article e70076, online 27 Jul 2026, no volume/issue assigned yet. Full author list: Froemer, Ting (Chih-Chung), Shenhav, Gluth.
7. **gluth2012** — Crossref has initials only (S. Gluth, J. Rieskamp, C. Buchel); expanded manually to Sebastian Gluth, Jörg Rieskamp, Christian Büchel.
8. **wald1945**, **howard1966** — given names expanded from Crossref's "A." and "Ronald" to Abraham and Ronald A.
9. **roe2001** — Crossref/APA metadata contains typos ("Jermone" Busemeyer, "connectionst"); corrected to Jerome / connectionist.
10. **leng2025** — Crossref 'issued' is 2024-12-20 (online); the print issue 9(3):521–533 is March 2025 (Europe PMC pubYear 2025). Year set to 2025. Use 2024 if you cite the online date.
11. Pages not in Crossref, taken from secondary sources: krajbich2012 (193), kovach2014 (46), manohar2013 (711) from OpenAlex; towal2013 E3858–E3867 (OpenAlex truncates the last page to "67"); russo1994 274–290 (RePEc/IDEAS); bates2015 1–48 (Semantic Scholar; CRAN lme4 citation page).
12. **rcoreteam2025** — CRAN's last 2025 release is R 4.5.2 (2025-10-31), so year = 2025 as requested. Note the R installed on this machine is 4.5.3 (2026-03-11) and its citation() gives year 2026; change the year if you cite the installed version.
13. **simonson1990** — two DOIs exist (JSTOR 10.2307/3172842; SAGE 10.1177/002224379002700203). SAGE DOI used (it carries the full page range).
14. Judgment and Decision Making entries (armel2008, milosavljevic2010, yang2021) use the Cambridge-registered DOIs (10.1017/s19302975...).
15. Normalisations: "The American Economic Review" → "American Economic Review" (2006 entry); "The Journal of Neuroscience" → "Journal of Neuroscience"; artifact issue numbers dropped for Nature Communications (tajima2016, fromer2019) and Annual Review of Neuroscience (shenhav2017). Titles are in sentence case with braces protecting EEG, MEG, lme4, Julia, JavaScript, Web, WebGazer, Bayesian, jsPsych; sims2016 title uses the published en dash (Rate--distortion).
16. **hebert2023** — JET vol. 208, article 105612 (no page range; verified).
