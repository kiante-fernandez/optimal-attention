#!/bin/bash
# Build the PLOS Computational Biology submission from paper/manuscript.md.
#   paper/plos/manuscript.tex + .pdf   (PLOS template, numbered Vancouver references via BibTeX)
#   paper/plos/Fig1.tif ...            (TIFF, <= 7.5 in wide, 300-600 dpi)
#   paper/manuscript.docx              (Word working copy with embedded figures, numbered refs via CSL)
# DRAFT=1 (default) stamps a watermark and line numbers; build the submission with DRAFT=0.
set -euo pipefail
cd "$(dirname "$0")/../.."
MD=paper/manuscript.md
OUT=paper/plos
export DRAFT=${DRAFT:-1}
HDR=$(mktemp); trap 'rm -f "$HDR"' EXIT
{ echo '\usepackage{lineno}\linenumbers'; [ "$DRAFT" = 1 ] && echo '\usepackage{draftwatermark}\SetWatermarkText{DRAFT}\SetWatermarkScale{1.1}\SetWatermarkColor[gray]{0.8}'; } > "$HDR"
# 1. PLOS LaTeX
pandoc "$MD" --from markdown --to latex --shift-heading-level-by=-1 --lua-filter=$OUT/plos.lua --template=$OUT/plos.latex \
  --no-highlight -o $OUT/manuscript.tex
python3 - <<'PY'
import re, os
p='paper/plos/manuscript.tex'; s=open(p).read()
s=re.sub(r'\\(sub)*section\{','\\\\\\1section*{',s)
s=s.replace('\\section*{Introduction}','\\clearpage\n\\newgeometry{top=0.85in,left=1in,right=1in,footskip=0.75in}\n\\linenumbers\n\\section*{Introduction}',1)
s=re.sub(r'\\section\*\{References\}\\label\{references\}\s*','',s)
s=s.replace('\\begin{figure}','\\begin{figure}[!htb]')
if os.environ.get('DRAFT') == '1':
    s=s.replace('\\usepackage[right]{lineno}','\\usepackage[right]{lineno}\n\\usepackage{draftwatermark}\\SetWatermarkText{DRAFT}\\SetWatermarkScale{1.1}\\SetWatermarkColor[gray]{0.8}',1)
open(p,'w').write(s)
PY
( cd $OUT && pdflatex -interaction=nonstopmode manuscript.tex >/dev/null && bibtex manuscript >/dev/null && pdflatex -interaction=nonstopmode manuscript.tex >/dev/null && pdflatex -interaction=nonstopmode manuscript.tex >/dev/null ) || { echo "LaTeX build failed; see $OUT/manuscript.log"; exit 1; }
# 2. S1 Appendix
pandoc $OUT/S1_Appendix.md -o $OUT/S1_Appendix.pdf --pdf-engine=xelatex -V mainfont="Times New Roman" -V geometry:margin=1in -V fontsize=11pt --include-in-header="$HDR"
# 3. Figures as TIFF within PLOS limits
i=1
for f in fig1_task_model fig2_policy fig3_psychometrics fig4_fixation_patterns fig5_uncertainty fig6_value_directed fig7_response_phases fig8_time_resolved fig9_choice; do
  src=figures/paper/$f.png; [ -f "$src" ] || { i=$((i+1)); continue; }
  w=$(sips -g pixelWidth "$src" | awk '/pixelWidth/{print $2}')
  dpi=$(python3 -c "import math;w=$w;print(max(300,min(600,math.ceil(w/7.5))))")
  sips -s format tiff -s formatOptions lzw -s dpiWidth $dpi -s dpiHeight $dpi "$src" --out $OUT/Fig$i.tif >/dev/null
  i=$((i+1))
done
# 4. Word working copy, with the appendix appended after the reference list
APP=$(mktemp -d)/s1.md
{ printf '::: {#refs}\n:::\n\n## S1 Appendix. Derivations, policies, parameter search, and validation\n\n'
  awk 'NR==1&&/^---$/{f=1;next} f&&/^---$/{f=0;next} !f' $OUT/S1_Appendix.md | sed 's/^## /### /'; } > "$APP"
MDW=$(mktemp).md; sed 's/\\tag{\([0-9]*\)}/\\qquad (\1)/' "$MD" > "$MDW"   # Word drops \tag; number the equations by hand
pandoc "$MDW" "$APP" --citeproc --shift-heading-level-by=-1 --reference-doc=$OUT/reference.docx -o paper/manuscript.docx; rm -f "$MDW"
rm -rf "$(dirname "$APP")"
echo "built: $OUT/manuscript.pdf, $OUT/S1_Appendix.pdf, $OUT/Fig*.tif, paper/manuscript.docx"
