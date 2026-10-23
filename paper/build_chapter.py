#!/usr/bin/env python3
"""Render the dissertation-chapter version of paper/manuscript.md as an APA 7 manuscript in Word.

Writes dissertation-papers/chapter-2/3_source/3_metalevel_MDP_multi_response_<version>.md (the converted source,
kept for reference) and dissertation-papers/chapter-2/3_metalevel_MDP_multi_response_<version>.docx, reusing the
APA machinery of the chapter's v5_source/build_v5.py (Times New Roman 12, double spacing, running head, APA
headings, table and figure fixes) and the APA 7 citation style from Zotero. Conversions from the PLOS source:
title page with author note (funding, contributions, correspondence), abstract page with keywords, headings in
title case one level up, author-date citations, "Figure N"/"Equation N" references, APA figure and table blocks,
S1 Appendix as the Appendix after the references.  Usage: python3 paper/build_chapter.py [version]  (default v3)
"""
import importlib.util
import pathlib
import re
import subprocess
import sys

REPO = pathlib.Path(__file__).resolve().parents[1]
CH2 = pathlib.Path("/Users/kiante/Documents/dissertation-papers/chapter-2")
SRC_DIR = CH2 / "3_source"
VERSION = sys.argv[1] if len(sys.argv) > 1 else "v3"
STEM = f"3_metalevel_MDP_multi_response_{VERSION}"
CSL = pathlib.Path("/Users/kiante/Zotero/styles/apa.csl")
BIB = REPO / "paper/plos/refs.bib"
RUNNING_HEAD = "ATTENTION IN MULTI-RESPONSE CHOICE"
KEYWORDS = ("attention, eye tracking, value-based decision making, metalevel Markov decision process, "
            "resource rationality, multi-response choice")
FUNDING = ("This work was supported by the National Science Foundation (CAREER Award 2148982 to Ian Krajbich) and by "
           "The Ohio State University's Decision Sciences Collaborative (to Kianté A. Fernandez).")
CONTRIB = ("Kianté A. Fernandez, Frederick Callaway, and Ian Krajbich designed the experiment and analyses. Kianté A. "
           "Fernandez programmed and conducted the experiment and analyzed the data. Frederick Callaway consulted on "
           "the model. Kianté A. Fernandez and Ian Krajbich wrote the article. Ian Krajbich supervised the project. "
           "Frederick Callaway provided feedback on the edited draft. We thank Uma R. Karmarkar for her contributions "
           "to the design of the experiment.")
CORRESP = ("Correspondence concerning this article should be addressed to Kianté A. Fernandez, Department of "
           "Psychology, University of California, Los Angeles, 1285 Franz Hall, Los Angeles, CA 90095. "
           "Email: kiantefernan@gmail.com")
PAGEBREAK = ('```{=openxml}\n<w:p><w:pPr><w:pageBreakBefore/><w:spacing w:before="0" w:after="0" w:line="20" '
             'w:lineRule="exact"/></w:pPr></w:p>\n```')
SMALL = {"a", "an", "the", "of", "in", "on", "and", "or", "for", "to", "by", "as", "at", "from", "nor", "but", "per",
         "via", "vs"}


def title_case(t):
    def cap(w):
        return "-".join(p[:1].upper() + p[1:] for p in w.split("-"))
    out = []
    for i, w in enumerate(t.split(" ")):
        if "$" in w or w.startswith("\\") or w[:1].isupper() or not w:
            out.append(w)
        elif i > 0 and w.lower() in SMALL:
            out.append(w.lower())
        else:
            out.append(cap(w))
    return " ".join(out)


def first_sentence(text):
    """Split 'Title. Rest.' at the first period outside parentheses; returns (title, rest)."""
    depth = 0
    for i, ch in enumerate(text):
        depth += (ch == "(") - (ch == ")")
        if ch == "." and depth == 0 and (i + 1 == len(text) or text[i + 1] == " "):
            return text[:i], text[i + 1:].strip()
    return text, ""


def name_cites(text):
    """'Callaway et al. [@callaway2021]' -> '@callaway2021' (renders as Callaway et al. (2021) in author-date style)."""
    pat = re.compile(r"([A-Z][\w'’\-]+(?:, [A-Z][\w'’\-]+)*(?:,? (?:and|&) [A-Z][\w'’\-]+)?(?: et al\.)?) \[@(\w+)\]")
    return pat.sub(lambda m: f"@{m.group(2)}" if (" et al." in m.group(1) or " and " in m.group(1)) else m.group(0), text)


def figure_block(m, counter):
    counter[0] += 1
    title, _ = first_sentence(m.group(1).strip())
    note = m.group(2).strip()
    path = REPO / m.group(3)
    assert path.exists(), path
    block = (f'::: {{custom-style="Figure Label"}}\n**Figure {counter[0]}**\n\n*{title_case(title)}*\n\n'
             f'![]({path}){{width=6.5in}}\n:::\n')
    if note:
        block += f'\n::: {{custom-style="No Indent"}}\n*Note.* {note}\n:::\n'
    return block


def tables(text):
    out, lines, i = [], text.split("\n"), 0
    while i < len(lines):
        m = re.match(r"^Table ([A-Z]?\d+)\. (.*)$", lines[i])
        if m and i + 2 < len(lines) and lines[i + 1] == "" and lines[i + 2].startswith("|"):
            title, note = first_sentence(m.group(2))
            out += [f'::: {{custom-style="No Indent"}}', f"**Table {m.group(1)}**", "", f"*{title_case(title)}*", ":::", ""]
            i += 2
            while i < len(lines) and lines[i].startswith("|"):
                out.append(lines[i]); i += 1
            if note:
                out += ["", '::: {custom-style="No Indent"}', f"*Note.* {note}", ":::"]
            continue
        out.append(lines[i]); i += 1
    return "\n".join(out)


def convert(text, counter, shift=True):
    text = name_cites(text)
    text = text.replace("[Fernandez KA, Callaway F, Karmarkar UR, Krajbich I. Rank-order preferences are not quickly "
                        "accessible. In preparation]", "(Fernandez, Callaway, Karmarkar, & Krajbich, in preparation)")
    text = text.replace("S1 Appendix", "Appendix")
    text = re.sub(r"\bFigs (\d)", r"Figures \1", text)
    text = re.sub(r"\bFig (\d)", r"Figure \1", text)
    text = re.sub(r"\bEqs (\d)", r"Equations \1", text)
    text = re.sub(r"\bEq (\d)", r"Equation \1", text)
    text = re.sub(r"\\tag\{(\d+)\}", r"\\qquad (\1)", text)
    text = re.sub(r"^!\[\*\*(.+?)\*\*\s*(.*?)\]\((figures/paper/[^)]+)\)(\{[^}]*\})?\s*$",
                  lambda m: figure_block(m, counter), text, flags=re.M)
    text = tables(text)

    def heading(m):
        level = len(m.group(1)) - (1 if shift else 0)
        return "#" * level + " " + title_case(m.group(2))
    return re.sub(r"^(#{2,4}) (.*)$", heading, text, flags=re.M)


def between(s, start, end):
    a = s.index(start) + len(start)
    b = s.index(end, a) if end else len(s)
    return s[a:b].strip("\n")


def main():
    SRC_DIR.mkdir(exist_ok=True)
    spec = importlib.util.spec_from_file_location("build_v5", CH2 / "v5_source/build_v5.py")
    b5 = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(b5)
    b5.HEADER = b5.HEADER.replace(b5.RUNNING_HEAD, RUNNING_HEAD)
    ref = SRC_DIR / "apa_reference.docx"
    if not ref.exists():
        b5.make_reference(ref)
    ms = (REPO / "paper/manuscript.md").read_text()
    app = (REPO / "paper/plos/S1_Appendix.md").read_text()
    title = title_case(re.search(r'^title: "(.*)"$', ms, flags=re.M).group(1))
    abstract = between(ms, "## Abstract\n", "## Author summary")
    body = between(ms, "## Introduction", "## Supporting information")   # APA: the title stands in for the Introduction heading
    appendix = re.sub(r"\A---\n.*?\n---\n", "", app, flags=re.S).strip("\n")
    counter = [0]
    md = "\n\n".join([
        '::: {custom-style="Centered"}\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n**' + title + '**\n\n&nbsp;\n\n'
        'Kianté A. Fernandez^1^, Frederick Callaway^2^, and Ian Krajbich^1^\n\n'
        '^1^Department of Psychology, University of California, Los Angeles\n\n'
        '^2^Department of Psychology, New York University\n\n&nbsp;\n\n&nbsp;\n\n&nbsp;\n\n**Author Note**\n:::',
        FUNDING, CONTRIB, CORRESP, PAGEBREAK, "# Abstract", abstract, f"*Keywords:* {KEYWORDS}", PAGEBREAK,
        f"# {title}", convert(body, counter), "# References", "::: {#refs}\n:::", PAGEBREAK, "# Appendix",
        convert(appendix, counter, shift=False), ""])
    assert counter[0] == 9, counter
    (SRC_DIR / f"{STEM}.md").write_text(md)
    out = CH2 / f"{STEM}.docx"
    subprocess.run(["pandoc", "-f", "markdown", "--columns", "40", "--citeproc", "--csl", str(CSL), "--bibliography",
                    str(BIB), "--reference-doc", str(ref), "--resource-path", f"{REPO}:{SRC_DIR}", "-o", str(out)],
                   input=md, text=True, check=True)
    b5.finish_docx(out)
    print("Built", out)


if __name__ == "__main__":
    main()
