# Revision Notes 2

**Manuscript:** EEREV-D-26-00102R2, "Rent Control? Bad in Theory, Not Always in Practice"
**Authors:** Shahar Rotberg and Lin Zhang

Dear Professor Pappa,

Thank you for the conditional acceptance of our paper. We are grateful to you, the Associate Editor and the referees. This note responds point by point to the items in your letter.

**Our submission now includes the manuscript, the figure source files and the replication files.**

## 1. Preparing the files for production

- **Labels.** Every file name includes the manuscript number and states what the file is: `EEREV-D-26-00102R2_manuscript`, `_appendix`, `_references`, `_Table_1` to `_Table_6`, `_Figure_1`, `_Figure_2`, `_ReadMe`.
- **Figures.** Each figure file carries the figure number used in the article: `EEREV-D-26-00102R2_Figure_1.eps` and `EEREV-D-26-00102R2_Figure_2.eps`. Following Elsevier's artwork instructions, both are vector EPS files with embedded Arial-compatible fonts, sized for full page width (190 mm), where all lettering prints at 7 pt or larger.
- **Underscores.** No file name contains spaces.
- **ReadMe.** The replication zip contains `EEREV-D-26-00102R2_ReadMe.pdf`. It lists every file in the package and explains the data sources and their availability, the software required, run times, how to run the code, and which output file produces each number in Tables 2-6 and Figure 2.
- **Size.** The replication files total about 18 MB, below the 100 MB limit, so we upload them directly. The same files are also available at <https://github.com/srotberg/rent_control_bad_in_theory_not_always_in_practice>, which is cited in the paper's first footnote.
- **Two zip folders.** `EEREV-D-26-00102R2_Source_files.zip` and `EEREV-D-26-00102R2_Replication_files.zip`.

## 2-4. File inventory

**Manuscript and LaTeX source files** (`EEREV-D-26-00102R2_Source_files.zip`):

- `EEREV-D-26-00102R2_manuscript.tex`: the manuscript. It reads the appendix, each table and each figure from the separate files below.
- `EEREV-D-26-00102R2_appendix.tex`: the appendix.
- `EEREV-D-26-00102R2_Table_1.tex` to `EEREV-D-26-00102R2_Table_6.tex`: one file per table, numbered as in the article.
- `EEREV-D-26-00102R2_references.bib`, `EEREV-D-26-00102R2_manuscript.bbl` and `aer.bst`: the bibliography, its compiled version, and the AER bibliography style.
- `EEREV-D-26-00102R2_manuscript.pdf`: the compiled manuscript.
- `EEREV-D-26-00102R2_Figure_1.eps` and `EEREV-D-26-00102R2_Figure_2.eps`: artwork (vector EPS). Vector PDF copies of the same two figures are included so that the LaTeX source compiles directly with pdfLaTeX.

**Replication files** (`EEREV-D-26-00102R2_Replication_files.zip`, supplementary material for online publication):

- `EEREV-D-26-00102R2_ReadMe.pdf`
- `code/`: the Fortran source that produces every simulation result, with instructions.
- `data/`: the data and Stata scripts behind the calibration targets and data moments.
- `outputs/`: the output file of every simulation, including the six reported in the paper.
- `plots/`: the Python scripts that draw Figures 1 and 2, and the figure files.

All data files in the package may be redistributed.

**Census and SFS microdata.** Several calibration targets and data moments (in Tables 2-4 and Figure 2) were computed from 2016 Census of Population microdata, and the resulting statistics were vetted. The package contains these vetted statistics and the Stata scripts that produced them, but not the microdata file itself. Likewise, the statistics we computed from the 2019 Survey of Financial Security microdata, and the scripts, are included, but not the microdata file; both microdata sources are Statistics Canada files. We ask the Editor to accept the vetted statistics and scripts in place of these two microdata files. All other data and all code are included.

## Corrections made while preparing the replication files

While checking every reported number against the simulation output files, we found and corrected the following. None of these changes affects the paper's conclusions.

- **Table 5.** Five cells now match the output files: Experiment 2 free-market rent (5.76 to 5.74) and homeownership (-1.08 to -1.02 p.p.); Experiment 3 homelessness (70.50 to 70.47); Experiment 4 controlled initial rent (-39.02 to -39.20) and homeownership (1.12 to 1.17 p.p.).
- **Figure 2.** The model lines were drawn from an earlier run of the benchmark; they are now drawn from the benchmark simulation reported in the paper. The patterns are unchanged, except that the model's homeownership rate after age 66 is now higher than in the data, so the sentence describing panel (a) in Section 4.3 now reads "the model over-predicts homeownership among middle-aged and older households and under-predicts it for young cohorts."
- **Figures 1 and 2.** Now supplied as vector artwork with the same content; the previous Figure 1 was a low-resolution image.
- **LaTeX.** Minor fixes so that the source compiles without errors; these do not change any content.

Sincerely,

Shahar Rotberg, on behalf of both authors
