# NHPI-OSA-Burden

[![DOI](https://img.shields.io/badge/DOI-10.5664%2Fjcsm.10472-blue)](https://doi.org/10.5664/jcsm.10472)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Citation File Format](https://img.shields.io/badge/citation-CITATION.cff-green)](CITATION.cff)

Stata code and historical aggregate outputs for the Journal of Clinical Sleep Medicine article **"Severity, comorbidities, and adherence to therapy in Native Hawaiians/Pacific Islanders with obstructive sleep apnea."** The workflow analyzes a restricted chart-review workbook to describe obstructive sleep apnea severity, comorbidities, symptoms, and positive airway pressure adherence among Native Hawaiian/Pacific Islander patients evaluated at a single academic sleep center.

## Article And Repository

| Item | Link or identifier |
| --- | --- |
| Final article | [Journal of Clinical Sleep Medicine](https://jcsm.aasm.org/doi/10.5664/jcsm.10472) |
| DOI | [10.5664/jcsm.10472](https://doi.org/10.5664/jcsm.10472) |
| PubMed | [PMID 36727487](https://pubmed.ncbi.nlm.nih.gov/36727487/) |
| NLM full-text record | [PMCID PMC10152360](https://pmc.ncbi.nlm.nih.gov/articles/PMC10152360/) |
| Code repository | <https://github.com/reblocke/NHPI-OSA-Burden> |
| License | MIT for repository code and documentation |

The PMC article is linked for open-access reading and machine discovery. Full manuscript text is not mirrored in this repository.

## Authors, Funding, And Disclosures

Article authors: Brian W. Locke, Divya J. Sundar, and Darin Ryujin. Repository maintainer: Brian W. Locke (`@reblocke`; ORCID `0000-0002-3588-5238`).

The final article is the source of record for funding, acknowledgments, and disclosures. It reports research support for Brian W. Locke from NIH Ruth L. Kirschstein National Research Service Award `5T32HL105321` and the American Thoracic Society; the authors reported no conflicts of interest.

## Data Access

The source workbook is restricted patient-level clinical data and is **not public**. Do not commit raw workbooks, row-level derived datasets, patient identifiers, logs containing row-level values, PHI, or private drafts.

Expected private input:

| Workbook | Sheet | Default local path |
| --- | --- | --- |
| `Pacific Islander Data New - ESS.xlsx` | `all` | `data/private/Pacific Islander Data New - ESS.xlsx` |

See [data_dictionary.md](data_dictionary.md) and [data_dictionary.csv](data_dictionary.csv) for expected source variables, dropped/private fields, derived variables, and output artifacts.

## Repository Layout

| Path | Role |
| --- | --- |
| `PI Stats - Final.do` | Main Stata workflow for importing, cleaning, deriving variables, creating tables/figures, and modeling PAP adherence |
| `CITATION.cff` | Structured repository and article citation metadata |
| `llms.txt` | Machine-readable repository summary and agent guidance |
| `AGENTS.md` | Repository-specific instructions for coding agents |
| `data_dictionary.md` / `data_dictionary.csv` | Human-readable and machine-usable data dictionary |
| Root `Figure*.eps`, histogram `.eps`, `Figure2.gph`, and `table 1*.xlsx` | Historical aggregate publication artifacts retained for reference |

## Quick Start

Install Stata 17 or newer, then install required user-written packages once:

```stata
ssc install mdesc, replace
ssc install nmissing, replace
ssc install catplot, replace
ssc install coefplot, replace
ssc install estout, replace
ssc install table1_mc, replace
ssc install tab3way, replace
net install spost13_ado, from("https://www.indiana.edu/~jslsoc/stata")
net install cleanplots, from("https://tdmize.github.io/data") replace
ssc install schemepack, replace
```

Run from the repository root with the default restricted workbook path:

```stata
do "PI Stats - Final.do"
```

Optional arguments allow a different private workbook and output root:

```stata
do "PI Stats - Final.do" "path/to/Pacific Islander Data New - ESS.xlsx" "outputs/stata"
```

Batch example:

```bash
stata-mp -b do "PI Stats - Final.do"
```

The script writes regenerated outputs to `outputs/stata/<date>/` by default. Full execution requires the restricted workbook.

## Workflow And Outputs

The Stata workflow:

1. Imports the restricted Excel sheet `all`, removes scratch rows, drops duplicates, and drops obvious PHI fields if present.
2. Derives analysis variables including `Female`, `HSAT`, `OSASeverity`, `wt_cat`, `age_cat`, `excessive_sleepiness`, `Goals`, `GoalsSens`, `age_decade`, `bmi_5`, `AHI_per_10`, `desat_per_10`, and `FlowAHI_per_10`.
3. Runs missingness summaries, descriptive tables, logistic regression, sensitivity analyses, multiple imputation, and coefficient plots.
4. Exports regenerated `.eps` figures and `.xlsx` tables under the output directory.

| Manuscript item | Script/output mapping |
| --- | --- |
| Figure 1 | OSA severity by age category and sex; `Figure1.eps` |
| Figure 2 | OSA severity by BMI category and sex; `Figure2.eps` plus historical `Figure2.gph` |
| Figure 3 | Adjusted odds ratios for PAP adherence target; `Figure3.eps` |
| Supplementary figures | Sex-stratified OSA severity and SpO2 time below threshold; `FigureS1.eps`, `FigureS2.eps` |
| Table 1 | Baseline demographics, comorbidities, sleep metrics, and adherence; `table 1.xlsx` and variants |

Severity thresholds used by the script: mild OSA is AHI 5-15 events/hour, moderate OSA is AHI 15-30 events/hour, and severe OSA is AHI greater than 30 events/hour.

## Historical Artifacts

The root EPS, GPH, and XLSX files are aggregate historical outputs from the manuscript workflow. They are retained for reference and reuse, but newly regenerated outputs should stay under ignored `outputs/stata/` paths unless intentionally reviewed for release.

## Citation

If using this repository or reproducing its results, cite the final article and the repository commit or release used.

> Locke BW, Sundar DJ, Ryujin D. Severity, comorbidities, and adherence to therapy in Native Hawaiians/Pacific Islanders with obstructive sleep apnea. *J Clin Sleep Med.* 2023;19(5):967-974. doi:10.5664/jcsm.10472

```bibtex
@article{Locke2023_NHPI_OSA_Burden,
  title   = {Severity, comorbidities, and adherence to therapy in Native Hawaiians/Pacific Islanders with obstructive sleep apnea},
  author  = {Locke, Brian W. and Sundar, Divya J. and Ryujin, Darin},
  journal = {Journal of Clinical Sleep Medicine},
  year    = {2023},
  volume  = {19},
  number  = {5},
  pages   = {967--974},
  doi     = {10.5664/jcsm.10472},
  pmid    = {36727487},
  pmcid   = {PMC10152360}
}
```

Machine-readable citation metadata are available in [CITATION.cff](CITATION.cff).

## License

Repository code and documentation are released under the MIT License; see [LICENSE](LICENSE). Restricted clinical data, source workbooks, row-level derived datasets, third-party materials, and publisher-formatted article files are excluded.

## Contact

For public repository issues, use GitHub issues or pull requests. Do not post private clinical data in issues, pull requests, or logs.
