# AGENTS

## Project Purpose

This public repository contains Stata analysis code and historical aggregate outputs for the Journal of Clinical Sleep Medicine article "Severity, comorbidities, and adherence to therapy in Native Hawaiians/Pacific Islanders with obstructive sleep apnea" (DOI `10.5664/jcsm.10472`, PMID `36727487`, PMCID `PMC10152360`).

## Public And Data-Safety Rules

- Treat the repository as public.
- Do not commit the source workbook, row-level derived datasets, patient identifiers, PHI, credentials, local absolute paths, private drafts, or publisher-formatted article files.
- Link the DOI, PubMed, and PMC records instead of copying full article text into Markdown.
- Keep regenerated outputs under ignored folders such as `outputs/stata/`.
- The tracked EPS/GPH/XLSX files are historical aggregate artifacts. Do not replace them from restricted data without explicit review.

## How To Orient Quickly
Consult only the entries relevant to the requested edit or run.

1. Read `README.md` for scope, article identifiers, run commands, data restrictions, citation, and license.
2. Read `llms.txt` for a compact machine-readable summary and agent cautions.
3. Use `data_dictionary.md` and `data_dictionary.csv` for expected workbook fields and derived variables.
4. Inspect `PI Stats - Final.do` before running; full execution requires the restricted private workbook.

## Workflow

From the repository root:

```stata
do "PI Stats - Final.do"
```

Optional explicit paths:

```stata
do "PI Stats - Final.do" "path/to/Pacific Islander Data New - ESS.xlsx" "outputs/stata"
```

Full execution should fail clearly if the restricted local workbook is absent.

## Verification Before Publishing Changes

- Validate `CITATION.cff` after citation edits.
- Parse `data_dictionary.csv` after dictionary edits.
- Run `git diff --check`.
- Search for stale generic LLM-readiness text, manual working-directory placeholders, root analysis datasets, and restricted workbook files before pushing.
- For analysis/runner changes, perform applicable Stata verification within the authorized workflow. It requires a licensed runtime and the approved inputs; executable availability alone does not authorize a restricted-data run. Inspect generated logs and report unavailable data/package/runtime gates separately from static checks.

## Documentation Standards

- Keep `README.md`, `llms.txt`, `AGENTS.md`, `CITATION.cff`, and data dictionary files internally consistent.
- Preserve DOI `10.5664/jcsm.10472`, PMID `36727487`, PMCID `PMC10152360`, and article metadata unless a source-backed correction is made.
- Mark inferred data dictionary fields as `needs_review` rather than inventing unavailable definitions.
