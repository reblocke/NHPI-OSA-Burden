# NHPI-OSA-Burden Data Dictionary

This dictionary documents the restricted workbook structure expected by `PI Stats - Final.do`. It is inferred from the public Stata script and should be reviewed against the governed source workbook before any formal data release or rerun report.

## Data Boundary

The source workbook is restricted patient-level clinical data and is not public. Do not commit the workbook, row-level `.dta` files, identifiers, PHI, or logs with row-level values. Regenerated outputs should remain under ignored `outputs/stata/` paths unless explicitly reviewed for release.

## Expected Source Workbook

| Workbook | Sheet | Unit of observation | Public status |
| --- | --- | --- | --- |
| `Pacific Islander Data New - ESS.xlsx` | `all` | Patient sleep-testing record | Restricted |

## Source Variables

| Variable | Role | Notes |
| --- | --- | --- |
| `Sex` | Source demographic | Recoded to `Female`; trims historical value `"M "` |
| `Ageatsleepstudy` | Source demographic | Rounded and used for age categories and model scaling |
| `BMI` | Source anthropometric measure | Rounded and used for BMI categories and model scaling |
| `Heightcm`, `WeightKg` | Source anthropometric fields | `WeightKg` is converted to `WeightKg_n`; `Heightcm` receives a label |
| `Smoking`, `Hypertension`, `DM`, `CAD`, `CHF`, `Renaldisease`, `Lungdisease` | Source comorbidity indicators | Used in tables and selected regression models |
| `Typeofsleepstudy` | Source sleep-study type | Recoded to `HSAT` vs in-lab PSG |
| `AHI`, `REMAHI`, `NREMAHI`, `SupineAHI`, `ODI3`, `ODI4`, `LowestSpO2`, `Timebelow89minutes`, `Timebelow89percentage` | Source sleep metrics | Several string fields are converted to numeric analysis variables |
| `EpworthSleepinessScaleAtDiag` | Source symptom score | Destringed and recoded to `excessive_sleepiness` |
| `PercentageofUsage`, `AvgUsagemin`, `FlowAHI` | Source PAP adherence and on-treatment metrics | Used in tables and adherence models |
| `Severity123` | Source OSA severity code | Converted to zero-based `OSASeverity`, then dropped |
| `duplicate_flag` | Source duplicate-review flag | Used to drop duplicate records, then dropped |
| `OriginalAge`, `Dateof1stsleepclinicvisit`, `Dateofsleepstudy`, `Heightftin`, `Heightin`, `Weightlbs`, `CV1Met2CVMet3none4`, `Miscellaneous`, `DurationofDownload` | Source cleanup fields | Dropped before analysis |
| `Lastname`, `Firstname`, `MRN`, `DOB` | Potential PHI fields | Dropped with `capture drop` if present |

## Derived Variables And Value Labels

| Variable | Definition or rule | Review status |
| --- | --- | --- |
| `Female` | `1 = Female`, `0 = Male`, derived from `Sex` | needs_review |
| `HSAT` | `1 = HSAT`, `0 = In Lab PSG`, derived from `Typeofsleepstudy` text values | needs_review |
| `WeightKg_n`, `AHI_n`, `REMAHI_n`, `NREMAHI_n`, `SupineAHI_n`, `ODI3_n`, `ODI4_n` | Numeric conversions of source string or numeric fields | needs_review |
| `MinBelow89`, `PercBelow89` | Numeric minutes and percent of sleep time below SpO2 threshold | needs_review |
| `excessive_sleepiness` | `0 = ESS 0-10`, `1 = ESS 11 or higher` | complete |
| `OSASeverity` | `0 = Mild`, `1 = Moderate`, `2 = Severe`; derived as `Severity123 - 1` | needs_review |
| `modsev_dz` | `0 = Mild`, `1 = Moderate and Severe` | complete |
| `wt_cat` | BMI category: underweight, normal, overweight, class 1, class 2, class 3 obesity | complete |
| `age_cat` | Age category: 18-29, 30-39, 40-49, 50-59, 60+ | complete |
| `CumulativeUse` | `PercentageofUsage * AvgUsagemin` | needs_review |
| `severe_dz` | `0 = Not severe`, `1 = Severe` | complete |
| `age_decade`, `bmi_5`, `AHI_per_10`, `desat_per_10`, `FlowAHI_per_10` | Scaled predictors for regression and plotting | complete |
| `Goals`, `GoalsSens` | PAP adherence outcome variables from source workbook, labeled in the script | needs_review |

## Generated Outputs

| Output | Default regenerated path | Notes |
| --- | --- | --- |
| `PI Stats - Final.log` | `outputs/stata/<date>/PI Stats - Final.log` | Stata run log |
| `table 1.xlsx`, `table 1 gender.xlsx`, `table 1 normals.xlsx` | `outputs/stata/<date>/` | Baseline table variants |
| `AHI_Histogram.eps`, `ESS_Histogram.eps` | `outputs/stata/<date>/` | Exploratory histograms |
| `Figure1.eps`, `Figure2.eps`, `Figure3.eps`, `FigureS1.eps`, `FigureS2.eps` | `outputs/stata/<date>/` | Manuscript and supplement figure exports |

## Review Flags

- Confirm exact source workbook types, missing-value conventions, and coding of comorbidity/adherence variables against the governed workbook.
- Confirm whether `Goals` and `GoalsSens` are source columns or precomputed in an earlier private workflow.
- Confirm that any regenerated aggregate artifacts are approved before replacing the tracked historical outputs.
