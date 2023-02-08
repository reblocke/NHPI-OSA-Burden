* Pacific Islander Sleep Disordered Breathing Statistics

capture log close
*log using temp.log
* Data processing
clear
cd "" // choose folder containing data
import excel "Pacific Islander Data New - ESS.xlsx", sheet("all") firstrow 
program define datetime 
end

capture mkdir "Results and Figures"
capture mkdir "Results and Figures/$S_DATE/" //make new folder for figure output if needed

*drop observations 146-223, these were just scratch pad calculations
drop in 146/223 
drop if !missing(duplicate_flag) //drop 4 identified duplicates

//Drop un-needed variables

//PHI if present
//drop Lastname Firstname MRN DOB
//remaining variables
drop OriginalAge Dateof1stsleepclinicvisit Dateofsleepstudy Heightftin Heightin Weightlbs CV1Met2CVMet3none4 Miscellaneous DurationofDownload duplicate_flag

*Generate and Label Female variable
replace Sex="M" if Sex== "M "
gen Female = 1 if Sex == "F"
replace Female = 0 if Sex == "M"
label define femalelab 0 "Male" 1 "Female"
label values Female femalelab

*Generate and label HSAT varible
gen HSAT = 1 if Typeofsleepstudy == "NOX" 
replace HSAT = 1 if Typeofsleepstudy == "Portable Cardiopulmonary"
replace HSAT = 1 if Typeofsleepstudy == "Portable Cardiopulmonary Sleep Study"
replace HSAT = 1 if Typeofsleepstudy == "unattended Cardiopulmonary"
replace HSAT = 1 if Typeofsleepstudy == "HST"
replace HSAT = 0 if Typeofsleepstudy == "Diagnostic"
replace HSAT = 0 if Typeofsleepstudy == "Split PSG"
replace HSAT = 0 if Typeofsleepstudy == "DPSG"
replace HSAT = 0 if Typeofsleepstudy == "DSPG"
replace HSAT = 0 if Typeofsleepstudy == "PSG"
replace HSAT = 0 if Typeofsleepstudy == "Split Night PSG"
replace HSAT = 0 if Typeofsleepstudy == "Cardiopulmonary"
replace HSAT = 0 if Typeofsleepstudy == "Diagnostic PSG"
label define hsatlab 0 "In Lab PSG" 1 "HSAT"
label values HSAT hsatlab

*Convert string columns to numbers
gen WeightKg_n = round(real(WeightKg), 0.1)
gen AHI_n = round(real(AHI), 0.1)
gen REMAHI_n = real(REMAHI)
gen NREMAHI_n = real(NREMAHI)
gen SupineAHI_n = real(SupineAHI)
gen ODI3_n = round(real(ODI3), 0.1)
gen ODI4_n = round(real(ODI4), 0.1)
gen MinBelow89 = real(Timebelow89minutes)
gen PercBelow89 = real(Timebelow89percentage)
replace Ageatsleepstudy = round(Ageatsleepstudy, 0.1)
replace BMI = round(BMI, 0.1)
destring EpworthSleepinessScaleAtDiag, replace force

//excessive_sleepiness? ESS 0-10 normal, 11 or higher = excessive daytime sleepiness
recode EpworthSleepinessScaleAtDiag min/10=0 11/max=1, gen(excessive_sleepiness)
label define sleepy_lab 0 "No Excessive Daytime Sleepinesss" 1 "Excessive Daytime Sleepiness"
label variable excessive_sleepiness "Excessive Daytime Sleepiness?"
label values excessive_sleepiness sleepy_lab

npresent EpworthSleepinessScaleAtDiag
nmissing EpworthSleepinessScaleAtDiag
tab excessive_sleepiness
summarize EpworthSleepinessScaleAtDiag, detail

*Relabel OSA Severity
gen OSASeverity = Severity123 - 1
label define osa_label 0 "Mild" 1 "Moderate" 2 "Severe"

label values OSASeverity osa_label
drop Severity123

recode OSASeverity 0=0 1=1 2=1, gen(modsev_dz)
label define modsev_label 0 "Mild" 1 "Moderate and Severe"
label variable modsev_dz "OSA Severity"
label values modsev_dz modsev_label

* Gen weight_cat
recode BMI min/18.5=0 18.5/25=2 25/30=3 30/35=4 35/40=5 40/max=6, gen(wt_cat)
label variable wt_cat "Weight Category (BMI (kg/m{superscript:{bf:2}}))"
label define wt_label 0 "Underweight" 2 "Normal" 3 "Overweight" 4 "Class 1 Obesity" 5 "Class 2 Obesity" 6 "Class 3 Obesity"
label values wt_cat wt_label

* Gen age_cat
recode Ageatsleepstudy min/29=0 30/39=1 40/49=2 50/59=3 60/max=4, gen(age_cat)
label variable age_cat "Age Category (at sleep study)"
label define age_label 0 "18-29" 1 "30-39" 2 "40-49" 3 "50-59" 4 "+60" 
label values age_cat age_label

gen CumulativeUse = PercentageofUsage * AvgUsagemin

* Labels
label variable Ageatsleepstudy "Age at Sleep Study (yrs)"
label variable Heightcm "Height (cm)"
label variable BMI "BMI (kg/m^2)"
label variable REMAHI_n "AHI during REM sleep (events/hr)"
label variable NREMAHI_n "AHI during NREM sleep (events/hr)"
label variable SupineAHI_n "AHI while supine (events/hr)"
label variable LowestSpO2 "Lowest SpO2 recorded (%)"
label variable ODI3_n "ODI, 3% desturation threshold (events/hr)"
label variable ODI4_n "ODI, 4% desaturation threshold (events/hr)"
label variable Female "Female gender"
label variable HSAT "Home Sleep Study"
label variable MinBelow89 "Minutes with saturation below 89%"
label variable OSASeverity "OSA Severity"
label variable PercBelow89 "% of time with SpO2 <89%"
label variable WeightKg_n "Weight (Kg)"
label variable AHI_n "AHI (events/hr)"
label variable PercentageofUsage "% of nights with any PAP usage"
label variable AvgUsagemin "Average hourly PAP usage per night"
label variable FlowAHI "On treatment AHI estimated by PAP machine"
label variable Goals "Meeting adherence goal: +70% nights, +4h used"
label variable GoalsSens "Meeting adherence goal of +70% nights used, missing = no"
label variable CumulativeUse "Nightly Usage"
label variable EpworthSleepinessScaleAtDiag "ESS (at diagnosis)"

* Generate severe disease indicator
recode OSASeverity 0=0  1=0  2=1, gen(severe_dz) 
label variable severe_dz "Is OSA Severe?"
label define sev_label 0 "Not Severe (AHI < 30)" 1 "Severe (AHI > 30)"
label values severe_dz sev_label

* Generate age per decade and BMI per 5 for regressions. Also AHI and %spo2 <89
gen age_decade = Ageatsleepstudy / 10
label variable age_decade "Age (per 10 years)"
gen bmi_5 = BMI / 5
//label variable bmi_5 "BMI (per 5  kg/m^2)" //version for tables
label variable bmi_5 "BMI (per 5  kg/m{superscript:{bf:2}})" //version for figs
gen AHI_per_10 = AHI_n / 10
label variable AHI_per_10 "AHI (per 10 events/hr)"
gen desat_per_10 = PercBelow89 / 10
label variable desat_per_10 "SpO2<89% (per 10% sleep time)"
gen FlowAHI_per_10 = FlowAHI/10
label variable FlowAHI_per_10 "AHI on Treatment"

save PI_data, replace

*End of Data clean up

*Begin data analysis

use PI_data, clear


mdesc //tabulate missing data

tab AHI_n OSASeverity //fidelity check
tabulate OSASeverity, summarize(EpworthSleepinessScaleAtDiag)
tabstat EpworthSleepinessScaleAtDiag , statistics(mean sd median p25 p75) by(OSASeverity)	

*Table 1 - version w normal distributions stratified by Gender
table1_mc, by(Female) ///
vars( ///
Ageatsleepstudy contn %4.1f \ ///
BMI contn %4.1f \ ///
wt_cat cat %4.0f \ ///
Smoking bin %4.0f \ ///
Hypertension bin %4.0f \ ///
DM bin %4.0f \ ///
CAD bin %4.0f \ ///
CHF bin %4.0f \ ///
Renaldisease bin %4.0f \ ///
Lungdisease bin %4.0f \ ///
HSAT bin %4.0f \ ///
AHI_n conts %4.0f \ ///
OSASeverity cat %4.0f \ ///
EpworthSleepinessScaleAtDiag conts %4.0f \ ///
excessive_sleepiness bin %4.0f \ ///
LowestSpO2 conts %4.0f \ ///
MinBelow89 conts %4.0f \ ///
PercentageofUsage conts %4.0f \ ///
Goals bin %4.0f \ ///
AvgUsagemin conts %4.0f \ ///
FlowAHI conts %4.0f \ ///
) ///
nospace percent_n onecol total(before) ///
saving("Results and Figures/$S_DATE/table 1 normals.xlsx", replace)


*Table 1 - version stratified by Gender
table1_mc, by(Female) ///
vars( ///
Ageatsleepstudy conts %4.0f \ ///
BMI conts %4.0f \ ///
wt_cat cat %4.0f \ ///
Smoking bin %4.0f \ ///
Hypertension bin %4.0f \ ///
DM bin %4.0f \ ///
CAD bin %4.0f \ ///
CHF bin %4.0f \ ///
Renaldisease bin %4.0f \ ///
Lungdisease bin %4.0f \ ///
HSAT bin %4.0f \ ///
AHI_n conts %4.0f \ ///
OSASeverity cat %4.0f \ ///
EpworthSleepinessScaleAtDiag conts %4.0f \ ///
excessive_sleepiness bin %4.0f \ ///
LowestSpO2 conts %4.0f \ ///
MinBelow89 conts %4.0f \ ///
PercentageofUsage conts %4.0f \ ///
Goals bin %4.0f \ ///
AvgUsagemin conts %4.0f \ ///
FlowAHI conts %4.0f \ ///
) ///
nospace percent_n onecol total(before) ///
saving("Results and Figures/$S_DATE/table 1 gender.xlsx", replace)

*Table 1 - version stratified by Severity
table1_mc, by(OSASeverity) ///
vars( ///
Ageatsleepstudy conts %4.0f \ ///
Female bin %4.0f \ ///
BMI conts %4.0f \ ///
wt_cat cat %4.0f \ ///
Smoking bin %4.0f \ ///
Hypertension bin %4.0f \ ///
DM bin %4.0f \ ///
CAD bin %4.0f \ ///
CHF bin %4.0f \ ///
Renaldisease bin %4.0f \ ///
Lungdisease bin %4.0f \ ///
HSAT bin %4.0f \ ///
AHI_n conts %4.0f \ ///
EpworthSleepinessScaleAtDiag conts %4.0f \ ///
excessive_sleepiness bin %4.0f \ ///
LowestSpO2 conts %4.0f \ ///
MinBelow89 conts %4.0f \ ///
PercentageofUsage conts %4.0f \ ///
Goals bin %4.0f \ ///
AvgUsagemin conts %4.0f \ ///
FlowAHI conts %4.0f \ ///
) ///
nospace percent_n onecol total(before) ///
saving("Results and Figures/$S_DATE/table 1.xlsx", replace)

*** Factors that might influence whether or not a patient meets adherence targets
*Non-imputed Logistic Regression for meeting goals or not
logistic Goals age_decade ib0.Female bmi_5 i.Smoking i.HSAT c.FlowAHI_per_10 c.AHI_per_10 c.desat_per_10 c.EpworthSleepinessScaleAtDiag
estimates store logistic_goals
fitstat
estat gof, group(10) table
lroc //0.699
esttab, ci pr2 wide

//Average Marginal effects of each predictor - not utilized in manuscript: 
margins, dydx(age_decade ib0.Female bmi_5 i.Smoking HSAT FlowAHI_per_10 AHI_per_10 desat_per_10 EpworthSleepinessScaleAtDiag) post
estimates store goals_margins
//outreg2 using goals_margins, word append ctitle(Meeting Adherence Targets) dec(2) sdec(2) stat(coef ci pval) title(Factors associated w treatment adherence) label 
marginsplot, horizontal xline(0) yscale(reverse) recast(scatter)

*Logistic Regression for meeting goals or not (sensitivity analysis where missing = no)
logistic GoalsSens c.age_decade i.Female c.bmi_5 i.Smoking i.HSAT c.FlowAHI_per_10 c.AHI_per_10 c.desat_per_10 c.EpworthSleepinessScaleAtDiag

*Linear Regression for percentage of nights used - non-normal distribution. Not all that close to negative binomial either - thus, this has been omitted from the manuscript and replaced with logistic regression diagnostic tests.
glm PercentageofUsage c.age_decade i.Female bmi_5 i.Smoking i.Hypertension i.HSAT c.FlowAHI_per_10 c.AHI_per_10 c.desat_per_10 c.EpworthSleepinessScaleAtDiag, family(nbinomial) 
//regress PercentageofUsage age_decade Female bmi_5 i.Smoking i.Hypertension HSAT FlowAHI_per_10 AHI_per_10 desat_per_10 EpworthSleepinessScaleAtDiag
estimates store nights_used_reg

*Linear Regression for average nightly ussage - not used
tab AvgUsagemin, plot
glm  AvgUsagemin age_decade Female bmi_5 Smoking HSAT FlowAHI_per_10 AHI_per_10 desat_per_10 EpworthSleepinessScaleAtDiag, family(nbinomial)
estimates store avg_usage_reg
vif, unc


// -------VISUALIZATIONS-------

set scheme cleanplots

//relabels for figure to allow superscripts and greater than or equal to's =
//label variable wt_cat "Weight Category (BMI (kg/m^2)"
//label define age_label 0 "18-29" 1 "30-39" 2 "40-49" 3 "50-59" 4 "{&ge}60" 
//label variable BMI "BMI (kg/m{superscript:{bf:2}})"

hist AHI_n, xsize(8) ysize(5)
graph export "Results and Figures/$S_DATE/AHI_Histogram.eps", as(eps) replace

hist EpworthSleepinessScaleAtDiag, discrete freq xsize(8) ysize(5) ytitle("Count (n)") xtitle("Epworth Sleepiness Score (ESS) at referral")
graph export "Results and Figures/$S_DATE/ESS_Histogram.eps", as(eps) replace

catplot OSASeverity, over(EpworthSleepinessScaleAtDiag) stack asyvars ytitle("Percentage ") recast(bar) b1title("Age") bar(1, color(gs12)) bar(2, color(gs8)) bar(3, color(gs4)) legend(pos(6) rows(1) size(small) title("Severity of Sleep Apnea", size(small)) symplacement(center)) xsize(8) ysize(5)

twoway kdensity EpworthSleepinessScaleAtDiag if OSASeverity==0, recast(area) fcolor(%70) lwidth(*0.25) || ///
kdensity EpworthSleepinessScaleAtDiag if OSASeverity==1, recast(area) fcolor(%70) lwidth(*0.25) || ///
kdensity EpworthSleepinessScaleAtDiag if OSASeverity==2, recast(area) fcolor(%70) lwidth(*0.25) ||, ///
legend(subtitle("Severity") order(1 "Mild" 2 "Moderate" 3 "Severe")) ///
xtitle("Epworth Sleepiness Scale") ///
ytitle("Kernel Density") ///
title("Kernel-Density Plots of Minutes of ESS by OSA Severity") ///
subtitle("Densities are group specific") scheme(white_w3d)

tab Female
label define PIfemalelab 0 "Male (n=97)" 1 "Female (n=44)"
label values Female PIfemalelab

histogram MinBelow89, by(Female, note("In-lab testing performed at ~1300m elevation above sea level")) percent bin(10) ytitle("Percentage, by gender") xsize(8) ysize(5)
graph export "Results and Figures/$S_DATE/FigureS2.eps", as(eps) replace

catplot OSASeverity, over(Female) stack asyvars percent(Female) yla(0(10)100) ytitle("Percentage (%)") graphregion(color(white)) recast(bar) bar(1, color(gs14)) bar(2, color(gs9)) bar(3, color(gs4)) legend(off) blabel(name, position(center) color(black)) xsize(8) ysize(5)
graph export "Results and Figures/$S_DATE/FigureS1.eps", as(eps) replace

catplot OSASeverity, percent(Female) over(age_cat) by(Female, note(""))  asyvars ytitle("Percentage of patients of the same gender (%)") recast(bar) b1title("Age") bar(1, color(gs12)) bar(2, color(gs8)) bar(3, color(gs4)) legend(rows(1) size(small) title("Severity of Sleep Apnea", size(small))symplacement(center)) xsize(8) ysize(5)
graph export "Results and Figures/$S_DATE/Figure1.eps", as(eps) replace

//figure 2
//3 category version - 
// TODO: [x]  manually remove redundant y axis label - this can be done in graph editor after the fact
// [ ] this one is used
catplot OSASeverity, percent(Female) over(wt_cat) by(Female, note(" ")) asyvars ytitle("Percentage of patients of the same gender (%)") bar(1, color(gs12)) bar(2, color(gs8)) bar(3, color(gs4)) legend(rows(1) size(small) symplacement(center) title("Severity of Sleep Apnea", size(small))) xsize(8) ysize(5)
graph export "Results and Figures/$S_DATE/Figure2.eps", as(eps) replace

//[ ] use in supplement. clean up labels etc. 
twoway kdensity MinBelow89 if Female==1 || ///
kdensity MinBelow89 if Female==0 ||, ///
legend(order(1 "Female" 2 "Male")) ///
xtitle("Minutes of sleep time with SpO2 <89%") ///
ytitle("Kernel Density") ///
title("Kernel-Density Plots of Minutes of Sleep Time with SpO2 <89% by Gender") ///
subtitle("Densities are group specific") scheme(white_tableau)

//sleepiness x ahi
graph twoway (lfitci AHI_n EpworthSleepinessScaleAtDiag) (scatter AHI_n EpworthSleepinessScaleAtDiag)

//sleepiness x spo2
graph twoway (lfitci PercBelow89 EpworthSleepinessScaleAtDiag) (scatter PercBelow89 EpworthSleepinessScaleAtDiag)

catplot excessive_sleepiness age_cat, over(Female) stack asyvars percent(age_cat Female) title("Proportion with excessive sleepiness, by gender and age") ytitle("Percentage (%)") graphregion(color(white)) bar(1, color(gs14)) bar(2, color(gs4)) legend(rows(2) size(small) symplacement(center) title("Severity of Sleep Apnea", size(small)))	scheme(white_tableau)

//TODO: add (n= to the labels) - easier to just do this in post
tab3way excessive_sleepiness Female wt_cat
catplot excessive_sleepiness wt_cat, over(Female) stack asyvars percent(wt_cat Female) title("Proportion with excessive sleepiness, by gender and weight") ytitle("Percentage (%)") graphregion(color(white)) bar(1, color(gs11)) bar(2, color(gs6)) legend(rows(1) pos(6) size(small) symplacement(center) title("Severity of Sleep Apnea", size(small))) blabel(total, position(center) gap(1) color(black)) xsize(8) ysize(5)
//no export because have to modify manually.

//IMPUTATION
*For 70% Usage
mi set mlong
mi register imputed Goals age_decade Female bmi_5 Smoking HSAT FlowAHI_per_10 AHI_per_10 desat_per_10 EpworthSleepinessScaleAtDiag
mi impute chained (regress) age_decade bmi_5 FlowAHI_per_10 AHI_per_10 desat_per_10 EpworthSleepinessScaleAtDiag (logit) Goals Female Smoking HSAT , add(10) rseed(900)

mi xeq 0 1 5: sum Goals age_decade Female bmi_5 Smoking HSAT FlowAHI_per_10 AHI_per_10 desat_per_10 EpworthSleepinessScaleAtDiag
mi estimate, or dots: logit Goals c.age_decade i.Female c.bmi_5 i.Smoking i.HSAT c.AHI_per_10 c.desat_per_10 c.FlowAHI_per_10 c.EpworthSleepinessScaleAtDiag, coeflegend

//mimrgns ,dydx(*)
//mimrgns , dydx(age_decade Female bmi_5 Smoking HSAT AHI_per_10 desat_per_10 FlowAHI_per_10 EpworthSleepinessScaleAtDiag) predict(pr)

estimates store mi_goals

//figure 3
coefplot mi_goals, drop(_cons) eform xscale(log) baselevels xtitle("Adjusted Odds Ratio of {&ge}70% Days Used for {&ge}4hrs", size(medsmall)) xlabel(0.125 0.25 0.5 1 2 4 8) xline(1) xscale(extend) yscale(extend)  ylabel(,labsize(medsmall)) ciopts(recast(rcap)) xsize(8) ysize(5)
graph export "Results and Figures/$S_DATE/Figure3.eps", as(eps) replace
//Note - improving the labels manually is the way to go

log close
