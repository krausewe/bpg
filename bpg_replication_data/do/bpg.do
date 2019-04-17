********************************************************************************
*** Replication Data for "Becoming part of the gang? Established and         ***
*** nonestablished populist parties and the role of external efficacy" 		 ***
***																			 ***
*** Party Politics, https://doi.org/10.1177/1354068819839210 				 ***
***																			 ***
*** Authors: Werner Krause & Aiko Wagner									 ***
*** WZB Berlin 																 ***
***																			 ***
*** werner.krause@wzb.eu													 ***
********************************************************************************

set more off
clear

******************************
*** Set path and load data ***

cd "SET YOUR PATH"
use "Data/bpg.dta"

************************
*** Prepare 2nd step ***

gen b_no_ext_eff = .
gen se_no_ext_eff = .
gen p_no_ext_eff = .


rename no_ext_eff f_no_ext_eff
label variable f_no_ext_eff "No External Efficacy"

************************************************
*** 1st step: Left-wing populists, table A.4 ***

foreach q in 8 10 15 17 21 24 25 30 32 36 {
reg ptvpop f_no_ext_eff no_int_eff ///
i.econ_worse_prosp i.econ_worse_comp_to_past ///
i.eu_mmb_bad dontfav_more_power_eu ///
pro_red_abs pro_taxes_abs ///
edu unemployed age female [aw=w23pol] if iso2c_pop==`q'

replace b_no_ext_eff=_b[f_no_ext_eff] in `q'
replace se_no_ext_eff=_se[f_no_ext_eff] in `q'
local t = _b[f_no_ext_eff]/_se[f_no_ext_eff]
replace p_no_ext_eff=2*ttail(e(df_r),abs(`t')) in `q'
}

*************************************************
*** 1st step: Right-wing populists, table A.5 ***

foreach q in 1 2 3 4 5 6 7 9 11 12 13 14 16 18 19 20 22 23 26 27 28 29 31 33 34 35 {
reg ptvpop f_no_ext_eff no_int_eff ///
i.econ_worse_prosp i.econ_worse_comp_to_past ///
i.eu_mmb_bad dontfav_more_power_eu ///
ag_marri_abs ag_immi_abs ///
edu unemployed age female [aw=w23pol] if iso2c_pop==`q'

replace b_no_ext_eff=_b[f_no_ext_eff] in `q'
replace se_no_ext_eff=_se[f_no_ext_eff] in `q'
local t = _b[f_no_ext_eff]/_se[f_no_ext_eff]
replace p_no_ext_eff=2*ttail(e(df_r),abs(`t')) in `q'
}

****************************************************
*** Prepare variables for 2nd step: party family ***

gen pfam_m=1 in 1/36
replace pfam_m=2 in 8
replace pfam_m=2 in 10 
replace pfam_m=2 in 15 
replace pfam_m=2 in 17 
replace pfam_m=2 in 21 
replace pfam_m=2 in 24 
replace pfam_m=2 in 25 
replace pfam_m=2 in 30
replace pfam_m=2 in 32
replace pfam_m=2 in 36
label define pfam_m2 1 "Right-Wing Populist" 2 "Left-Wing Populist"
label value pfam_m pfam_m2
label variable pfam_m "Left vs. Right"

************************************************
*** Save macro dataset *************************

gen m_number=_n in 1/36
preserve
keep pfam_m m_number b_* se_* p_*
keep in 1/36
save "data/tmp_macro.dta", replace
restore

****************************************************
*** Collapse micro dataset *************************
*save "Data/after_1ststep.dta", replace

collapse p_age_rel party_size abs_pos_lrgen_fm gov_ever2 gov_ever east iso2c, by(iso2c_pop)
rename iso2c_pop m_number 

***********************************************************
*** Merge micro and macro dataset *************************

merge 1:1 m_number using "data/tmp_macro.dta"
drop _merge

***********************************************************
*** 2nd level variables ***********************************

*** Log of party age
gen log_party_age_relevant = log( p_age_rel )
recode log_party_age_relevant ( . = 0 ) // replace missing log-values to 0 (equals 1 year existence)

*** Establishment measure w supporters
pca gov_ever log_party_age_relevant party_size, components(1)
cap drop f_established
predict f_established
label variable f_established "Establishment"

*** Establishment measure wo supporters
pca gov_ever2 log_party_age_relevant party_size, components(1)
cap drop f_established2
predict f_established2
label variable f_established2 "Establishment w/o support"

*** Establishment measure wo party age
pca gov_ever party_size, components(1)
cap drop f_established3
predict f_established3
label variable f_established3 "Establishment w/o party age"

*** Establishment measure wo party size
pca gov_ever log_party_age_relevant, components(1)
cap drop f_established4
predict f_established4
label variable f_established4 "Establishment w/o party size"

*** Establishment measure wo government participation
pca log_party_age_relevant party_size, components(1)
cap drop f_established5
predict f_established5
label variable f_established5 "Establishment w/o govt participation"

*** Number of populist parties
bys iso2c: gen nb_pop_parties=_N
recode nb_pop_parties (1 = 0 ) ( 3 2 = 1 )
label variable nb_pop_parties "Multiple Populist Parties"

*** weights: mean-centered reciprocal value of standard errors from 1st step regression
gen wei_2nd_step=1/se_no_ext_eff
sum wei_2nd_step
replace wei_2nd_step=wei_2nd_step/r(mean)

save "data/tmp_2ndstep.dta", replace

*** Figure 1
sort f_established 
gen seq = _n 
graph bar (asis) f_established, over( seq ) 

*** Figure 2
sort b_no_ext_eff 
replace seq = _n 
gen lb = b_no_ext_eff - se_no_ext_eff * 1.96
gen ub = b_no_ext_eff + se_no_ext_eff * 1.96
tw scatter  b_no_ext_eff seq || rcap lb ub seq, ///
 ytitle("Coefficient of No External Efficacy") ///
 xtitle("Populist Party") legend(off) xla(, notick nolabel)

*** Figure 3
tw (lfitci b_no_ext_eff f_established) (scatter b_no_ext_eff f_established), ///
 ytitle("Coefficient of No External Efficacy") ///
 xtitle("Establishment") legend(off)


***********************************************************
*** 2nd step **********************************************

*** Table 1
*** Model 1
reg b_no_ext_eff f_established east nb_pop_parties, vce(cluster iso2c)

*** Figure 4 - Left panel
margins, at(f_established= (-2 (1) 3)) vsquish
marginsplot,  level(90)  ///
 recast(line) recastci(rarea) ///
 ytitle("Effect of No External Efficacy") xtitle("Measure of Populist Party Establishment")

*** Model 2
reg b_no_ext_eff f_established east nb_pop_parties
cap drop cd
predict cd, c
list b_no_ext_eff f_established east nb_pop_parties m_num cd if cd>4/36
reg b_no_ext_eff f_established east nb_pop_parties if cd<=4/36, vce(cluster iso2c)

*** Figure 4 - right panel
margins, at(f_established= (-2 (1) 3)) vsquish
marginsplot,  level(90)  ///
 recast(line) recastci(rarea) ///
 ytitle("Effect of No External Efficacy") xtitle("Measure of Populist Party Establishment")

 
*** Table 2
*** Model 1
reg b_no_ext_eff f_established2 east nb_pop_parties, vce(cluster iso2c)

*** Model 2
reg b_no_ext_eff f_established3 east nb_pop_parties, vce(cluster iso2c)

*** Model 3
reg b_no_ext_eff f_established4 east nb_pop_parties, vce(cluster iso2c)

*** Model 4
reg b_no_ext_eff f_established5 east nb_pop_parties, vce(cluster iso2c)

*** Model 5
reg b_no_ext_eff f_established east nb_pop_parties i.pfam_m, vce(cluster iso2c)

*** Model 6
reg b_no_ext_eff f_established east nb_pop_parties abs_pos_lrgen_fm, vce(cluster iso2c)

* See line 236ff. for model 7
* See line 312ff. for model 8

*** Table A.2
*** Model 1
edvreg b_no_ext_eff f_established east nb_pop_parties, dvste(se_no_ext_eff)
	
*** Model 2
reg b_no_ext_eff f_established east nb_pop_parties

*** Model 3
reg b_no_ext_eff f_established east nb_pop_parties, vce(cluster iso2c), [w=wei_2nd_step]
 

*** Table 1, Model 7
use "Data/bpg.dta", clear

gen b_no_ext_eff=.
gen se_no_ext_eff=.
gen p_no_ext_eff=.

foreach q in 8 10 15 17 21 24 25 30 32 36 {
reg ptvpop no_ext_eff no_int_eff ///
i.econ_worse_prosp i.econ_worse_comp_to_past ///
i.eu_mmb_bad dontfav_more_power_eu ///
pro_red_abs pro_taxes_abs ///
edu unemployed age female pid [aw=w23pol] if iso2c_pop==`q'

replace b_no_ext_eff=_b[no_ext_eff] in `q'
replace se_no_ext_eff=_se[no_ext_eff] in `q'
local t = _b[no_ext_eff]/_se[no_ext_eff]
replace p_no_ext_eff=2*ttail(e(df_r),abs(`t')) in `q'
}

foreach q in 1 2 3 4 5 6 7 9 11 12 13 14 16 18 19 20 22 23 26 27 28 29 31 33 34 35 {
reg ptvpop no_ext_eff no_int_eff ///
i.econ_worse_prosp i.econ_worse_comp_to_past ///
i.eu_mmb_bad dontfav_more_power_eu ///
ag_marri_abs ag_immi_abs ///
edu unemployed age female pid [aw=w23pol] if iso2c_pop==`q'

replace b_no_ext_eff=_b[no_ext_eff] in `q'
replace se_no_ext_eff=_se[no_ext_eff] in `q'
local t = _b[no_ext_eff]/_se[no_ext_eff]
replace p_no_ext_eff=2*ttail(e(df_r),abs(`t')) in `q'
}

gen pfam_m=1 in 1/36
replace pfam_m=2 in 8
replace pfam_m=2 in 10 
replace pfam_m=2 in 15 
replace pfam_m=2 in 17 
replace pfam_m=2 in 21 
replace pfam_m=2 in 24 
replace pfam_m=2 in 25 
replace pfam_m=2 in 30
replace pfam_m=2 in 32
replace pfam_m=2 in 36
label define pfam_m2 1 "Right-Wing Populist" 2 "Left-Wing Populist"
label value pfam_m pfam_m2
label variable pfam_m "Left vs. Right"

gen m_number=_n in 1/36
preserve
keep pfam_m m_number b_* se_* p_*
keep in 1/36
save "data/tmp_macro.dta", replace
restore

collapse p_age_rel party_size abs_pos_lrgen_fm gov_ever2 gov_ever east iso2c, by(iso2c_pop)

rename iso2c_pop m_number 

merge 1:1 m_number using "data/tmp_macro.dta"
drop _merge

gen log_party_age_relevant = log( p_age_rel )
recode log_party_age_relevant ( . = 0 ) // missings durch 0 Jahre auf 0 setzen (nun dasselbe wie 1 Jahr)

pca gov_ever log_party_age_relevant party_size, components(1)
cap drop f_established
predict f_established
label variable f_established "Establishment"

bys iso2c: gen nb_pop_parties=_N
recode nb_pop_parties ( 1 = 0 ) ( 3 2 = 1 )
label variable nb_pop_parties "Multiple Populist Parties"

reg b_no_ext_eff f_established east nb_pop_parties, vce(cluster iso2c)

*** Table 1, Model 8
use "data/bpg.dta", clear

gen b_no_ext_eff = .
gen se_no_ext_eff = .
gen p_no_ext_eff = .

egen f_no_ext_eff = rowmean(dont_trst_prl no_ext_eff) 
label variable f_no_ext_eff "No External Efficacy/Trust"


foreach q in 8 10 15 17 21 24 25 30 32 36 {
reg ptvpop f_no_ext_eff no_int_eff ///
i.econ_worse_prosp i.econ_worse_comp_to_past ///
i.eu_mmb_bad dontfav_more_power_eu ///
pro_red_abs pro_taxes_abs ///
edu unemployed age female [aw=w23pol] if iso2c_pop==`q'

replace b_no_ext_eff=_b[f_no_ext_eff] in `q'
replace se_no_ext_eff=_se[f_no_ext_eff] in `q'
local t = _b[f_no_ext_eff]/_se[f_no_ext_eff]
replace p_no_ext_eff=2*ttail(e(df_r),abs(`t')) in `q'
}

foreach q in 1 2 3 4 5 6 7 9 11 12 13 14 16 18 19 20 22 23 26 27 28 29 31 33 34 35 {
reg ptvpop f_no_ext_eff no_int_eff ///
i.econ_worse_prosp i.econ_worse_comp_to_past ///
i.eu_mmb_bad dontfav_more_power_eu ///
ag_marri_abs ag_immi_abs ///
edu unemployed age female [aw=w23pol] if iso2c_pop==`q'

replace b_no_ext_eff=_b[f_no_ext_eff] in `q'
replace se_no_ext_eff=_se[f_no_ext_eff] in `q'
local t = _b[f_no_ext_eff]/_se[f_no_ext_eff]
replace p_no_ext_eff=2*ttail(e(df_r),abs(`t')) in `q'
}

gen pfam_m=1 in 1/36
replace pfam_m=2 in 8
replace pfam_m=2 in 10 
replace pfam_m=2 in 15 
replace pfam_m=2 in 17 
replace pfam_m=2 in 21 
replace pfam_m=2 in 24 
replace pfam_m=2 in 25 
replace pfam_m=2 in 30
replace pfam_m=2 in 32
replace pfam_m=2 in 36
label define pfam_m2 1 "Right-Wing Populist" 2 "Left-Wing Populist"
label value pfam_m pfam_m2
label variable pfam_m "Left vs. Right"

gen m_number=_n in 1/36
preserve
keep pfam_m m_number b_* se_* p_*
keep in 1/36
save "data/tmp_macro.dta", replace
restore

collapse p_age_rel party_size abs_pos_lrgen_fm gov_ever2 gov_ever east iso2c, by(iso2c_pop)

rename iso2c_pop m_number 

merge 1:1 m_number using "data/tmp_macro.dta"
drop _merge

gen log_party_age_relevant = log( p_age_rel )
recode log_party_age_relevant ( . = 0 )

pca gov_ever log_party_age_relevant party_size, components(1)
cap drop f_established
predict f_established
label variable f_established "Establishment"

bys iso2c: gen nb_pop_parties=_N
recode nb_pop_parties (1 = 0 ) ( 3 2 = 1 )
label variable nb_pop_parties "Multiple Populist Parties"

reg b_no_ext_eff f_established east nb_pop_parties, vce(cluster iso2c)

*** Table A.3, Model 1 + Figure A.1 a)
use "data/bpg.dta" , clear
rename iso2c_pop m_number
merge m:1 m_number using "data/tmp_2ndstep.dta"

xtmixed ptvpop no_int_eff ib2.econ_worse_prosp ib2.econ_worse_comp_to_past ///
 ib2.eu_mmb_bad dontfav_more_power_eu pro_red_abs pro_taxes_abs ///
 ag_marri_abs ag_immi_abs i.edu unemployed age female ///
 c.no_ext_eff##c.f_established ///
 c.no_ext_eff##i.east ///
 c.no_ext_eff##i.nb_pop_parties ///
 || m_number: c.no_ext_eff , variance covariance(unstructured)

margins, dydx(no_ext_eff) at(f_established= (-2 (1) 3)) vsquish
marginsplot,  level(90)  ///
 recast(line) recastci(rarea) ///
 ytitle("Effect of No External Efficacy") xtitle("Measure of Populist Party Establishment")
 
*** Table A.3, Model 2 + Figure A.1 b)
use "data/bpg.dta" , clear
rename iso2c_pop m_number
merge m:1 m_number using "data/tmp_2ndstep.dta"

xtmelogit vc_prosp_pop no_int_eff ib2.econ_worse_prosp ///
 ib2.econ_worse_comp_to_past ib2.eu_mmb_bad dontfav_more_power_eu ///
 pro_red_abs pro_taxes_abs ag_marri_abs ag_immi_abs i.edu unemployed age ///
 female ///
 c.no_ext_eff##c.f_established ///
 c.no_ext_eff##i.east ///
 c.no_ext_eff##i.nb_pop_parties ///
 || m_number: c.no_ext_eff , variance covariance(unstructured)

margins, dydx(no_ext_eff) at(f_established= (-2 (1) 3)) vsquish
marginsplot,  level(90)  ///
 recast(line) recastci(rarea) ///
 ytitle("Effect of No External Efficacy") xtitle("Measure of Populist Party Establishment")
 
