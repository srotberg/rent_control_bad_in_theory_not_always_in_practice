* -----------------------------------------------------------------------------
* Set datadir to the folder that holds this script's input microdata file; the
* Stata log is written there too. "." means the current working directory.
global datadir "."
* -----------------------------------------------------------------------------

clear 
set more off

log using "$datadir/log_prop_renter_control", replace

use "$datadir/Census_2016.dta"

bysort HHINC: drop if HHINC<=0




* Set survey data structure to compute weighted results using household identifier: HMAIN.



svyset FRAME_ID [pw=COMPW1]




**************Household Income Distribution for Table 4*************
*-------------------------------------------------------------------------------	 
	 
*Create a quintile variable

xtile QUINT_GINC = HHINC, nq(5)

* Create a categorical variable to identify four types of households:
*1: Renters living in controlled units; 
*2: Renters living in uncontrolled units; 


gen HH_TYPE = 1 if TENUR == 4 & BUILT <= 6 
replace HH_TYPE = 2 if 	 TENUR == 4 & BUILT >= 7
replace HH_TYPE =3 if TENUR == 3 & PRESMORTG == 1
replace HH_TYPE = 4 if TENUR == 3 & PRESMORTG == 0
	 
	 
* Unweighted 
forvalues i = 1/5 {
	 display `i'
	 tab HH_TYPE if HMAIN ==1  & QUINT_GINC == `i'
	 }

	 
* Weighted 

forvalues i = 1/5 {
	 display `i'
	 svy: tab HH_TYPE if HMAIN ==1  & QUINT_GINC == `i'
	 }


log close 
