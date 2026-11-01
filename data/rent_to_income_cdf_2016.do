* -----------------------------------------------------------------------------
* Set datadir to the folder that holds this script's input microdata file; the
* Stata log is written there too. "." means the current working directory.
global datadir "."
* -----------------------------------------------------------------------------
clear 
set more off

log using "$datadir/log_Rent_Income", replace

use "$datadir/Census_2016.dta"

bysort HHINC: drop if HHINC<=0




* Set survey data structure to compute weighted results using household identifier: HMAIN.



svyset FRAME_ID [pw=COMPW1]

bysort TENUR: drop if TENUR != 4

* Create a variable for rent to income ratio

gen RENT = 12*GROSRT
gen RINC = RENT/HHINC

drop if RINC>1

* Total Rent and Total Income

*Total Rents
*Unweighted
total RENT if HMAIN ==1

*Weighted
svy: total RENT if HMAIN ==1

*Total Household Income 
*Unweighted
total HHINC if HMAIN ==1

*Weighted
svy: total HHINC if HMAIN ==1

*Unweighted CDF of RINC

mat rinc_x= J(101,1,0)
forvalues i = 1(1)101{
	 gen RINC_dummy_`i' = (HMAIN ==1 & RINC>= (`i'-1)/100)
	 dis `i'
	tab RINC_dummy_`i' if HMAIN ==1, matcell(x)
	matrix list x
	mata: st_matrix("xx", st_matrix("x")/colsum(rowsum(st_matrix("x"))))
	matrix list xx
	mat rinc_x[`i',1]=xx[2,1]
	 }

forvalues i = 1(1)101{
	 
	 dis rinc_x[`i',1]
	
	 }

	 
 *Weighted CDF of RINC

mat rinc_weighted_x= J(101,1,0)
forvalues i = 1(1)101{
	 gen RINC_weighted_dummy_`i' = (HMAIN ==1 & RINC>= (`i'-1)/100)
	 dis `i'
	svy: tab RINC_weighted_dummy_`i' if HMAIN ==1
	matrix list e(b)
	mat rinc_weighted_x[`i',1]=e(b)[1,2]
	 }

forvalues i = 1(1)101{
	 
	 dis rinc_weighted_x[`i',1]
	
	 }


log close