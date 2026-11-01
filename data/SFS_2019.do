* -----------------------------------------------------------------------------
* Set datadir to the folder that holds this script's input microdata file; the
* Stata log is written there too. "." means the current working directory.
global datadir "."
* -----------------------------------------------------------------------------

clear
set more off

log using "$datadir/log_SFS_2019", replace

use "$datadir/SFS_2019.dta"

*-----------------------------------------------------------------*;
*--------Loan to Value by Age of Family Major Income Earner-------*;
*-----------------------------------------------------------------*;
keep if ppvres == 35

svyset pefamid [pw=pweight]

* Create a loan to value variable for homeowners with a mortgage

gen LTV = 0 if pftenur!= "Do not own"
replace LTV=pwdprmor/pwaprval if pftenur== "Own with mortgage"

* LTV by age group: 
*Unweighted

forvalues i = 1/14 {
   display `i'
   mean LTV if pftenur!= "Do not own" & pagemieg == `i'
   }
   
   
*Weighted

forvalues i = 1/14 {
   display `i'
   svy: mean LTV if pftenur!= "Do not own" & pagemieg == `i'
   }

*-----------------------------------------------------------------*;
*--------Total Mortgage Debt and Total House Value-------*;
*-----------------------------------------------------------------*;
   
* 1) Total Mortgage Debt
* Unwighted
total pwdprmor if pftenur== "Own with mortgage"
* Weighted
svy: total pwdprmor if pftenur== "Own with mortgage"
   
* 2) Total House Value of Homeowners with a Mortgage
* Unwighted
total pwaprval if pftenur== "Own with mortgage"
* Weighted
svy: total pwaprval if pftenur== "Own with mortgage"
   
* 3) Total House Value of Homeowners without a Mortgage
* Unwighted
total pwaprval if pftenur== "Own without mortgage"
* Weighted
svy: total pwaprval if pftenur== "Own without mortgage"

* 4) Total House Value of Homeowners 
* Unwighted
total pwaprval if pftenur!= "Do not own"
* Weighted
svy: total pwaprval if pftenur!= "Do not own"

*5) Average House Value of Homeowners

* Unwighted
mean pwaprval if pftenur!= "Do not own"
* Weighted
svy: mean pwaprval if pftenur!= "Do not own"





*-----------------------------------------------------------------*;
*--------Net Wealth Distribution-------*;
*-----------------------------------------------------------------*;

* Estimate net wealth share
*Unweighted

pshare estimate pwnetwpg, nq(100)

*Weighted

pshare estimate pwnetwpg [pw=pweight], nq(100)

* Estimate Gini coefficient

*Unweighted
ineqdec0 pwnetwpg 
*Weighted
ineqdec0 pwnetwpg [pw=pweight] 
   
*-----------------------------------------------------------------*;
*--------Net Wealth Bequest------- -------------------------------*;
*-----------------------------------------------------------------*;

*Unweighted

total pwnetwpg 

*Weighted

svy: total pwnetwpg

* Total Net Wealth by age group: 
*Unweighted

forvalues i = 1/14 {
   display `i'
   total pwnetwpg if  pagemieg == `i'
   }
   
   
*Weighted

forvalues i = 1/14 {
   display `i'
   svy: total pwnetwpg if  pagemieg == `i'
   }
   
**************************************************************** 
*Proportion of homeowners with a mortgage

drop if pftenur== "Do not own"

gen MORGT=(pftenur=="Own with mortgage")

*Unweighted
tab MORGT

*Weighted
svy: tab MORGT
   
log close
