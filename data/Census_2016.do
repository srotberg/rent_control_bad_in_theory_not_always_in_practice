* -----------------------------------------------------------------------------
* Set datadir to the folder that holds this script's input microdata file; the
* Stata log is written there too. "." means the current working directory.
global datadir "."
* -----------------------------------------------------------------------------
clear 
set more off

log using "$datadir/log_census_2016", replace

* Census of Population 2016 for Toronto CMA


use "$datadir/Census_2016.dta"

*-------------------------------------------------------------------------------*
*---------------PREPARE DATASET FOR EMPIRICAL ANALYSIS--------------------------*
*-------------------------------------------------------------------------------*

* hhttinc: household before-tax income, drop negative or null income
bysort HHINC: drop if HHINC<=0




* Set survey data structure to compute weighted results using household identifier: HMAIN.



svyset FRAME_ID [pw=COMPW1]

*************** Proportion of 26-34 living with at least a parent****************
*-------------------------------------------------------------------------------
drop if AGE<26 | AGE>34
gen CHILD_MP=0
replace CHILD_MP = 1 if (CFSTATSIMPLE==4 | CFSTATSIMPLE==5) & AGE>=26 & AGE<=34 & LFTag!= 18 & LFTag!=19 & LFTag!=20 & LFTag!=21

tab CHILD_MP
svy: tab CHILD_MP



clear 
set more off

log using "$datadir/log_census_2016", replace

bysort HHINC: drop if HHINC<=0




* Set survey data structure to compute weighted results using household identifier: HMAIN.



svyset FRAME_ID [pw=COMPW1]

*************** Homeownership Rate**********************************************
*-------------------------------------------------------------------------------
*1) Overall homeownership rate
*Unweighted
tab TENUR if HMAIN==1
*Weighted
svy: tab TENUR if HMAIN==1

*2) Homeownership rate for age groups from 26 to 85

*Unweighted

forvalues t = 26/85 {
	 display `t'
	 tab TENUR if HMAIN==1 & AGE == `t'
	 }
	 
*Weighted

forvalues t = 26/85 {
	 display `t'
	 svy: tab TENUR if HMAIN==1 & AGE == `t'
	 }

* 3) Homeownership rate for age 65 and over

*Unweighted
tab TENUR if HMAIN==1 & AGE>=65 & AGE<=85
*Weighted
svy: tab TENUR if HMAIN==1 & AGE>=65 & AGE<=85

**************Average Household Size by Age*************************************
*-------------------------------------------------------------------------------

*Unweighted
mean NUNITS if HMAIN ==1

*Weighted
svy: mean NUNITS if HMAIN ==1


*Unweighted

forvalues t = 26/85 {
	 display `t'
	 mean NUNITS if HMAIN==1 & AGE == `t'
	 }
	 
*Weighted

forvalues t = 26/85 {
	 display `t'
	 svy: mean NUNITS if HMAIN==1 & AGE == `t'
	 }




**************Household Income by Age*******************************************
*-------------------------------------------------------------------------------

*Unweighted

forvalues t = 26/85 {
	 display `t'
	 mean HHINC if HMAIN==1 & AGE == `t'
	 }
	 
*Weighted

forvalues t = 26/85 {
	 display `t'
	 svy: mean HHINC if HMAIN==1 & AGE == `t'
	 }


**************Estimating Progressivity of Income Taxe***************************
*-------------------------------------------------------------------------------	 
	 
gen HHINC_LN = ln(HHINC)
gen HHINC_AT_LN = ln(HHINC_AT)

* Unweighted

reg HHINC_AT_LN HHINC_LN if HMAIN ==1

* Weighted

svy: reg HHINC_AT_LN HHINC_LN if HMAIN ==1
	 

**************Household Income Distribution for Table 4*************
*-------------------------------------------------------------------------------	 
	 
*Create a quintile variable

xtile QUINT_GINC = HHINC, nq(5)

* Create a categorical variable to identify four types of households:
*1: Renters living in controlled units; 
*2: Renters living in uncontrolled units; 
*3: Homeowners with a mortgage; 
*4: Homeowners without a mortgage. 

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


**************Compute Gini Coefficeint of Household (Employment) Income **********
*-------------------------------------------------------------------------------

* Create household employment income

bys FRAME_ID: egen HHEMPIN = total(EMPIN)
ineqdeco HHEMPIN [pw=COMPW1] if HMAIN==1

* Compute Gini coefficient of household income 

ineqdeco HHINC [pw=COMPW1] if HMAIN==1



* Compute Standard Deviation of log household employment income
drop if HHEMPIN<=0
gen HHEMPIN_LN=ln(HHEMPIN)

summarize HHEMPIN_LN if HMAIN==1

svy: mean HHEMPIN_LN if HMAIN==1

estat sd



**************Total Household Income and Tax ***********************************
*-------------------------------------------------------------------------------

*Total Household Income 
*Unweighted
total HHINC if HMAIN ==1

*Weighted
svy: total HHINC if HMAIN ==1

*Total Household Income Tax

gen HHINC_TAX = HHINC - HHINC_AT

*Unweighted
total HHINC_TAX if HMAIN ==1

*Weighted
svy: total HHINC_TAX if HMAIN ==1


	 
**************Household Income by Controlled vs Uncontrolled Market*************
*-------------------------------------------------------------------------------

* 1) Porportions of renters living in controlled units: TENUR==4 & BUILT<=6 or 1990; uncontrolled units TENUR==4 & BUILT>=7 or 1991

bysort TENUR: drop if TENUR != 4

gen CONTR = (TENUR == 4 & BUILT <= 6)

* Unweighted
tab CONTR if HMAIN == 1

*Weighted

svy: tab CONTR if HMAIN == 1

* 2) Quintile distribution of household income by CONTR

*generate a quintile variable

xtile QUINT = HHINC, nq(5)

* Unweighted for controlled units

forvalues i = 1/5 {
	 display `i'
	 mean HHINC if HMAIN ==1 & CONTR ==1 & QUINT == `i'
	 }

* Unweighted for uncontrolled units
forvalues i = 1/5 {
	 display `i'
	 mean HHINC if HMAIN ==1 & CONTR ==0 & QUINT == `i'
	 }
	 
* Weighted for controlled units

forvalues i = 1/5 {
	 display `i'
	 svy: mean HHINC if HMAIN ==1 & CONTR ==1 & QUINT == `i'
	 }

* Weighted for uncontrolled units
forvalues i = 1/5 {
	 display `i'
	 svy: mean HHINC if HMAIN ==1 & CONTR ==0 & QUINT == `i'
	 }
	 
*3) Average income by CONTR

* Unweighted for controlled units
mean HHINC if HMAIN ==1 & CONTR == 1
* Weighted for controlled units
svy: mean HHINC if HMAIN ==1 & CONTR == 1

* Unweighted for uncontrolled units
mean HHINC if HMAIN ==1 & CONTR == 0
* Weighted for uncontrolled units
svy: mean HHINC if HMAIN ==1 & CONTR == 0

**************Tenancy Duration by Controlled vs. Uncontrolled*******************
*-------------------------------------------------------------------------------
* Mobility a year ago using Mob1

* Unweighted for controlled units
tab Mob1 if HMAIN ==1 & CONTR == 1
* Weighted for controlled units
svy: tab Mob1 if HMAIN ==1 & CONTR == 1

* Unweighted for uncontrolled units
tab Mob1 if HMAIN ==1 & CONTR == 0
* Weighted for uncontrolled units
svy: tab Mob1  if HMAIN ==1 & CONTR == 0

* Mobility 5 years ago using Mob5

* Unweighted for controlled units
tab Mob5 if HMAIN ==1 & CONTR == 1
* Weighted for controlled units
svy: tab Mob5 if HMAIN ==1 & CONTR == 1

* Unweighted for uncontrolled units
tab Mob5 if HMAIN ==1 & CONTR == 0
* Weighted for uncontrolled units
svy: tab Mob5  if HMAIN ==1 & CONTR == 0

* Mobility between one and five years

gen Mob1_5 = (Mob1==6 & Mob5!=6)

* Unweighted for controlled units
tab Mob1_5 if HMAIN ==1 & CONTR == 1
* Weighted for controlled units
svy: tab Mob1_5 if HMAIN ==1 & CONTR == 1

* Unweighted for uncontrolled units
tab Mob1_5 if HMAIN ==1 & CONTR == 0
* Weighted for uncontrolled units
svy: tab Mob1_5  if HMAIN ==1 & CONTR == 0


**************Age by Controlled vs Uncontrolled Market*************
*-------------------------------------------------------------------------------

*Unweighted

forvalues t = 26/85 {
	 display `t'
	 tab CONTR if HMAIN ==1 & AGE == `t'
	 }
	 
*Weighted

forvalues t = 26/85 {
	 display `t'
	 svy: tab CONTR if HMAIN ==1 & AGE == `t'
	 }


**************Rent by Controlled vs Uncontrolled Market*************************
*-------------------------------------------------------------------------------

* Create a variable for annual rents
gen RENT = 12*GROSRT

*1) Average rent by CONTR

* Unweighted for controlled units
mean RENT if HMAIN ==1 & CONTR == 1 
* Weighted for controlled units
svy: mean RENT  if HMAIN ==1 & CONTR == 1

* Unweighted for uncontrolled units
mean RENT  if HMAIN ==1 & CONTR == 0
* Weighted for uncontrolled units
svy: mean RENT  if HMAIN ==1 & CONTR == 0

*2) Average rent by CONTR and bedroom

*Regroup the number of bedrooms over 4 to 4
replace BEDRM = 4 if BEDRM>=5

* Unweighted for controlled units

forvalues i = 0/4 {
	 display `i'
	 mean RENT if HMAIN ==1 & CONTR ==1 & BEDRM == `i'
	 }

* Unweighted for uncontrolled units
forvalues i = 0/4 {
	 display `i'
	 mean RENT if HMAIN ==1 & CONTR ==0 & BEDRM == `i'
	 }
	 
* Weighted for controlled units

forvalues i = 0/4 {
	 display `i'
	 svy: mean  RENT if HMAIN ==1 & CONTR ==1 & BEDRM == `i'
	 }

* Weighted for uncontrolled units
forvalues i = 0/4 {
	 display `i'
	 svy: mean  RENT if HMAIN ==1 & CONTR ==0 & BEDRM == `i'
	 }

*3) Fraction of renters by BEDRM and CONTR

*Unweighted for controlled units
tab BEDRM if HMAIN ==1 & CONTR == 1
* Weighted for controlled units
svy: tab BEDRM if HMAIN ==1 & CONTR == 1
*Unweighted for uncontrolled units
tab BEDRM if HMAIN ==1 & CONTR == 0
* Weighted for uncontrolled units
svy: tab BEDRM if HMAIN ==1 & CONTR == 0



**************Rent by Recent Movers in by CONTR************************
*-------------------------------------------------------------------------------


*1) Average rent of recent movers by CONTR

* Unweighted for controlled units
mean RENT if HMAIN ==1 & Mob1!=6 & CONTR == 1 
* Weighted for controlled units
svy: mean RENT  if HMAIN ==1 & Mob1!=6 & CONTR == 1

* Unweighted for uncontrolled units
mean RENT  if HMAIN ==1 & Mob1!=6 & CONTR == 0
* Weighted for uncontrolled units
svy: mean RENT  if HMAIN ==1 & Mob1!=6 &  CONTR == 0

**************Estimate  the correlation between rent and income for recent movers in controlled market************************
*-------------------------------------------------------------------------------

*gen RENT_LN = ln(RENT)
*gen HHINC_LN = ln(HHINC)
*gen GCOND=(RPAIR==1)
*gen RENO =(RPAIR==4)

*reg RENT_LN HHINC_LN AGE BEDRM GCOND RENO if HMAIN ==1 & Mob1!=6 & CONTR == 1 

*svy: reg RENT_LN HHINC_LN AGE BEDRM GCOND RENO if HMAIN ==1 & Mob1!=6 & CONTR == 1 
*2) Average rent by CONTR and bedroom


* Unweighted for controlled units

forvalues i = 0/4 {
	 display `i'
	 mean RENT if HMAIN ==1 & Mob1!=6 &  CONTR ==1 & BEDRM == `i'
	 }

* Unweighted for uncontrolled units
forvalues i = 0/4 {
	 display `i'
	 mean RENT if HMAIN ==1 & Mob1!=6 &  CONTR ==0 & BEDRM == `i'
	 }
	 
* Weighted for controlled units

forvalues i = 0/4 {
	 display `i'
	 svy: mean  RENT if HMAIN ==1 & Mob1!=6 & CONTR ==1 & BEDRM == `i'
	 }

* Weighted for uncontrolled units
forvalues i = 0/4 {
	 display `i'
	 svy: mean  RENT if HMAIN ==1 & Mob1!=6 & CONTR ==0 & BEDRM == `i'
	 }

*3) Fraction of renters by BEDRM and CONTR

*Unweighted for controlled units
tab BEDRM if HMAIN ==1 & Mob1!=6 & CONTR == 1
* Weighted for controlled units
svy: tab BEDRM if HMAIN ==1 & Mob1!=6 &  CONTR == 1
*Unweighted for uncontrolled units
tab BEDRM if HMAIN ==1 & Mob1!=6 & CONTR == 0
* Weighted for uncontrolled units
svy: tab BEDRM if HMAIN ==1 & Mob1!=6 & CONTR == 0




**************Rent to Income Ratio by Controlled vs Uncontrolled Market*********
*-------------------------------------------------------------------------------




* Create a variable for rent to income ratio

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


*CDF of RINC

forvalues i = 0.00(0.01)1{
	 display `i'
	count if HMAIN ==1 & RINC>= `i'
	 }


**************Income Distribution of Homeowners with or without mortgage *******
*-------------------------------------------------------------------------------

clear 
set more off


use "$datadir/Census_2016.dta"

* Drop negative or null income

bysort HHINC: drop if HHINC<=0



* Set survey data structure to compute weighted results using household identifier: HMAIN.

svyset FRAME_ID [pw=COMPW1]

* Drop non-homeowners to create a dummy for bearing a mortgage or not

bysort TENUR: drop if TENUR != 3
bysort PRESMORTG: drop if PRESMORTG == -3

*1) Proportion of homeowners with and without a mortage, respectively
*Unweighted 
tab PRESMORTG if HMAIN ==1 
*Weighted
svy: tab PRESMORTG  if HMAIN ==1 
*2) Income disbribution of homeowners by PRESMORTG

*generate a quintile variable

xtile QUINT_INC = HHINC, nq(5)

* Unweighted for homeowners with a mortgage

forvalues i = 1/5 {
	 display `i'
	 mean HHINC if HMAIN ==1 & PRESMORTG ==1 & QUINT_INC == `i'
	 }

* Unweighted for homeowners without a mortgage
forvalues i = 1/5 {
	 display `i'
	 mean HHINC if HMAIN ==1 & PRESMORTG ==0 & QUINT_INC == `i'
	 }
	 
* Weighted for homeowners with a mortgage

forvalues i = 1/5 {
	 display `i'
	 svy: mean HHINC if HMAIN ==1 & PRESMORTG ==1 & QUINT_INC == `i'
	 }

* Weighted for homeowners without a mortgage
forvalues i = 1/5 {
	 display `i'
	 svy: mean HHINC if HMAIN ==1 & PRESMORTG ==0 & QUINT_INC == `i'
	 }
	
	
	
*3) Average income by PRESMORTG

* Unweighted for homeowners with a mortgage
mean HHINC if HMAIN ==1 & PRESMORTG == 1
* Weighted for homeowners with a mortgage
svy: mean HHINC if HMAIN ==1 & PRESMORTG == 1

* Unweighted for homeowners without a mortgage
mean HHINC if HMAIN ==1 & PRESMORTG == 0
* Weighted for homeowners without a mortgage
svy: mean HHINC if HMAIN ==1 & PRESMORTG == 0


**************Shelter Cost to Income Ratio*************************************
*-------------------------------------------------------------------------------
clear 
set more off
use "$datadir/Census_2016.dta"

* Drop negative or null income

bysort HHINC: drop if HHINC<=0



* Set survey data structure to compute weighted results using household identifier: HMAIN.

svyset FRAME_ID [pw=COMPW1]


**************Shelter Cost to Income Ratio*************************************
*-------------------------------------------------------------------------------

bysort STIR: drop if STIR>=100

* Average of shelter cost to income ratio

* Unweighted
mean STIR if HMAIN==1 & TENUR == 4
mean STIR if HMAIN==1 

* Weighted

svy: mean STIR if HMAIN==1 & TENUR == 4
svy: mean STIR if HMAIN==1

* Total shelter costs and household income

*Unweighted
total SHELCO if HMAIN ==1

*Weighted
svy: total SHELCO if HMAIN ==1

*Total Household Income 
*Unweighted
total HHINC if HMAIN ==1

*Weighted
svy: total HHINC if HMAIN ==1





log close
