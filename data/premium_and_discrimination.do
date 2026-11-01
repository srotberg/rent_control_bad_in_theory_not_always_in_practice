clear

local year=2016

local path="G:\My Drive\Academic\can_rent_control_outperform_decontrol_a_welfare_analysis\data\census_"+"`year'"

cd "`path'"

use census_pumf_clean_`year'.dta, replace

keep if CMA==535
keep if TENUR==2
drop if SHELCO==0 | SHELCO==. |SHELCO>5000
gen is_controlled=0
replace is_controlled=1 if BUILT<=6
gen is_new_tenant=0
replace is_new_tenant=1 if MOB1>1 & MOB1<=6
gen is_in_good_condition=1
replace is_in_good_condition=0 if REPAIR==3

reg SHELCO TOTINC i.AGEGRP BEDRM ROOM i.REPAIR if is_controlled==1 & is_new_tenant==1

mat average_free_rent=J(6,2,0)

local i=1

while `i'<7 {

	capture drop has_bedroom_x
	gen has_bedroom_x=0
	replace has_bedroom_x=1 if BEDRM==`i'-1

	local j=1

	while `j'<2 {

		capture drop total_controlled_rent
		capture drop total_renters

		if `j'==1 {

			egen total_controlled_rent=sum(WEIGHT*has_bedroom_x*(1-is_controlled)*is_in_good_condition*SHELCO)
			egen total_renters=sum(WEIGHT*has_bedroom_x*(1-is_controlled)*is_in_good_condition)

		}

		if `j'==2 {

			egen total_controlled_rent=sum(WEIGHT*has_bedroom_x*(1-is_controlled)*(1-is_in_good_condition)*SHELCO)
			egen total_renters=sum(WEIGHT*has_bedroom_x*(1-is_controlled)*(1-is_in_good_condition))

		}

		mat average_free_rent[`i',`j']=total_controlled_rent[1]/total_renters[1]
		display average_free_rent[`i',`j']

		local j=`j'+1

	}

	local i=`i'+1

}

local scaling=1.2401-1.0055^8.55

gen premium=100*(SHELCO/(average_free_rent[BEDRM+1,is_in_good_condition]/(1+`scaling'))-1)

reg premium if is_controlled==1 & is_new_tenant==1

sum SHELCO if is_controlled==1 & is_new_tenant==1
