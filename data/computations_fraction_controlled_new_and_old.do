clear

local year=2016

local path="G:\My Drive\Academic\can_rent_control_outperform_decontrol_a_welfare_analysis\data\census_"+"`year'"

cd "`path'"

local utilitization_of_condo=1

use census_pumf_clean_`year'.dta, replace

keep if CMA==535
*825 Calgary, 835 Edmonton, 535=toronto

gen single_family=0
replace single_family=1 if DTYPE==1

gen condo=0
replace condo=1 if DTYPE==2

egen total_single_family=sum(WEIGHT*single_family)
egen total_condo=sum(WEIGHT*condo)

gen is_new=1
replace is_new=0 if BUILT<=6

gen is_renter=0
replace is_renter=1 if TENUR==2

display total_condo[1]/total_single_family[1]

if `utilitization_of_condo'==1 {

	drop if single_family==1

}

if `utilitization_of_condo'==0 {

	drop if condo==1

}


egen total_renters=sum(WEIGHT*is_renter)
egen total_hosueholds=sum(WEIGHT)

egen total_new_units=sum(WEIGHT*is_new)
egen total_old_units=sum(WEIGHT*(1-is_new))

egen total_new_rentals_unit=sum(WEIGHT*is_new*is_renter)
egen total_old_rentals_unit=sum(WEIGHT*(1-is_new)*is_renter)

display total_new_rentals_unit[1]/total_new_units[1]
display total_old_rentals_unit[1]/total_old_units[1]

display total_renters[1]/total_hosueholds[1]

display total_new_units[1]/(total_new_units[1]+total_old_units[1])

