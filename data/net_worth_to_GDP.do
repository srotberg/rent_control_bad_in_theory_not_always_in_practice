clear
local year = 2019

local path = "G:\My Drive\Academic\can_rent_control_outperform_decontrol_a_welfare_analysis\data\sfs_" + "`year'"
cd "`path'"

use SFS_`year'.dta, replace

keep if pregion==3
egen total_wealth=sum(pweight*pwnetwpg)
display total_wealth/885400000000
