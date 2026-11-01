* -----------------------------------------------------------------------------
* Set datadir to the folder that holds this script's input microdata file; the
* Stata log is written there too. "." means the current working directory.
global datadir "."
* -----------------------------------------------------------------------------
clear
local year = 2019

local path = "$datadir"
cd "`path'"

use SFS_`year'.dta, replace

keep if pregion==3
egen total_wealth=sum(pweight*pwnetwpg)
display total_wealth/885400000000
