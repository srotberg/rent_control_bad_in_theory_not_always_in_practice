# Replication package for "Rent Control? Bad in Theory, Not Always in Practice"

Shahar Rotberg and Lin Zhang. *European Economic Review*, manuscript EEREV-D-26-00102R2.

Repository: <https://github.com/srotberg/rent_control_bad_in_theory_not_always_in_practice>

Contact: Shahar Rotberg, shahar.rotberg@gmail.com

## 1. Overview

The paper builds a quantitative life-cycle model of the Toronto housing market with tenancy rent control, calibrates it to Canadian data, and compares the benchmark with four counterfactual policies. This package contains:

- `code/`: the Fortran source for the model, which produces every simulation result in the paper, and instructions for running it (`code/readme.txt`).
- `data/`: the data and Stata scripts behind the calibration targets and the data moments in Tables 1-4 and Figure 2.
- `outputs/`: the output file of every simulation, including the six that the paper reports.
- `plots/`: the Python scripts that draw Figures 1 and 2, and the figures they produce.

The package is about 18 MB. Section 6 lists every file.

The paper's tables are typed from the simulation output files; Section 5 explains, for each table cell, which output line it comes from. Figure 1 is a conceptual diagram and uses no data.

## 2. Data availability and provenance

All data files included in this package may be redistributed.

| Source | Used for | In this package |
|---|---|---|
| 2016 Census of Population, confidential microdata (Statistics Canada) | Toronto CMA moments: homeownership by age, household size, income quintiles, Gini, income taxes, rent-to-income distribution, shares of renters in controlled units | Vetted results only: `data/2016_Census_vetted.xlsx`. Scripts: `Census_2016.do`, `proportions_of_renters_market_and_quintiles_2016.do`, `rent_to_income_cdf_2016.do` |
| 2016 Census Hierarchical Public Use Microdata File (Statistics Canada) | Controlled-rent entry premium, discrimination tests, shares of new and old rental units (Appendix A) | Yes: `data/census_pumf_clean_2016.dta` and the user guide. Scripts: `premium_and_discrimination.do`, `computations_fraction_controlled_new_and_old.do` |
| 2019 Survey of Financial Security (Statistics Canada) | LTV by age, mortgage debt to house value, share of owners with a mortgage, wealth distribution, net worth to GDP, bequests | Results: `data/2019_SFS.xlsx`. Scripts: `SFS_2019.do`, `net_worth_to_GDP.do` |
| CMHC Rental Market Survey | Toronto average rents by bedroom type | Yes: `data/CMHC_RMS.csv` |
| World Health Organization life tables for Canada | Death rates used to compute bequests | Yes: `data/WHO_death_rates_Canada.xlsx` (source URL inside the file) |
| 5-year variable mortgage rates and 10-year bond yields, 2002-2017 | Mortgage interest rate premium (Table 1) | Yes: `data/bond_yields_mortgage_rates.csv` |
| Published estimates | Parameters cited in Table 1 (e.g., Guvenen et al. 2023; Gruber and Martin 2003) | Values set in the Fortran code; references in the paper |

**Confidential Census microdata.** The authors computed the statistics in `data/2016_Census_vetted.xlsx` at the Canada Mortgage and Housing Corporation from the confidential 2016 Census microdata, using the three Census scripts listed above; the results were vetted for release. The microdata file (`Census_2016.dta`) is confidential and is not included. Researchers can apply to Statistics Canada for access; the scripts are included so the computations can be rerun by anyone with access.

**SFS microdata.** The 2019 SFS microdata file (`SFS_2019.dta`) is distributed by Statistics Canada and is not included. The statistics computed from it are in `data/2019_SFS.xlsx`.

## 3. Computational requirements

- **Model (Fortran).** gfortran with OpenMP; no other libraries. The code compiles with GNU Fortran 13.3 on Linux (Ubuntu 24.04) using the command in `code/readme.txt`. The paper's simulations were run on a multi-core workstation.
- **Data scripts (Stata).** The `.do` files use standard Stata commands (`svy`, `xtile`, `egen`). They need the microdata files described in Section 2.
- **Figures (Python).** Python 3 with `matplotlib`; Figure 2 also needs `openpyxl` (tested with Python 3.13, matplotlib 3.10, openpyxl 3.1). The scripts use the Liberation Sans font (metrically identical to Arial) if it is installed.

**Run times.** Each output file records its run time on its second line (`hours to finish simulaion`). For the simulations reported in the paper:

| Simulation | Run time |
|---|---|
| Benchmark | 7.7 hours |
| Experiment 1 | 8.5 hours |
| Experiment 2 | 5.7 hours |
| Experiment 3 | 5.5 hours |
| Experiment 4 (one discount value) | 168 hours (about 7 days) |
| Experiment 4 with income conditioning | 155 hours (about 6.5 days) |

The full search over the Experiment 4 discount took between 1.5 and 7 days per grid point. The figure scripts and the Stata scripts each run in under a minute.

## 4. Instructions

1. **Simulations.** Follow `code/readme.txt`. Run the benchmark first, then the counterfactuals in the same folder, because every counterfactual reads the benchmark's `.dat` files. Each run writes `outputs<simulation name>.txt`; the six that the paper uses are listed in Section 5.
2. **Tables 2-6.** Read the values from the output files as described in Section 5.
3. **Figures.** Run `python plots/figure_1_rent_path_diagram.py` for Figure 1. For Figure 2, run `python plots/figure_2_homeownership_and_LTV.py`. It reads `data/2016_Census_vetted.xlsx`, `data/2019_SFS.xlsx` and the benchmark output file, and writes `plots/Figure_2_homeownership_and_LTV.pdf` and `.eps`.
4. **Data moments.** Each Stata script loads its microdata with a `use` command near the top, preceded by a `cd` or `log using` line with the authors' local paths; change those paths to where the microdata are stored, then run the script. The results are the values reported in `2016_Census_vetted.xlsx` and `2019_SFS.xlsx`.

## 5. From output files to tables and figures

The six simulations reported in the paper (all files are in `outputs/`):

| Simulation | Output file |
|---|---|
| Benchmark (tenancy control) | `outputs_control_sorting_benchmark_marginal_profits.txt` |
| Experiment 1: decontrol, income tax cuts | `outputs0_control_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| Experiment 2: decontrol, transfers to renters and the homeless | `outputs0_control_GE_counter_marginal_pi_lump_sum_renters.txt` |
| Experiment 3: decontrol, full profit tax, transfers to newborns | `outputs0_control_GE_counter_marginal_pi_lump_sum_young_pi_full_tax.txt` |
| Experiment 4: optimal unit control | `outputs28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| Experiment 4, controlled units for low-income renters only | `outputs28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top_controlled_rentals_to_low_income_only.txt` |

Each output file is a list of lines of the form `name value`. In what follows, "line `x`" means the value on the line whose name is `x` (the first such line unless stated otherwise).

**Table 1 (externally assigned parameters).** The values are set in the Fortran code. Sources are listed in the table and in Appendix A; the data-based values come from the files in `data/` (Section 2).

**Table 2 (internally calibrated parameters).** Target moments: `2016_Census_vetted.xlsx`, `2019_SFS.xlsx`, Zhang and Guo (2022) and the Homelessness Hub, as stated in the table notes. Model moments, from the benchmark output:

| Moment | Output line |
|---|---|
| Mortgages / housing value | `model mortgage debt to gross housing wealth` |
| Average rent / income | `model_average_rent_to_income_ratio` |
| Total bequests / total net wealth | `model_bequests/model_net_wealth` |
| Average income tax rate | `model_average_labour_income_tax` |
| Labor income Gini | `gini_coefficient_income` |
| Average controlled rent / free-market rent | second `model_average_rent` line divided by the first |
| Homeownership rate | `model_homeownership_rate` |
| Renters with rent/income above 30% | 1 minus `rent_to_income_cdf_30` |
| Homelessness rate (%) | `model_fraction_homeless (%)` |

**Table 3 (non-targeted statistics).** Data: `2019_SFS.xlsx`, `2016_Census_vetted.xlsx`, `net_worth_to_GDP.do` and Chatterjee and Eyigungor (2015). Model, from the benchmark output:

| Statistic | Output line |
|---|---|
| House price to GDP per household | `housing_price(j)*model_average_owner_occupied_unit_size(j)/(model_GDP_incl_imput_rent(j)/total_population)` |
| Owners with a mortgage | `model_fraction_of_homeowners_with_mortgage` |
| Rental-to-owner unit value | `model_average_rental_unit_size(j)/model_average_owner_occupied_unit_size(j)` |
| Net wealth to GDP | `model total net wealth to output` |
| Renters in the free market | `model_fraction_living_in_free` |
| Renters spending over 50% / 75% of income on rent | 1 minus `rent_to_income_cdf_50` / `rent_to_income_cdf_75` |
| Wealth shares of the top 1%, 10%, 20% | `model_wealth_of_top_x_perc(k)/model_net_wealth` for k = 1, 2, 3 |
| Wealth share of the bottom 50% | 1 minus the same line for k = 4 |
| Wealth Gini | `gini_coefficient` |

**Table 4 (households in the controlled market by income quintile).** Data row: `2016_Census_vetted.xlsx`, sheet `rent_control` (`proportions_of_renters_market_and_quintiles_2016.do`). Model rows: lines `market_1_quintile_1` to `market_1_quintile_5` in the benchmark and Experiment 4 output files.

**Table 5 (aggregate effects).** Each row compares an experiment's output file with the benchmark's:

| Column | Computation |
|---|---|
| Newborn welfare (%) | $100\,[(W_1/W_0)^{1/((1-\gamma)(1-\sigma))}-1]$, where $W$ is line `average_welfare_of_youngest_cohort`, $\gamma=0.385$ and $\sigma=2$ (Section 5.2 of the paper) |
| Controlled initial rent (%) | second `rent/rent_calibrated` line minus 1 (Experiment 4 only) |
| Free-market rent (%) | first value on line `model_average_rent(:)/average_rent_calibrated(:)` minus 1 |
| Rental quantity (%) | third `total_rental_supply` line, relative to the benchmark |
| Housing price (%) | `housing_price/calibrated_stationary_housing_price` minus 1 |
| Homeownership (p.p.) | difference in `model_homeownership_rate`, times 100 |
| Homelessness (%) | ratio of `model_fraction_homeless (%)` to the benchmark's, minus 1 |

**Table 6 (welfare by income quintile).** The welfare formula above applied to lines `welfare_of_quintile 1` to `welfare_of_quintile 5`.

Values in the paper are rounded to two decimals; a few cells differ from the output files by 0.01 because of rounding.

**Figure 1.** Conceptual diagram; drawn by `plots/figure_1_rent_path_diagram.py` and uses no data.

**Figure 2.** `plots/figure_2_homeownership_and_LTV.py`. Data: `2016_Census_vetted.xlsx` (sheet `homeownership_housholds_szie`) and `2019_SFS.xlsx` (sheet `LTV by Age Group`). Model: lines `homeownership_age1` to `homeownership_age60` (ages 26-85) and `LTV_age_group1` to `LTV_age_group12` in the benchmark output.

**Numbers in the text.**

- Experiment 4's discount of 35.6% relative to the benchmark free-market rent: the second `model_average_rent` line of the Experiment 4 output divided by the first `model_average_rent` line of the benchmark output.
- The 2.57% welfare gain with income conditioning (footnote in Section 5.1): the welfare formula applied to the income-conditioned Experiment 4 output.
- The optimal discount: Experiment 4 was run for a grid of values of `discount_in_unit_control` (files `outputsNN_discount_...`; see `code/readme.txt`). The run labelled 28 (`discount_in_unit_control=0.27`) gives the highest newborn welfare.

## 6. List of all files

**code/**

- `code/readme.txt` (5 KB). How to compile and run the model and which switches select each simulation.
- `code/rent_control_bad_in_theory_not_always_in_practice_Rotberg_Zhang_v11.f90` (1.1 MB). Fortran source for the model; produces every simulation (benchmark and counterfactuals).

**data/**

- `data/2016_Census_Hierarchical_PUMF_User_Guide.pdf` (3.3 MB). Statistics Canada user guide for the 2016 Census Hierarchical PUMF.
- `data/2016_Census_vetted.xlsx` (18 KB). Vetted statistics from the confidential 2016 Census microdata (Toronto CMA): rent control shares by income quintile, rent-to-income distribution, income and Gini, homeownership and household size by age.
- `data/2019_SFS.xlsx` (23 KB). Statistics computed from the 2019 Survey of Financial Security: LTV by age group, mortgage debt to house value, wealth distribution, bequests.
- `data/CMHC_RMS.csv` (4 KB). CMHC Rental Market Survey: Toronto average rents by bedroom type, 1990-2024.
- `data/Census_2016.do` (16 KB). Stata: homeownership by age, household size, income quintiles, Gini, income taxes, controlled vs. uncontrolled renters (confidential 2016 Census microdata; results in `2016_Census_vetted.xlsx`).
- `data/SFS_2019.do` (3 KB). Stata: LTV by age, mortgage debt, house values, wealth shares, share of owners with a mortgage (2019 SFS microdata; results in `2019_SFS.xlsx`).
- `data/WHO_death_rates_Canada.xlsx` (16 KB). WHO life-table death rates for Canada; used to compute bequests (`2019_SFS.xlsx`, sheet "Wealth Bequest").
- `data/bond_yields_mortgage_rates.csv` (31 KB). 5-year variable mortgage rates and 10-year bond yields, 2002-2017; average spread gives the mortgage premium in Table 1.
- `data/census_pumf_clean_2016.dta` (9.6 MB). 2016 Census Hierarchical Public Use Microdata File (cleaned), Stata format.
- `data/computations_fraction_controlled_new_and_old.do` (1 KB). Stata (uses `census_pumf_clean_2016.dta`): shares of new and old rental units, condo vs. single-family stock.
- `data/net_worth_to_GDP.do` (0 KB). Stata: aggregate net worth to GDP (2019 SFS microdata).
- `data/premium_and_discrimination.do` (2 KB). Stata (uses `census_pumf_clean_2016.dta`): controlled-rent entry premium and age/income discrimination (Appendix A).
- `data/proportions_of_renters_market_and_quintiles_2016.do` (1 KB). Stata: share of households in controlled and free-market rentals by income quintile (Table 4, data row).
- `data/rent_to_income_cdf_2016.do` (2 KB). Stata: distribution of renters' rent-to-income ratios (Tables 2 and 3).

**outputs/**

- `outputs/outputs0_control_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 1: decontrol with income tax cuts (Tables 5-6, row 1).
- `outputs/outputs0_control_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top_higher_elasticity.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (higher rental supply elasticity).
- `outputs/outputs0_control_GE_counter_marginal_pi_lump_sum_bottom_renters.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (transfers to bottom-quintile renters).
- `outputs/outputs0_control_GE_counter_marginal_pi_lump_sum_renters.txt` (58 KB). Experiment 2: decontrol with transfers to renters and the homeless (Tables 5-6, row 2).
- `outputs/outputs0_control_GE_counter_marginal_pi_lump_sum_renters_higher_elasticity.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (higher rental supply elasticity).
- `outputs/outputs0_control_GE_counter_marginal_pi_lump_sum_to_all_by_income_and_age.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (transfers to all households by income and age).
- `outputs/outputs0_control_GE_counter_marginal_pi_lump_sum_two_bottom_renters.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (transfers to bottom-two-quintile renters).
- `outputs/outputs0_control_GE_counter_marginal_pi_lump_sum_young_pi_full_tax.txt` (58 KB). Experiment 3: decontrol, full profit tax, transfers to newborns (Tables 5-6, row 3).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top_sr.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top_sr_fixed_stocks.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (housing stocks held fixed; short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top_sr_fixed_stocks_relative_to_unit_control.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; housing stocks held fixed; short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top_sr_relative_to_unit_control.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_lump_sum_renters_sr.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_lump_sum_renters_sr_fixed_stocks.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (housing stocks held fixed; short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_lump_sum_renters_sr_fixed_stocks_relative_to_unit_control.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; housing stocks held fixed; short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_lump_sum_renters_sr_relative_to_unit_control.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_lump_sum_young_pi_full_tax_sr.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_lump_sum_young_pi_full_tax_sr_fixed_stocks.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (housing stocks held fixed; short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_lump_sum_young_pi_full_tax_sr_fixed_stocks_relative_to_unit_control.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; housing stocks held fixed; short run; bequests held fixed).
- `outputs/outputs0_control_beq_constant_counter_marginal_pi_lump_sum_young_pi_full_tax_sr_relative_to_unit_control.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; short run; bequests held fixed).
- `outputs/outputs0_control_rents_and_hp_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (rents and house prices held at benchmark values).
- `outputs/outputs0_control_rents_and_hp_constant_counter_marginal_pi_lump_sum_renters.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (rents and house prices held at benchmark values).
- `outputs/outputs0_control_rents_and_hp_constant_counter_marginal_pi_lump_sum_young_pi_full_tax.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (rents and house prices held at benchmark values).
- `outputs/outputs16_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4 grid point (discount label 16); used to choose the welfare-maximizing discount.
- `outputs/outputs24_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4 grid point (discount label 24); used to choose the welfare-maximizing discount.
- `outputs/outputs25_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4 grid point (discount label 25); used to choose the welfare-maximizing discount.
- `outputs/outputs26_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4 grid point (discount label 26); used to choose the welfare-maximizing discount.
- `outputs/outputs28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4: optimal unit control, discount_in_unit_control=0.27 (Tables 4-6).
- `outputs/outputs28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top_controlled_rentals_to_low_income_only.txt` (58 KB). Experiment 4 with controlled units for low-income renters only (2.57% welfare gain, footnote in Section 5.1).
- `outputs/outputs29_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4 grid point (discount label 29); used to choose the welfare-maximizing discount.
- `outputs/outputs30_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4 grid point (discount label 30); used to choose the welfare-maximizing discount.
- `outputs/outputs31_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4 grid point (discount label 31); used to choose the welfare-maximizing discount.
- `outputs/outputs32_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Experiment 4 grid point (discount label 32); used to choose the welfare-maximizing discount.
- `outputs/outputs_0_control_sorting_benchmark_marginal_profits_higher_elasticity.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (higher rental supply elasticity).
- `outputs/outputs_25_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Separate run of a discount grid point (not used in the paper); the space the code wrote into the file name is replaced by an underscore.
- `outputs/outputs_28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Separate run of a discount grid point (not used in the paper); the space the code wrote into the file name is replaced by an underscore.
- `outputs/outputs_31_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` (58 KB). Separate run of a discount grid point (not used in the paper); the space the code wrote into the file name is replaced by an underscore.
- `outputs/outputs_control_sorting_benchmark_marginal_profits.txt` (58 KB). Benchmark (tenancy control): Tables 2-4 and Figure 2 (model); baseline for Tables 5-6.
- `outputs/outputs_control_sorting_benchmark_marginal_profits_higher_elasticity.txt` (58 KB). Additional simulation from an earlier revision, not reported in the final paper (higher rental supply elasticity).

**plots/**

- `plots/Figure_1_rent_path_diagram.eps` (41 KB). Figure 1 (vector EPS), produced by `figure_1_rent_path_diagram.py`.
- `plots/Figure_1_rent_path_diagram.pdf` (16 KB). Figure 1 (vector PDF), produced by `figure_1_rent_path_diagram.py`.
- `plots/Figure_2_homeownership_and_LTV.eps` (52 KB). Figure 2 (vector EPS), produced by `figure_2_homeownership_and_LTV.py`.
- `plots/Figure_2_homeownership_and_LTV.pdf` (15 KB). Figure 2 (vector PDF), produced by `figure_2_homeownership_and_LTV.py`.
- `plots/figure_1_rent_path_diagram.py` (4 KB). Python: draws Figure 1 (conceptual diagram, no data).
- `plots/figure_2_homeownership_and_LTV.py` (5 KB). Python: draws Figure 2 from data/ and the benchmark output file.

The GitHub repository also contains `writing/`, the LaTeX source of the paper; it is not part of the replication package.
