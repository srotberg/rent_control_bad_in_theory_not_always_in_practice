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

All data files in this package are vetted and public, and may be redistributed.

| Source | Used for | In this package |
|---|---|---|
| 2016 Census of Population microdata (Statistics Canada) | Toronto CMA moments: homeownership by age, household size, income quintiles, Gini, income taxes, rent-to-income distribution, shares of renters in controlled units | Vetted statistics: `data/2016_Census_vetted.xlsx`. Scripts: `Census_2016.do`, `proportions_of_renters_market_and_quintiles_2016.do`, `rent_to_income_cdf_2016.do` |
| 2016 Census Hierarchical Public Use Microdata File (Statistics Canada) | Controlled-rent entry premium, discrimination tests, shares of new and old rental units (Appendix A) | Yes: `data/census_pumf_clean_2016.dta` and the user guide. Scripts: `premium_and_discrimination.do`, `computations_fraction_controlled_new_and_old.do` |
| 2019 Survey of Financial Security (Statistics Canada) | LTV by age, mortgage debt to house value, share of owners with a mortgage, wealth distribution, net worth to GDP, bequests | Statistics: `data/2019_SFS.xlsx`. Scripts: `SFS_2019.do`, `net_worth_to_GDP.do` |
| CMHC Rental Market Survey | Toronto average rents by bedroom type | Yes: `data/CMHC_RMS.csv` |
| World Health Organization life tables for Canada | Death rates used to compute bequests | Yes: `data/WHO_death_rates_Canada.xlsx` (source URL inside the file) |
| 5-year variable mortgage rates and 10-year bond yields, 2002-2017 | Mortgage interest rate premium (Table 1) | Yes: `data/bond_yields_mortgage_rates.csv` |
| Published estimates | Parameters cited in Table 1 (e.g., Guvenen et al. 2023; Gruber and Martin 2003) | Values set in the Fortran code; references in the paper |

**How the data were used to build the final data sets.** The model reads no data files: every parameter is set in the Fortran code. Data enter the paper only through the externally assigned parameters (Table 1), the calibration targets (Table 2), and the data columns of Tables 3-4 and Figure 2. The final data sets behind these numbers are the statistics in `2016_Census_vetted.xlsx`, `2019_SFS.xlsx`, `CMHC_RMS.csv`, `bond_yields_mortgage_rates.csv` and `WHO_death_rates_Canada.xlsx`, all included. The two spreadsheets were built from intermediate microdata with the Stata scripts below. The scripts print their results to a Stata log, and the results are recorded in the spreadsheets.

| Script | Reads (intermediate data) | Results recorded in |
|---|---|---|
| `Census_2016.do` | `Census_2016.dta` (2016 Census microdata; not included) | `2016_Census_vetted.xlsx`: homeownership and household size by age, income by age, Gini, controlled vs. uncontrolled renters |
| `proportions_of_renters_market_and_quintiles_2016.do` | `Census_2016.dta` | `2016_Census_vetted.xlsx`, sheet `rent_control` (Table 4, data row) |
| `rent_to_income_cdf_2016.do` | `Census_2016.dta` | `2016_Census_vetted.xlsx`, sheet `rent_to_income_cdf` |
| `SFS_2019.do` | `SFS_2019.dta` (2019 SFS microdata; not included) | `2019_SFS.xlsx` |
| `net_worth_to_GDP.do` | `SFS_2019.dta` | Net worth to GDP (Table 3) |
| `premium_and_discrimination.do` | `census_pumf_clean_2016.dta` (included) | Controlled-rent entry premium and discrimination tests (Appendix A) |
| `computations_fraction_controlled_new_and_old.do` | `census_pumf_clean_2016.dta` (included) | Shares of new and old rental units |

**Intermediate data.** The statistics in `2016_Census_vetted.xlsx` were computed from 2016 Census microdata and vetted; those in `2019_SFS.xlsx` were computed from the 2019 SFS microdata. The Stata scripts that read these microdata files (`Census_2016.dta`, `SFS_2019.dta`) are included; the microdata files themselves are Statistics Canada files, available from Statistics Canada. As the replication policy asks, the authors will cooperate fully with any researcher who requests details of these computations (contact: shahar.rotberg@gmail.com).

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

1. **Simulations.** Follow `code/readme.txt`. Run the benchmark first, then the counterfactuals in the same folder, because every counterfactual reads the benchmark's `.dat` files. Each run writes `outputs<simulation name>.txt`. Because the journal limits file names to 64 characters, the copies of these files in `outputs/` have shorter names; the table at the end of Section 5 gives the name the program writes for each one.
2. **Tables 2-6.** Read the values from the output files as described in Section 5.
3. **Figures.** Run `python plots/figure_1_rent_path_diagram.py` for Figure 1. For Figure 2, run `python plots/figure_2_homeownership_and_LTV.py`. It reads `data/2016_Census_vetted.xlsx`, `data/2019_SFS.xlsx` and the benchmark output file, and writes its two panels as `plots/Figure_2a_homeownership_by_age` and `plots/Figure_2b_LTV_by_age_group` (`.pdf` and `.eps`).
4. **Data moments.** At the top of each Stata script, set `global datadir` to the folder that holds the script's input microdata file (the Stata log is written there too). The default `"."` is the current working directory: running `premium_and_discrimination.do` or `computations_fraction_controlled_new_and_old.do` from `data/` works as is, because `census_pumf_clean_2016.dta` is there. The results are the values recorded in `2016_Census_vetted.xlsx` and `2019_SFS.xlsx` (Section 2).

## 5. From output files to tables and figures

The six simulations reported in the paper (all files are in `outputs/`):

| Simulation | Output file |
|---|---|
| Benchmark (tenancy control) | `benchmark.txt` |
| Experiment 1: decontrol, income tax cuts | `exp1_decontrol_tax_cuts.txt` |
| Experiment 2: decontrol, transfers to renters and the homeless | `exp2_decontrol_transfers_renters.txt` |
| Experiment 3: decontrol, full profit tax, transfers to newborns | `exp3_decontrol_transfers_newborns.txt` |
| Experiment 4: optimal unit control | `exp4_unit_control.txt` |
| Experiment 4, controlled units for low-income renters only | `exp4_unit_control_low_income_only.txt` |

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
- The optimal discount: Experiment 4 was run for a grid of values of `discount_in_unit_control` (files `grid_unit_control_labelNN.txt`; see `code/readme.txt`). The run labelled 28 (`discount_in_unit_control=0.27`) gives the highest newborn welfare.

**Output file names.** The program names each results file `outputs<simulation name>.txt`. Those names are longer than the 64 characters the journal allows, so the files in `outputs/` are renamed as follows; the contents are unchanged.

| File in `outputs/` | Name written by the program |
|---|---|
| `benchmark.txt` | `outputs_control_sorting_benchmark_marginal_profits.txt` |
| `exp1_decontrol_tax_cuts.txt` | `outputs0_control_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `exp2_decontrol_transfers_renters.txt` | `outputs0_control_GE_counter_marginal_pi_lump_sum_renters.txt` |
| `exp3_decontrol_transfers_newborns.txt` | `outputs0_control_GE_counter_marginal_pi_lump_sum_young_pi_full_tax.txt` |
| `exp4_unit_control.txt` | `outputs28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `exp4_unit_control_low_income_only.txt` | `outputs28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top_controlled_rentals_to_low_income_only.txt` |
| `extra_benchmark_higher_elasticity.txt` | `outputs_control_sorting_benchmark_marginal_profits_higher_elasticity.txt` |
| `extra_benchmark_higher_elasticity_run2.txt` | `outputs_0_control_sorting_benchmark_marginal_profits_higher_elasticity.txt` |
| `extra_fixed_prices_tax_cuts.txt` | `outputs0_control_rents_and_hp_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `extra_fixed_prices_transfers_newborns.txt` | `outputs0_control_rents_and_hp_constant_counter_marginal_pi_lump_sum_young_pi_full_tax.txt` |
| `extra_fixed_prices_transfers_renters.txt` | `outputs0_control_rents_and_hp_constant_counter_marginal_pi_lump_sum_renters.txt` |
| `extra_sr_fixed_stocks_tax_cuts.txt` | `outputs0_control_beq_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top_sr_fixed_stocks.txt` |
| `extra_sr_fixed_stocks_transfers_newborns.txt` | `outputs0_control_beq_constant_counter_marginal_pi_lump_sum_young_pi_full_tax_sr_fixed_stocks.txt` |
| `extra_sr_fixed_stocks_transfers_renters.txt` | `outputs0_control_beq_constant_counter_marginal_pi_lump_sum_renters_sr_fixed_stocks.txt` |
| `extra_sr_fixed_stocks_vs_unit_control_tax_cuts.txt` | `outputs0_control_beq_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top_sr_fixed_stocks_relative_to_unit_control.txt` |
| `extra_sr_fixed_stocks_vs_unit_transfers_newborns.txt` | `outputs0_control_beq_constant_counter_marginal_pi_lump_sum_young_pi_full_tax_sr_fixed_stocks_relative_to_unit_control.txt` |
| `extra_sr_fixed_stocks_vs_unit_transfers_renters.txt` | `outputs0_control_beq_constant_counter_marginal_pi_lump_sum_renters_sr_fixed_stocks_relative_to_unit_control.txt` |
| `extra_sr_tax_cuts.txt` | `outputs0_control_beq_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top_sr.txt` |
| `extra_sr_transfers_newborns.txt` | `outputs0_control_beq_constant_counter_marginal_pi_lump_sum_young_pi_full_tax_sr.txt` |
| `extra_sr_transfers_renters.txt` | `outputs0_control_beq_constant_counter_marginal_pi_lump_sum_renters_sr.txt` |
| `extra_sr_vs_unit_control_tax_cuts.txt` | `outputs0_control_beq_constant_counter_marginal_pi_inc_tax_reduction_pi_to_top_sr_relative_to_unit_control.txt` |
| `extra_sr_vs_unit_control_transfers_newborns.txt` | `outputs0_control_beq_constant_counter_marginal_pi_lump_sum_young_pi_full_tax_sr_relative_to_unit_control.txt` |
| `extra_sr_vs_unit_control_transfers_renters.txt` | `outputs0_control_beq_constant_counter_marginal_pi_lump_sum_renters_sr_relative_to_unit_control.txt` |
| `extra_tax_cuts_higher_elasticity.txt` | `outputs0_control_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top_higher_elasticity.txt` |
| `extra_transfers_bottom_renters.txt` | `outputs0_control_GE_counter_marginal_pi_lump_sum_bottom_renters.txt` |
| `extra_transfers_by_income_and_age.txt` | `outputs0_control_GE_counter_marginal_pi_lump_sum_to_all_by_income_and_age.txt` |
| `extra_transfers_renters_higher_elasticity.txt` | `outputs0_control_GE_counter_marginal_pi_lump_sum_renters_higher_elasticity.txt` |
| `extra_transfers_two_bottom_renters.txt` | `outputs0_control_GE_counter_marginal_pi_lump_sum_two_bottom_renters.txt` |
| `grid_unit_control_label16.txt` | `outputs16_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label24.txt` | `outputs24_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label25.txt` | `outputs25_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label25_run2.txt` | `outputs_25_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label26.txt` | `outputs26_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label28_run2.txt` | `outputs_28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label29.txt` | `outputs29_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label30.txt` | `outputs30_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label31.txt` | `outputs31_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label31_run2.txt` | `outputs_31_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |
| `grid_unit_control_label32.txt` | `outputs32_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt` |

## 6. List of all files

Each entry gives the file's format, size and purpose. File formats:

| Extension | Format |
|---|---|
| `.f90` | Fortran 90 source code (plain text) |
| `.do` | Stata do-file (plain text) |
| `.dta` | Stata dataset (`.dta` format 114; readable by Stata 10 or later, R and Python) |
| `.xlsx` | Microsoft Excel workbook |
| `.csv` | Comma-separated text (UTF-8) |
| `.txt` in `outputs/` | Plain text written by the model: one statistic per line, as `name value` |
| `.txt` in `code/` | Plain-text instructions |
| `.py` | Python 3 script |
| `.pdf`, `.eps` in `plots/` | Vector figure files |
| `.pdf` in `data/` | PDF document |

**code/**

- `code/readme.txt` (plain text, 5 KB). How to compile and run the model and which switches select each simulation.
- `code/rent_control_Rotberg_Zhang_v11.f90` (Fortran 90 source, 1.1 MB). Fortran source for the model; produces every simulation (benchmark and counterfactuals).

**data/**

- `data/2016_Census_Hierarchical_PUMF_User_Guide.pdf` (PDF document, 3.3 MB). Statistics Canada user guide for the 2016 Census Hierarchical PUMF.
- `data/2016_Census_vetted.xlsx` (Excel workbook, 18 KB). Vetted statistics computed from 2016 Census microdata (Toronto CMA): rent control shares by income quintile, rent-to-income distribution, income and Gini, homeownership and household size by age.
- `data/2019_SFS.xlsx` (Excel workbook, 23 KB). Statistics computed from the 2019 Survey of Financial Security: LTV by age group, mortgage debt to house value, wealth distribution, bequests.
- `data/CMHC_RMS.csv` (CSV text, 4 KB). CMHC Rental Market Survey: Toronto average rents by bedroom type, 1990-2024.
- `data/Census_2016.do` (Stata do-file, 16 KB). Stata: homeownership by age, household size, income quintiles, Gini, income taxes, controlled vs. uncontrolled renters (reads the 2016 Census microdata file `Census_2016.dta`, not included; results in `2016_Census_vetted.xlsx`).
- `data/SFS_2019.do` (Stata do-file, 3 KB). Stata: LTV by age, mortgage debt, house values, wealth shares, share of owners with a mortgage (2019 SFS microdata; results in `2019_SFS.xlsx`).
- `data/WHO_death_rates_Canada.xlsx` (Excel workbook, 16 KB). WHO life-table death rates for Canada; used to compute bequests (`2019_SFS.xlsx`, sheet "Wealth Bequest").
- `data/bond_yields_mortgage_rates.csv` (CSV text, 31 KB). 5-year variable mortgage rates and 10-year bond yields, 2002-2017; average spread gives the mortgage premium in Table 1.
- `data/census_pumf_clean_2016.dta` (Stata dataset, 9.6 MB). 2016 Census Hierarchical Public Use Microdata File (cleaned), Stata format.
- `data/computations_fraction_controlled_new_and_old.do` (Stata do-file, 2 KB). Stata (uses `census_pumf_clean_2016.dta`): shares of new and old rental units, condo vs. single-family stock.
- `data/net_worth_to_GDP.do` (Stata do-file, 1 KB). Stata: aggregate net worth to GDP (2019 SFS microdata).
- `data/premium_and_discrimination.do` (Stata do-file, 2 KB). Stata (uses `census_pumf_clean_2016.dta`): controlled-rent entry premium and age/income discrimination (Appendix A).
- `data/proportions_of_renters_market_and_quintiles_2016.do` (Stata do-file, 1 KB). Stata: share of households in controlled and free-market rentals by income quintile (Table 4, data row).
- `data/rent_to_income_cdf_2016.do` (Stata do-file, 2 KB). Stata: distribution of renters' rent-to-income ratios (Tables 2 and 3).

**outputs/**

- `outputs/benchmark.txt` (model output, text, 58 KB). Benchmark (tenancy control): Tables 2-4 and Figure 2 (model); baseline for Tables 5-6.
- `outputs/exp1_decontrol_tax_cuts.txt` (model output, text, 58 KB). Experiment 1: decontrol with income tax cuts (Tables 5-6, row 1).
- `outputs/exp2_decontrol_transfers_renters.txt` (model output, text, 58 KB). Experiment 2: decontrol with transfers to renters and the homeless (Tables 5-6, row 2).
- `outputs/exp3_decontrol_transfers_newborns.txt` (model output, text, 58 KB). Experiment 3: decontrol, full profit tax, transfers to newborns (Tables 5-6, row 3).
- `outputs/exp4_unit_control.txt` (model output, text, 58 KB). Experiment 4: optimal unit control, discount_in_unit_control=0.27 (Tables 4-6).
- `outputs/exp4_unit_control_low_income_only.txt` (model output, text, 58 KB). Experiment 4 with controlled units for low-income renters only (2.57% welfare gain, footnote in Section 5.1).
- `outputs/extra_benchmark_higher_elasticity.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (higher rental supply elasticity).
- `outputs/extra_benchmark_higher_elasticity_run2.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (higher rental supply elasticity).
- `outputs/extra_fixed_prices_tax_cuts.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (rents and house prices held at benchmark values).
- `outputs/extra_fixed_prices_transfers_newborns.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (rents and house prices held at benchmark values).
- `outputs/extra_fixed_prices_transfers_renters.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (rents and house prices held at benchmark values).
- `outputs/extra_sr_fixed_stocks_tax_cuts.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (housing stocks held fixed; short run; bequests held fixed).
- `outputs/extra_sr_fixed_stocks_transfers_newborns.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (housing stocks held fixed; short run; bequests held fixed).
- `outputs/extra_sr_fixed_stocks_transfers_renters.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (housing stocks held fixed; short run; bequests held fixed).
- `outputs/extra_sr_fixed_stocks_vs_unit_control_tax_cuts.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; housing stocks held fixed; short run; bequests held fixed).
- `outputs/extra_sr_fixed_stocks_vs_unit_transfers_newborns.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; housing stocks held fixed; short run; bequests held fixed).
- `outputs/extra_sr_fixed_stocks_vs_unit_transfers_renters.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; housing stocks held fixed; short run; bequests held fixed).
- `outputs/extra_sr_tax_cuts.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (short run; bequests held fixed).
- `outputs/extra_sr_transfers_newborns.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (short run; bequests held fixed).
- `outputs/extra_sr_transfers_renters.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (short run; bequests held fixed).
- `outputs/extra_sr_vs_unit_control_tax_cuts.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; short run; bequests held fixed).
- `outputs/extra_sr_vs_unit_control_transfers_newborns.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; short run; bequests held fixed).
- `outputs/extra_sr_vs_unit_control_transfers_renters.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (measured against the unit-control economy; short run; bequests held fixed).
- `outputs/extra_tax_cuts_higher_elasticity.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (higher rental supply elasticity).
- `outputs/extra_transfers_bottom_renters.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (transfers to bottom-quintile renters).
- `outputs/extra_transfers_by_income_and_age.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (transfers to all households by income and age).
- `outputs/extra_transfers_renters_higher_elasticity.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (higher rental supply elasticity).
- `outputs/extra_transfers_two_bottom_renters.txt` (model output, text, 58 KB). Additional simulation from an earlier revision, not reported in the final paper (transfers to bottom-two-quintile renters).
- `outputs/grid_unit_control_label16.txt` (model output, text, 58 KB). Experiment 4 grid point (discount label 16); used to choose the welfare-maximizing discount.
- `outputs/grid_unit_control_label24.txt` (model output, text, 58 KB). Experiment 4 grid point (discount label 24); used to choose the welfare-maximizing discount.
- `outputs/grid_unit_control_label25.txt` (model output, text, 58 KB). Experiment 4 grid point (discount label 25); used to choose the welfare-maximizing discount.
- `outputs/grid_unit_control_label25_run2.txt` (model output, text, 58 KB). Separate run of a discount grid point (not used in the paper).
- `outputs/grid_unit_control_label26.txt` (model output, text, 58 KB). Experiment 4 grid point (discount label 26); used to choose the welfare-maximizing discount.
- `outputs/grid_unit_control_label28_run2.txt` (model output, text, 58 KB). Separate run of a discount grid point (not used in the paper).
- `outputs/grid_unit_control_label29.txt` (model output, text, 58 KB). Experiment 4 grid point (discount label 29); used to choose the welfare-maximizing discount.
- `outputs/grid_unit_control_label30.txt` (model output, text, 58 KB). Experiment 4 grid point (discount label 30); used to choose the welfare-maximizing discount.
- `outputs/grid_unit_control_label31.txt` (model output, text, 58 KB). Experiment 4 grid point (discount label 31); used to choose the welfare-maximizing discount.
- `outputs/grid_unit_control_label31_run2.txt` (model output, text, 58 KB). Separate run of a discount grid point (not used in the paper).
- `outputs/grid_unit_control_label32.txt` (model output, text, 58 KB). Experiment 4 grid point (discount label 32); used to choose the welfare-maximizing discount.

**plots/**

- `plots/Figure_1_rent_path_diagram.eps` (EPS figure, 41 KB). Figure 1 (vector EPS), produced by `figure_1_rent_path_diagram.py`.
- `plots/Figure_1_rent_path_diagram.pdf` (PDF figure, 16 KB). Figure 1 (vector PDF), produced by `figure_1_rent_path_diagram.py`.
- `plots/Figure_2a_homeownership_by_age.eps` (EPS figure, 37 KB). Figure 2, panel (a) (vector EPS), produced by `figure_2_homeownership_and_LTV.py`.
- `plots/Figure_2a_homeownership_by_age.pdf` (PDF figure, 13 KB). Figure 2, panel (a) (vector PDF), produced by `figure_2_homeownership_and_LTV.py`.
- `plots/Figure_2b_LTV_by_age_group.eps` (EPS figure, 37 KB). Figure 2, panel (b) (vector EPS), produced by `figure_2_homeownership_and_LTV.py`.
- `plots/Figure_2b_LTV_by_age_group.pdf` (PDF figure, 12 KB). Figure 2, panel (b) (vector PDF), produced by `figure_2_homeownership_and_LTV.py`.
- `plots/figure_1_rent_path_diagram.py` (Python script, 4 KB). Python: draws Figure 1 (conceptual diagram, no data).
- `plots/figure_2_homeownership_and_LTV.py` (Python script, 5 KB). Python: draws Figure 2 from data/ and the benchmark output file.

The GitHub repository also contains `writing/`, the LaTeX source of the paper; it is not part of the replication package.
