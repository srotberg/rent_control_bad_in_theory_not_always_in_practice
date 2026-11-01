This file shows you how to run the simulations in "Rent Control? Bad in Theory, Not Always in Practice", by Shahar Rotberg and Lin Zhang (European Economic Review, manuscript EEREV-D-26-00102R2).

All simulations use one source file:
    rent_control_bad_in_theory_not_always_in_practice_Rotberg_Zhang_v11.f90

-------------------------------------------------------------------------
1. Compiling and running
-------------------------------------------------------------------------

Requirements: gfortran with OpenMP (tested with GNU Fortran 13.3 on Linux). No other libraries are needed.

Compile (run this in the "code" folder):
gfortran rent_control_bad_in_theory_not_always_in_practice_Rotberg_Zhang_v11.f90 -I/usr/local/include -lm -mcmodel=medium -fopenmp -g -o rent_control_bad_in_theory_not_always_in_practice

Run:
./rent_control_bad_in_theory_not_always_in_practice

The compiler prints warnings about array bounds inside code paths that are switched off in all of the paper's simulations. They can be ignored.

The program writes its results to the folder it is run from. The main results file is "outputs<simulation name>.txt"; it also writes .dat files (value functions, stationary distributions, prices) that later simulations read.

IMPORTANT: run the benchmark first. Every counterfactual reads the benchmark's .dat files, so the benchmark and all counterfactuals must be run in the same folder.

-------------------------------------------------------------------------
2. Choosing a simulation
-------------------------------------------------------------------------

At the very top of the Fortran code there is a module titled "Global_Vars". Each simulation is selected by setting the switches below, recompiling, and running. Switches not listed keep the values they have in the distributed code.

Benchmark (tenancy control):

    remove_controlled_market=0
    random_assignment=0
    redistribute_as_income_tax_reduction=1
    give_lump_sum_to_renters=0
    govt_get_full_profits=0
    is_counterfactual=0

    Output: outputs_control_sorting_benchmark_marginal_profits.txt

Experiment 1: full decontrol, income tax cuts

    remove_controlled_market=1
    redistribute_as_income_tax_reduction=1
    give_lump_sum_to_renters=0
    govt_get_full_profits=0
    random_assignment=0
    is_counterfactual=1

    Output: outputs0_control_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt

Experiment 2: full decontrol, lump-sum transfers to renters and the homeless

    remove_controlled_market=1
    redistribute_as_income_tax_reduction=0
    give_lump_sum_to_renters=1
    govt_get_full_profits=0
    random_assignment=0
    is_counterfactual=1

    Output: outputs0_control_GE_counter_marginal_pi_lump_sum_renters.txt

Experiment 3: full decontrol, full taxation of excess landlord profits, transfers to newborns only

    remove_controlled_market=1
    redistribute_as_income_tax_reduction=0
    give_lump_sum_to_renters=1
    govt_get_full_profits=1
    random_assignment=0
    is_counterfactual=1

    Output: outputs0_control_GE_counter_marginal_pi_lump_sum_young_pi_full_tax.txt

Experiment 4: optimal unit control (exempt share held at its benchmark level)

    remove_controlled_market=0
    redistribute_as_income_tax_reduction=1
    give_lump_sum_to_renters=0
    govt_get_full_profits=0
    random_assignment=1
    is_counterfactual=1
    discount_in_unit_control=0.27

    Output: outputs28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt

    Note: the number at the start of the file name is ceiling(100*discount_in_unit_control) computed in single precision, so discount_in_unit_control=0.27 produces a file labelled "28". The welfare-maximizing discount was found by running this experiment over a grid of values of discount_in_unit_control; the grid's results are the files outputsNN_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top.txt (NN = 16, 24, ..., 32) in the "outputs" folder.

Experiment 4 with income conditioning (controlled units assigned to low-income renters only; the 2.57% welfare gain reported in the text)

    Same switches as Experiment 4, plus:
    assign_controlled_units_to_low_income_renters_only=1
    kick_households_out=1

    Output: outputs28_discount_control_random_GE_counter_marginal_pi_inc_tax_reduction_pi_to_top_controlled_rentals_to_low_income_only.txt

-------------------------------------------------------------------------
3. Run times
-------------------------------------------------------------------------

Each output file reports its own run time in its second line ("hours to finish simulaion"). On the multi-core workstation used for the paper:
    Benchmark                          about 8 hours
    Experiments 1, 2, 3                about 6 to 9 hours each
    Experiment 4 (each grid point)     about 1.5 to 7 days
    Experiment 4, income conditioning  about 6.5 days
