This file shows you how to run the simulations in "Can Rent Control Outperform Decontrol? A Welfare Analysis", by Shahar Rotberg and Lin Zhang.

Before running each simulation you will have to run the following commands:

Write the command in the line below and press enter:
gfortran can_rent_control_outperform_decontrol_a_welfare_analysis_Rotberg_Zhang_v3.f90 -I/usr/local/include -lm -mcmodel=medium -fopenmp -g -o can_rent_control_outperform_decontrol_a_welfare_analysis

Write the command in the line below and press enter:
./can_rent_control_outperform_decontrol_a_welfare_analysis

At the very top of the Fortran code there is a module titled "Global_Vars". At the top of this module you will see eight variables, which we describe below how to set for each simulation.

For generating the benchmark, please set:

	remove_controlled_market=0
        remove_free_market=0
        random_assignment=0
        redistribute_as_income_tax_reduction=0
        govt_get_full_profits=0
        give_lump_sum_to_renters=0      
        give_to_bottom_of_distribution=0
        distribute_to_below_these_productivities=9
        distribution_to_below_these_age=1
        is_counterfactual=0

Experiments 1, 2, 3, and 4

1. For Experiment 1, please set:

	remove_controlled_market=1
        remove_free_market=0
        random_assignment=0
        redistribute_as_income_tax_reduction=1
        govt_get_full_profits=0
        give_lump_sum_to_renters=0      
        give_to_bottom_of_distribution=0
        distribute_to_below_these_productivities=9
        distribution_to_below_these_age=1
        is_counterfactual=1


2. Experiment 2:

	remove_controlled_market=1
        remove_free_market=0
        random_assignment=0
        redistribute_as_income_tax_reduction=0
        govt_get_full_profits=0
        give_lump_sum_to_renters=1
        give_to_bottom_of_distribution=0
        distribute_to_below_these_productivities=9
        distribution_to_below_these_age=1
        is_counterfactual=1

3. Experiment 3:

	remove_controlled_market=1
        remove_free_market=0
        random_assignment=0
        redistribute_as_income_tax_reduction=0
        govt_get_full_profits=0
        give_lump_sum_to_renters=1      
        give_to_bottom_of_distribution=0
        distribute_to_below_these_productivities=9
        distribution_to_below_these_age=60
        is_counterfactual=1

4. Experiment 4:

	remove_controlled_market=1
        remove_free_market=0
        random_assignment=0
        redistribute_as_income_tax_reduction=0
        govt_get_full_profits=1
        give_lump_sum_to_renters=1   
        give_to_bottom_of_distribution=0
        distribute_to_below_these_productivities=9
        distribution_to_below_these_age=1
        is_counterfactual=1
        