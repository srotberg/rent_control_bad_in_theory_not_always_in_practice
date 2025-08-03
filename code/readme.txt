This file shows you how to run the simulations in "Can Rent Control Outperform Decontrol? A Welfare Analysis", by Shahar Rotberg and Lin Zhang.

Before running each simulation you will have to run the following commands:

Write the command in the line below and press enter:
gfortran can_rent_control_outperform_decontrol_a_welfare_analysis_Rotberg_Zhang_v2.f90 -I/usr/local/include -lm -mcmodel=medium -fopenmp -g -o can_rent_control_outperform_decontrol_a_welfare_analysis

Write the command in the line below and press enter:
./can_rent_control_outperform_decontrol_a_welfare_analysis

At the very top of the Fortran code there is a module titled "Global_Vars". At the top of this module you will see eight variables, which we describe below how to set for each simulation.

For generating the benchmark set:

	remove_controlled_market=0
        remove_free_market=0
        random_assignment=0
        distribute_profits_to_everyone=0
        rent_discount=0.025
        include_premium_to_depreciate=1
        is_counterfactual=0

Experiments 1, 2, 3, and 4 in which excess government revenues are rebated using income tax cuts:

a. For generating the full decontrol simulation set:

	remove_controlled_market=1
        remove_free_market=0
        random_assignment=0
        distribute_profits_to_everyone=0
        rent_discount=0.025
        include_premium_to_depreciate=1
        is_counterfactual=1

b. For generating the unit control simulation, please set:

	remove_controlled_market=0
        remove_free_market=0
        random_assignment=1
        distribute_profits_to_everyone=0
        rent_discount=0.025
        include_premium_to_depreciate=1
        is_counterfactual=1
       
c. For generating the tenancy control simulation in which tenancy control only curbs housing over-consumption please set:

	remove_controlled_market=0
        remove_free_market=1
        random_assignment=0
        distribute_profits_to_everyone=0
        rent_discount=0.006
        include_premium_to_depreciate=1
        is_counterfactual=1

d. For generating the full decontrol simulation in which there is no premium faced by free-market renters who want to reduce their housing consumption please set:

	remove_controlled_market=1
        remove_free_market=0
        random_assignment=0
        distribute_profits_to_everyone=0
        rent_discount=0.025
        include_premium_to_depreciate=0
        is_counterfactual=1
        
For Experiment 5 in which excess government revenues are rebated using lump-sum transfers to renters only:

a. For generating the full decontrol simulation set:

	remove_controlled_market=1
        remove_free_market=0
        random_assignment=0
        distribute_profits_to_everyone=1
        rent_discount=0.025
        include_premium_to_depreciate=1
        is_counterfactual=1

b. For generating the unit control simulation, please set:

	remove_controlled_market=0
        remove_free_market=0
        random_assignment=1
        distribute_profits_to_everyone=1
        rent_discount=0.025
        include_premium_to_depreciate=1
        is_counterfactual=1
       
c. For generating the tenancy control simulation in which tenancy control only curbs housing over-consumption please set:

	remove_controlled_market=0
        remove_free_market=1
        random_assignment=0
        distribute_profits_to_everyone=1
        rent_discount=0.025
        include_premium_to_depreciate=1
        is_counterfactual=1