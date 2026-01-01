    module Global_Vars

		implicit none
        
        ! Variables to set (5 variables)
        integer, parameter :: remove_controlled_market=0
        integer, parameter :: remove_free_market=0
        integer, parameter :: random_assignment=0
        
        integer, parameter :: is_counterfactual=1       
        
        real(8), parameter :: share_of_free_market_rent=&
                            (1-is_counterfactual)*(1-0.69528965484644489)+is_counterfactual*0.36 !(1-0.69528965484644489)
                            
        ! *****************************************************************************
        ! ****************************************************************************
                            
        real(8), parameter :: share_of_free_market_rent_calibrated=(1-0.69528965484644489)
        
        integer, parameter :: upload_random_assignment_vars=0
        integer, parameter :: benchmark_has_random_assignment=0

        integer, parameter :: two_rental_markets=1
        integer, parameter :: include_rent_decrease=1
        integer, parameter :: include_shocks_to_free_market_rent=1
        integer, parameter :: include_rental_depreciation=1
        integer, parameter :: re_do_simulation_with_fixed_parameters=1
        
        integer, parameter :: include_moving_back=1
        integer, parameter :: include_homelessness=1
        
        integer, parameter :: rerun_benchmark_with_change=0
        integer, parameter :: partial_equilibrium=0
        integer, parameter :: adjust_rents=1
        integer, parameter :: adjust_house_prices=1
        integer, parameter :: adjust_taxes=1
        integer, parameter :: adjust_bequests=1
                        
        character*100 :: file_number
        character*100 :: old_results
        character*100 :: benchmark_simulation
        character*100 :: long_run_simulation
        character*100 :: old_transition_resuls
        
        integer, parameter :: calibrate_to_rent_control=1
        integer, parameter :: use_GKKOC_process=0
        integer, parameter :: compute_support=1
        integer, parameter :: compute_full_govt_budget=1
        integer, parameter :: benchmark_has_two_rental_markets=1
        integer, parameter :: include_rental_depreciation_benchmark=1
        
        integer, parameter :: lowest_housing_grid_point=-include_moving_back-include_homelessness
        integer, parameter :: second_lowest_housing_grid_point=-include_homelessness
        integer, parameter :: third_lowest_housing_grid_point=0
        integer, parameter :: cannot_move_back_age=35

        integer, parameter :: use_old_results=is_counterfactual*1+(1-is_counterfactual)*0
        integer, parameter :: use_old_distribution=0
        integer, parameter :: hold_distribution_fixed=0
        integer, parameter :: skip_vf_and_dist=0
    
        integer, parameter :: provide_subsidy=0
        integer, parameter :: give_lump_sum_to_owners=1
        integer, parameter :: give_lump_sum_to_renters=1
        
        integer, parameter :: add_net_worth_bequest=1
        integer, parameter :: do_internal_computations=1
        
        integer, parameter :: life_span=60
        integer, parameter :: retirement_span=20
        
        integer, parameter :: rent_shock_grid_size=include_shocks_to_free_market_rent*7+(1-include_shocks_to_free_market_rent)*1
        
        integer, parameter :: set_max_tracking_to_1=0             
        integer, parameter :: max_tracking=set_max_tracking_to_1+(1-set_max_tracking_to_1)*&
                            (include_rent_decrease*(life_span-40)+&
                            (1-include_rent_decrease)*(include_shocks_to_free_market_rent*rent_shock_grid_size+&
                            (1-include_shocks_to_free_market_rent)*1))
        integer, parameter :: max_tracking_benchmark=life_span-40
        integer, parameter :: include_shocks_to_free_market_rent_benchmark=1
        integer, parameter :: rent_shock_grid_size_benchmark=include_shocks_to_free_market_rent_benchmark*7
        
        integer, parameter :: have_probablistic_death=0
        integer, parameter :: check_tax_incidence=0
        integer, parameter :: check_elascitity=0
                    
        real(8) :: average_owner_occupied_house_size_calibrated=0.90548276779194659
                    
        integer, parameter :: calibrate_rental_management_costs=1
        integer, parameter :: infinite_elasticity_of_rental_supply=0                        
        integer, parameter :: other_rental_supply_elasticity=0                                                                                                                                                                                                                                                                                                                          

        real(8) :: free_market_rent_increase_due_to_shock
                                                                                                                                                 
        integer, parameter :: calibrate_only_rental_management_costs=0
                                                                                                                                                                     
        integer, parameter :: tax_system_type=2                                         
                                                                    
        integer, parameter :: solve_transition_dynamics=is_counterfactual*0+(1-is_counterfactual)*0                                                                                                             
        integer, parameter :: use_old_transition_results=1
        integer, parameter :: transition_length=(1-solve_transition_dynamics)*1+solve_transition_dynamics*100
                                                                                                                                                    
        integer :: negative_consumption
        
        real(8), parameter :: minimum_downpayment=0.2
        real(8), parameter :: min_down_of_smallest=0.15
        real(8), parameter :: min_down_of_second_smallest=0.1
                                                  
        integer, parameter :: include_housing_depreciation_in_housing_costs=0 
        real(8), parameter :: fraction_of_property_taxes_in_GDS=1        
        real(8), parameter :: max_debt_to_income_ratio=0.39     
        real(8), parameter :: max_debt_to_income_ratio_below_80=0.39
        real(8), parameter :: fraction_of_average_house_price_data=real(500000)/real(767336) 
        real(8), parameter :: himmelberg_etal_rent_to_price=0.0524
        real(8), parameter :: free_market_rent_to_house_price=himmelberg_etal_rent_to_price
        integer, parameter :: calibrate_size_of_smallest_house_hhld_can_buy=1
        integer, parameter :: calibrate_smallest_house_size=1 
        real(8), parameter :: income_gini_data=0.49
        integer, parameter :: aftr_how_mny_lps_start_update=0
       
        real(8) :: stopping_rule_housing_market=random_assignment*2*10.d0**(-2)+(1-random_assignment)*1*10.d0**(-2)
        real(8) :: stopping_rule_rental_market=is_counterfactual*2*10.d0**(-2)+(1-is_counterfactual)*5*10.d0**(-4)
                                                           
        real(8), parameter :: stopping_rule_value_function=5*10.d0**(-8)
        real(8) :: stopping_rule_stationary_distribution=10.d0**(-8)
        real(8) :: stopping_rule_govt_budget=1*10.d0**(-2)
                                                                
        real(8) :: weight_on_housing_price=0.05
                                                                                  
        real(8), parameter :: risk_aversion_consumption=2
        real(8), parameter :: risk_aversion_housing=2
        
        real(8) :: relative_share_of_housing=0.21113320516022088
        real(8) :: distance_relative_share_of_housing
        real(8) :: relative_share_of_housing_adjuster=5*10.d0**(-2)
        real(8) :: stopping_rule_relative_share_of_housing=10.d0**(-3)
        
        real(8), parameter :: FRED_housing_services_to_GDP=0.254
                
        real(8) :: advantage_to_owning=0
        real(8) :: advantage_to_owning_adjuster=0 !10.d0**(-1)
        real(8) :: distance_advantage_to_owning
        real(8) :: stopping_rule_advantage_to_owning=10.d0**(-4)
        
        real(8) :: housing_status_grid_expander=1.47
        real(8) :: housing_status_grid_expander_adjuster=1*10.d0**(-1)
        real(8) :: distance_housing_status_grid_expander
        real(8) :: stopping_rule_housing_status_grid_expander=10.d0**(-3)
        real(8), parameter :: SCF_homeownership_rate=0.665
        integer, parameter :: index_of_smallest_house_hhld_can_buy=3
        integer, parameter :: housing_status_grid_size=7
        real(8) :: largest_house_size=1.65
        
        real(8) :: smallest_house_size
        real(8) :: smallest_rental_relative_to_lowest_income=1.59
        real(8) :: smallest_house_size_adjuster=3*10.d0**(-1)
        real(8) :: distance_smallest_house_size
        real(8) :: stopping_rule_smallest_house_size=10.d0**(-3) 
        integer, parameter :: target_30_perc=1
        real(8), parameter :: renters_spend_over_X_data=target_30_perc*0.41+(1-target_30_perc)*0.151  
        
        real(8), parameter :: avg_household_income_data=real(109704)
        
        real(8) :: size_move_back=include_moving_back*0.27
        real(8) :: size_move_back_adjuster=10.d0**(-1)
        real(8) :: distance_size_move_back
        real(8) :: stopping_rule_size_move_back=10.d0**(-3)
        real(8), parameter :: fraction_living_with_parents_data=0.223 
        real(8), parameter :: cost_of_homelessness_to_city=real(90270)/avg_household_income_data
        
        real(8) :: size_homeless=include_moving_back*0.062
        real(8) :: size_homeless_adjuster=10.d0**(-1)
        real(8) :: distance_size_homeless
        real(8) :: stopping_rule_size_homeless=10.d0**(-3)
        real(8), parameter :: fraction_homeless_data=0.0026 
               
        real(8) :: discount_factor=0.961056858436398795
        real(8) :: discount_factor_adjuster=5*10.d0**(-2)
        real(8) :: distance_discount_factor
        real(8) :: stopping_rule_discount_factor=10.d0**(-3)
        integer, parameter :: target_households_with_mortgage=0
        real(8), parameter :: SFS_households_with_mortgage=0.55
        real(8), parameter :: SCF_mortgage_to_gross_housing_wealth=0.215  
        integer, parameter :: positive_financial_grid_size=100
        integer, parameter :: negative_financial_grid_size=100
        
        real(8) :: discount_factor_on_child=160
        real(8) :: discount_factor_on_child_adjuster=0
        real(8) :: distance_discount_factor_on_child
        real(8) :: stopping_rule_discount_factor_on_child=10.d0**(-3)
        integer, parameter :: target_bequests=0
        real(8), parameter :: homeownership_rate_retired_data=0.751
        real(8), parameter :: bequests_to_net_wealth_data=0.022
        
        real(8) :: max_initial_endowment=7
        real(8) :: max_initial_endowment_adjuster=0
        real(8) :: distance_max_initial_endowment
        real(8) :: stopping_rule_max_initial_endowment=10.d0**(-3)
                
        real(8) :: proportion_inheritance=add_net_worth_bequest*0.15228260849771139
        real(8) :: proportion_inheritance_adjuster=0.01
        real(8) :: proportion_inheritance_weight=0.5
        real(8) :: distance_proportion_inheritance
        real(8) :: stopping_rule_proportion_inheritance=5*10.d0**(-4)
        real(8) :: previous_proportion_inheritance
        
        integer, parameter :: min_age_inheritance=1
        integer, parameter :: max_age_inheritance=60        
        integer, parameter :: include_exogenous_endowment=0
                                                
        real(8) :: disutility_of_landlords=24.66662599
                                                                         
        real(8) :: rental_management_costs=3.2595523918066526E-027
        real(8) :: distance_rental_management_costs
        real(8) :: rental_management_costs_adjuster=1*10.d0**(-2)
        real(8) :: stopping_rule_rental_management_costs=10.d0**(-3)
                
        real(8) :: labor_income_tax_rate=0.87740784950554330
        real(8) :: labor_income_tax_rate_adjuster=5*10.d0**(-1)
        real(8) :: stopping_rule_labor_income_tax_rate=10.d0**(-3)
        
        real(8), parameter :: average_labor_income_tax_data=0.1962 
                
        real(8) :: curvature_of_labor_income_taxes=0.9275
        
        real(8), parameter :: housing_depreciation_rate=0.006
        real(8), parameter :: deterioration_controlled_units=0.0055       
        real(8), parameter :: fraction_in_free_market_data=0.29
        real(8), parameter :: rent_control_premium_data=0.070508056727008812115446
        real(8), parameter :: rent_decrease_per_year=&
            include_rent_decrease*((1-random_assignment)*0.01365+random_assignment*deterioration_controlled_units)
        
        real(8), dimension(0:random_assignment) :: probability_rental_opportunity
        real(8) :: set_rental_opportunity_probability_adjuster=0.01
        real(8) :: set_rental_opportunity_probability=0.10284144057805887
        integer :: max_years_random=max_tracking+1
            
        real(8) :: average_years_in_same_rental_data=8.2
        
        real(8) :: fixed_cost_to_moving=0 !(real(1000))/avg_household_income_data
        real(8) :: fixed_cost_to_moving_adjuster=0 
        real(8) :: distance_fixed_cost_to_moving
        real(8) :: stopping_rule_fixed_cost_to_moving=5*10.d0**(-6)

        real(8), parameter :: empirical_housing_supplpy_elasticity=0.89
        real(8), parameter :: elasticity_of_rental_supply_data=0.3
            
        real(8), parameter :: fraction_to_repay=0.017
        real(8), parameter :: average_fraction_of_mortgage_repaid=0.017
        real(8), parameter :: mortgage_origination_cost=(real(750))/avg_household_income_data
        real(8), parameter :: penalty_months_of_interest=real(1)/real(4)
        
        real(8), parameter :: investment_income_tax=0.15
        real(8), parameter :: fraction_of_capital_income_taxable=0.5                  
        
        real(8) :: consumption_tax=0.13
        real(8), dimension(1-solve_transition_dynamics:transition_length) :: property_tax=0.0044
        real(8) :: property_tax_on_rental_housing=is_counterfactual*check_tax_incidence*0.01
        real(8), parameter :: property_tax_benchmark=0.0044
        
        real(8), parameter :: mortgage_interest_gap=0.0019
                    
        real(8), parameter :: cost_of_selling=0.06
        real(8), parameter :: cost_of_buying=0.02
                                                  
        real(8), parameter :: life_cycle_component_a=0.923405
        real(8), parameter :: life_cycle_component_b=0.0900592
        real(8), parameter :: life_cycle_component_c=-0.0016101
                                                       
        integer, parameter :: financial_grid_size=positive_financial_grid_size+negative_financial_grid_size
                                          
        integer :: fin_num_of_internal=15
        real(8), dimension(financial_grid_size) :: financial_grid
        real(8), dimension(negative_financial_grid_size) :: financial_grid_distance_negative
        real(8), dimension(financial_grid_size) :: financial_grid_distance_positive
                
        integer, parameter :: max_amortization_years=2  
                            
        real(8), parameter :: max_times_labour_income_saved=30
                                              
        real(8) :: increase_in_labour_income_tax=is_counterfactual*0
        
        real(8), dimension(1-solve_transition_dynamics:transition_length) :: housing_price
        
        real(8), dimension(transition_length) :: distance_housing_market=1,distance_rental_market=1
        
        real(8) :: rental_market_adjuster
                
        integer, parameter :: employment_grid_size=7
        integer, dimension(employment_grid_size) :: initial_endowment
        real(8), dimension(employment_grid_size) :: relative_endowments
        real(8), dimension(employment_grid_size) :: fraction_with_positive_wealth
        real(8) :: value_of_initial_assets,value_of_bequests
        real(8), dimension(life_span) :: value_of_bequests_by_age,bequests_mass_by_age
                                
        real(8), dimension(life_span,employment_grid_size) :: log_employment_productivity_grid
        real(8), dimension(life_span,employment_grid_size-1) :: log_employment_productivity_grid_mid_point
               
        real(8), dimension(life_span,employment_grid_size) :: employment_grid
        
        real(8), dimension(0:include_homelessness,life_span,employment_grid_size,employment_grid_size) :: employment_process
        real(8), dimension(employment_grid_size,employment_grid_size) :: intergenerational_employment_process
        real(8), dimension(employment_grid_size) :: unconditional_employment_probability
                                                   
        real(8), dimension(include_shocks_to_free_market_rent*rent_shock_grid_size+(1-include_shocks_to_free_market_rent)*1) :: &
            probability_of_rent_shock,shock_to_rent
                                                                
        real(8), dimension(life_span) :: survival_probability
        
        real(8), dimension(life_span) :: actual_survival_probability=(/&
                    0.999057437351439000,0.999047139252070000,0.999028513848316000,0.999002128606662000,&
                    0.998970763757824000,0.998937236843630000,0.998900839709677000,0.998862511711195000,&
                    0.998819889500737000,0.998765300260856000,0.998697769246064000,0.998622532351874000,&
                    0.998539241380058000,0.998442850308492000,0.998336586169898000,0.998207374825142000,&
                    0.998037528945133000,0.997822653735056000,0.997576926602050000,0.997324218973517000,&
                    0.997068772325292000,0.996794620528817000,0.996494883671402000,0.996169835096225000,&
                    0.995823254343122000,0.995465271640568000,0.995096642058342000,0.994716361630707000,&
                    0.994315713178366000,0.993882585782557000,0.993410578928887000,0.992905301041901000,&
                    0.992374176625162000,0.991819550283253000,0.991233213804662000,0.990602982230484000,&
                    0.989915066398680000,0.989137337543070000,0.988242183811962000,0.987190308049321000,&
                    0.985988594591617000,0.984710191376507000,0.983398867771029000,0.981995487585663000,&
                    0.980451723560690000,0.978705510497093000,0.976724572479724000,0.974471978843212000,&
                    0.971939291805028000,0.969180349260568000,0.966224979609251000,0.962747562676668000,&
                    0.958864040672779000,0.954589251428842000,0.949853785336017000,0.944555081427097000,&
                    0.938727524131536000,0.932235985994338000,0.924182154238224000,0.000000000000000000/)
                    
        real(8), dimension(life_span) :: actual_household_size=(/1.95,2.01,2.08,2.15,2.25,2.37,2.49,2.64,&
                    2.81,2.87,3.05,3.16,3.28,3.35,3.40,3.45,3.46,3.45,3.45,3.43,3.39,3.39,3.37,3.28,3.26,&
                    3.21,3.19,3.09,3.02,3.00,2.93,2.85,2.78,2.69,2.65,2.54,2.47,2.41,2.37,2.30,2.25,2.19,&
                    2.13,2.12,2.10,2.08,2.03,2.01,2.00,2.00,1.98,1.92,1.90,1.88,1.86,1.86,1.86,1.82,1.79,1.72/)
                    
        real(8), dimension(life_span) :: scaled_household_size
                                                
        real(8), dimension(life_span) :: labour_deterministic_efficiency
        
        real(8), parameter :: risk_free_rate_data=0.0277
    
        real(8), dimension(transition_length) :: risk_free_rate=risk_free_rate_data
        
        real(8), parameter :: prepayment_penalty_starts_from=0.15*(1+fraction_to_repay+risk_free_rate_data+mortgage_interest_gap)+&
                                                            fraction_to_repay
    
        real(8), parameter :: financial_grid_expander_positive=1.5
        real(8) :: financial_grid_expander_negative=1.5
                                
        integer :: largest_possible_financial_index,largest_possible_mortgage_index
        real(8) :: value_of_house_without_min_down_payment
                                                            
        real(8), dimension(lowest_housing_grid_point:housing_status_grid_size-1,0:1,max_tracking+1,1-two_rental_markets:2) :: &
            housing_status_utility_function
        real(8), dimension(lowest_housing_grid_point:housing_status_grid_size-1) :: housing_size
        
        real(8), dimension(lowest_housing_grid_point:housing_status_grid_size-1):: housing_status_grid_distance
        integer, parameter :: largest_house_size_hhld_can_rent=(housing_status_grid_size-1)
        real(8) :: size_of_smallest_house_hhld_can_buy=0.15
                                                                                                          
        integer :: largest_housing_size_consumed_in_equilibrium,largest_financial_chosen_in_equilibrium,&
            largest_years_holding_onto_asset_in_equilibrium,smallest_financial_chosen_in_equilibrium
        real(8) :: fraction_with_largest_housing_size_consumed_in_eqilibrium,&
            fraction_with_largest_years_holding_onto_asset_in_eqilibrium,fraction_with_smallest_financial_in_eqilibrium,&
            largest_financial_chosen_in_equilibrium_calibrated,smallest_financial_chosen_in_equilibrium_calibrated
                                
        real(8), dimension(1-solve_transition_dynamics:transition_length) :: total_housing_demand
        real(8), dimension(transition_length,(1-two_rental_markets):1) :: total_rent_paid
        real(8), dimension(transition_length) :: total_housing_spending
            
        real(8), dimension(1-solve_transition_dynamics:transition_length,(1-two_rental_markets):1) :: &
            total_rental_demand,total_rental_supply
                    
        real(8), dimension(financial_grid_size) :: consumption_vector
        real(8), dimension(financial_grid_size) :: flow_utility_vector
        real(8), dimension(financial_grid_size) :: future_utility_vector
        
        real(8) :: flow_utility_internal,future_utility_internal
        
        real(8), parameter :: min_consumption_guaranteed=0
        real(8), parameter :: minimum_consumption_lowest_income=0
                    
        real(8) :: rent_to_housing_price=himmelberg_etal_rent_to_price
        
        real(8) :: rent_to_price_adjuster=0.5
        real(8) :: distance_rent_to_price
        real(8) :: stopping_rule_rent_to_price=10.d0**(-3)
                                                                                                                                   
        real(8), dimension(1-solve_transition_dynamics:transition_length,(1-two_rental_markets):1) :: rent
        real(8), dimension(life_span) :: average_rent_by_age,mass_of_renters_by_age

        real(8), dimension(transition_length) :: total_imputed_rents
        real(8), dimension(transition_length) :: total_renters_income
                                                   
        real(8) :: rental_management_costs_calibrated=2.9157271938684660E-002
                                                               
        real(8), dimension(1-two_rental_markets:1) :: rent_calibrated=1.1817290765807111
    
        real(8), dimension(1-two_rental_markets:2) :: average_rent_calibrated=1.1817290765807111
                                   
        real(8), dimension(1-two_rental_markets:1) :: long_run_rent=1.1817290765807111
                                 
        real(8) :: calibrated_stationary_housing_price=13
                                
        real(8) :: long_run_stationary_housing_price=13
                                        
        real(8) :: calibrated_housing_stock=5
                                
        real(8) :: long_run_housing_stock=5
                                                                            
        real(8) :: c_min_calibrated=5.2
        
        real(8) :: c_1_calibrated=4
        
        real(8) :: cutoff_housing_investment_calibrated=0.11
                                                                          
        real(8) :: net_govt_revenue_constraint=5
                                                                                              
        real(8) :: distance_govt_budget
                                          
        real(8), dimension(1-two_rental_markets:1) :: total_rental_supply_calibrated=5.5844219551867003
        real(8), dimension(1-benchmark_has_two_rental_markets:1) :: total_rental_supply_upload=5.5844219551867003
                                                                                            
        real(8) :: average_labor_income_calibrated=2.0168354163605922
                                                                                                           
        real(8) :: calibrated_risk_free_rate=risk_free_rate_data
        real(8), dimension(transition_length) :: mortgage_interest_rate
                                                                                                                                              
        real(8) :: slope_of_housing_stock_guess,slope_of_housing_price_guess,slope_of_rent_guess
                                                                              
        real(8), dimension(1-solve_transition_dynamics:transition_length) :: housing_stock
        
        real(8), parameter :: empirical_land_to_housing_stock=0.314395015 
        real(8) :: land_to_housing_stock
        real(8) :: c_min,c_1  
        real(8) :: cutoff_housing_investment
        real(8), dimension(2) :: cutoff_housing_investment_bisection                 
        real(8), parameter :: stopping_rule_cutoff_housing_investment=10.d0**(-5) 
        real(8) :: distance_cutoff_housing_investment 
        real(8), dimension(transition_length) :: housing_investment,housing_investment_value 
        
        real(8) :: land_value
        
        real(8) :: gross_wage_income,wage_income_tax_paid,investment_income,&
            investment_income_tax_paid,total_household_working_wealth,gross_housing_wealth,&
            housing_depreciation_expenditures,rent_spending,total_household_wealth,&
            property_taxes_paid,spending_on_housing,rent_subsidy,after_tax_wealth_net_of_expenses,originate_new_mortgage
            
        real(8), dimension(1-solve_transition_dynamics:transition_length) :: lump_sum_transfer
            
        real(8) :: optimal_after_tax_wealth_net_of_expenses
                            
        real(8) :: optimal_gross_wage_income_owner,optimal_wage_income_tax_paid_owner,&
                    optimal_after_tax_wealth_net_of_expenses_owner,optimal_consumption_owner,optimal_rent_subsidy_owner

        real(8) :: optimal_gross_wage_income_renter,optimal_wage_income_tax_paid_renter,&
                    optimal_after_tax_wealth_net_of_expenses_renter,optimal_consumption_renter,optimal_rent_subsidy_renter
                    
        real(8) :: optimal_gross_wage_income_renter_renter,optimal_wage_income_tax_paid_renter_renter,&
                    optimal_after_tax_wealth_net_of_expenses_renter_renter,optimal_consumption_renter_renter,&
                    optimal_rent_subsidy_renter_renter
                    
        real(8) :: optimal_gross_wage_income_renter_owner,optimal_wage_income_tax_paid_renter_owner,&
                    optimal_after_tax_wealth_net_of_expenses_renter_owner,optimal_consumption_renter_owner
                    
        real(8) :: optimal_gross_wage_income_owner_owner_stay,optimal_wage_income_tax_paid_owner_owner_stay,&
                    optimal_after_tax_wealth_net_of_expenses_owner_owner_stay,optimal_consumption_owner_owner_stay
                    
        real(8) :: optimal_gross_wage_income_owner_owner_move,optimal_wage_income_tax_paid_owner_owner_move,&
                    optimal_after_tax_wealth_net_of_expenses_owner_owner_move,optimal_consumption_owner_owner_move
                    
        real(8) :: optimal_gross_wage_income_owner_renter,optimal_wage_income_tax_paid_owner_renter,&
                    optimal_after_tax_wealth_net_of_expenses_owner_renter,optimal_after_tax_wealth_owner_renter,&
                    optimal_consumption_owner_renter,optimal_rent_subsidy_owner_renter
                    
        integer :: optimal_space_lived_in_owner,optimal_space_lived_in_renter,&
                    optimal_space_lived_in_renter_renter,optimal_space_lived_in_renter_owner,&
                    optimal_space_lived_in_owner_renter,optimal_space_lived_in_owner_owner_move,&
                    optimal_space_lived_in_owner_owner_stay,optimal_years_holding_onto_asset_owner,&
                    optimal_years_holding_onto_asset_renter,optimal_years_holding_onto_asset_renter_renter,&
                    optimal_years_holding_onto_asset_renter_owner,optimal_years_holding_onto_asset_owner_owner_stay,&
                    optimal_years_holding_onto_asset_owner_owner_move,optimal_years_holding_onto_asset_owner_renter,&
                    optimal_household_can_consume_owner,optimal_household_can_consume_renter,&
                    optimal_household_can_consume_renter_renter,optimal_household_can_consume_renter_owner,&
                    optimal_household_can_consume_owner_owner_stay,optimal_household_can_consume_owner_owner_move,&
                    optimal_household_can_consume_owner_renter
                                                            
        integer, dimension(3) :: optimal_owner_indeces,optimal_renter_indeces,&
                                 optimal_owner_owner_move_indeces,optimal_owner_owner_stay_indeces,&
                                 optimal_owner_renter_indeces,optimal_renter_renter_indeces,optimal_renter_owner_indeces
                                    
        integer :: time_index,age_index,employment_index,years_left_on_mortgage_index,&
                   housing_status_index,space_lived_in_index,space_lived_in_index_feasible,&
                   years_holding_onto_asset_index,rental_market_index,years_in_rental,rental_opportunity_index
                                                                   
        integer :: future_financial_index,future_mortgage_index,future_housing_status_index,&
                   future_years_of_amortization_index,future_employment_index,future_rent_shock_index,&
                   future_rent_control_index,future_rental_opportunity_index
                    
        integer :: past_employment_index_1,past_employment_index_2
        
        type :: jagged_matrix_transition_real 
            real(8), dimension(:,:,:,:,:,:,:), allocatable  :: vector_transition_real
        end type jagged_matrix_transition_real
            
        type :: jagged_matrix_transition_integer
            integer, dimension(:,:,:,:,:,:,:), allocatable  :: vector_transition_integer
        end type jagged_matrix_transition_integer
        
        type :: jagged_matrix_youngest
            real(8), dimension(:,:,:,:,:), allocatable  :: vector_youngest_real
        end type jagged_matrix_youngest
        
        type :: jagged_matrix_cpu_real
            real(8), dimension(:,:,:,:,:,:,:), allocatable  :: vector_cpu_real
        end type jagged_matrix_cpu_real
        
        type :: jagged_matrix_real 
            real(8), dimension(:,:,:,:,:,:), allocatable  :: vector_real
        end type jagged_matrix_real
        
        type(jagged_matrix_transition_real), dimension(1-two_rental_markets:2) :: value_function,&
            policy_function_wage_income_tax_paid,policy_function_consumption,&
            policy_function_space_lived_in_size,policy_function_rent_subsidy,&
            frac_mov_optimal_1,distribution_guess,distribution_implied,distribution_stationary
                    
        type(jagged_matrix_transition_integer), dimension(1-two_rental_markets:2) :: &
            policy_function_financial_index,policy_function_housing_status_index,policy_function_space_lived_in_index,&
            policy_function_years_of_amortization_index,policy_function_years_holding_onto_asset,fin_index_frac_mov_to_optimal
           
        integer, parameter :: max_CPUs=40
           
        type(jagged_matrix_cpu_real), dimension(1-two_rental_markets:2) :: distribution_implied_private
           
        type(jagged_matrix_real), dimension(1-benchmark_has_two_rental_markets:2) :: &
            value_function_benchmark,distribution_stationary_benchmark
            
        type(jagged_matrix_youngest), dimension(1-two_rental_markets:2) :: value_function_guess
        
        type(jagged_matrix_transition_real), dimension(1-benchmark_has_two_rental_markets:2) :: &
            distribution_to_fetch,policy_function_space_lived_in_size_to_fetch
            
        real(8) :: value_function_renter,value_function_renter_renter,value_function_renter_owner,&
                value_function_owner,value_function_owner_owner_stay,value_function_owner_owner_move,&
                value_function_owner_renter                                
        
        real(8), dimension(transition_length,101) :: rent_to_income_cdf,rent_to_income_ratios
                                
        real(8) :: distance_value_function
                            
        real(8) :: val_fun_internal
        
        real(8) :: largest_value_function_value
        integer, dimension(5) :: distance_value_function_indeces_youngest
                                                                   
        real(8) :: slope_of_financial_internal,slope_of_space_lived_in_internal
        
        real(8) :: dis_prev_index_fin,dis_next_index_fin,dis_prev_index_space_lived_in,dis_next_index_space_lived_in
        
        real(8) :: dis_internal_from_optimal_fin
                
        real(8) :: financial_internal,space_lived_in_internal
        
        real(8) :: consumption_internal_optimal_1
                                                                                   
        real(8) :: frac_mov_to_another_cell_1
                                                                            
        integer :: fin_index_frac_mov_to
                                        
        real(8) :: largest_value
        
        integer :: choose_to_own
                                                                             
        real(8) :: distance_distribution=1
        
        real(8), dimension(life_span) :: cohort_population
        real(8) :: total_population
        
        integer :: i,j,k,m,loop_index,age_group
        
        real(8) :: distance_calibration=1     
        
        real(8), dimension(transition_length) :: &
            model_homeownership_rate,model_financial,model_financial_renters,model_gross_housing_wealth,&
            model_net_housing_wealth,model_mortgages,model_total_net_wealth,model_GDP_incl_imput_rent,&
            model_fraction_of_homeowners_with_mortgage,model_total_investment_income,model_output,model_GDP,&
            model_average_labour_income,model_homeownership_rate_young,model_mortgages_young,&
            model_total_workers,model_average_labour_income_tax,model_average_rental_unit_size,&
            model_average_owner_occupied_unit_size,mass_of_owners,mass_of_owners_with_mortgage,model_total_efficiency_units,&
            mass_of_renters,model_bequests,model_fraction_of_homeowners_with_mortgage_over_80_perc,&
            model_average_net_wealth,model_housing_value_per_capita,model_fraction_of_property_tax_paid_by_renters,&
            model_wealth_of_owners,model_wealth_of_renters,model_average_financial,model_mass_with_positive_financial,&
            model_homeownership_rate_retired,model_homeownership_rate_oldest,model_fraction_living_in_controlled,&
            model_fraction_living_with_parents,model_average_rent_to_income_ratio,&
            model_fraction_cannot_consume_market_housing,model_fraction_homeless,model_fraction_homeless_not_lowest_employment,&
            model_fraction_living_with_parents_above_max_age,model_average_housing_cost_to_income_ratio
            
        real(8), dimension(transition_length,employment_grid_size) :: model_fraction_living_with_parents_by_employment
            
        real(8), dimension(transition_length,12) :: model_LTV_by_age_group,mass_LTV_by_age_group
        
        real(8), dimension(transition_length,employment_grid_size) :: &
            average_age_of_homeowner_by_employment,mass_homeowners_by_employment
            
        real(8), dimension(transition_length,1-two_rental_markets:1) :: &
            model_average_income_by_market,mass_by_market,model_average_age_by_market,model_average_rental_unit_size_by_market,&
            model_average_rent_spending_by_market
            
        real(8), dimension(transition_length,life_span,employment_grid_size,1-two_rental_markets:1) :: &
            model_share_in_market_by_age_and_employment,model_share_with_parents_by_age_and_employment
            
        real(8), dimension(transition_length,life_span,employment_grid_size) :: mass_by_age_and_employment
            
        real(8), dimension(transition_length,employment_grid_size) :: model_share_in_controlled_market_by_employment
                        
        real(8), dimension(transition_length,life_span) :: model_share_in_controlled_market_by_age,mass_renters_by_age,&
            model_share_in_free_market_by_age,homelessness_by_age
                    
        real(8), dimension(transition_length,(1-two_rental_markets):2) :: model_average_rent,model_years_in_same_rental
                                                                                    
        real(8), dimension(transition_length) :: total_constrcution_profits,construction_company_profits,&
            total_rental_company_profits,total_profits
                                                                
        real(8), dimension(transition_length) :: frac_in_support,frac_in_support_youngest,frac_in_support_retired,&
            frac_in_support_using_current_dist,frac_in_support_homeowners,frac_in_support_renters,&
            frac_in_support_incumbent_renters,mass_homeowners,mass_renters,mass_incumbent_renters
            
        real(8), dimension(transition_length,life_span) :: frac_in_support_by_age
        
        real(8), dimension(transition_length,employment_grid_size,1-two_rental_markets:3) :: &
            frac_in_support_by_tenure_and_employment,mass_by_tenure_and_employment
                                            
        real(8), dimension(employment_grid_size) :: model_ss_benefits,model_lifetime_income_conditional_on_last_period_shock
            
        real(8) :: model_average_lifetime_income_conditional_on_last_period_shock
            
        real(8), dimension(employment_grid_size) :: probability_came_from_past_employment_index_1,&
            probability_came_from_past_employment_index_2
                                                
        integer :: in_retirement,not_allowed_to_move_back,newborn_household
        
        integer :: at_least_one_period_before_retirement,has_mortgage,can_get_mortgage,can_be_homeless
        
        real(8) :: max_mortgage_payments_as_fraction_of_income  
        
        real(8), dimension(transition_length,employment_grid_size) :: &
            welfare_by_employment,welfare_by_employment_youngest,welfare_by_employment_youngest_old_dist,&
            mass_by_employment,mass_by_employment_youngest,MID_share_by_employment,wealth_by_employment,&
            fraction_with_mortgage,fraction_renters_by_employment,mass_of_renters_by_employment,&
            average_age_of_homeowner,average_income,average_housing_costs,mass_inheritance,&
            average_consumption,average_house_inheritance,average_house_non_inheritance,&
            mass_non_inheritance,average_tenure_duration_of_renter_by_employment
            
        real(8), dimension(transition_length,employment_grid_size,3) :: &
            welfare_by_employment_and_tenure,mass_by_employment_and_tenure,&
            mortgage_by_employment_and_tenure,wealth_by_employment_and_tenure,&
            consumption_by_employment_and_tenure,housing_by_employment_and_tenure
            
        real(8), dimension(transition_length,life_span) :: mortgage_to_house_value_ratio_of_owners_by_age,&
            mass_mortgage_to_house_value_ratio_of_owners_by_age
                                 
        real(8), dimension(transition_length,employment_grid_size,3,life_span/3) :: &
            welfare_by_employment_tenure_and_age,mass_by_employment_tenure_and_age,mortgage_by_employment_tenure_and_age
            
        real(8), dimension(transition_length,employment_grid_size) :: mortgage_by_employment,mass_by_mortgage_and_employment
                                    
        real(8), dimension(transition_length,life_span) :: welfare_by_age
        
        real(8), dimension(transition_length,employment_grid_size,life_span) :: &
            welfare_by_employment_and_age,mass_by_employment_and_age
                        
        real(8), dimension(transition_length,life_span) :: homeownership_by_age,mass_by_age,housing_size_by_age
            
        real(8), dimension(transition_length,employment_grid_size) :: homeownership_by_employment,housing_size_by_employment
                        
        real(8), dimension(transition_length,employment_grid_size,1-two_rental_markets:3) :: &
            tenure_by_employment,housing_size_by_employment_and_tenure
            
        integer, parameter :: include_retirees_in_income_cdf=1
        integer, parameter :: number_of_quintiles=5
            
        real(8), dimension(transition_length,number_of_quintiles,1-two_rental_markets:3) :: tenure_by_income_quintile
            
        real(8), dimension(transition_length,number_of_quintiles) :: welfare_by_income_quintile_youngest
            
        real(8), dimension(transition_length,employment_grid_size,life_span) :: &
            housing_size_by_employment_and_age,consumption_by_employment_and_age,wealth_by_employment_and_age
            
        real(8), dimension(transition_length,life_span) :: wealth_by_age_renter,mass_by_age_and_renter,income_by_age_renter,&
            consumption_by_age_renter,housing_consumption_by_age_renter,consumption_by_age
                
        real(8), dimension(transition_length,financial_grid_size,lowest_housing_grid_point:housing_status_grid_size-1,&
            (1-two_rental_markets):max_amortization_years) :: wealth_matrix,wealth_matrix_saved,wealth_matrix_gini
                    
        integer :: wealth_counter
        
        real(8), dimension(transition_length,life_span,employment_grid_size,(1-two_rental_markets):max_amortization_years) :: &
            income_matrix,income_matrix_saved,income_matrix_gini
                    
        integer :: income_counter
        
        integer, parameter :: wealth_brackets=5
        
        real(8) :: sd_log_income_1
        
        real(8), dimension(wealth_brackets) :: model_wealth_of_top_x_perc,pop_of_top_wealth_x_perc,&
            distance_pop_of_top_wealth_x_perc,stopping_rule_wealth_dist
                            
        integer, parameter :: number_of_thresholds=4
        real(8) :: max_wealth,min_wealth_gini,min_income_gini
        integer, dimension(3) :: max_wealth_index,min_wealth_index_gini,min_income_index_gini
        
        real(8), dimension(transition_length) :: gini_coefficient_numerator,gini_coefficient,gini_coefficient_income
        
        real(8), dimension(transition_length,2) :: gini_coefficient_cumulative_wealth,gini_coefficient_cumulative_income
        
        real(8), dimension(transition_length,number_of_thresholds) :: &
            mass_of_renters_at_housing_spending,mass_of_homeowners_at_housing_spending,&
            frac_of_inc_spent_on_hous_threshold
            
        real(8), dimension(transition_length,life_span,number_of_thresholds) :: model_rent_burden_by_age
                                        
        real(8) :: fraction_of_income_spent_on_housing
        
        integer :: thread_num,thread_index,total_number_of_threads,choose_to_be_homeless_1
                        
        real(8), dimension(transition_length) :: total_govt_revenues,govt_revenues_from_labour_income,&
            govt_revenues_from_consumption,govt_revenues_from_investment_income,govt_revenues_from_property_tax,&
            govt_spending_on_ss,net_govt_revenues,govt_rent_subsidies,govt_lump_sum_transfers,&
            govt_spending_homelessness,govt_lump_sum_transfers_to_renters,net_govt_revenues_1
            
        real(8), dimension(transition_length) :: average_welfare_of_youngest_cohort,average_welfare_of_all_cohorts,&
            average_welfare_of_retired
        
        real(8), dimension(transition_length) :: aggregate_consumption,aggregate_housing_consumption,&
            aggregate_mortgage_consumption,aggregate_housing_transaction_costs,aggregate_rent_spending,&
            aggregate_origination_costs,aggregate_prepayment_costs,aggregate_moving_costs
            
        real(8), dimension(transition_length,life_span*financial_grid_size*employment_grid_size*(1+max_amortization_years)) :: &
            model_incomes_earned,model_incomes_earned_saved,model_incomes_earned_youngest,model_incomes_earned_saved_youngest,&
            model_mass_of_households_by_income_earned,model_cdf_of_income_earned,&
            model_mass_of_households_by_income_earned_youngest,model_cdf_of_income_earned_youngest
                        
        real(8), dimension(number_of_quintiles-1) :: income_cutoffs_of_quintiles
        real(8), dimension(number_of_quintiles) :: mass_of_quintile
        real(8), dimension(number_of_quintiles) :: mass_of_quintile_youngest
                        
        real(8) :: largest_housing_market_distance,largest_rental_market_distance
                                        
        real(8) :: x
        real(8) :: large_negative_num
        real(8) :: large_num=10.d0**(4)
        
        real(8) :: cut_off_1,cut_off_2
        
        real(8) ::  value_function_iteration_start_time,value_function_iteration_stop_time
        
        real(8) :: simulation_start_time,simulation_stop_time
        
        integer :: loop_count,loop_count_gini,loop_count_dist,counter,loop_over_future_rent_shocks
        integer :: simulation_ended
        
        integer :: financial_index_copy,financial_index_copy_1
        integer :: amortization_used_dist,years_holding_onto_asset_dist_used,housing_status_index_dist_used
        real(8) :: fraction_copy
        real(8) :: sum_of_distribution,sum_of_distribution_not_move_back,sum_of_distribution_youngest,sum_distribution_can_move
                                                
        !$omp threadprivate(gross_wage_income)
        !$omp threadprivate(spending_on_housing)
        !$omp threadprivate(wage_income_tax_paid)
        !$omp threadprivate(total_household_working_wealth)
        !$omp threadprivate(investment_income)
        !$omp threadprivate(investment_income_tax_paid)
        !$omp threadprivate(rent_spending)
        !$omp threadprivate(rent_subsidy)
        !$omp threadprivate(total_household_wealth)
        !$omp threadprivate(property_taxes_paid)
        !$omp threadprivate(gross_housing_wealth)
        !$omp threadprivate(housing_depreciation_expenditures)
        !$omp threadprivate(after_tax_wealth_net_of_expenses)
        !$omp threadprivate(originate_new_mortgage)
        !$omp threadprivate(years_in_rental)
        
        !$omp threadprivate(consumption_vector)
        !$omp threadprivate(largest_possible_financial_index)
        !$omp threadprivate(flow_utility_vector)
        !$omp threadprivate(future_utility_vector)
        
        !$omp threadprivate(future_employment_index)
        !$omp threadprivate(space_lived_in_index)
        !$omp threadprivate(space_lived_in_index_feasible)
        !$omp threadprivate(future_housing_status_index)
        !$omp threadprivate(future_mortgage_index)
        !$omp threadprivate(future_financial_index)
        !$omp threadprivate(future_years_of_amortization_index)
        !$omp threadprivate(future_rent_shock_index)
        !$omp threadprivate(rental_market_index)  
        !$omp threadprivate(future_rent_control_index)
        !$omp threadprivate(future_rental_opportunity_index)
        
        !$omp threadprivate(loop_count)
        !$omp threadprivate(loop_over_future_rent_shocks)
        !$omp threadprivate(thread_num)
        !$omp threadprivate(thread_index)
        !$omp threadprivate(largest_value)
        !$omp threadprivate(i)
        
        !$omp threadprivate(financial_index_copy)
        !$omp threadprivate(financial_index_copy_1)
        !$omp threadprivate(amortization_used_dist)
        !$omp threadprivate(years_holding_onto_asset_dist_used)
        !$omp threadprivate(housing_status_index_dist_used)
        !$omp threadprivate(fraction_copy)
        
        !$omp threadprivate(dis_prev_index_space_lived_in)
        !$omp threadprivate(dis_next_index_space_lived_in)
        !$omp threadprivate(dis_prev_index_fin)
        !$omp threadprivate(dis_next_index_fin)
        !$omp threadprivate(dis_internal_from_optimal_fin)
        !$omp threadprivate(slope_of_space_lived_in_internal)
        !$omp threadprivate(slope_of_financial_internal)

        !$omp threadprivate(flow_utility_internal)
        !$omp threadprivate(future_utility_internal)
        !$omp threadprivate(val_fun_internal)
        !$omp threadprivate(space_lived_in_internal)
        !$omp threadprivate(financial_internal)
        !$omp threadprivate(fin_index_frac_mov_to)
        !$omp threadprivate(frac_mov_to_another_cell_1)
        !$omp threadprivate(consumption_internal_optimal_1)
        
        !$omp threadprivate(max_mortgage_payments_as_fraction_of_income)
        
        !$omp threadprivate(optimal_after_tax_wealth_net_of_expenses)
        
        !$omp threadprivate(value_function_owner)
        !$omp threadprivate(optimal_owner_indeces)
        !$omp threadprivate(optimal_consumption_owner)
        !$omp threadprivate(optimal_space_lived_in_owner)
        !$omp threadprivate(optimal_gross_wage_income_owner)
        !$omp threadprivate(optimal_wage_income_tax_paid_owner)
        !$omp threadprivate(optimal_rent_subsidy_owner)
        !$omp threadprivate(optimal_after_tax_wealth_net_of_expenses_owner)
        !$omp threadprivate(optimal_years_holding_onto_asset_owner)
        !$omp threadprivate(optimal_household_can_consume_owner)
                            
        !$omp threadprivate(value_function_renter)
        !$omp threadprivate(optimal_renter_indeces)
        !$omp threadprivate(optimal_consumption_renter)
        !$omp threadprivate(optimal_space_lived_in_renter)
        !$omp threadprivate(optimal_gross_wage_income_renter)
        !$omp threadprivate(optimal_wage_income_tax_paid_renter)
        !$omp threadprivate(optimal_rent_subsidy_renter) 
        !$omp threadprivate(optimal_after_tax_wealth_net_of_expenses_renter)
        !$omp threadprivate(optimal_years_holding_onto_asset_renter)
        !$omp threadprivate(optimal_household_can_consume_renter)
                        
        !$omp threadprivate(value_function_renter_renter)
        !$omp threadprivate(optimal_renter_renter_indeces)
        !$omp threadprivate(optimal_gross_wage_income_renter_renter)
        !$omp threadprivate(optimal_wage_income_tax_paid_renter_renter)
        !$omp threadprivate(optimal_after_tax_wealth_net_of_expenses_renter_renter)
        !$omp threadprivate(optimal_consumption_renter_renter)
        !$omp threadprivate(optimal_space_lived_in_renter_renter)
        !$omp threadprivate(optimal_rent_subsidy_renter_renter)
        !$omp threadprivate(optimal_years_holding_onto_asset_renter_renter)
        !$omp threadprivate(optimal_household_can_consume_renter_renter)

        !$omp threadprivate(value_function_renter_owner)
        !$omp threadprivate(optimal_renter_owner_indeces)
        !$omp threadprivate(optimal_gross_wage_income_renter_owner)
        !$omp threadprivate(optimal_wage_income_tax_paid_renter_owner)
        !$omp threadprivate(optimal_after_tax_wealth_net_of_expenses_renter_owner)
        !$omp threadprivate(optimal_consumption_renter_owner)
        !$omp threadprivate(optimal_space_lived_in_renter_owner)
        !$omp threadprivate(optimal_years_holding_onto_asset_renter_owner)
        !$omp threadprivate(optimal_household_can_consume_renter_owner)
        
        !$omp threadprivate(value_function_owner_renter)
        !$omp threadprivate(optimal_owner_renter_indeces)
        !$omp threadprivate(optimal_gross_wage_income_owner_renter)
        !$omp threadprivate(optimal_wage_income_tax_paid_owner_renter)
        !$omp threadprivate(optimal_after_tax_wealth_net_of_expenses_owner_renter)
        !$omp threadprivate(optimal_after_tax_wealth_owner_renter)
        !$omp threadprivate(optimal_consumption_owner_renter)
        !$omp threadprivate(optimal_space_lived_in_owner_renter)
        !$omp threadprivate(optimal_rent_subsidy_owner_renter)
        !$omp threadprivate(optimal_years_holding_onto_asset_owner_renter)
        !$omp threadprivate(optimal_household_can_consume_owner_renter)
                    
        !$omp threadprivate(value_function_owner_owner_move)
        !$omp threadprivate(optimal_owner_owner_move_indeces)
        !$omp threadprivate(optimal_gross_wage_income_owner_owner_move)
        !$omp threadprivate(optimal_wage_income_tax_paid_owner_owner_move)
        !$omp threadprivate(optimal_after_tax_wealth_net_of_expenses_owner_owner_move)
        !$omp threadprivate(optimal_consumption_owner_owner_move)
        !$omp threadprivate(optimal_space_lived_in_owner_owner_move)
        !$omp threadprivate(optimal_years_holding_onto_asset_owner_owner_move)
        !$omp threadprivate(optimal_household_can_consume_owner_owner_move)
        
        !$omp threadprivate(value_function_owner_owner_stay)
        !$omp threadprivate(optimal_owner_owner_stay_indeces)
        !$omp threadprivate(optimal_gross_wage_income_owner_owner_stay)
        !$omp threadprivate(optimal_wage_income_tax_paid_owner_owner_stay)
        !$omp threadprivate(optimal_after_tax_wealth_net_of_expenses_owner_owner_stay)
        !$omp threadprivate(optimal_consumption_owner_owner_stay)
        !$omp threadprivate(optimal_space_lived_in_owner_owner_stay)
        !$omp threadprivate(optimal_years_holding_onto_asset_owner_owner_stay)
        !$omp threadprivate(optimal_household_can_consume_owner_owner_stay)
        
        !$omp threadprivate(choose_to_own)
        !$omp threadprivate(choose_to_be_homeless_1)

	end module Global_Vars
    
    program main_program

        use Global_Vars
        
        use omp_lib
        
        implicit none
        
        real(8) :: a_real_number
            
        if (is_counterfactual==1) then
            
            large_negative_num=-1.7976931348623157E+10 !-HUGE(a_real_number)
            
        else
        
            large_negative_num=-1.7976931348623157E+10
            
        end if
                
        write(*,*) "large_negative_num=", large_negative_num
        
        simulation_ended=0
        
        call set_files_names()
                                     
        if (is_counterfactual==0) then
                                    
            call calibrating_model()
                
            call write_outputs(1)
                
        elseif ((is_counterfactual==1) .AND. (solve_transition_dynamics==0)) then
        
            call long_run_counter_factual()
            
            call write_outputs(transition_length)
            
        elseif ((is_counterfactual==1) .AND. (solve_transition_dynamics==1)) then
        
            call transition_simulation()
            
            call write_outputs(1)
                        
        end if
        
        call cleanup_matrix()
                                
    end program main_program
    
    subroutine set_files_names()
    
        use Global_Vars
        
        implicit none
        
        character*100 :: file_name,string_share,sorting_or_random,string_1
        integer :: model_case,counterfactual_case_1,current_simulation_share,old_simulation_share
            
        if (random_assignment==0) then
        
            sorting_or_random='sorting'
        
        else
        
            sorting_or_random='random'
        
        end if
            
        if (remove_controlled_market==1) then
        
            current_simulation_share=0
            
        end if
        
        if (remove_free_market==1) then
        
            current_simulation_share=100
            
        end if
        
        if ((remove_controlled_market==0) .AND. (remove_free_market==0)) then
        
            current_simulation_share=int(ceiling(100*(1-share_of_free_market_rent)))
            
        end if
        
        old_simulation_share=int(ceiling(100*(1-share_of_free_market_rent_calibrated)))
                                    
        if (remove_controlled_market==1) then
        
            model_case=1
            
        elseif (remove_free_market==1) then
        
            model_case=2

         else
        
            model_case=3
        
        end if  

        counterfactual_case_1=0    
        
        if (adjust_rents==0) then
        
            counterfactual_case_1=1
        
        elseif (adjust_house_prices==0) then
        
            counterfactual_case_1=2
            
        elseif (adjust_taxes==0) then
        
            counterfactual_case_1=3
        
        elseif (adjust_bequests==0) then
        
            counterfactual_case_1=4
        
        end if 
        
        if (model_case==1) then
        
            string_share='_0'
            
        elseif (model_case==2) then
        
            string_share='_100'

        elseif (model_case>=3) then
        
            if (current_simulation_share<10) then                
                write(string_share,'(I1)') current_simulation_share   
            elseif (current_simulation_share<100) then                
                write(string_share,'(I2)') current_simulation_share   
            else            
                write(string_share,'(I3)') current_simulation_share                
            end if
        
        end if
                    
        if (is_counterfactual==0) then
        
            string_share=trim(string_share)//'_control_'//trim(sorting_or_random)   
            file_name=trim(string_share)
            file_number=trim(file_name)//'_benchmark'
                            
            old_results=file_number
            benchmark_simulation=file_number
            long_run_simulation=file_number
            old_transition_resuls=file_number
            
        else
                
            if (old_simulation_share<10) then                
                write(string_share,'(I1)') old_simulation_share
            elseif (old_simulation_share<100) then
                write(string_share,'(I2)') old_simulation_share                  
            else          
                write(string_share,'(I3)') old_simulation_share                
            end if
            
            if (upload_random_assignment_vars==1) then
            
                string_share=trim(string_share)//'_control_random_GE_counterfactual'
                old_results=trim(string_share)
                
                if (old_simulation_share<10) then                
                    write(string_share,'(I1)') old_simulation_share
                elseif (old_simulation_share<100) then
                    write(string_share,'(I2)') old_simulation_share                  
                else          
                    write(string_share,'(I3)') old_simulation_share                
                end if
                
                string_share=trim(string_share)//'_control_sorting'
                benchmark_simulation=trim(string_share)//'_benchmark'
                
            else
            
                string_share=trim(string_share)//'_control_sorting'
                old_results=trim(string_share)//'_benchmark'
                benchmark_simulation=old_results
            
            end if
                           
            long_run_simulation=old_results
            old_transition_resuls=old_results  
            
            if (current_simulation_share<10) then                
                write(string_share,'(I1)') current_simulation_share     
            elseif (current_simulation_share<100) then
                write(string_share,'(I2)') current_simulation_share                  
            else          
                write(string_share,'(I3)') current_simulation_share                
            end if
            string_share=trim(string_share)//'_control_'//trim(sorting_or_random)
            
            if (counterfactual_case_1==1) then
            
                string_share=trim(string_share)//'_rents_constant'
            
            elseif (counterfactual_case_1==2) then
            
                string_share=trim(string_share)//'_house_prices_constant'
            
            elseif (counterfactual_case_1==3) then
            
                string_share=trim(string_share)//'_taxes_constant'
            
            elseif (counterfactual_case_1==4) then
            
                string_share=trim(string_share)//'_bequests_constant'
            
            end if
            
            if (partial_equilibrium==1) then
            
                string_share=trim(string_share)//'_PE'
            
            else
            
                string_share=trim(string_share)//'_GE'
            
            end if
                        
            file_number=trim(string_share)//'_counterfactual'
                                                                
        end if
            
    end subroutine
    
    subroutine calculate_cdf_of_incomes_earned(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,time_period_used,financial_index,location,previous_index,has_house,&
            housing_status_index_used,loop_over_rent_shocks
        integer, dimension(1) :: min_income_earned_index,quintile
        real(8) :: total_income,sum_of_distribution_1,size_of_quintile=real(1)/real(number_of_quintiles)
        
        time_period_used=time_period-solve_transition_dynamics
        
        model_mass_of_households_by_income_earned(time_period,:)=0
        model_incomes_earned(time_period,:)=-1
        
        i=1
        
        sum_of_distribution_1=0
        
        write(*,*) "size_of_quintile=", size_of_quintile
        write(*,*) "computing incomes for cdf"
                              
        do age_index=1,(1-include_retirees_in_income_cdf)*(life_span-retirement_span)+include_retirees_in_income_cdf*life_span
        
            if (age_index>(life_span-retirement_span)) then
            
                in_retirement=1
                
            else
            
                in_retirement=0
                
            end if
            
            do rental_opportunity_index=0,random_assignment
                                    
                do years_left_on_mortgage_index=1-two_rental_markets,max_amortization_years
                
                    if (years_left_on_mortgage_index>1) then
                        
                        has_house=1
                        
                    else
                    
                        has_house=0
                        
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                            loop_over_rent_shocks=1
                                                    
                        end if
                        
                    end if
                                                                    
                    do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                    
                        do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                            (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                            
                            do employment_index=1,employment_grid_size
                                            
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                        has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                                                                                                                        
                                    housing_status_index_used=policy_function_housing_status_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    if (housing_status_index_used>=third_lowest_housing_grid_point) then
                                                                    
                                        if (in_retirement==1) then
                                    
                                            total_income=model_ss_benefits(employment_index)
                                            
                                        else
                                                        
                                            total_income=employment_grid(age_index,employment_index)*&
                                                labour_deterministic_efficiency(age_index)
                                                                    
                                        end if
                                                                                
                                        total_income=total_income+risk_free_rate(time_period)*&
                                            max(financial_grid(financial_index),0.d0)
                                                                                 
                                        if (i==1) then
                                            
                                            location=i
                                            
                                        else
                                        
                                            location=-1    
                                                
                                        end if
                                                    
                                        do j=1,i
                                            
                                            if (abs(model_incomes_earned(time_period,j)-total_income)<10.d0**(-4)) then
                                                    
                                                location=j
                                                    
                                            end if
                                                
                                        end do
                                                                                                    
                                        sum_of_distribution=&
                                            distribution_stationary(years_left_on_mortgage_index)%vector_transition_real&
                                            (time_period_used,age_index,financial_index,employment_index,housing_status_index,&
                                            years_holding_onto_asset_index,rental_opportunity_index)                                        
                                        
                                        sum_of_distribution_1=sum_of_distribution_1+sum_of_distribution
                                        
                                        if (sum_of_distribution>0) then
                                                                                
                                            if (i==1) then
                                                                                                                            
                                                model_mass_of_households_by_income_earned(time_period,location)=&
                                                    model_mass_of_households_by_income_earned(time_period,location)+&
                                                    sum_of_distribution
                                                        
                                                model_incomes_earned(time_period,location)=total_income
                                                    
                                                i=i+1
                                                
                                            elseif ((i>1) .AND. (location==-1)) then
                                                    
                                                model_mass_of_households_by_income_earned(time_period,i+1)=&
                                                    model_mass_of_households_by_income_earned(time_period,i+1)+&
                                                    sum_of_distribution
                                                        
                                                model_incomes_earned(time_period,i+1)=total_income
                                                    
                                                i=i+1
                                                
                                            elseif ((i>1) .AND. (location>-1)) then
                                            
                                                model_mass_of_households_by_income_earned(time_period,location)=&
                                                    model_mass_of_households_by_income_earned(time_period,location)+&
                                                    sum_of_distribution
                                                
                                            end if
                                            
                                        end if
                                                                                     
                                    end if
                                        
                                end do
                                                                
                            end do
                                                                                             
                        end do
                        
                    end do
                    
                end do
                    
            end do
            
        end do
        
        model_incomes_earned_saved(time_period,:)=model_incomes_earned(time_period,:)
        
        model_cdf_of_income_earned(time_period,:)=0
        
        i=1
        previous_index=1
        
        quintile=1
        mass_of_quintile(:)=0
                
        write(*,*) "computing incomes at each quintile"
        
        do while (i<=(life_span*financial_grid_size*employment_grid_size*years_left_on_mortgage_index))
                        
            min_income_earned_index=minloc(model_incomes_earned(time_period,:),mask=model_incomes_earned(time_period,:)>=0)
                
            if (min_income_earned_index(1)>0) then
            
                if (model_mass_of_households_by_income_earned(time_period,min_income_earned_index(1))>0)  then
                                                                                                    
                    model_cdf_of_income_earned(time_period,min_income_earned_index(1))=&
                        model_cdf_of_income_earned(time_period,previous_index)+&
                        model_mass_of_households_by_income_earned(time_period,min_income_earned_index(1))/sum_of_distribution_1
                                                            
                    if (sum(model_cdf_of_income_earned(time_period,:))>0) then
                    
                        if (floor(model_cdf_of_income_earned(time_period,min_income_earned_index(1))/size_of_quintile)>&
                            floor(model_cdf_of_income_earned(time_period,previous_index)/size_of_quintile) .AND. &
                            (model_cdf_of_income_earned(time_period,previous_index)<0.99)) then
                                            
                            income_cutoffs_of_quintiles(quintile)=model_incomes_earned(time_period,min_income_earned_index(1))
                            
                            quintile=quintile+1
                                                    
                        end if
                        
                        mass_of_quintile(quintile)=mass_of_quintile(quintile)+&
                            model_mass_of_households_by_income_earned(time_period,min_income_earned_index(1))
                        
                    end if
                                    
                    previous_index=min_income_earned_index(1)
                    
                end if
                
                model_incomes_earned(time_period,min_income_earned_index(1))=-1
                                    
            end if
                    
            i=i+1
                                                        
        end do
                 
        model_incomes_earned(time_period,:)=model_incomes_earned_saved(time_period,:)
                
        write(*,*) "sum(model_mass_of_households_by_income_earned(time_period,:)=", &
            sum(model_mass_of_households_by_income_earned(time_period,:))
                
        do i=1,number_of_quintiles
            
            write(*,*) "mass_of_quintile(i)=", mass_of_quintile(i)
            
        end do
        
        write(*,*) "sum(mass_of_quintile)=", sum(mass_of_quintile(:))
                    
    end subroutine
    
    subroutine stationary_equilibrium_statistics(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: financial_index,time_period,has_house,loop_over_rent_shocks,&
            time_period_used,housing_status_index_used,is_end_of_life,&
            financial_index_used,amortization_used,years_holding_onto_asset_used,fin_frac_used
        real(8) :: total_income,lived_in_size_used,distribution_mass_used,frac_used,bequest_2,&
            total_renters_to_consider,rent_to_income
        integer, dimension(1) :: rent_to_income_index
                
        time_period_used=time_period-solve_transition_dynamics
        
        rent_to_income_cdf(time_period,:)=0
        model_total_net_wealth(time_period)=0
        model_financial(time_period)=0
        model_mass_with_positive_financial(time_period)=0
        model_average_financial(time_period)=0
        model_financial_renters(time_period)=0
        model_output(time_period)=0
        model_total_efficiency_units(time_period)=0
        model_net_housing_wealth(time_period)=0
        model_gross_housing_wealth(time_period)=0
        model_mortgages(time_period)=0
        model_bequests(time_period)=0
        value_of_bequests_by_age(:)=0
        bequests_mass_by_age(:)=0
        mass_of_owners(time_period)=0
        total_renters_income(time_period)=0
        model_fraction_living_with_parents(time_period)=0
        model_fraction_living_with_parents_above_max_age(time_period)=0
        model_fraction_living_with_parents_by_employment(time_period,:)=0
        model_fraction_homeless(time_period)=0
        model_fraction_homeless_not_lowest_employment(time_period)=0
        model_fraction_cannot_consume_market_housing(time_period)=0
        mass_by_employment(time_period,:)=0
        
        model_total_workers(time_period)=0
        model_average_labour_income(time_period)=0
        
        total_rent_paid(time_period,:)=0
        total_housing_spending(time_period)=0
        total_imputed_rents(time_period)=0  
        model_average_rent_to_income_ratio(time_period)=0
        model_average_housing_cost_to_income_ratio(time_period)=0
          
        average_rent_by_age=0
        mass_of_renters_by_age=0
        
        do i=1,101
        
            rent_to_income_ratios(time_period,i)=(real(i)-real(1))/real(100)
            
        end do
                
        sum_of_distribution_not_move_back=0
        sum_distribution_can_move=0
                             
        do age_index=1,life_span
        
            is_end_of_life=0
            
            if (age_index==life_span) then
            
                is_end_of_life=1
                
            end if
        
            if (age_index>(life_span-retirement_span)) then
            
                in_retirement=1
                
            else
            
                in_retirement=0
                
            end if
                        
            if (age_index==1) then
            
                newborn_household=1
                
            else
            
                newborn_household=0
            
            end if
            
            do rental_opportunity_index=0,random_assignment
                    
                do years_left_on_mortgage_index=newborn_household+(1-newborn_household)*(1-two_rental_markets),&
                    newborn_household+(1-newborn_household)*max_amortization_years
                                
                    if (years_left_on_mortgage_index>1) then
                        
                        has_house=1
                        
                    else
                    
                        has_house=0
                        
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                        
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                            loop_over_rent_shocks=1
                                                    
                        end if
                        
                    end if
                                                    
                    do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                        (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                                            
                        do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                        
                            do employment_index=1,employment_grid_size
                                        
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                    has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                                                                                              
                                    lived_in_size_used=policy_function_space_lived_in_size(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                                                    
                                    housing_status_index_used=&
                                        policy_function_housing_status_index(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    financial_index_used=policy_function_financial_index(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                            
                                    amortization_used=&
                                        policy_function_years_of_amortization_index(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    years_holding_onto_asset_used=&
                                        policy_function_years_holding_onto_asset(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    frac_used=frac_mov_optimal_1(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    fin_frac_used=fin_index_frac_mov_to_optimal(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period_used,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    mass_by_employment(time_period,employment_index)=&
                                        mass_by_employment(time_period,employment_index)+distribution_mass_used
                                        
                                    if ((include_moving_back==1) .OR. (include_homelessness==1)) then
                                    
                                        if (housing_status_index_used==lowest_housing_grid_point) then
                                        
                                            model_fraction_homeless(time_period)=&
                                                model_fraction_homeless(time_period)+distribution_mass_used
                                                
                                            sum_of_distribution_not_move_back=sum_of_distribution_not_move_back+&
                                                distribution_mass_used
                                                
                                            if (employment_index .NE. 1) then
                                            
                                                model_fraction_homeless_not_lowest_employment(time_period)=&
                                                    model_fraction_homeless_not_lowest_employment(time_period)+&
                                                    distribution_mass_used
                                            
                                            end if
                                                
                                    
                                        elseif ((housing_status_index_used==second_lowest_housing_grid_point) .AND. &
                                           (include_moving_back==1)) then
                                           
                                            model_fraction_living_with_parents_by_employment&
                                                (time_period,employment_index)=&
                                                model_fraction_living_with_parents_by_employment&
                                                (time_period,employment_index)+&
                                                distribution_mass_used
                                        
                                            if (age_index>=cannot_move_back_age) then
                                            
                                                model_fraction_living_with_parents_above_max_age(time_period)=&
                                                    model_fraction_living_with_parents_above_max_age(time_period)+&
                                                    distribution_mass_used
                                            
                                            end if
                                        
                                            model_fraction_living_with_parents(time_period)=&
                                                model_fraction_living_with_parents(time_period)+distribution_mass_used
                                                
                                        else
                                        
                                            sum_of_distribution_not_move_back=sum_of_distribution_not_move_back+&
                                                distribution_mass_used
                                                
                                        end if
                                        
                                    else
                                    
                                        sum_of_distribution_not_move_back=sum_of_distribution_not_move_back+&
                                            distribution_mass_used
                                        
                                    end if
                                    
                                    if (not_allowed_to_move_back==0) then
                                        
                                        sum_distribution_can_move=sum_distribution_can_move+distribution_mass_used
                                    
                                    end if
                                                                                                   
                                    if (in_retirement==0) then
                                                            
                                        model_output(time_period)=model_output(time_period)+&
                                            (employment_grid(age_index,employment_index)*&
                                            labour_deterministic_efficiency(age_index))*distribution_mass_used
                                            
                                        model_total_efficiency_units(time_period)=&
                                            model_total_efficiency_units(time_period)+&
                                            (employment_grid(age_index,employment_index)*&
                                            labour_deterministic_efficiency(age_index))*distribution_mass_used
                                                                        
                                    end if
                                    
                                    if (age_index>(life_span-retirement_span)) then
                                
                                        total_income=model_ss_benefits(employment_index)
                                        
                                    else
                                                    
                                        total_income=employment_grid(age_index,employment_index)*&
                                            labour_deterministic_efficiency(age_index)
                                                                
                                    end if
                                           
                                    total_income=total_income+risk_free_rate(time_period)*&
                                        max(financial_grid(financial_index),0.d0)
                                        
                                    if (rent(time_period,1-two_rental_markets)*housing_size(0)>=total_income) then
                                                                        
                                        model_fraction_cannot_consume_market_housing(time_period)=&
                                            model_fraction_cannot_consume_market_housing(time_period)+distribution_mass_used
                                            
                                    end if
                                    
                                    if ((amortization_used<=1) .AND. &
                                        (housing_status_index_used>=third_lowest_housing_grid_point)) then 
                                                                                                                                                                                        
                                        fraction_of_income_spent_on_housing=(rent(time_period,amortization_used)/&
                                            ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                            (years_holding_onto_asset_used-1)))*lived_in_size_used
                                                                                                                                        
                                        if ((years_left_on_mortgage_index==0) .OR. &
                                            ((years_left_on_mortgage_index==1) .AND. (two_rental_markets==0) .AND. &
                                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1))) then
                                        
                                            fraction_of_income_spent_on_housing=fraction_of_income_spent_on_housing+&
                                                shock_to_rent(min(rent_shock_grid_size,years_holding_onto_asset_index))*&
                                                rent(time_period,years_left_on_mortgage_index)*&
                                                housing_size(housing_status_index)
                                                
                                        end if
                                            
                                        rent_to_income=fraction_of_income_spent_on_housing/total_income
                                        
                                        model_average_rent_to_income_ratio(time_period)=&
                                            model_average_rent_to_income_ratio(time_period)+&
                                            rent_to_income*distribution_mass_used
                                            
                                        model_average_housing_cost_to_income_ratio(time_period)=&
                                            model_average_housing_cost_to_income_ratio(time_period)+&
                                            rent_to_income*distribution_mass_used
                                                                                    
                                        if (rent_to_income<=1) then
                                        
                                            rent_to_income_index=minloc(rent_to_income_ratios(time_period,:),&
                                                mask=rent_to_income_ratios(time_period,:)-rent_to_income>=0)
                                                
                                        else
                                        
                                            rent_to_income_index(1)=101
                                            
                                        end if
                                                                            
                                        if (((in_retirement==1) .AND. (include_retirees_in_income_cdf==1)) .OR. &
                                            (in_retirement==0)) then
                                            
                                            rent_to_income_cdf(time_period,rent_to_income_index(1))=&
                                                rent_to_income_cdf(time_period,rent_to_income_index(1))+&
                                                distribution_mass_used
                                                
                                            total_renters_to_consider=total_renters_to_consider+distribution_mass_used
                                                                                    
                                        end if
                                        
                                        total_renters_income(time_period)=&
                                            total_renters_income(time_period)+&
                                            total_income*distribution_mass_used
                                            
                                    elseif (amortization_used>1) then
                                    
                                        fraction_of_income_spent_on_housing=&
                                            (-min(financial_grid(financial_index),0.d0)*&
                                            (mortgage_interest_rate(time_period)+average_fraction_of_mortgage_repaid)+&
                                            (property_tax(time_period)+housing_depreciation_expenditures)*&
                                            housing_price(time_period)*lived_in_size_used)
                                            
                                        fraction_of_income_spent_on_housing=&
                                            fraction_of_income_spent_on_housing/total_income
                                        
                                        model_average_housing_cost_to_income_ratio(time_period)=&
                                            model_average_housing_cost_to_income_ratio(time_period)+&
                                            fraction_of_income_spent_on_housing*distribution_mass_used
                                            
                                    end if
                                                                           
                                    model_financial(time_period)=&
                                        model_financial(time_period)+&
                                        max(financial_grid(financial_index),0.d0)*distribution_mass_used
                                        
                                    if (max(financial_grid(financial_index),0.d0)>=0) then
                                    
                                        model_mass_with_positive_financial(time_period)=&
                                            model_mass_with_positive_financial(time_period)+distribution_mass_used
                                    
                                    end if
                                                                    
                                    model_total_net_wealth(time_period)=model_total_net_wealth(time_period)+&
                                        (max(min(years_left_on_mortgage_index-1,1),0)*&
                                        (housing_price(time_period)*housing_size(housing_status_index))+&
                                        min(financial_grid(financial_index),0.d0)+&
                                        max(financial_grid(financial_index),0.d0))*distribution_mass_used
                                                                        
                                    if (years_left_on_mortgage_index>1) then
                                                                                                                                                                                                
                                        model_gross_housing_wealth(time_period)=model_gross_housing_wealth(time_period)+&
                                            housing_price(time_period)*housing_size(housing_status_index)*distribution_mass_used
                                                                                                        
                                        model_mortgages(time_period)=model_mortgages(time_period)-&
                                            min(financial_grid(financial_index),0.d0)*distribution_mass_used
                                        
                                    end if
                                                                
                                    if (age_index==life_span) then
                                    
                                        if (amortization_used<=1) then
                                    
                                            model_bequests(time_period)=model_bequests(time_period)+&
                                                (frac_used*financial_grid(fin_frac_used)+&
                                                (1-frac_used)*financial_grid(financial_index_used))*distribution_mass_used
                                                
                                        else
                                                                                
                                            model_bequests(time_period)=model_bequests(time_period)+&
                                                (housing_price(time_period)*housing_size(housing_status_index_used)+&
                                                (frac_used*financial_grid(fin_frac_used)+&
                                                (1-frac_used)*financial_grid(financial_index_used)))*distribution_mass_used
                                        
                                        end if
                                                                                                                    
                                    end if
                                    
                                    bequest_2=employment_grid(age_index,employment_index)*proportion_inheritance
                                                                                                                            
                                    value_of_bequests_by_age(age_index)=value_of_bequests_by_age(age_index)+&
                                        bequest_2*distribution_mass_used
                                        
                                    bequests_mass_by_age(age_index)=bequests_mass_by_age(age_index)+distribution_mass_used
                                                                                                                                    
                                    if (amortization_used>=2) then
                                    
                                        mass_of_owners(time_period)=mass_of_owners(time_period)+distribution_mass_used
                                        
                                        model_wealth_of_owners(time_period)=model_wealth_of_owners(time_period)+&
                                            (max(years_left_on_mortgage_index-1,0)*&
                                            housing_price(time_period)*housing_size(housing_status_index)+&
                                            min(financial_grid(financial_index),0.d0)+&
                                            max(financial_grid(financial_index),0.d0))*distribution_mass_used
                                    
                                    end if
                                    
                                    if (age_index<=(life_span-retirement_span)) then
                                    
                                        model_total_workers(time_period)=model_total_workers(time_period)+distribution_mass_used
                                        
                                        model_average_labour_income(time_period)=model_average_labour_income(time_period)+&
                                            employment_grid(age_index,employment_index)*&
                                            labour_deterministic_efficiency(age_index)*distribution_mass_used
                                            
                                    end if
                                    
                                    if (amortization_used<=1) then
                                    
                                        if (housing_status_index_used>=third_lowest_housing_grid_point) then
                                    
                                            if (include_rent_decrease==0) then
                                                                        
                                                total_rent_paid(time_period,amortization_used)=&
                                                    total_rent_paid(time_period,amortization_used)+&
                                                    (rent(time_period,amortization_used)*&
                                                    (1+(1-is_end_of_life)*free_market_rent_increase_due_to_shock)/&
                                                    ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                    (years_holding_onto_asset_used-1)))*&
                                                    lived_in_size_used*distribution_mass_used
                                                    
                                                total_housing_spending(time_period)=&
                                                    total_housing_spending(time_period)+(rent(time_period,amortization_used)*&
                                                    (1+(1-is_end_of_life)*free_market_rent_increase_due_to_shock)/&
                                                    ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                    (years_holding_onto_asset_used-1)))*&
                                                    lived_in_size_used*distribution_mass_used

                                                average_rent_by_age(age_index)=average_rent_by_age(age_index)+&
                                                    (rent(time_period,amortization_used)*&
                                                    (1+(1-is_end_of_life)*free_market_rent_increase_due_to_shock)/&
                                                    ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                    (years_holding_onto_asset_used-1)))*&
                                                    lived_in_size_used*distribution_mass_used
                                                
                                            else
                                            
                                                total_rent_paid(time_period,amortization_used)=&
                                                    total_rent_paid(time_period,amortization_used)+&
                                                    (rent(time_period,amortization_used)*&
                                                    (1+(1-amortization_used)*(1-is_end_of_life)*&
                                                    free_market_rent_increase_due_to_shock)/&
                                                    ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                    (years_holding_onto_asset_used-1)))*&
                                                    lived_in_size_used*distribution_mass_used
                                                    
                                                total_housing_spending(time_period)=&
                                                    total_housing_spending(time_period)+(rent(time_period,amortization_used)*&
                                                    (1+(1-amortization_used)*(1-is_end_of_life)*&
                                                    free_market_rent_increase_due_to_shock)/&
                                                    ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                    (years_holding_onto_asset_used-1)))*&
                                                    lived_in_size_used*distribution_mass_used

                                                average_rent_by_age(age_index)=average_rent_by_age(age_index)+&
                                                    (rent(time_period,amortization_used)*&
                                                    (1+(1-amortization_used)*(1-is_end_of_life)*&
                                                    free_market_rent_increase_due_to_shock)/&
                                                    ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                    (years_holding_onto_asset_used-1)))*&
                                                    lived_in_size_used*distribution_mass_used
                                            
                                            end if
                                                                                                                                
                                            mass_of_renters_by_age(age_index)=mass_of_renters_by_age(age_index)+&
                                                distribution_mass_used
                                                
                                        end if
                                                                                        
                                    elseif (amortization_used>1) then
                                    
                                        total_imputed_rents(time_period)=&
                                            total_imputed_rents(time_period)+model_average_rent(time_period,2)*&
                                            lived_in_size_used*distribution_mass_used
                                            
                                        total_housing_spending(time_period)=total_housing_spending(time_period)+&
                                            (-min(financial_grid(financial_index),0.d0)*&
                                            (mortgage_interest_rate(time_period)+average_fraction_of_mortgage_repaid)+&
                                            (property_tax(time_period)+housing_depreciation_rate)*&
                                            housing_price(time_period)*lived_in_size_used)*&
                                            distribution_mass_used
                                    
                                    end if
                                                                        
                                end do
                                
                            end do
                            
                        end do
                                                                            
                    end do
                    
                end do
                
            end do
            
        end do
        
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
            
        end do
        
        do i=2,101
        
            rent_to_income_cdf(time_period,i)=rent_to_income_cdf(time_period,i-1)+rent_to_income_cdf(time_period,i)
                    
        end do
                
        rent_to_income_cdf(time_period,:)=rent_to_income_cdf(time_period,:)/total_renters_to_consider
        
        model_average_rent_to_income_ratio(time_period)=&
            model_average_rent_to_income_ratio(time_period)/total_renters_to_consider
                
        model_total_investment_income(time_period)=model_financial(time_period)*risk_free_rate(time_period)
        
        model_average_financial(time_period)=model_financial(time_period)/model_mass_with_positive_financial(time_period)
        
        model_GDP(time_period)=model_output(time_period)
                    
        model_average_net_wealth(time_period)=model_total_net_wealth(time_period)/sum_of_distribution
            
        model_wealth_of_owners(time_period)=model_wealth_of_owners(time_period)/mass_of_owners(time_period)
        
        model_fraction_living_with_parents(time_period)=model_fraction_living_with_parents(time_period)/&
            sum_of_distribution_not_move_back
            
        do employment_index=1,employment_grid_size
            
            model_fraction_living_with_parents_by_employment(time_period,employment_index)=&
                model_fraction_living_with_parents_by_employment(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
        end do
            
        model_fraction_living_with_parents_above_max_age(time_period)=&
            model_fraction_living_with_parents_above_max_age(time_period)/sum_of_distribution_not_move_back
            
        model_average_housing_cost_to_income_ratio(time_period)=&
            model_average_housing_cost_to_income_ratio(time_period)/sum_of_distribution_not_move_back
            
        model_fraction_homeless(time_period)=&
            model_fraction_homeless(time_period)/sum_of_distribution_not_move_back
            
        model_fraction_cannot_consume_market_housing(time_period)=&
            model_fraction_cannot_consume_market_housing(time_period)/sum_of_distribution
                                             
        if (solve_transition_dynamics==0) then
                              
            write(*,*) "model_financial(",time_period,")=", model_financial(time_period)
            write(*,*) "model_mortgages(",time_period,")=", model_mortgages(time_period)
            write(*,*) "model_net_housing_wealth(",time_period,")=", model_net_housing_wealth(time_period)
            write(*,*) "model_gross_housing_wealth(",time_period,")=", model_gross_housing_wealth(time_period)
            write(*,*) "model_total_net_wealth(",time_period,")=", model_total_net_wealth(time_period)
            write(*,*) "model_bequests(",time_period,")=", model_bequests(time_period)
            write(*,*) "model_average_net_wealth(time_period)=", model_average_net_wealth(time_period)
            
        end if
        
        mortgage_interest_rate(time_period)=risk_free_rate(time_period)+mortgage_interest_gap
                                                
        model_average_labour_income(time_period)=model_average_labour_income(time_period)/model_total_workers(time_period)
        
        do age_index=1,life_span
        
            average_rent_by_age(age_index)=average_rent_by_age(age_index)/mass_of_renters_by_age(age_index) 
            value_of_bequests_by_age(age_index)=value_of_bequests_by_age(age_index)/bequests_mass_by_age(age_index)
        
        end do
                
        model_GDP_incl_imput_rent(time_period)=model_GDP(time_period)+total_imputed_rents(time_period)
        
        total_housing_spending(time_period)=total_housing_spending(time_period)/model_GDP(time_period)
                                                                    
    end subroutine
        
    subroutine stationary_equilibirum_homeownership_rate(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,time_period_used,financial_index_used,financial_index,has_house,l,loop_over_rent_shocks,&
            housing_status_index_used,amortization_used,years_holding_onto_asset_used,is_end_of_life
        real(8) :: lived_in_size_used,distribution_mass_used,mass_of_retired,mass_of_oldest
                
        time_period_used=time_period-solve_transition_dynamics
                        
        model_fraction_of_homeowners_with_mortgage(time_period)=0
        model_fraction_of_homeowners_with_mortgage_over_80_perc(time_period)=0
        
        model_average_owner_occupied_unit_size(time_period)=0
        model_average_rental_unit_size(time_period)=0
        model_average_rental_unit_size_by_market(time_period,:)=0
        model_average_rent_spending_by_market(time_period,:)=0
        model_years_in_same_rental(time_period,:)=0
        mass_by_market(time_period,:)=0
        mass_of_renters(time_period)=0
        model_wealth_of_renters(time_period)=0
        mass_of_owners(time_period)=0    
        mass_of_owners_with_mortgage(time_period)=0
                
        largest_housing_size_consumed_in_equilibrium=0
        fraction_with_largest_housing_size_consumed_in_eqilibrium=0
        fraction_with_largest_years_holding_onto_asset_in_eqilibrium=0
        fraction_with_smallest_financial_in_eqilibrium=0
        largest_financial_chosen_in_equilibrium=0
        smallest_financial_chosen_in_equilibrium=financial_grid_size
        largest_years_holding_onto_asset_in_equilibrium=0
        
        total_housing_demand(time_period)=0
        total_rental_demand(time_period,:)=0
                
        model_average_income_by_market(time_period,:)=0
        model_fraction_living_in_controlled(time_period)=0
        average_age_of_homeowner_by_employment(time_period,:)=0
        mass_homeowners_by_employment(time_period,:)=0
        model_average_age_by_market(time_period,:)=0
        model_share_in_controlled_market_by_age(time_period,:)=0
        model_share_in_free_market_by_age(time_period,:)=0
        homelessness_by_age(time_period,:)=0
        model_share_in_controlled_market_by_employment(time_period,:)=0
        mass_renters_by_age(time_period,:)=0
        mass_by_employment(time_period,:)=0
        mass_by_employment_youngest(time_period,:)=0
        
        model_share_in_market_by_age_and_employment(time_period,:,:,:)=0
        model_share_with_parents_by_age_and_employment(time_period,:,:,:)=0
        mass_by_age_and_employment(time_period,:,:)=0
        
        model_homeownership_rate_retired(time_period)=0
        model_homeownership_rate_oldest(time_period)=0
        mass_of_retired=0
        mass_of_oldest=0
        
        do age_index=1,life_span
        
            is_end_of_life=0
            
            if (age_index==life_span) then
            
                is_end_of_life=1
                
            end if
            
            do rental_opportunity_index=0,random_assignment
                                        
                do years_left_on_mortgage_index=(1-two_rental_markets),max_amortization_years
                                
                    if (years_left_on_mortgage_index>1) then
                        
                        has_house=1
                        
                    else
                    
                        has_house=0
                        
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                            loop_over_rent_shocks=1
                                                    
                        end if
                        
                    end if
                                    
                    do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                        (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                                            
                        do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                        
                            do employment_index=1,employment_grid_size
                                        
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                    has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                                                                                       
                                    lived_in_size_used=policy_function_space_lived_in_size(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                                                    
                                    housing_status_index_used=policy_function_housing_status_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    financial_index_used=policy_function_financial_index(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                            
                                    amortization_used=policy_function_years_of_amortization_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    years_holding_onto_asset_used=&
                                        policy_function_years_holding_onto_asset(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    if (random_assignment==1) then
                                    
                                        years_holding_onto_asset_used=min(max_years_random,years_holding_onto_asset_used)
                                        
                                    end if
                                        
                                    distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period_used,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                                                
                                    if ((housing_status_index_used>largest_housing_size_consumed_in_equilibrium) .AND. &
                                        (distribution_mass_used>0)) then
                                       
                                        largest_housing_size_consumed_in_equilibrium=housing_status_index_used
                                       
                                    end if
                                    
                                    if ((financial_index_used>largest_financial_chosen_in_equilibrium) .AND. &
                                        (distribution_mass_used>0)) then
                                    
                                        largest_financial_chosen_in_equilibrium=financial_index_used
                                       
                                    end if
                                    
                                    if ((financial_index_used<smallest_financial_chosen_in_equilibrium) .AND. &
                                        (distribution_mass_used>0)) then
                                    
                                        smallest_financial_chosen_in_equilibrium=financial_index_used
                                       
                                    end if
                                    
                                    if ((years_holding_onto_asset_used>largest_years_holding_onto_asset_in_equilibrium) .AND. &
                                        (distribution_mass_used>0)) then
                                    
                                        largest_years_holding_onto_asset_in_equilibrium=years_holding_onto_asset_used
                                       
                                    end if
                            
                                    if ((amortization_used<=1) .AND. &
                                        (housing_status_index_used>=third_lowest_housing_grid_point)) then
                                    
                                        model_average_rental_unit_size(time_period)=&
                                            model_average_rental_unit_size(time_period)+(lived_in_size_used/&
                                            (1+amortization_used*include_rent_decrease*include_rental_depreciation*&
                                            deterioration_controlled_units)**&
                                            (years_holding_onto_asset_used-1))*distribution_mass_used
                                                                            
                                        model_average_rental_unit_size_by_market(time_period,amortization_used)=&
                                            model_average_rental_unit_size_by_market(time_period,amortization_used)+&
                                            (lived_in_size_used/(1+amortization_used*include_rent_decrease*&
                                            include_rental_depreciation*deterioration_controlled_units)**&
                                            (years_holding_onto_asset_used-1))*distribution_mass_used
                                            
                                        if (include_rent_decrease==0) then
                                                                                                               
                                            model_average_rent_spending_by_market(time_period,amortization_used)=&
                                                model_average_rent_spending_by_market(time_period,amortization_used)+&
                                                (rent(time_period,amortization_used)*&
                                                (1+(1-is_end_of_life)*free_market_rent_increase_due_to_shock)/&
                                                ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                (years_holding_onto_asset_used-1)))*lived_in_size_used*distribution_mass_used
                                                
                                        else
                                        
                                            model_average_rent_spending_by_market(time_period,amortization_used)=&
                                                model_average_rent_spending_by_market(time_period,amortization_used)+&
                                                (rent(time_period,amortization_used)*&
                                                (1+(1-amortization_used)*(1-is_end_of_life)*&
                                                free_market_rent_increase_due_to_shock)/&
                                                ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                (years_holding_onto_asset_used-1)))*lived_in_size_used*distribution_mass_used
                                        
                                        end if
                                                                                                                                                                                                               
                                        model_years_in_same_rental(time_period,amortization_used)=&
                                            model_years_in_same_rental(time_period,amortization_used)+&
                                            max(years_holding_onto_asset_used,0)*distribution_mass_used
                                                                                        
                                        mass_by_market(time_period,amortization_used)=&
                                            mass_by_market(time_period,amortization_used)+distribution_mass_used
                                                                                                                           
                                        mass_of_renters(time_period)=mass_of_renters(time_period)+distribution_mass_used
                                        
                                        model_wealth_of_renters(time_period)=model_wealth_of_renters(time_period)+&
                                            max(financial_grid(financial_index),0.d0)*distribution_mass_used
                                                                                                                            
                                    elseif (amortization_used>1) then
                        
                                        model_average_owner_occupied_unit_size(time_period)=&
                                            model_average_owner_occupied_unit_size(time_period)+&
                                            lived_in_size_used*distribution_mass_used
                                            
                                        mass_of_owners(time_period)=mass_of_owners(time_period)+distribution_mass_used
                                        
                                        if (financial_grid(min(financial_index_used,negative_financial_grid_size))<0) then
                                        
                                            mass_of_owners_with_mortgage(time_period)=&
                                                mass_of_owners_with_mortgage(time_period)+distribution_mass_used
                                    
                                            model_fraction_of_homeowners_with_mortgage(time_period)=&
                                                model_fraction_of_homeowners_with_mortgage(time_period)+distribution_mass_used
                                                
                                            if (-financial_grid(min(financial_index_used,negative_financial_grid_size))/&
                                                (housing_price(time_period)*lived_in_size_used)>0.8) then
                                                
                                                model_fraction_of_homeowners_with_mortgage_over_80_perc(time_period)=&
                                                    model_fraction_of_homeowners_with_mortgage_over_80_perc(time_period)+&
                                                    distribution_mass_used

                                            end if
                                                                        
                                        end if
                                                            
                                    end if
                                    
                                    if ((housing_status_index_used==(housing_status_grid_size-1)) .AND. &
                                        (distribution_mass_used>0)) then
                                        
                                        fraction_with_largest_housing_size_consumed_in_eqilibrium=&
                                            fraction_with_largest_housing_size_consumed_in_eqilibrium+&
                                            distribution_mass_used
                                        
                                    end if
                                    
                                    if ((years_holding_onto_asset_used==largest_years_holding_onto_asset_in_equilibrium) .AND. &
                                        (distribution_mass_used>0)) then
                                        
                                        fraction_with_largest_years_holding_onto_asset_in_eqilibrium=&
                                            fraction_with_largest_years_holding_onto_asset_in_eqilibrium+&
                                            distribution_mass_used
                                        
                                    end if
                                    
                                    if ((financial_index_used==smallest_financial_chosen_in_equilibrium) .AND. &
                                        (distribution_mass_used>0)) then
                                        
                                        fraction_with_smallest_financial_in_eqilibrium=&
                                            fraction_with_smallest_financial_in_eqilibrium+&
                                            distribution_mass_used
                                        
                                    end if
                                    
                                    if (amortization_used>1) then
                                    
                                        if (housing_status_index_used>=third_lowest_housing_grid_point) then
                                    
                                            total_housing_demand(time_period)=total_housing_demand(time_period)+&
                                                lived_in_size_used*distribution_mass_used
                                                
                                        end if
                                            
                                    else
                                    
                                        total_housing_demand(time_period)=total_housing_demand(time_period)+&
                                            (lived_in_size_used/(1+amortization_used*include_rental_depreciation*&
                                            include_rent_decrease*deterioration_controlled_units)**&
                                            (years_holding_onto_asset_used-1))*distribution_mass_used
                                    
                                    end if
                                                                            
                                    mass_by_employment(time_period,employment_index)=&
                                        mass_by_employment(time_period,employment_index)+distribution_mass_used
                                        
                                    mass_by_age_and_employment(time_period,age_index,employment_index)=&
                                        mass_by_age_and_employment(time_period,age_index,employment_index)+&
                                        distribution_mass_used
                                        
                                    if (age_index>(life_span-retirement_span)) then
                                    
                                        mass_of_retired=mass_of_retired+distribution_mass_used
                                        
                                        if (age_index==life_span) then
                                        
                                            mass_of_oldest=mass_of_oldest+distribution_mass_used
                                            
                                        end if
                                        
                                    end if
                                                                         
                                    if (amortization_used<=1) then
                                    
                                        mass_renters_by_age(time_period,age_index)=&
                                            mass_renters_by_age(time_period,age_index)+distribution_mass_used
                                    
                                        if (housing_status_index_used>=third_lowest_housing_grid_point) then
                                                                                                                                            
                                            total_rental_demand(time_period,amortization_used)=&
                                                total_rental_demand(time_period,amortization_used)+(lived_in_size_used/&
                                                (1+amortization_used*include_rental_depreciation*include_rent_decrease*&
                                                deterioration_controlled_units)**(years_holding_onto_asset_used-1))*&
                                                distribution_mass_used
                                                                                            
                                            model_average_income_by_market(time_period,amortization_used)=&
                                                model_average_income_by_market(time_period,amortization_used)+&
                                                ((1-in_retirement)*employment_grid(age_index,employment_index)*&
                                                labour_deterministic_efficiency(age_index)+&
                                                in_retirement*model_ss_benefits(employment_index)+&
                                                max(financial_grid(financial_index),0.d0)*&
                                                risk_free_rate(time_period))*distribution_mass_used
                                                
                                            model_average_age_by_market(time_period,amortization_used)=&
                                                model_average_age_by_market(time_period,amortization_used)+&
                                                age_index*distribution_mass_used
                                                
                                            if (amortization_used==0) then
                                            
                                                model_share_in_free_market_by_age(time_period,age_index)=&
                                                    model_share_in_free_market_by_age(time_period,age_index)+&
                                                    distribution_mass_used
                                            
                                            end if
                                                
                                            if (amortization_used==1) then
                                            
                                                model_share_in_controlled_market_by_age(time_period,age_index)=&
                                                    model_share_in_controlled_market_by_age(time_period,age_index)+&
                                                    distribution_mass_used
                                                    
                                                model_fraction_living_in_controlled(time_period)=&
                                                    model_fraction_living_in_controlled(time_period)+&
                                                    distribution_mass_used
                                                    
                                                if (age_index==1) then
                                                    
                                                    model_share_in_controlled_market_by_employment&
                                                        (time_period,employment_index)=&
                                                        model_share_in_controlled_market_by_employment&
                                                        (time_period,employment_index)+distribution_mass_used
                                                        
                                                end if
                                                    
                                            end if    
                                            
                                            if (age_index==1) then
                                            
                                                mass_by_employment_youngest(time_period,employment_index)=&
                                                    mass_by_employment_youngest(time_period,employment_index)+&
                                                    distribution_mass_used
                                                
                                            end if
                                                                                                                                        
                                            model_share_in_market_by_age_and_employment&
                                                (time_period,age_index,employment_index,amortization_used)=&
                                                model_share_in_market_by_age_and_employment&
                                                (time_period,age_index,employment_index,amortization_used)+&
                                                distribution_mass_used
                                                
                                        elseif (housing_status_index_used==second_lowest_housing_grid_point) then
                                        
                                            model_share_with_parents_by_age_and_employment&
                                                (time_period,age_index,employment_index,amortization_used)=&
                                                model_share_with_parents_by_age_and_employment&
                                                (time_period,age_index,employment_index,amortization_used)+&
                                                distribution_mass_used

                                        elseif (housing_status_index_used==lowest_housing_grid_point) then

                                            homelessness_by_age(time_period,age_index)=&
                                                homelessness_by_age(time_period,age_index)+distribution_mass_used
                                                
                                        end if
                                                                                      
                                    else
                                    
                                        if (age_index>(life_span-retirement_span)) then
                                    
                                            model_homeownership_rate_retired(time_period)=&
                                                model_homeownership_rate_retired(time_period)+distribution_mass_used
                                                
                                            if (age_index==life_span) then
                                            
                                                model_homeownership_rate_oldest(time_period)=&
                                                    model_homeownership_rate_oldest(time_period)+distribution_mass_used
                                                    
                                            end if
                                                                                            
                                        end if
                                    
                                        average_age_of_homeowner_by_employment(time_period,employment_index)=&
                                            average_age_of_homeowner_by_employment(time_period,employment_index)+&
                                            age_index*distribution_mass_used
                                            
                                        mass_homeowners_by_employment(time_period,employment_index)=&
                                            mass_homeowners_by_employment(time_period,employment_index)+distribution_mass_used
                                            
                                    end if
                                    
                                end do
                                    
                            end do
                            
                        end do
                        
                    end do
                    
                end do
                                            
            end do
            
        end do
        
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
            
        end do
        
        if (solve_transition_dynamics==0) then
        
            write(*,*) "sum_of_distribution=", sum_of_distribution
            
        end if
                          
        model_average_rental_unit_size(time_period)=&
            model_average_rental_unit_size(time_period)/mass_of_renters(time_period)
                        
        model_average_owner_occupied_unit_size(time_period)=&
            model_average_owner_occupied_unit_size(time_period)/mass_of_owners(time_period)
                    
        model_years_in_same_rental(time_period,2)=&
            sum(model_years_in_same_rental(time_period,1-two_rental_markets:1))/&
            sum(mass_by_market(time_period,1-two_rental_markets:1))
            
        do i=1-two_rental_markets,1
            
            model_years_in_same_rental(time_period,i)=&
                model_years_in_same_rental(time_period,i)/mass_by_market(time_period,i)
                
            model_average_rental_unit_size_by_market(time_period,i)=&
                model_average_rental_unit_size_by_market(time_period,i)/mass_by_market(time_period,i)
                
            model_average_rent_spending_by_market(time_period,i)=&
                model_average_rent_spending_by_market(time_period,i)/mass_by_market(time_period,i)
                
        end do
        
        model_homeownership_rate(time_period)=mass_of_owners(time_period)/sum_of_distribution_not_move_back
        
        model_homeownership_rate_retired(time_period)=model_homeownership_rate_retired(time_period)/mass_of_retired
        model_homeownership_rate_oldest(time_period)=model_homeownership_rate_oldest(time_period)/mass_of_oldest
        
        model_fraction_of_homeowners_with_mortgage(time_period)=&
            model_fraction_of_homeowners_with_mortgage(time_period)/mass_of_owners(time_period)
            
        model_fraction_of_homeowners_with_mortgage_over_80_perc(time_period)=&
            model_fraction_of_homeowners_with_mortgage_over_80_perc(time_period)/&
            (model_fraction_of_homeowners_with_mortgage(time_period)*mass_of_owners(time_period))
            
        model_wealth_of_renters(time_period)=model_wealth_of_renters(time_period)/mass_of_renters(time_period)
            
        if (solve_transition_dynamics==0) then
            
            write(*,*) "model_homeownership_rate(time_period)", model_homeownership_rate(time_period)
            write(*,*) "model_average_rental_unit_size(time_period)=", model_average_rental_unit_size(time_period)
            write(*,*) "model_average_owner_occupied_unit_size(time_period)=", model_average_owner_occupied_unit_size(time_period)
            write(*,*) "model_average_rental_unit_size(time_period)/model_average_owner_occupied_unit_size(time_period)=", &
                model_average_rental_unit_size(time_period)/model_average_owner_occupied_unit_size(time_period)
            write(*,*) "model_fraction_of_homeowners_with_mortgage(time_period)=", &
                model_fraction_of_homeowners_with_mortgage(time_period)
                                
        end if
                
        fraction_with_largest_housing_size_consumed_in_eqilibrium=&
            fraction_with_largest_housing_size_consumed_in_eqilibrium/sum_of_distribution
            
        fraction_with_largest_years_holding_onto_asset_in_eqilibrium=&
            fraction_with_largest_years_holding_onto_asset_in_eqilibrium/sum_of_distribution
            
        fraction_with_smallest_financial_in_eqilibrium=fraction_with_smallest_financial_in_eqilibrium/sum_of_distribution
                                               
        do i=(1-two_rental_markets),1
        
            if (total_rental_demand(time_period,i)==0) then
            
                model_average_rent(time_period,i)=rent(time_period,i)
                
            else

                model_average_rent(time_period,i)=total_rent_paid(time_period,i)/total_rental_demand(time_period,i)
                
            end if
                
            model_average_income_by_market(time_period,i)=&
                model_average_income_by_market(time_period,i)/mass_by_market(time_period,i)
                
            model_average_age_by_market(time_period,i)=model_average_age_by_market(time_period,i)/mass_by_market(time_period,i)
            
        end do
        
        model_fraction_living_in_controlled(time_period)=model_fraction_living_in_controlled(time_period)/&
            mass_of_renters(time_period)
        
        do i=1-two_rental_markets,1
        
            do age_index=1,life_span
            
                do employment_index=1,employment_grid_size
                
                    model_share_in_market_by_age_and_employment(time_period,age_index,employment_index,i)=&
                        model_share_in_market_by_age_and_employment(time_period,age_index,employment_index,i)/&
                        mass_by_age_and_employment(time_period,age_index,employment_index)
                        
                    model_share_with_parents_by_age_and_employment(time_period,age_index,employment_index,i)=&
                        model_share_with_parents_by_age_and_employment(time_period,age_index,employment_index,i)/&
                        mass_by_age_and_employment(time_period,age_index,employment_index)
                
                end do
                
            end do
        
        end do
        
        do i=1,employment_grid_size
                        
            average_age_of_homeowner_by_employment(time_period,i)=&
                average_age_of_homeowner_by_employment(time_period,i)/mass_homeowners_by_employment(time_period,i)
        
        end do
        
        model_average_rent(time_period,2)=sum(total_rent_paid(time_period,:))/sum(total_rental_demand(time_period,:))
            
        do age_index=1,life_span
                                            
            model_share_in_controlled_market_by_age(time_period,age_index)=&
                model_share_in_controlled_market_by_age(time_period,age_index)/mass_renters_by_age(time_period,age_index)
                
            model_share_in_free_market_by_age(time_period,age_index)=&
                model_share_in_free_market_by_age(time_period,age_index)/mass_renters_by_age(time_period,age_index)

            homelessness_by_age(time_period,age_index)=&
                homelessness_by_age(time_period,age_index)/sum_of_distribution
                
        end do
        
        do employment_index=1,employment_grid_size
                                            
            model_share_in_controlled_market_by_employment(time_period,employment_index)=&
                model_share_in_controlled_market_by_employment(time_period,employment_index)/&
                mass_by_employment_youngest(time_period,employment_index)
                                            
        end do
                                  
        total_rental_supply(time_period,:)=0
    
        if (is_counterfactual==1) then

            do i=1,(1-two_rental_markets),-1
        
                if (infinite_elasticity_of_rental_supply==1) then
                
                    total_rental_supply(time_period,i)=total_rental_demand(time_period,i)
                        
                else

                    if (i==1) then
                    
                        if (two_rental_markets==1) then
                        
                            if (remove_free_market==1) then
                            
                                l=1
                            
                            else

                                l=0
                                
                            end if
                            
                            total_rental_supply(time_period,l)=max(((model_average_rent(time_period,l)-&
                                housing_price(time_period)+(1/(1+risk_free_rate(time_period)))*&
                                (housing_price(time_period-solve_transition_dynamics)*&
                                (1-housing_depreciation_rate-property_tax(time_period-solve_transition_dynamics)-&
                                property_tax_on_rental_housing)))/(rental_management_costs_calibrated*&
                                disutility_of_landlords)),0.d0)**(1/(disutility_of_landlords-1))
                                
                            if (remove_free_market==1) then
                            
                                total_rental_supply(time_period,l-1)=0
                            
                            elseif (remove_controlled_market==1) then
                                
                                total_rental_supply(time_period,l+1)=0

                            else
                            
                                total_rental_supply(time_period,l+1)=&
                                    (1-share_of_free_market_rent)*total_rental_supply(time_period,l)

                                total_rental_supply(time_period,l)=&
                                    share_of_free_market_rent*total_rental_supply(time_period,l)
                                                                        
                            end if
                                                                                            
                        else

                            l=1
                            
                            total_rental_supply(time_period,l)=max(((model_average_rent(time_period,l)-&
                                housing_price(time_period)+(1/(1+risk_free_rate(time_period)))*&
                                (housing_price(time_period-solve_transition_dynamics)*&
                                (1-housing_depreciation_rate-property_tax(time_period-solve_transition_dynamics)-&
                                property_tax_on_rental_housing)))/(rental_management_costs_calibrated*&
                                disutility_of_landlords)),0.d0)**(1/(disutility_of_landlords-1))

                        end if
                                                                            
                    end if
                                    
                end if

            end do
    
        else

            do i=1,(1-two_rental_markets),-1
        
                 if (infinite_elasticity_of_rental_supply==1) then
                
                    total_rental_supply(time_period,i)=total_rental_demand(time_period,i)
                        
                else

                    if (i==1) then

                        if (two_rental_markets==1) then

                            l=0

                        else

                            l=1

                        end if

                        total_rental_supply(time_period,l)=max(((model_average_rent(1,l)-&
                            housing_price(time_period)+(1/(1+risk_free_rate(time_period)))*&
                            (housing_price(time_period-solve_transition_dynamics)*(1-housing_depreciation_rate-&
                            property_tax(time_period-solve_transition_dynamics)-property_tax_on_rental_housing)))/&
                            (rental_management_costs*disutility_of_landlords)),0.d0)**(1/(disutility_of_landlords-1))

                    else
                    
                        if ((share_of_free_market_rent<1) .AND. (share_of_free_market_rent>0)) then
                                                
                            total_rental_supply(time_period,i+1)=min(total_rental_demand(time_period,i+1),&
                                total_rental_supply(time_period,l))
                                                            
                            total_rental_supply(time_period,i)=max(total_rental_supply(time_period,i)-&
                                total_rental_supply(time_period,i+1),0.d0)
                                
                        else
                        
                            total_rental_supply(time_period,i)=0
                        
                        end if
                                                                
                    end if
                 
                end if

            end do
            
        end if
                                        
    end subroutine
    
    subroutine stationary_equilibrium_wealth_distribution(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: financial_index,loop_over_complete_distribution,wealth_counter_2,time_period,wealth_of_top_x_perc_index,has_house
        real(8) :: frac_of_household_mas_to_add,sum_distribution_used
                
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
            
        end do
        
        wealth_counter=0
        
        wealth_matrix(time_period,:,:,:)=-housing_price(time_period)*housing_size(housing_status_grid_size-1)
        
        do years_left_on_mortgage_index=(1-two_rental_markets),max_amortization_years
        
            if (years_left_on_mortgage_index==2) then
            
                has_house=1
                
            else
            
                has_house=0
                
            end if
            
            do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
            
                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                    has_house*index_of_smallest_house_hhld_can_buy,(housing_status_grid_size-1)
                                                    
                    if (sum(distribution_stationary(years_left_on_mortgage_index)%&
                        vector_transition_real(time_period,:,financial_index,:,housing_status_index,:,:))>0) then
                        
                        wealth_matrix(time_period,financial_index,housing_status_index,years_left_on_mortgage_index)=&
                            min(financial_grid(financial_index),0.d0)+&
                            max(financial_grid(financial_index),0.d0)+max(min(years_left_on_mortgage_index-1,1),0)*&
                            (housing_price(time_period)*housing_size(housing_status_index))
                                                    
                        wealth_counter=wealth_counter+1    
                                                               
                    end if
                                            
                end do
                                                                
            end do
                            
        end do
                                    
        wealth_matrix_saved(time_period,:,:,:)=wealth_matrix(time_period,:,:,:)
        
        write(*,*) "begining wealth distribution" 
        
        do wealth_of_top_x_perc_index=1,(1-simulation_ended)*1+simulation_ended*(wealth_brackets-1)
        
            write(*,*) "compute for top ", 100*stopping_rule_wealth_dist(wealth_of_top_x_perc_index)
                
            wealth_matrix(time_period,:,:,:)=wealth_matrix_saved(time_period,:,:,:)
        
            pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)=0
            
            model_wealth_of_top_x_perc(wealth_of_top_x_perc_index)=0
                            
            distance_pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)=0
            
            if (wealth_of_top_x_perc_index==wealth_brackets) then
            
                loop_over_complete_distribution=1
            
            else
            
                loop_over_complete_distribution=0
            
            end if
            
            wealth_counter_2=1
                        
            do while (((loop_over_complete_distribution*wealth_counter_2)+(1-loop_over_complete_distribution)*&
                     (distance_pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)+10.d0**(-6)))&
                     <=(loop_over_complete_distribution*wealth_counter+&
                     (1-loop_over_complete_distribution)*stopping_rule_wealth_dist(wealth_of_top_x_perc_index)))
                                          
                wealth_counter_2=wealth_counter_2+1
                                
                max_wealth=maxval(wealth_matrix(time_period,:,:,:),mask=wealth_matrix(time_period,:,:,:)>&
                    -housing_price(time_period)*housing_size(housing_status_grid_size-1))
                                    
                max_wealth_index=maxloc(wealth_matrix(time_period,:,:,:),mask=wealth_matrix(time_period,:,:,:)>&
                    -housing_price(time_period)*housing_size(housing_status_grid_size-1))
                    
                
                    
                sum_distribution_used=sum(distribution_stationary(max_wealth_index(3)-two_rental_markets)%&
                    vector_transition_real(time_period,:,max_wealth_index(1),:,&
                    max_wealth_index(2)-1-include_moving_back-include_homelessness,:,:))
                                               
                if (((pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)+sum_distribution_used)/&
                    sum_of_distribution)>stopping_rule_wealth_dist(wealth_of_top_x_perc_index)) then
                                                                        
                    frac_of_household_mas_to_add=(sum_of_distribution*stopping_rule_wealth_dist(wealth_of_top_x_perc_index)-&
                        pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index))/sum_distribution_used
                        
                    distance_pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)=&
                        stopping_rule_wealth_dist(wealth_of_top_x_perc_index)+0.000001
                        
                else
                
                    frac_of_household_mas_to_add=1
                    
                    distance_pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)=&
                        pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)/sum_of_distribution
                
                end if
                
                pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)=pop_of_top_wealth_x_perc(wealth_of_top_x_perc_index)+&
                    frac_of_household_mas_to_add*sum_distribution_used
                     
                model_wealth_of_top_x_perc(wealth_of_top_x_perc_index)=model_wealth_of_top_x_perc(wealth_of_top_x_perc_index)+&
                    frac_of_household_mas_to_add*max_wealth*sum_distribution_used
                    
                wealth_matrix(time_period,max_wealth_index(1),&
                    max_wealth_index(2)-1-include_moving_back-include_homelessness,max_wealth_index(3)-two_rental_markets)=&
                    -housing_price(time_period)*housing_size(housing_status_grid_size-1)
                    
            end do
                                        
            if (solve_transition_dynamics==0) then
                
                write(*,*) "model_wealth_of_top_x_perc(",wealth_of_top_x_perc_index,")/model_net_wealth=",&
                    model_wealth_of_top_x_perc(wealth_of_top_x_perc_index)/model_total_net_wealth(time_period)
                write(*,*) "stopping_rule_wealth_dist(",wealth_of_top_x_perc_index,")=",&
                    stopping_rule_wealth_dist(wealth_of_top_x_perc_index)
                                                        
            end if
                                      
        end do
        
        if (solve_transition_dynamics==0) then
        
            write(*,*) "file number=", file_number
            
        end if
        
    end subroutine
    
    subroutine calculate_gini_coefficient_wealth(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: financial_index,time_period,has_house
        real(8) :: scalar_1=10,sum_distribution_used
        
        loop_count_gini=0
        
        wealth_matrix_gini(time_period,:,:,:)=-scalar_1*housing_price(time_period)*housing_size(housing_status_grid_size-1)
        
        do years_left_on_mortgage_index=(1-two_rental_markets),max_amortization_years
        
            if (years_left_on_mortgage_index==2) then
            
                has_house=1
                
            else
            
                has_house=0
                
            end if
        
            do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
            
                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                    has_house*index_of_smallest_house_hhld_can_buy,(housing_status_grid_size-1)
                                        
                    if (sum(distribution_stationary(years_left_on_mortgage_index)%&
                        vector_transition_real(time_period,:,financial_index,:,housing_status_index,:,:))>0) then
                        
                        wealth_matrix_gini(time_period,financial_index,housing_status_index,years_left_on_mortgage_index)=&
                            min(financial_grid(financial_index),0.d0)+&
                            max(financial_grid(financial_index),0.d0)+max(min(years_left_on_mortgage_index-1,1),0)*&
                            (housing_price(time_period)*housing_size(housing_status_index))
                           
                        loop_count_gini=loop_count_gini+1
                                                               
                    end if
                                            
                end do
                                                                
            end do
                            
        end do
                                            
        gini_coefficient_numerator(time_period)=0
        gini_coefficient_cumulative_wealth(time_period,:)=0
            
        counter=1
                        
        do while (counter<=loop_count_gini)
            
            min_wealth_gini=minval(wealth_matrix_gini(time_period,:,:,:),mask=wealth_matrix_gini(time_period,:,:,:)>&
                -scalar_1*housing_price(time_period)*housing_size(housing_status_grid_size-1))
                
            min_wealth_index_gini=minloc(wealth_matrix_gini(time_period,:,:,:),mask=wealth_matrix_gini(time_period,:,:,:)>&
                -scalar_1*housing_price(time_period)*housing_size(housing_status_grid_size-1))
                
            sum_distribution_used=sum(distribution_stationary(min_wealth_index_gini(3)-two_rental_markets)%&
                vector_transition_real(time_period,:,min_wealth_index_gini(1),:,&
                min_wealth_index_gini(2)-1-include_moving_back-include_homelessness,:,:))
                                           
            gini_coefficient_cumulative_wealth(time_period,2)=gini_coefficient_cumulative_wealth(time_period,1)+&
                sum_distribution_used*min_wealth_gini
                                                
            wealth_matrix_gini(time_period,min_wealth_index_gini(1),&
                min_wealth_index_gini(2)-1-include_moving_back-include_homelessness,&
                min_wealth_index_gini(3)-two_rental_markets)=-2*scalar_1*housing_price(time_period)*&
                housing_size(housing_status_grid_size-1)
                                        
            gini_coefficient_numerator(time_period)=gini_coefficient_numerator(time_period)+&
                (sum_distribution_used/sum_of_distribution)*(gini_coefficient_cumulative_wealth(time_period,2)+&
                gini_coefficient_cumulative_wealth(time_period,1))
                
            gini_coefficient_cumulative_wealth(time_period,1)=gini_coefficient_cumulative_wealth(time_period,2)
            
            counter=counter+1
                             
        end do
        
        gini_coefficient(time_period)=1-(gini_coefficient_numerator(time_period)/&
            gini_coefficient_cumulative_wealth(time_period,2))
        
        write(*,*) "gini_coefficient(", time_period,")=", gini_coefficient(time_period)
        
    end subroutine
    
    subroutine calculate_gini_coefficient_income(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        real(8) :: sum_distribution_used
        
        loop_count_gini=0
        
        income_matrix_gini(time_period,:,:,:)=0
        
        do age_index=1,life_span-retirement_span
                
            do employment_index=1,employment_grid_size
            
                do years_left_on_mortgage_index=1-two_rental_markets,max_amortization_years
                                        
                    if (sum(distribution_stationary(years_left_on_mortgage_index)%&
                        vector_transition_real(time_period,age_index,:,employment_index,:,:,:))>0) then
                        
                        income_matrix_gini(time_period,age_index,employment_index,years_left_on_mortgage_index)=&
                            labour_deterministic_efficiency(age_index)*employment_grid(age_index,employment_index)
                           
                        loop_count_gini=loop_count_gini+1
                                                                   
                    end if
                
                end do
                                                                
            end do
                            
        end do
                                            
        gini_coefficient_numerator(time_period)=0
        gini_coefficient_cumulative_income(time_period,:)=0
            
        counter=1
                        
        do while (counter<=loop_count_gini)
            
            min_income_gini=minval(income_matrix_gini(time_period,:,:,:),mask=income_matrix_gini(time_period,:,:,:)>0)
            
            if (min_income_gini>0) then
                
                min_income_index_gini=&
                    minloc(income_matrix_gini(time_period,:,:,:),mask=income_matrix_gini(time_period,:,:,:)>0)
                                        
                sum_distribution_used=sum(distribution_stationary(min_income_index_gini(3)-two_rental_markets)%&
                    vector_transition_real(time_period,min_income_index_gini(1),:,min_income_index_gini(2),:,:,:))
                                               
                gini_coefficient_cumulative_income(time_period,2)=gini_coefficient_cumulative_income(time_period,1)+&
                    sum_distribution_used*min_income_gini
                                                    
                income_matrix_gini(time_period,min_income_index_gini(1),min_income_index_gini(2),&
                    min_income_index_gini(3)-two_rental_markets)=0
                                            
                gini_coefficient_numerator(time_period)=gini_coefficient_numerator(time_period)+&
                    (sum_distribution_used/sum_of_distribution)*(gini_coefficient_cumulative_income(time_period,2)+&
                    gini_coefficient_cumulative_income(time_period,1))
                    
                gini_coefficient_cumulative_income(time_period,1)=gini_coefficient_cumulative_income(time_period,2)
                
            end if
            
            counter=counter+1
                             
        end do
        
        gini_coefficient_income(time_period)=1-(gini_coefficient_numerator(time_period)/&
            gini_coefficient_cumulative_income(time_period,2))
        
        write(*,*) "income_gini_coefficient=", gini_coefficient_income(time_period)
        
    end subroutine
    
    subroutine calculate_sd_log_earnings(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        real(8) :: average_income_1,total_distribution_1
        
        average_income_1=0
        sd_log_income_1=0
        
        do i=1,2
        
            total_distribution_1=0
                    
            do age_index=1,life_span-retirement_span
                        
                do employment_index=1,employment_grid_size
                
                    do years_left_on_mortgage_index=1-two_rental_markets,max_amortization_years
                    
                        total_distribution_1=total_distribution_1+&
                            sum(distribution_stationary(years_left_on_mortgage_index)%&
                            vector_transition_real(time_period,age_index,:,employment_index,:,:,:))
                                        
                        if (i==1) then
                                                                    
                            average_income_1=log(labour_deterministic_efficiency(age_index)*&
                                employment_grid(age_index,employment_index))*&
                                sum(distribution_stationary(years_left_on_mortgage_index)%&
                                vector_transition_real(time_period,age_index,:,employment_index,:,:,:))
                        
                        elseif (i==2) then
                        
                            sd_log_income_1=sd_log_income_1+&
                                sum(distribution_stationary(years_left_on_mortgage_index)%&
                                vector_transition_real(time_period,age_index,:,employment_index,:,:,:))*&
                                (log(labour_deterministic_efficiency(age_index)*&
                                employment_grid(age_index,employment_index))-average_income_1)**2                        
                            
                        end if
                                                
                    end do
                                                                    
                end do
                                                
            end do 
            
            if (i==1) then
            
                average_income_1=average_income_1/total_distribution_1
                
            else
            
                sd_log_income_1=sd_log_income_1/total_distribution_1
            
            end if
            
        end do
                
        sd_log_income_1=sqrt(sd_log_income_1)
                                        
    end subroutine
    
    subroutine govt_revenues_calc(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: financial_index,time_period,has_house,loop_over_rent_shocks,housing_status_index_used,&
            amortization_used,time_period_used,only_count_property_taxes=1,years_holding_onto_asset_index_used
        real(8) :: lived_in_size_used,distribution_mass_used,income_tax_used,consumption_used,&
            rent_subsidy_used,distribution_mass_used_previous_period
                        
        time_period_used=time_period-solve_transition_dynamics
        
        govt_revenues_from_labour_income(time_period)=0
        govt_revenues_from_consumption(time_period)=0
        govt_revenues_from_investment_income(time_period)=0
        govt_revenues_from_property_tax(time_period)=0
        govt_spending_homelessness(time_period)=0
        govt_rent_subsidies(time_period)=0
        govt_lump_sum_transfers(time_period)=0
        govt_lump_sum_transfers_to_renters(time_period)=0
        govt_spending_on_ss(time_period)=0
        net_govt_revenues(time_period)=0
                
        if (compute_full_govt_budget==1) then
                
            do age_index=1,life_span
            
                if (age_index>(life_span-retirement_span)) then
                
                    in_retirement=1
                    
                else
                
                    in_retirement=0
                    
                end if
                
                do rental_opportunity_index=0,random_assignment
                                            
                    do years_left_on_mortgage_index=(1-two_rental_markets),max_amortization_years
                                    
                        if (years_left_on_mortgage_index>1) then
                                        
                            has_house=1
                            
                        else
                        
                            has_house=0
                            
                        end if
                        
                        loop_over_rent_shocks=0
                        
                        if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                            
                            loop_over_rent_shocks=1
                            
                        else
                        
                            if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                                (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                            
                                loop_over_rent_shocks=1
                                
                            end if
                            
                        end if
                                        
                        do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                            (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                                                    
                            do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                                               
                                do employment_index=1,employment_grid_size
                                            
                                    do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                        has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                                                                                                                                   
                                        lived_in_size_used=policy_function_space_lived_in_size(years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period_used,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                            
                                        housing_status_index_used=policy_function_housing_status_index&
                                            (years_left_on_mortgage_index)%&
                                            vector_transition_integer(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                                                                    
                                        amortization_used=&
                                            policy_function_years_of_amortization_index(years_left_on_mortgage_index)%&
                                            vector_transition_integer(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                            
                                        years_holding_onto_asset_index_used=&
                                            policy_function_years_holding_onto_asset(years_left_on_mortgage_index)%&
                                            vector_transition_integer(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                            
                                        if (random_assignment==1) then
                                        
                                            years_holding_onto_asset_index_used=&
                                                min(max_years_random,years_holding_onto_asset_index_used)
                                            
                                        end if
                                                                                                              
                                        income_tax_used=policy_function_wage_income_tax_paid(years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                                                                    
                                        consumption_used=&
                                            policy_function_consumption(years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                        
                                        rent_subsidy_used=policy_function_rent_subsidy(years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                    
                                        distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                            
                                        distribution_mass_used_previous_period=&
                                            distribution_stationary(years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period_used,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                                                                                                                                                                  
                                        govt_revenues_from_labour_income(time_period)=&
                                            govt_revenues_from_labour_income(time_period)+income_tax_used*distribution_mass_used
                                                
                                        if (in_retirement==1) then
                                        
                                            govt_spending_on_ss(time_period)=govt_spending_on_ss(time_period)+&
                                                model_ss_benefits(employment_index)*distribution_mass_used
                                                                               
                                        end if
                                                                            
                                        if (consumption_tax>0) then
                                                        
                                            govt_revenues_from_consumption(time_period)=&
                                                govt_revenues_from_consumption(time_period)+&
                                                consumption_tax*consumption_used*distribution_mass_used
                                                
                                        end if
                                            
                                        govt_rent_subsidies(time_period)=govt_rent_subsidies(time_period)+&
                                            rent_subsidy_used*distribution_mass_used
                                            
                                        if (fraction_of_capital_income_taxable==0) then
                                            
                                            govt_revenues_from_investment_income(time_period)=&
                                                govt_revenues_from_investment_income(time_period)+investment_income_tax*&
                                                (risk_free_rate(time_period)*max(financial_grid(financial_index),0.d0))*&
                                                distribution_mass_used
                                                
                                        end if
                                            
                                        govt_lump_sum_transfers(time_period)=govt_lump_sum_transfers(time_period)+&
                                            lump_sum_transfer(time_period)*distribution_mass_used
                                            
                                        if (amortization_used<=1) then
                                        
                                            govt_lump_sum_transfers_to_renters(time_period)=&
                                                govt_lump_sum_transfers_to_renters(time_period)+&
                                                lump_sum_transfer(time_period)*distribution_mass_used
                                        
                                        end if
                                        
                                        if ((housing_status_index_used==lowest_housing_grid_point) .AND. &
                                            (include_homelessness==1)) then
                                        
                                            govt_spending_homelessness(time_period_used)=&
                                                govt_spending_homelessness(time_period)+&
                                                cost_of_homelessness_to_city*&
                                                (is_counterfactual*average_labor_income_calibrated+&
                                                (1-is_counterfactual)*model_average_labour_income(time_period))*&
                                                distribution_mass_used
                                        
                                        end if
                                        
                                        if (housing_status_index_used>=third_lowest_housing_grid_point) then
                                                                                                       
                                            if ((time_period>1) .OR. (solve_transition_dynamics==0)) then
                                                                                            
                                                if (amortization_used<=1) then
                                                                           
                                                    govt_revenues_from_property_tax(time_period)=&
                                                        govt_revenues_from_property_tax(time_period)+&
                                                        property_tax(time_period)*housing_price(time_period)*&
                                                        (lived_in_size_used/(1+amortization_used*include_rent_decrease*&
                                                        include_rental_depreciation*deterioration_controlled_units)**&
                                                        (years_holding_onto_asset_index_used-1))*&
                                                        distribution_mass_used_previous_period
                                                        
                                                else
                                                
                                                    govt_revenues_from_property_tax(time_period)=&
                                                        govt_revenues_from_property_tax(time_period)+&
                                                        property_tax(time_period)*housing_price(time_period)*&
                                                        lived_in_size_used*distribution_mass_used_previous_period
                                                
                                                end if
                                                                                                                                                                                      
                                            end if
                                                                                    
                                        end if
                                                                               
                                    end do
                                                                        
                                end do
                                
                            end do
                                                                           
                        end do
                
                    end do
                    
                end do
                
            end do
            
        end if
                                    
        if ((time_period==1) .AND. (solve_transition_dynamics==1)) then
        
            do age_index=1,life_span
            
                if (age_index>(life_span-retirement_span)) then
                
                    in_retirement=1
                    
                else
                
                    in_retirement=0
                    
                end if
                
                do rental_opportunity_index=0,random_assignment
                                            
                    do years_left_on_mortgage_index=(1-benchmark_has_two_rental_markets),max_amortization_years
                                    
                        if (years_left_on_mortgage_index>1) then
                                        
                            has_house=1
                            
                        else
                        
                            has_house=0
                            
                        end if
                        
                        loop_over_rent_shocks=0
                        
                        if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                            
                            loop_over_rent_shocks=1
                            
                        else
                        
                            if ((benchmark_has_two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                                (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                            
                                loop_over_rent_shocks=1
                                
                            end if
                            
                        end if
                                        
                        do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                            (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*&
                            min(age_index,max_tracking_benchmark))
                                                    
                            do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                                               
                                do employment_index=1,employment_grid_size
                                            
                                    do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                        has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                                                                                                        
                                        lived_in_size_used=policy_function_space_lived_in_size_to_fetch&
                                            (years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                            
                                        housing_status_index_used=&
                                            policy_function_housing_status_index(years_left_on_mortgage_index)%&
                                            vector_transition_integer(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                                                              
                                        distribution_mass_used=distribution_to_fetch(years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                            
                                        if (housing_status_index_used>=third_lowest_housing_grid_point) then
                                                                                                                                                                                                                                               
                                            govt_revenues_from_property_tax(time_period)=&
                                                govt_revenues_from_property_tax(time_period)+&
                                                property_tax(time_period)*housing_price(time_period)*lived_in_size_used*&
                                                distribution_mass_used
                                                
                                        end if
                                        
                                    end do
                                    
                                end do
                                
                            end do
                                                                               
                        end do
                        
                    end do
            
                end do
                
            end do

        end if
        
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
            
        end do
                           
        model_average_labour_income_tax(time_period)=&
            govt_revenues_from_labour_income(time_period)/(model_total_efficiency_units(time_period)+&
            fraction_of_capital_income_taxable*model_financial(time_period)*risk_free_rate(time_period))
                
        total_govt_revenues(time_period)=&
            govt_revenues_from_labour_income(time_period)+govt_revenues_from_consumption(time_period)+&
            govt_revenues_from_investment_income(time_period)+govt_revenues_from_property_tax(time_period)+&
            govt_rent_subsidies(time_period)-govt_lump_sum_transfers(time_period)-govt_spending_homelessness(time_period)
                    
        net_govt_revenues(time_period)=total_govt_revenues(time_period)-govt_spending_on_ss(time_period)
        
        net_govt_revenues_1(time_period)=net_govt_revenues(time_period)+govt_spending_homelessness(time_period)
        
        if (only_count_property_taxes==1) then
        
            net_govt_revenues(time_period)=govt_revenues_from_property_tax(time_period)-&
                govt_spending_homelessness(time_period)
        
        end if
                                
        if (solve_transition_dynamics==0) then
                
            write(*,*) "net_govt_revenues=", net_govt_revenues(time_period)
            
        end if
            
    end subroutine
        
    subroutine calculate_household_housing_spending(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: financial_index,loop_over_rent_shocks
        integer :: time_period,has_house,time_period_used,amortization_used,financial_index_used,&
            housing_status_index_used,years_holding_onto_asset_used
        real(8) :: total_income,mass_of_renters_1,distribution_mass_used,lived_in_size_used
                
        time_period_used=time_period-solve_transition_dynamics
                
        mass_of_renters_at_housing_spending(time_period,:)=0
        mass_of_homeowners_at_housing_spending(time_period,:)=0
        
        model_rent_burden_by_age(time_period,:,:)=0
        mass_renters_by_age(time_period,:)=0
        
        mass_of_renters_1=0
        
        do age_index=1,life_span
        
            if (age_index==1) then
            
                newborn_household=1
                
            else
            
                newborn_household=0
            
            end if
            
            do rental_opportunity_index=0,random_assignment
                    
                do years_left_on_mortgage_index=newborn_household+(1-newborn_household)*(1-two_rental_markets),&
                    newborn_household+(1-newborn_household)*max_amortization_years
                                
                    if (years_left_on_mortgage_index>1) then
                        
                        has_house=1
                        
                    else
                    
                        has_house=0
                        
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                            loop_over_rent_shocks=1
                                                    
                        end if
                        
                    end if
                                                                    
                    do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                        (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                                            
                        do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                
                            do employment_index=1,employment_grid_size
                                        
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                    has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                        
                                    lived_in_size_used=policy_function_space_lived_in_size&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                    
                                    amortization_used=policy_function_years_of_amortization_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                
                                    financial_index_used=policy_function_financial_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    housing_status_index_used=policy_function_housing_status_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    years_holding_onto_asset_used=&
                                        policy_function_years_holding_onto_asset(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period_used,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)                                  
                                                                                                        
                                    if ((amortization_used<=1) .AND. &
                                        (housing_status_index_used>=third_lowest_housing_grid_point)) then
                                    
                                        mass_of_renters_1=mass_of_renters_1+distribution_mass_used
                                    
                                    end if
                                            
                                    if (age_index>(life_span-retirement_span)) then
                                
                                        total_income=model_ss_benefits(employment_index)
                                        
                                    else
                                
                                        total_income=employment_grid(age_index,employment_index)*&
                                            labour_deterministic_efficiency(age_index)
                                
                                    end if
                                                                
                                    total_income=total_income+risk_free_rate(time_period)*&
                                        max(financial_grid(financial_index),0.d0)
                                                                                                 
                                    if (amortization_used<=1) then
                                    
                                        fraction_of_income_spent_on_housing=0
                                    
                                        if  (housing_status_index_used>=third_lowest_housing_grid_point) then
                                                                            
                                            fraction_of_income_spent_on_housing=(rent(time_period,amortization_used)/&
                                                ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                (years_holding_onto_asset_used-1)))*lived_in_size_used
                                                
                                        end if
                                                                                                            
                                    else
                                                                                                                   
                                        fraction_of_income_spent_on_housing=&
                                            (-financial_grid(min(financial_index_used,negative_financial_grid_size))*&
                                            (mortgage_interest_rate(time_period)+average_fraction_of_mortgage_repaid))+&
                                            housing_price(time_period-solve_transition_dynamics)*lived_in_size_used*&
                                            (property_tax(time_period-solve_transition_dynamics)+&
                                            include_housing_depreciation_in_housing_costs*housing_depreciation_rate)
                                                                                                                                                                                                                                                                                    
                                    end if
                                    
                                    if (housing_status_index_used>=third_lowest_housing_grid_point) then
                                    
                                        if ((years_left_on_mortgage_index==0) .OR. &
                                            ((years_left_on_mortgage_index==1) .AND. (two_rental_markets==0) .AND. &
                                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1))) then
                                        
                                            fraction_of_income_spent_on_housing=fraction_of_income_spent_on_housing+&
                                                shock_to_rent(min(rent_shock_grid_size,years_holding_onto_asset_index))*&
                                                rent(time_period,years_left_on_mortgage_index)*&
                                                housing_size(housing_status_index)
                                                
                                        end if
                                        
                                    end if
                                        
                                    fraction_of_income_spent_on_housing=fraction_of_income_spent_on_housing/total_income
                                                
                                    frac_of_inc_spent_on_hous_threshold(time_period,1)=0.3
                                    frac_of_inc_spent_on_hous_threshold(time_period,2)=0.5
                                    frac_of_inc_spent_on_hous_threshold(time_period,3)=0.75
                                    frac_of_inc_spent_on_hous_threshold(time_period,4)=1 
                                            
                                    if (amortization_used<=1) then
                                    
                                        if (housing_status_index_used>=third_lowest_housing_grid_point) then
                                                                                    
                                            do i=1,number_of_thresholds
                                                    
                                                if (fraction_of_income_spent_on_housing>&
                                                    frac_of_inc_spent_on_hous_threshold(time_period,i)) then
                                                                
                                                    mass_of_renters_at_housing_spending(time_period,i)=&
                                                        mass_of_renters_at_housing_spending(time_period,i)+&
                                                        distribution_mass_used
                                                        
                                                    model_rent_burden_by_age(time_period,age_index,i)=&
                                                        model_rent_burden_by_age(time_period,age_index,i)+&
                                                        distribution_mass_used
                                                                                                        
                                                end if
                                                                
                                            end do
                                                                                
                                            mass_renters_by_age(time_period,age_index)=&
                                                mass_renters_by_age(time_period,age_index)+distribution_mass_used
                                                
                                        end if
                                                 
                                    else
                                                                                                                                    
                                        do i=1,number_of_thresholds    
                                                    
                                            if (fraction_of_income_spent_on_housing>&
                                                frac_of_inc_spent_on_hous_threshold(time_period,i)) then
                                                            
                                                mass_of_homeowners_at_housing_spending(time_period,i)=&
                                                    mass_of_homeowners_at_housing_spending(time_period,i)+distribution_mass_used
                                                            
                                            end if
                                            
                                        end do
                                                                                          
                                    end if
                                                                             
                                end do
                                
                            end do
                            
                        end do
                        
                    end do
                                                                
                end do
        
            end do
            
        end do
        
        do age_index=1,life_span
        
            do i=1,number_of_thresholds
        
                model_rent_burden_by_age(time_period,age_index,i)=&
                    model_rent_burden_by_age(time_period,age_index,i)/mass_renters_by_age(time_period,age_index)
                    
            end do
        
        end do
        
        if (solve_transition_dynamics==1) then
        
            open(unit=1,file="percentage_spending_on_housing"//trim(file_number)//".txt",action="write",status="replace")
                
        else
        
            open(unit=1,file="percentage_spending_on_housing"//trim(file_number)//".txt",action="write",status="replace")
        
        end if
        
        do i=1,number_of_thresholds
        
            mass_of_renters_at_housing_spending(time_period,i)=&
                mass_of_renters_at_housing_spending(time_period,i)/mass_of_renters_1
                
            mass_of_homeowners_at_housing_spending(time_period,i)=&
                mass_of_homeowners_at_housing_spending(time_period,i)/mass_of_owners_with_mortgage(time_period)
                
            if (solve_transition_dynamics==0) then
            
                if (i<=number_of_thresholds) then
                
                    write(*,*) "percentage of renters spending more than ", &
                        frac_of_inc_spent_on_hous_threshold(time_period,i)," on their income on housing=",&
                        mass_of_renters_at_housing_spending(time_period,i)
                        
                    write(*,*) "percentag of homeowners with a mortgage spending more than ", &
                        frac_of_inc_spent_on_hous_threshold(time_period,i)," on their income on housing=",&
                        mass_of_homeowners_at_housing_spending(time_period,i)
                        
                end if
                    
            end if
                                
            write(1,*) "percentage of renters spending more than ", &
                frac_of_inc_spent_on_hous_threshold(time_period,i)," on their income on housing=",&
                mass_of_renters_at_housing_spending(time_period,i)
            write(1,*) "percentage of homeowners with a mortgage spending more than ", &
                frac_of_inc_spent_on_hous_threshold(time_period,i)," on their income on housing=",&
                mass_of_homeowners_at_housing_spending(time_period,i)
                
        end do
        
        close(1)
                
    end subroutine
           
    subroutine calculate_welfare(time_period)
    
        use Global_Vars
    
        implicit none
        
        integer :: financial_index
        integer :: time_period,has_house,loop_over_rent_shocks,time_period_used
        real(8) :: distribution_mass_used,value_function_used,sum_of_distribution_retired
                
        time_period_used=time_period-solve_transition_dynamics
        
        average_welfare_of_all_cohorts(time_period)=0
        average_welfare_of_youngest_cohort(time_period)=0
        average_welfare_of_retired(time_period)=0
        
        sum_of_distribution_retired=0
        
        do age_index=1,life_span
        
            if (age_index>life_span-retirement_span) then
            
                in_retirement=1
            
            else
            
                in_retirement=0
            
            end if
            
            do rental_opportunity_index=0,random_assignment
                                        
                do years_left_on_mortgage_index=(1-two_rental_markets),max_amortization_years
                
                    if (years_left_on_mortgage_index>1) then
                        
                        has_house=1
                        
                    else
                    
                        has_house=0
                        
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                            loop_over_rent_shocks=1
                            
                        end if
                        
                    end if
                                                    
                    do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                        (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                                            
                        do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                                           
                            do employment_index=1,employment_grid_size
                                        
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                    has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                            
                                    value_function_used=value_function(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                                                    
                                    distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period_used,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    if (value_function_used>large_negative_num) then
                                
                                        average_welfare_of_all_cohorts(time_period)=&
                                            average_welfare_of_all_cohorts(time_period)+&
                                            value_function_used*distribution_mass_used
                                                                                        
                                        if (age_index==1) then
                                                                                
                                            if (distribution_mass_used>0) then
                                        
                                                average_welfare_of_youngest_cohort(time_period)=&
                                                    average_welfare_of_youngest_cohort(time_period)+&
                                                    value_function_used*distribution_mass_used
                                                    
                                            end if
                                                
                                        end if
                                        
                                        if (in_retirement==1) then
                                        
                                            if (distribution_mass_used>0) then
                                        
                                                average_welfare_of_retired(time_period)=&
                                                    average_welfare_of_retired(time_period)+&
                                                    value_function_used*distribution_mass_used
                                                    
                                                sum_of_distribution_retired=sum_of_distribution_retired+&
                                                    distribution_mass_used
                                                    
                                            end if
                                        
                                        end if
                                        
                                    end if
                                                                    
                                end do
                                
                            end do
                            
                        end do
                            
                    end do
                
                end do
                
            end do
            
        end do        
        
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
            
        end do
        
        sum_of_distribution_youngest=0
                
        do i=1-two_rental_markets,2
                
            sum_of_distribution_youngest=sum_of_distribution_youngest+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,1,:,:,:,:,:))
            
        end do
        
        average_welfare_of_all_cohorts(time_period)=average_welfare_of_all_cohorts(time_period)/sum_of_distribution
        
        average_welfare_of_youngest_cohort(time_period)=average_welfare_of_youngest_cohort(time_period)/sum_of_distribution_youngest
        
        average_welfare_of_retired(time_period)=average_welfare_of_retired(time_period)/sum_of_distribution_retired
            
        if (solve_transition_dynamics==0) then
        
            write(*,*) "average_welfare_of_youngest_cohort=", average_welfare_of_youngest_cohort(time_period)
            write(*,*) "average_welfare_of_retired=", average_welfare_of_retired(time_period)
            write(*,*) "average_welfare_of_all_cohorts=", average_welfare_of_all_cohorts(time_period)
            
        end if
        
    end subroutine
    
    subroutine calculate_welfare_by_characteristics(time_period)
    
        use Global_Vars
    
        implicit none
        
        integer :: financial_index,l,time_period_used
        integer :: time_period,has_house,loop_over_rent_shocks,amortization_used,financial_index_used,&
            housing_status_index_used,years_holding_onto_asset_used
        real(8) :: distribution_mass_used,value_function_used,MID_used,consumption_used,lived_in_size_used
                
        time_period_used=time_period-solve_transition_dynamics
                
        welfare_by_age(time_period,:)=0
        mass_by_age(time_period,:)=0
        
        welfare_by_employment_and_age(time_period,:,:)=0
        mass_by_employment_and_age(time_period,:,:)=0
        
        welfare_by_employment(time_period,:)=0
        welfare_by_employment_youngest(time_period,:)=0
        welfare_by_employment_youngest_old_dist(time_period,:)=0
        mass_by_employment(time_period,:)=0
        mass_by_employment_youngest(time_period,:)=0
        wealth_by_employment(time_period,:)=0
        fraction_with_mortgage(time_period,:)=0
        fraction_renters_by_employment(time_period,:)=0
        average_age_of_homeowner(time_period,:)=0
        MID_share_by_employment(time_period,:)=0
        
        average_tenure_duration_of_renter_by_employment(time_period,:)=0
        mass_of_renters_by_employment(time_period,:)=0
                
        average_income(time_period,:)=0
        average_housing_costs(time_period,:)=0
        average_consumption(time_period,:)=0
        
        average_house_inheritance(time_period,:)=0
        average_house_non_inheritance(time_period,:)=0
        
        mass_inheritance(time_period,:)=0
        mass_non_inheritance(time_period,:)=0
        
        welfare_by_employment_and_tenure(time_period,:,:)=0
        wealth_by_employment_and_tenure(time_period,:,:)=0
        consumption_by_employment_and_tenure(time_period,:,:)=0
        housing_by_employment_and_tenure(time_period,:,:)=0
        mortgage_by_employment_and_tenure(time_period,:,:)=0
        mortgage_by_employment(time_period,:)=0
        mass_by_mortgage_and_employment(time_period,:)=0
        mass_by_employment_and_tenure(time_period,:,:)=0
        
        welfare_by_employment_tenure_and_age(time_period,:,:,:)=0
        mortgage_by_employment_tenure_and_age(time_period,:,:,:)=0
        mass_by_employment_tenure_and_age(time_period,:,:,:)=0
                
        mortgage_to_house_value_ratio_of_owners_by_age(time_period,:)=0
        mass_mortgage_to_house_value_ratio_of_owners_by_age(time_period,:)=0
        
        model_LTV_by_age_group(time_period,:)=0
        mass_LTV_by_age_group(time_period,:)=0
        
        do age_index=1,life_span
        
            do rental_opportunity_index=0,random_assignment
                                    
                do years_left_on_mortgage_index=(1-two_rental_markets),max_amortization_years
                                
                    if (years_left_on_mortgage_index>1) then
                        
                        has_house=1
                        
                    else
                    
                        has_house=0
                        
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                            loop_over_rent_shocks=1
                                                    
                        end if
                        
                    end if
                                                    
                    do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                        (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                                            
                        do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                                           
                            do employment_index=1,employment_grid_size
                                        
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                    has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                            
                                    lived_in_size_used=policy_function_space_lived_in_size(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                                                             
                                    amortization_used=policy_function_years_of_amortization_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                
                                    financial_index_used=policy_function_financial_index(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    housing_status_index_used=policy_function_housing_status_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    years_holding_onto_asset_used=&
                                        policy_function_years_holding_onto_asset(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    consumption_used=policy_function_consumption(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    value_function_used=value_function(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period_used,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)  
                                                                                    
                                    welfare_by_age(time_period,age_index)=&
                                        welfare_by_age(time_period,age_index)+value_function_used*distribution_mass_used
                                        
                                    welfare_by_employment_and_age(time_period,employment_index,age_index)=&
                                        welfare_by_employment_and_age(time_period,employment_index,age_index)+&
                                        value_function_used*distribution_mass_used
                                        
                                    mass_by_age(time_period,age_index)=mass_by_age(time_period,age_index)+&
                                        distribution_mass_used
                                                                            
                                    mass_by_employment_and_age(time_period,employment_index,age_index)=&
                                        mass_by_employment_and_age(time_period,employment_index,age_index)+&
                                        distribution_mass_used
                                                                                                                        
                                    if (amortization_used>=2) then
                                
                                        mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)=&
                                            mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)-&
                                            (financial_grid(min(financial_index_used,negative_financial_grid_size))/&
                                            (housing_price(time_period)*housing_size(housing_status_index_used)))*&
                                            distribution_mass_used
                                            
                                        if (age_index<=4) then
                                        
                                            l=1
                                            
                                        elseif (age_index<=9) then
                                        
                                            l=2
                                        
                                        elseif (age_index<=14) then
                                        
                                            l=3
                                        
                                        elseif (age_index<=19) then
                                        
                                            l=4
                                        
                                        elseif (age_index<=24) then
                                        
                                            l=5
                                            
                                        elseif (age_index<=29) then
                                        
                                            l=6
                                            
                                        elseif (age_index<=34) then
                                        
                                            l=7
                                            
                                        elseif (age_index<=39) then
                                        
                                            l=8
                                            
                                        elseif (age_index<=44) then
                                        
                                            l=9
                                            
                                        elseif (age_index<=49) then
                                        
                                            l=10
                                            
                                        elseif (age_index<=54) then
                                        
                                            l=11
                                            
                                        elseif (age_index>=55) then
                                        
                                            l=12
                                        
                                        end if
                                        
                                        model_LTV_by_age_group(time_period,l)=model_LTV_by_age_group(time_period,l)+&
                                            (-financial_grid(min(financial_index_used,negative_financial_grid_size))/&
                                            (housing_price(time_period)*housing_size(housing_status_index_used)))*&
                                            distribution_mass_used
                                        
                                        mass_LTV_by_age_group(time_period,l)=mass_LTV_by_age_group(time_period,l)+&
                                            distribution_mass_used
                                                                                    
                                        mass_mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)=&
                                            mass_mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)+&
                                            distribution_mass_used
                                                                                    
                                    end if                                      
                                        
                                    welfare_by_employment(time_period,employment_index)=&
                                        welfare_by_employment(time_period,employment_index)+&
                                        value_function_used*distribution_mass_used
                                        
                                    if (years_left_on_mortgage_index<=1) then
                                    
                                        mass_of_renters_by_employment(time_period,employment_index)=&
                                            mass_of_renters_by_employment(time_period,employment_index)+&
                                            distribution_mass_used
                                    
                                        if (years_left_on_mortgage_index==1) then
                                            
                                            average_tenure_duration_of_renter_by_employment(time_period,employment_index)=&
                                                average_tenure_duration_of_renter_by_employment(time_period,employment_index)+&
                                                years_holding_onto_asset_index*distribution_mass_used
                                                                                            
                                        else
                                        
                                            average_tenure_duration_of_renter_by_employment(time_period,employment_index)=&
                                                average_tenure_duration_of_renter_by_employment(time_period,employment_index)+&
                                                distribution_mass_used
                                        
                                        end if
                                            
                                    end if
                                        
                                    if (age_index==1) then
                                    
                                        welfare_by_employment_youngest(time_period,employment_index)=&
                                            welfare_by_employment_youngest(time_period,employment_index)+&
                                            value_function_used*distribution_mass_used
                                            
                                        mass_by_employment_youngest(time_period,employment_index)=&
                                            mass_by_employment_youngest(time_period,employment_index)+&
                                            distribution_mass_used
                                    
                                    end if
                                        
                                    MID_share_by_employment(time_period,employment_index)=&
                                        MID_share_by_employment(time_period,employment_index)+&
                                        MID_used*distribution_mass_used
                                        
                                    wealth_by_employment(time_period,employment_index)=&
                                        wealth_by_employment(time_period,employment_index)+&
                                        (max(min(years_left_on_mortgage_index-1,1),0)*&
                                        housing_price(time_period)*housing_size(housing_status_index)+&
                                        min(financial_grid(financial_index),0.d0)+&
                                        max(financial_grid(financial_index),0.d0))*distribution_mass_used
                                        
                                    average_income(time_period,employment_index)=average_income(time_period,employment_index)+&
                                        (employment_grid(age_index,employment_index)*&
                                        labour_deterministic_efficiency(age_index)+max(financial_grid(financial_index),0.d0)*&
                                        risk_free_rate(time_period)+lump_sum_transfer(time_period))*distribution_mass_used
                                        
                                    if (age_index==1) then
                                    
                                        if (years_left_on_mortgage_index>1) then
                                        
                                            average_house_inheritance(time_period,employment_index)=&
                                                average_house_inheritance(time_period,employment_index)+&
                                                housing_size(housing_status_index)*distribution_mass_used
                                                
                                            mass_inheritance(time_period,employment_index)=&
                                                mass_inheritance(time_period,employment_index)+distribution_mass_used
                                        
                                        end if
                                        
                                        average_house_non_inheritance(time_period,employment_index)=&
                                            average_house_non_inheritance(time_period,employment_index)+&
                                            max(financial_grid(financial_index),0.d0)*distribution_mass_used
                                            
                                        mass_non_inheritance(time_period,employment_index)=&
                                            mass_non_inheritance(time_period,employment_index)+distribution_mass_used
                                                                        
                                    end if
                                        
                                    if (financial_grid(financial_index)<0) then
                                        
                                        fraction_with_mortgage(time_period,employment_index)=&
                                            fraction_with_mortgage(time_period,employment_index)+distribution_mass_used
                                            
                                    end if
                                    
                                    average_consumption(time_period,employment_index)=&
                                        average_consumption(time_period,employment_index)+&
                                        consumption_used*distribution_mass_used
                                    
                                    if (amortization_used<=1) then
                                    
                                        fraction_renters_by_employment(time_period,employment_index)=&
                                            fraction_renters_by_employment(time_period,employment_index)+&
                                            distribution_mass_used
                                            
                                        average_housing_costs(time_period,employment_index)=&
                                            average_housing_costs(time_period,employment_index)+&
                                            (rent(time_period,amortization_used)/&
                                            ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                            (years_holding_onto_asset_used-1)))*lived_in_size_used*&
                                            distribution_mass_used                                            
                                        
                                    else
                                    
                                        average_age_of_homeowner(time_period,employment_index)=&
                                            average_age_of_homeowner(time_period,employment_index)+age_index*&
                                            distribution_mass_used
                                            
                                        average_housing_costs(time_period,employment_index)=&
                                            average_housing_costs(time_period,employment_index)+&
                                            (housing_price(time_period)*(property_tax(time_period)+&
                                            housing_depreciation_rate)*housing_size(housing_status_index)-&
                                            min(financial_grid(financial_index),0.d0)*mortgage_interest_rate(time_period)-&
                                            MID_used)*distribution_mass_used
                                            
                                    end if
                                    
                                    if ((years_left_on_mortgage_index==0) .OR. &
                                        ((years_left_on_mortgage_index==1) .AND. (two_rental_markets==0) .AND. &
                                        (age_index>1) .AND. (include_shocks_to_free_market_rent==1))) then
                                    
                                        average_housing_costs(time_period,employment_index)=&
                                            average_housing_costs(time_period,employment_index)+&
                                            shock_to_rent(min(rent_shock_grid_size,years_holding_onto_asset_index))*&
                                            rent(time_period,years_left_on_mortgage_index)*&
                                            housing_size(housing_status_index)
                                            
                                    end if
                                    
                                    mass_by_employment(time_period,employment_index)=&
                                        mass_by_employment(time_period,employment_index)+distribution_mass_used
                                                                            
                                    if (years_left_on_mortgage_index<=1) then
                                    
                                        i=1
                                        
                                    elseif (years_left_on_mortgage_index==3) then
                                    
                                        i=2
                                        
                                    else
                                    
                                        i=3
                                    
                                    end if
                                    
                                    if (age_index==1) then
                                    
                                        welfare_by_employment_youngest_old_dist(time_period,employment_index)=&
                                            welfare_by_employment_youngest_old_dist(time_period,employment_index)+&
                                            value_function_used*distribution_mass_used
                                            
                                    end if
                                                                                
                                    mass_by_employment_and_tenure(time_period,employment_index,i)=&
                                        mass_by_employment_and_tenure(time_period,employment_index,i)+distribution_mass_used
                                    
                                    if (value_function_used>-200000) then
                                    
                                        welfare_by_employment_and_tenure(time_period,employment_index,i)=&
                                            welfare_by_employment_and_tenure(time_period,employment_index,i)+&
                                            value_function_used*distribution_mass_used
                                            
                                        wealth_by_employment_and_tenure(time_period,employment_index,i)=&
                                            wealth_by_employment_and_tenure(time_period,employment_index,i)+&
                                            (max(min(years_left_on_mortgage_index-1,1),0)*housing_price(time_period)*&
                                            housing_size(housing_status_index)+min(financial_grid(financial_index),0.d0)+&
                                            max(financial_grid(financial_index),0.d0))*distribution_mass_used
                                            
                                        consumption_by_employment_and_tenure(time_period,employment_index,i)=&
                                            consumption_by_employment_and_tenure(time_period,employment_index,i)+&
                                            consumption_used*distribution_mass_used
                                            
                                        housing_by_employment_and_tenure(time_period,employment_index,i)=&
                                            housing_by_employment_and_tenure(time_period,employment_index,i)+&
                                            lived_in_size_used*distribution_mass_used
                                            
                                        mortgage_by_employment_and_tenure(time_period,employment_index,i)=&
                                            mortgage_by_employment_and_tenure(time_period,employment_index,i)+&
                                            (-min(financial_grid(financial_index),0.d0))*distribution_mass_used
                                            
                                        if (financial_grid(financial_index)<0) then
                                        
                                            mortgage_by_employment(time_period,employment_index)=&
                                                mortgage_by_employment(time_period,employment_index)+&
                                                (-min(financial_grid(financial_index),0.d0))*distribution_mass_used
                                                
                                            mass_by_mortgage_and_employment(time_period,employment_index)=&
                                                mass_by_mortgage_and_employment(time_period,employment_index)+&
                                                distribution_mass_used
                                        
                                        end if
                                                                                    
                                        age_group=int(floor(real(age_index)/real(10))+1)
                                            
                                        welfare_by_employment_tenure_and_age(time_period,employment_index,i,age_group)=&
                                            welfare_by_employment_tenure_and_age(time_period,employment_index,i,age_group)+&
                                            value_function_used*distribution_mass_used
                                                                                
                                        mortgage_by_employment_tenure_and_age(time_period,employment_index,i,age_group)=&
                                            mortgage_by_employment_tenure_and_age(time_period,employment_index,i,age_group)+&
                                            (-min(financial_grid(financial_index),0.d0)/(housing_price(time_period)*&
                                            housing_size(housing_status_index)))*distribution_mass_used
                                                
                                        mass_by_employment_tenure_and_age(time_period,employment_index,i,age_group)=&
                                            mass_by_employment_tenure_and_age(time_period,employment_index,i,age_group)+&
                                            distribution_mass_used
                                            
                                    end if
                                                                    
                                end do
                                
                            end do
                            
                        end do
                                                        
                    end do
                    
                end do
                                        
            end do
            
        end do
        
        do age_index=1,life_span
                
            welfare_by_age(time_period,age_index)=welfare_by_age(time_period,age_index)/&
                mass_by_age(time_period,age_index)
                                    
        end do
        
        do age_index=1,life_span
        
            do employment_index=1,employment_grid_size
                
                welfare_by_employment_and_age(time_period,employment_index,age_index)=&
                    welfare_by_employment_and_age(time_period,employment_index,age_index)/&
                    mass_by_employment_and_age(time_period,employment_index,age_index)
                    
            end do
                                    
        end do
                            
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
            
        end do
        
        do age_index=1,life_span
        
            mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)=&
                mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)/&
                mass_mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)
        
        end do
        
        do age_index=1,life_span
        
            mass_mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)=&
                mass_mortgage_to_house_value_ratio_of_owners_by_age(time_period,age_index)/&
                sum_of_distribution
        
        end do
        
        do i=1,12
        
            model_LTV_by_age_group(time_period,i)=model_LTV_by_age_group(time_period,i)/&
                mass_LTV_by_age_group(time_period,i)
        
        end do
        
        do employment_index=1,employment_grid_size
                
            welfare_by_employment(time_period,employment_index)=&
                welfare_by_employment(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
            welfare_by_employment_youngest_old_dist(time_period,employment_index)=&
                welfare_by_employment_youngest_old_dist(time_period,employment_index)/&
                mass_by_employment_and_age(time_period,employment_index,1)
                
            average_tenure_duration_of_renter_by_employment(time_period,employment_index)=&
                average_tenure_duration_of_renter_by_employment(time_period,employment_index)/&
                mass_of_renters_by_employment(time_period,employment_index)
                                
            welfare_by_employment_youngest(time_period,employment_index)=&
                welfare_by_employment_youngest(time_period,employment_index)/&
                mass_by_employment_youngest(time_period,employment_index)
                                
            wealth_by_employment(time_period,employment_index)=&
                wealth_by_employment(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
            fraction_with_mortgage(time_period,employment_index)=&
                fraction_with_mortgage(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
            fraction_renters_by_employment(time_period,employment_index)=&
                fraction_renters_by_employment(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
            average_age_of_homeowner(time_period,employment_index)=&
                average_age_of_homeowner(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
            average_income(time_period,employment_index)=&
                average_income(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
            average_housing_costs(time_period,employment_index)=&
                average_housing_costs(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
            average_consumption(time_period,employment_index)=&
                average_consumption(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                
            average_house_inheritance(time_period,employment_index)=&
                average_house_inheritance(time_period,employment_index)/&
                mass_inheritance(time_period,employment_index)
                
            average_house_non_inheritance(time_period,employment_index)=&
                average_house_non_inheritance(time_period,employment_index)/&
                mass_non_inheritance(time_period,employment_index)
                                                
        end do
                   
        do employment_index=1,employment_grid_size
        
            mortgage_by_employment(time_period,employment_index)=&
                mortgage_by_employment(time_period,employment_index)/&
                mass_by_mortgage_and_employment(time_period,employment_index)
        
            do i=1,3
            
                welfare_by_employment_and_tenure(time_period,employment_index,i)=&
                    welfare_by_employment_and_tenure(time_period,employment_index,i)/&
                    mass_by_employment_and_tenure(time_period,employment_index,i)
                    
                wealth_by_employment_and_tenure(time_period,employment_index,i)=&
                    wealth_by_employment_and_tenure(time_period,employment_index,i)/&
                    mass_by_employment_and_tenure(time_period,employment_index,i)
                    
                consumption_by_employment_and_tenure(time_period,employment_index,i)=&
                    consumption_by_employment_and_tenure(time_period,employment_index,i)/&
                    mass_by_employment_and_tenure(time_period,employment_index,i)
                    
                housing_by_employment_and_tenure(time_period,employment_index,i)=&
                    housing_by_employment_and_tenure(time_period,employment_index,i)/&
                    mass_by_employment_and_tenure(time_period,employment_index,i)
                    
                mortgage_by_employment_and_tenure(time_period,employment_index,i)=&
                    mortgage_by_employment_and_tenure(time_period,employment_index,i)/&
                    mass_by_employment_and_tenure(time_period,employment_index,i)
            
            end do
            
        end do
        
        do age_index=1,int((real(life_span)/real(10)))
        
            do employment_index=1,employment_grid_size
            
                do i=1,3
                                
                    welfare_by_employment_tenure_and_age(time_period,employment_index,i,age_index)=&
                        welfare_by_employment_tenure_and_age(time_period,employment_index,i,age_index)/&
                        mass_by_employment_tenure_and_age(time_period,employment_index,i,age_index)
                        
                    mortgage_by_employment_tenure_and_age(time_period,employment_index,i,age_index)=&
                        mortgage_by_employment_tenure_and_age(time_period,employment_index,i,age_index)/&
                        mass_by_employment_tenure_and_age(time_period,employment_index,i,age_index)
                                        
                end do
                
            end do
            
        end do
                
    end subroutine
        
    subroutine calculate_homeownership_by_characteristics(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,has_house,financial_index,loop_over_rent_shocks,time_period_used,&
            amortization_used,housing_status_index_used
        real(8) :: distribution_mass_used,lived_in_size_used,consumption_used,total_income,value_function_used
        real(8), dimension(number_of_quintiles) :: mass_by_quintile
        real(8), dimension(number_of_quintiles) :: mass_by_quintile_youngest
                        
        time_period_used=time_period-solve_transition_dynamics
        
        homeownership_by_employment(time_period,:)=0
        tenure_by_employment(time_period,:,:)=0
        tenure_by_income_quintile(time_period,:,:)=0
        housing_size_by_employment(time_period,:)=0
        housing_size_by_employment_and_tenure(time_period,:,:)=0
        mass_by_tenure_and_employment(time_period,:,:)=0
        housing_size_by_employment_and_age(time_period,:,:)=0
        consumption_by_employment_and_age(time_period,:,:)=0
        wealth_by_employment_and_age(time_period,:,:)=0
        mass_by_employment(time_period,:)=0
        mass_by_employment_and_age(time_period,:,:)=0
        homeownership_by_age(time_period,:)=0
        housing_size_by_age(time_period,:)=0
        mass_by_age(time_period,:)=0
        mass_by_quintile(:)=0
        
        wealth_by_age_renter(time_period,:)=0
        income_by_age_renter(time_period,:)=0
        consumption_by_age_renter(time_period,:)=0
        consumption_by_age(time_period,:)=0
        housing_consumption_by_age_renter(time_period,:)=0
        mass_by_age_and_renter(time_period,:)=0
        welfare_by_income_quintile_youngest(time_period,:)=0
        mass_by_quintile_youngest(:)=0
        
        if (simulation_ended==1) then
        
            call calculate_cdf_of_incomes_earned(time_period)
        
        end if
        
        do age_index=1,life_span
                    
            if (age_index>(life_span-retirement_span)) then
            
                in_retirement=1
                
            else
            
                in_retirement=0
                
            end if
            
            do rental_opportunity_index=0,random_assignment
                                
                do years_left_on_mortgage_index=(1-two_rental_markets),max_amortization_years
                                
                    if (years_left_on_mortgage_index>1) then
                        
                        has_house=1
                        
                    else
                    
                        has_house=0
                        
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                            loop_over_rent_shocks=1
                                                    
                        end if
                        
                    end if
                                                    
                    do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                        (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                                    
                        do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                                           
                            do employment_index=1,employment_grid_size
                                        
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                    has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1                                 
                                            
                                    housing_status_index_used=policy_function_housing_status_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                
                                    value_function_used=value_function(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period_used,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)

                                    distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period_used,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                    
                                    if (in_retirement==1) then
                                            
                                        total_income=model_ss_benefits(employment_index)
                                    
                                    else
                                    
                                        total_income=labour_deterministic_efficiency(age_index)*&
                                            employment_grid(age_index,employment_index)
                                        
                                    end if
                                    
                                    total_income=total_income+risk_free_rate(time_period)*&
                                        max(financial_grid(financial_index),0.d0)
                                    
                                    if ((age_index==1) .AND. (simulation_ended==1)) then
                                        
                                        j=number_of_quintiles
                                        
                                        quintile_loop: do k=1,number_of_quintiles-1
                                        
                                            if (total_income<income_cutoffs_of_quintiles(k)) then
                                        
                                                j=k
                                                
                                                exit quintile_loop
                                            
                                            end if
                                        
                                        end do quintile_loop

                                        mass_by_quintile_youngest(j)=mass_by_quintile_youngest(j)+distribution_mass_used
                                                                                                                                                                                                                                                                    
                                        welfare_by_income_quintile_youngest(time_period,j)=&
                                            welfare_by_income_quintile_youngest(time_period,j)+&
                                            value_function_used*distribution_mass_used
                                            
                                    end if
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
                                    if ((simulation_ended==1) .AND. &
                                        (housing_status_index_used>=third_lowest_housing_grid_point)) then

                                        lived_in_size_used=policy_function_space_lived_in_size&
                                            (years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                                                
                                        amortization_used=policy_function_years_of_amortization_index&
                                            (years_left_on_mortgage_index)%&
                                            vector_transition_integer(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)
                                                                                
                                        consumption_used=policy_function_consumption(years_left_on_mortgage_index)%&
                                            vector_transition_real(time_period,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                            rental_opportunity_index)

                                        if (years_left_on_mortgage_index==0) then
                                    
                                            i=0
                                
                                        elseif (years_left_on_mortgage_index==1) then
                                        
                                            i=1
                                            
                                        elseif (years_left_on_mortgage_index==2) then
                                        
                                            if (financial_grid(financial_index)<0) then
                                        
                                                i=2
                                                
                                            else
                                            
                                                i=3
                                                
                                            end if
                                            
                                        end if

                                        if (((in_retirement==1) .AND. (include_retirees_in_income_cdf==1)) .OR. &
                                            (in_retirement==0))  then

                                            j=number_of_quintiles
                                        
                                            quintile_loop_1: do k=1,number_of_quintiles-1
                                            
                                                if (total_income<income_cutoffs_of_quintiles(k)) then
                                            
                                                    j=k
                                                    
                                                    exit quintile_loop_1
                                                
                                                end if
                                            
                                            end do quintile_loop_1

                                            tenure_by_income_quintile(time_period,j,i)=&
                                                tenure_by_income_quintile(time_period,j,i)+distribution_mass_used
        
                                        end if
                                                                                                                                                                                                                                                                                                                   
                                        tenure_by_employment(time_period,employment_index,i)=&
                                            tenure_by_employment(time_period,employment_index,i)+distribution_mass_used
                                            
                                        housing_size_by_employment_and_tenure(time_period,employment_index,i)=&
                                            housing_size_by_employment_and_tenure(time_period,employment_index,i)+&
                                            lived_in_size_used*distribution_mass_used
                                            
                                        mass_by_tenure_and_employment(time_period,employment_index,i)=&
                                            mass_by_tenure_and_employment(time_period,employment_index,i)+&
                                            distribution_mass_used
                            
                                        homeownership_by_employment(time_period,employment_index)=&
                                            homeownership_by_employment(time_period,employment_index)+&
                                            max(min(years_left_on_mortgage_index-1,1),0)*&
                                            distribution_mass_used
                                            
                                        housing_size_by_employment(time_period,employment_index)=& 
                                            housing_size_by_employment(time_period,employment_index)+&
                                            lived_in_size_used*distribution_mass_used
                                            
                                        housing_size_by_employment_and_age(time_period,employment_index,age_index)=& 
                                            housing_size_by_employment_and_age(time_period,employment_index,age_index)+&
                                            lived_in_size_used*distribution_mass_used
                                            
                                        consumption_by_employment_and_age(time_period,employment_index,age_index)=& 
                                            consumption_by_employment_and_age(time_period,employment_index,age_index)+&
                                            consumption_used*distribution_mass_used
                                            
                                        wealth_by_employment_and_age(time_period,employment_index,age_index)=&
                                            wealth_by_employment_and_age(time_period,employment_index,age_index)+&
                                            (max(min(years_left_on_mortgage_index-1,1),0)*&
                                            housing_price(time_period)*housing_size(housing_status_index)+&
                                            min(financial_grid(financial_index),0.d0)+&
                                            max(financial_grid(financial_index),0.d0))*distribution_mass_used
                                                                    
                                        mass_by_employment(time_period,employment_index)=&
                                            mass_by_employment(time_period,employment_index)+distribution_mass_used
                                            
                                        mass_by_employment_and_age(time_period,employment_index,age_index)=&
                                            mass_by_employment_and_age(time_period,employment_index,age_index)+&
                                            distribution_mass_used
                                            
                                        consumption_by_age(time_period,age_index)=consumption_by_age(time_period,age_index)+&
                                            consumption_used*distribution_mass_used
                                                                    
                                        homeownership_by_age(time_period,age_index)=&
                                            homeownership_by_age(time_period,age_index)+&
                                            max(min(amortization_used-1,1),0)*distribution_mass_used
                                        
                                        housing_size_by_age(time_period,age_index)=housing_size_by_age(time_period,age_index)+&
                                            lived_in_size_used*distribution_mass_used
                                        
                                        mass_by_age(time_period,age_index)=mass_by_age(time_period,age_index)+&
                                            distribution_mass_used
                                            
                                        if (amortization_used<=1) then
                                            
                                            wealth_by_age_renter(time_period,age_index)=&
                                                wealth_by_age_renter(time_period,age_index)+&
                                                (max(min(years_left_on_mortgage_index-1,1),0)*&
                                                housing_price(time_period)*housing_size(housing_status_index)+&
                                                min(financial_grid(financial_index),0.d0)+&
                                                max(financial_grid(financial_index),0.d0))*distribution_mass_used
                                                
                                            if (in_retirement==1) then
                                            
                                                income_by_age_renter(time_period,age_index)=&
                                                    income_by_age_renter(time_period,age_index)+&
                                                    (((1-is_counterfactual)*model_average_labour_income(time_period)+&
                                                    is_counterfactual*average_labor_income_calibrated)+&
                                                    risk_free_rate(time_period)*&
                                                    max(financial_grid(financial_index),0.d0))*distribution_mass_used
                                            
                                            else
                                                
                                                income_by_age_renter(time_period,age_index)=&
                                                    income_by_age_renter(time_period,age_index)+&
                                                    ((employment_grid(age_index,employment_index)*&
                                                    labour_deterministic_efficiency(age_index))-&
                                                    max((employment_grid(age_index,employment_index)*&
                                                    labour_deterministic_efficiency(age_index)),0.d0)**&
                                                    curvature_of_labor_income_taxes+risk_free_rate(time_period)*&
                                                    max(financial_grid(financial_index),0.d0))*distribution_mass_used
                                                    
                                            end if
                                            
                                            consumption_by_age_renter(time_period,age_index)=&
                                                consumption_by_age_renter(time_period,age_index)+&
                                                consumption_used*distribution_mass_used
                                                
                                            housing_consumption_by_age_renter(time_period,age_index)=&
                                                housing_consumption_by_age_renter(time_period,age_index)+&
                                                lived_in_size_used*distribution_mass_used
                                                
                                            mass_by_age_and_renter(time_period,age_index)=&
                                                mass_by_age_and_renter(time_period,age_index)+&
                                                distribution_mass_used
                                                                        
                                        end if
                                    
                                    end if
                                                                    
                                end do
                                
                            end do
                            
                        end do
                            
                    end do
                    
                end do
                                                    
            end do
                
        end do
        
        do age_index=1,life_span
        
            wealth_by_age_renter(time_period,age_index)=&
                wealth_by_age_renter(time_period,age_index)/&
                mass_by_age_and_renter(time_period,age_index)
                
            income_by_age_renter(time_period,age_index)=&
                income_by_age_renter(time_period,age_index)/&
                mass_by_age_and_renter(time_period,age_index)
                
            consumption_by_age_renter(time_period,age_index)=&
                consumption_by_age_renter(time_period,age_index)/&
                mass_by_age_and_renter(time_period,age_index)
            
            housing_consumption_by_age_renter(time_period,age_index)=&
                housing_consumption_by_age_renter(time_period,age_index)/&
                mass_by_age_and_renter(time_period,age_index)
            
        end do
        
        do i=1,3
        
            do employment_index=1,employment_grid_size
            
                housing_size_by_employment_and_tenure(time_period,employment_index,i)=&
                    housing_size_by_employment_and_tenure(time_period,employment_index,i)/&
                    mass_by_tenure_and_employment(time_period,employment_index,i)
            
            end do
            
        end do
                         
        do employment_index=1,employment_grid_size 
        
            homeownership_by_employment(time_period,employment_index)=&
                homeownership_by_employment(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
            
            housing_size_by_employment(time_period,employment_index)=&
                housing_size_by_employment(time_period,employment_index)/&
                mass_by_employment(time_period,employment_index)
                                
        end do
        
        do age_index=1,life_span
        
            do employment_index=1,employment_grid_size 
        
                housing_size_by_employment_and_age(time_period,employment_index,age_index)=&
                    housing_size_by_employment_and_age(time_period,employment_index,age_index)/&
                    mass_by_employment_and_age(time_period,employment_index,age_index)
            
                consumption_by_employment_and_age(time_period,employment_index,age_index)=&
                    consumption_by_employment_and_age(time_period,employment_index,age_index)/&
                    mass_by_employment_and_age(time_period,employment_index,age_index)
                    
                wealth_by_employment_and_age(time_period,employment_index,age_index)=&
                    wealth_by_employment_and_age(time_period,employment_index,age_index)/&
                    mass_by_employment_and_age(time_period,employment_index,age_index)
                    
                consumption_by_age(time_period,age_index)=consumption_by_age(time_period,age_index)/&
                    mass_by_age(time_period,age_index)
                    
            end do
                                
        end do
        
        do age_index=1,life_span
                
            homeownership_by_age(time_period,age_index)=homeownership_by_age(time_period,age_index)/&
                mass_by_age(time_period,age_index)
          
            housing_size_by_age(time_period,age_index)=housing_size_by_age(time_period,age_index)/&
                mass_by_age(time_period,age_index)
                                                        
        end do
        
        do i=1,number_of_quintiles
        
            welfare_by_income_quintile_youngest(time_period,i)=&
                welfare_by_income_quintile_youngest(time_period,i)/mass_by_quintile_youngest(i)
        
        end do
        
        if (simulation_ended==1)  then
                
            do k=1,number_of_quintiles
            
                write(*,*) "tenure_by_income_quintile=", sum(tenure_by_income_quintile(time_period,k,:))
                write(*,*) "mass_of_quintile=", mass_of_quintile(k)
            
            end do
            
        end if
            
    end subroutine
    
    subroutine frac_of_households_in_support_of_policy(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: financial_index,time_period,has_house,loop_over_rent_shocks
        character*1 :: string_used
        real(8) :: distribution_mass_used,sum_of_distribution_retired,current_distribution_mass_used,&
            value_function_used,value_function_benchmark_used,frac_to_use,&
            average_welfare_benchmark,average_welfare,sum_of_distribution_1,average_welfare_1,average_welfare_2,&
            welfare_retired,welfare_retired_benchmark,mass_1
        real(8), dimension(life_span) :: sum_of_distribution_by_age
                        
        frac_in_support(time_period)=0
        frac_in_support_by_age(time_period,:)=0
        frac_in_support_using_current_dist(time_period)=0
        frac_in_support_youngest(time_period)=0
        frac_in_support_retired(time_period)=0
        
        frac_in_support_homeowners(time_period)=0
        frac_in_support_renters(time_period)=0
        frac_in_support_incumbent_renters(time_period)=0
        
        mass_homeowners(time_period)=0
        mass_renters(time_period)=0
        mass_incumbent_renters(time_period)=0
                
        average_welfare_benchmark=0
        average_welfare=0
        average_welfare_1=0
        average_welfare_2=0
        
        welfare_retired=0
        welfare_retired_benchmark=0
                
        sum_of_distribution_retired=0
        sum_of_distribution_1=0
        
        frac_in_support_by_tenure_and_employment(time_period,:,:)=0
        mass_by_tenure_and_employment(time_period,:,:)=0
                
        do i=1-benchmark_has_two_rental_markets,2
        
            write(string_used,'(i1)') i
    
            open(unit=9,file="value_function"//trim(string_used)//trim(benchmark_simulation)//".dat",action='read')
                read(9,'(F40.18)') value_function_benchmark(i)%vector_real(:,:,:,:,:,:)
            close(9)
                    
            open(unit=10,file="distribution_stationary"//trim(string_used)//trim(benchmark_simulation)//".dat",action='read')
                read(10,'(F30.20)') distribution_stationary_benchmark(i)%vector_real(:,:,:,:,:,:)
            close(10)
                        
        end do
                                    
        do age_index=1,life_span
                    
            if (age_index>life_span-retirement_span) then
            
                in_retirement=1
            
            else
            
                in_retirement=0
            
            end if
            
            do rental_opportunity_index=0,random_assignment
                    
                do years_left_on_mortgage_index=(1-benchmark_has_two_rental_markets),max_amortization_years
                                
                    if (years_left_on_mortgage_index>1) then
                        
                        has_house=1
                        
                    else
                    
                        has_house=0
                        
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1) .AND. &
                        (include_shocks_to_free_market_rent_benchmark==1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent_benchmark==1)) then
                        
                            loop_over_rent_shocks=1
                                                    
                        end if
                        
                    end if
                                    
                    do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                        (loop_over_rent_shocks*rent_shock_grid_size_benchmark+&
                        (1-loop_over_rent_shocks)*min(age_index,max_tracking_benchmark))
                        
                        do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                                           
                            do employment_index=1,employment_grid_size
                                        
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                    has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                                                            
                                    if (years_left_on_mortgage_index==0) then
                                    
                                        i=0
                                    
                                    elseif (years_left_on_mortgage_index==1) then
                                    
                                        i=1
                                        
                                    elseif (years_left_on_mortgage_index==2) then
                                    
                                        if (financial_grid(financial_index)<0) then
                                            
                                            i=3
                                            
                                        else
                                        
                                            i=2
                                            
                                        end if
                                        
                                    end if
                                                            
                                    distribution_mass_used=distribution_stationary_benchmark(years_left_on_mortgage_index)%&
                                        vector_real(age_index,financial_index,employment_index,housing_status_index,&
                                        years_holding_onto_asset_index,&
                                        min(rental_opportunity_index,benchmark_has_random_assignment))
                                        
                                    value_function_used=value_function&
                                        (max(years_left_on_mortgage_index,1-two_rental_markets))%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,&
                                        min(years_holding_onto_asset_index,max_tracking),rental_opportunity_index)
                                        
                                    value_function_benchmark_used=value_function_benchmark(years_left_on_mortgage_index)%&
                                        vector_real(age_index,financial_index,employment_index,housing_status_index,&
                                        years_holding_onto_asset_index,&
                                        min(rental_opportunity_index,benchmark_has_random_assignment))
                                                                                                                                                   
                                    current_distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,employment_index,&
                                        housing_status_index,min(years_holding_onto_asset_index,max_tracking),&
                                        rental_opportunity_index)
                                        
                                    mass_1=sum(distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,employment_index,&
                                        housing_status_index,min(years_holding_onto_asset_index,max_tracking),:))
                                        
                                    if (mass_1>0) then
                                                                                
                                        frac_to_use=distribution_mass_used*(current_distribution_mass_used/mass_1)
                                        
                                    else
                                    
                                        frac_to_use=0
                                            
                                    end if
                                    
                                    sum_of_distribution_1=sum_of_distribution_1+current_distribution_mass_used
                                
                                    mass_by_tenure_and_employment&
                                        (time_period,employment_index,max(i,1-two_rental_markets))=&
                                        mass_by_tenure_and_employment&
                                        (time_period,employment_index,max(i,1-two_rental_markets))+&
                                        distribution_mass_used
                                        
                                    if (has_house==1) then
                                    
                                        mass_homeowners(time_period)=mass_homeowners(time_period)+frac_to_use
                                        
                                    else
                                    
                                        mass_renters(time_period)=mass_renters(time_period)+frac_to_use
                                    
                                    end if
                                    
                                    if ((solve_transition_dynamics==1) .AND. (time_period==1)) then
                                        
                                        if (has_house==0) then
                                    
                                            mass_incumbent_renters(time_period)=mass_incumbent_renters(time_period)+&
                                                frac_to_use
                                                
                                        end if
                                        
                                    end if
                                        
                                    if (in_retirement==1) then
                                        
                                        sum_of_distribution_retired=sum_of_distribution_retired+&
                                            frac_to_use
                                            
                                    end if
                                    
                                    average_welfare_benchmark=average_welfare_benchmark+&
                                        value_function_benchmark_used*frac_to_use
                                        
                                    if (current_distribution_mass_used>0) then
                                        
                                        average_welfare=average_welfare+&
                                            value_function_used*current_distribution_mass_used
                                            
                                    end if
                                    
                                    if (current_distribution_mass_used>=0) then
                                                                           
                                        if (value_function_used>value_function_benchmark_used) then
                                                                                                                                
                                            if (in_retirement==1) then
                                                
                                                welfare_retired=welfare_retired+&
                                                    value_function_used*current_distribution_mass_used
                                                    
                                                welfare_retired_benchmark=welfare_retired_benchmark+&
                                                    value_function_benchmark_used*frac_to_use
                                                    
                                                if ((frac_to_use>0) .AND. &
                                                    (value_function_benchmark_used<-30)) then
                                                                                                    
    !                                                write(*,*) "value_function_benchmark_used=", &
    !                                                    value_function_benchmark_used
    !                                                write(*,*) "value_function_used=", value_function_used
                                                
                                                end if
                                                    
                                            end if
                                                                                                                                                
                                            if (age_index==1) then
                                                    
                                                frac_in_support_youngest(time_period)=&
                                                    frac_in_support_youngest(time_period)+frac_to_use
                                                                                                    
                                            end if
                                            
                                            frac_in_support_by_age(time_period,age_index)=&
                                                frac_in_support_by_age(time_period,age_index)+frac_to_use
                                            
                                            if (in_retirement==1) then
                                            
                                                frac_in_support_retired(time_period)=&
                                                    frac_in_support_retired(time_period)+frac_to_use
                                                                                        
                                            end if
                                                            
                                            frac_in_support(time_period)=frac_in_support(time_period)+frac_to_use
                                            
                                            if (has_house==0) then
                                            
                                                frac_in_support_renters(time_period)=frac_in_support_renters(time_period)+&
                                                    frac_to_use
                                            
                                            else
                                            
                                                frac_in_support_homeowners(time_period)=&
                                                    frac_in_support_homeowners(time_period)+frac_to_use
                                                
                                            end if
                                            
                                            if ((solve_transition_dynamics==1) .AND. (time_period==1)) then
                                        
                                                if (has_house==0) then
                                                
                                                    frac_in_support_incumbent_renters(time_period)=&
                                                        frac_in_support_incumbent_renters(time_period)+frac_to_use
                                                        
                                                end if
                                                
                                            end if
                                            
                                            frac_in_support_using_current_dist(time_period)=&
                                                frac_in_support_using_current_dist(time_period)+current_distribution_mass_used
                                                                                        
                                            frac_in_support_by_tenure_and_employment&
                                                (time_period,employment_index,max(i,1-two_rental_markets))=&
                                                frac_in_support_by_tenure_and_employment&
                                                (time_period,employment_index,max(i,1-two_rental_markets))+&
                                                distribution_mass_used
                                                
                                        else
                                        
                                            if (value_function_used>large_negative_num) then
                                                                                            
                                                if (in_retirement==1) then
                                                
                                                    welfare_retired=welfare_retired+&
                                                        value_function_used*frac_to_use
                                                        
                                                    welfare_retired_benchmark=welfare_retired_benchmark+&
                                                        value_function_benchmark_used*frac_to_use
                                                        
                                                end if
                                                    
                                                if (value_function_used>value_function_benchmark_used) then
                                                
    !                                                write(*,*) "value_function_benchmark_used=", value_function_benchmark_used
    !                                                write(*,*) "value_function_used=", value_function_used
                                                
                                                end if
                                                    
                                            end if
                                                
                                        end if
                                        
                                    end if
                                                                 
                                 end do
                                 
                            end do
                             
                        end do
                        
                    end do
                    
                end do
                    
            end do
            
        end do
        
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
            
        end do
        
        sum_of_distribution_youngest=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution_youngest=sum_of_distribution_youngest+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,1,:,:,:,:,:))
            
        end do
        
        sum_of_distribution_by_age(:)=0
        
        do age_index=1,life_span
        
            do i=1-two_rental_markets,2
                    
                sum_of_distribution_by_age(age_index)=sum_of_distribution_by_age(age_index)+&
                    sum(distribution_stationary(i)%vector_transition_real(time_period,age_index,:,:,:,:,:))
                
            end do
            
        end do
                
        do i=1-two_rental_markets,3
        
            do employment_index=1,employment_grid_size
                            
                frac_in_support_by_tenure_and_employment(time_period,employment_index,i)=&
                    frac_in_support_by_tenure_and_employment(time_period,employment_index,i)/&
                    mass_by_tenure_and_employment(time_period,employment_index,i)
                                    
            end do
            
        end do
                
        welfare_retired=welfare_retired/sum_of_distribution_retired
        welfare_retired_benchmark=welfare_retired_benchmark/sum_of_distribution_retired
                
        average_welfare=average_welfare/sum_of_distribution_1
        average_welfare_benchmark=average_welfare_benchmark/sum_of_distribution
                                        
        frac_in_support(time_period)=frac_in_support(time_period)/sum_of_distribution
        
        frac_in_support_homeowners(time_period)=frac_in_support_homeowners(time_period)/mass_homeowners(time_period)
        frac_in_support_renters(time_period)=frac_in_support_renters(time_period)/mass_renters(time_period)
        
        if ((solve_transition_dynamics==1) .AND. (time_period==1)) then
        
            frac_in_support_incumbent_renters(time_period)=&
                frac_in_support_incumbent_renters(time_period)/mass_incumbent_renters(time_period)
                
        end if
        
        do age_index=1,life_span
        
            frac_in_support_by_age(time_period,age_index)=frac_in_support_by_age(time_period,age_index)/&
                sum_of_distribution_by_age(age_index)
            
        end do

        frac_in_support_using_current_dist(time_period)=frac_in_support_using_current_dist(time_period)/sum_of_distribution        
        
        frac_in_support_youngest(time_period)=frac_in_support_youngest(time_period)/sum_of_distribution_youngest
        
        frac_in_support_retired(time_period)=frac_in_support_retired(time_period)/sum_of_distribution_retired
        
        if (solve_transition_dynamics==0) then
        
            write(*,*) "frac_in_support=", frac_in_support(time_period)
            write(*,*) "frac_in_support_using_current_dist=", frac_in_support_using_current_dist
            write(*,*) "frac_in_support_retired=", frac_in_support_retired(time_period)
            write(*,*) "frac_in_support_youngest=", frac_in_support_youngest(time_period)
            write(*,*) "frac_in_support_homeowners=", frac_in_support_homeowners(time_period)
            write(*,*) "frac_in_support_renters=", frac_in_support_renters(time_period)
            
        end if
            
    end subroutine
        
    subroutine solve_value_function_and_policy_functions(time_period)
    
        use Global_Vars
        !$ use omp_lib
        
        implicit none
        
        integer :: financial_index,time_period,max_future_mortgage_index_1=negative_financial_grid_size,&
            run_internal_grid,number_of_threads_used,has_house,loop_over_rent_shocks_1
        real(8) :: max_mortgage_value_allowed_1
        
        do i=1-two_rental_markets,2
                        
            value_function(i)%vector_transition_real(time_period,:,:,:,:,:,:)=large_negative_num
            value_function(i)%vector_transition_real(time_period,life_span+1,:,:,:,:,:)=0
            
        end do
                
        do i=1-two_rental_markets,2
        
            policy_function_financial_index(i)%vector_transition_integer(time_period,:,:,:,:,:,:)=negative_financial_grid_size
            policy_function_housing_status_index(i)%vector_transition_integer(time_period,:,:,:,:,:,:)=0        
            policy_function_consumption(i)%vector_transition_real(time_period,:,:,:,:,:,:)=0
            policy_function_space_lived_in_index(i)%vector_transition_integer(time_period,:,:,:,:,:,:)=0
            policy_function_space_lived_in_size(i)%vector_transition_real(time_period,:,:,:,:,:,:)=0
            policy_function_wage_income_tax_paid(i)%vector_transition_real(time_period,:,:,:,:,:,:)=0
            policy_function_years_of_amortization_index(i)%vector_transition_integer(time_period,:,:,:,:,:,:)=1   
            policy_function_rent_subsidy(i)%vector_transition_real(time_period,:,:,:,:,:,:)=0
            policy_function_years_holding_onto_asset(i)%vector_transition_integer(time_period,:,:,:,:,:,:)=1
            frac_mov_optimal_1(i)%vector_transition_real(time_period,:,:,:,:,:,:)=0
            fin_index_frac_mov_to_optimal(i)%vector_transition_integer(time_period,:,:,:,:,:,:)=negative_financial_grid_size
                
        end do
        
        negative_consumption=0
        
        call cpu_time(value_function_iteration_start_time)
        
        if (solve_transition_dynamics>=0) then
        
            number_of_threads_used=max_CPUs

            CALL OMP_SET_NUM_THREADS(number_of_threads_used)
        
            !$omp parallel
            !$omp master
            !$ write(*,*) "number of threads = ", omp_get_num_threads()
            !$omp end master
            !$omp end parallel
            
        end if
                       
        do age_index=life_span,1,-1
                
            if (age_index>(life_span-retirement_span)) then
                
                in_retirement=1
            
            else
            
                in_retirement=0 
            
            end if
                        
            if (age_index==1) then
            
                newborn_household=1
                
            else
            
                newborn_household=0
                
            end if
            
            if (age_index==life_span) then
            
                can_get_mortgage=0
                                
            else
                            
                if (age_index<=life_span-retirement_span) then
                
                    can_get_mortgage=1
                    
                else
                
                    can_get_mortgage=0
                
                end if
                
            end if
            
            if (age_index>=(life_span-retirement_span)) then
                
                at_least_one_period_before_retirement=1
            
            else
                
                at_least_one_period_before_retirement=0
            
            end if
            
            do rental_opportunity_index=0,random_assignment
                        
                do years_left_on_mortgage_index=newborn_household+(1-newborn_household)*(1-two_rental_markets),&
                    newborn_household*1+(1-newborn_household)*max_amortization_years

                    has_house=0

                    if (years_left_on_mortgage_index>1) then

                        has_house=1

                    end if
                    
                    loop_over_rent_shocks_1=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks_1=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                            loop_over_rent_shocks_1=1
                                                    
                        end if
                        
                    end if
                    
                    do employment_index=1,employment_grid_size
                    
                        can_be_homeless=0
                    
                        if (employment_index==1) then
                        
                            can_be_homeless=1
                            
                        end if
                                        
                        do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                            has_house*index_of_smallest_house_hhld_can_buy,(1-newborn_household)*(housing_status_grid_size-1)
                                                                                                        
                            do years_holding_onto_asset_index=1,newborn_household*1+&
                                (1-newborn_household)*(has_house*1+(1-has_house)*&
                                (loop_over_rent_shocks_1*rent_shock_grid_size+&
                                (1-loop_over_rent_shocks_1)*min(age_index,max_tracking)))
                                                                        
                                !$omp parallel do default(none) &
                                !$omp& shared(frac_mov_optimal_1,fin_index_frac_mov_to_optimal,&
                                !$omp& policy_function_consumption,policy_function_space_lived_in_size,&
                                !$omp& policy_function_space_lived_in_index,&
                                !$omp& policy_function_financial_index,policy_function_wage_income_tax_paid,&
                                !$omp& policy_function_housing_status_index,&
                                !$omp& policy_function_rent_subsidy,policy_function_years_holding_onto_asset,&
                                !$omp& policy_function_years_of_amortization_index,housing_status_index,&
                                !$omp& model_average_rental_unit_size,rent,negative_consumption,has_mortgage,&
                                !$omp& model_average_labour_income,housing_status_utility_function,&
                                !$omp& net_govt_revenues,relative_share_of_housing,survival_probability,discount_factor,&
                                !$omp& value_function_guess,value_function,employment_process,&
                                !$omp& years_left_on_mortgage_index,years_holding_onto_asset_index,employment_index,&
                                !$omp& large_negative_num,labour_deterministic_efficiency,model_ss_benefits,housing_size,&
                                !$omp& housing_price,mortgage_interest_rate,property_tax,can_be_homeless,&
                                !$omp& curvature_of_labor_income_taxes,labor_income_tax_rate,&
                                !$omp& financial_grid,consumption_tax,increase_in_labour_income_tax,&
                                !$omp& intergenerational_employment_process,unconditional_employment_probability,&
                                !$omp& age_index,discount_factor_on_child,risk_free_rate,time_period,&
                                !$omp& rental_management_costs,can_get_mortgage,not_allowed_to_move_back,&
                                !$omp& employment_grid,run_internal_grid,at_least_one_period_before_retirement,&
                                !$omp& disutility_of_landlords,size_of_smallest_house_hhld_can_buy,&
                                !$omp& model_average_owner_occupied_unit_size,has_house,&
                                !$omp& in_retirement,newborn_household,lump_sum_transfer,max_mortgage_value_allowed_1,&
                                !$omp& max_future_mortgage_index_1,loop_over_rent_shocks_1,scaled_household_size,&
                                !$omp& probability_rental_opportunity,rental_opportunity_index)
                        
                                do financial_index=newborn_household*negative_financial_grid_size+&
                                    (1-newborn_household)*(has_house*1+(1-has_house)*negative_financial_grid_size),&
                                    include_exogenous_endowment*financial_grid_size+(1-include_exogenous_endowment)*&
                                    (newborn_household*negative_financial_grid_size+(1-newborn_household)*financial_grid_size)
                                                   
                                    if (in_retirement==1) then
                        
                                        max_mortgage_value_allowed_1=-min(financial_grid(financial_index),0.d0)*&
                                            (1-fraction_to_repay)
                                    
                                        max_future_mortgage_index_1=&
                                            min(max(minloc(max_mortgage_value_allowed_1+&
                                            financial_grid(1:negative_financial_grid_size),1,&
                                            mask=(max_mortgage_value_allowed_1+&
                                            financial_grid(1:negative_financial_grid_size))>0),1),&
                                            negative_financial_grid_size)
                                                                        
                                    end if
                                                                                                                  
                                    total_household_wealth=has_house*housing_price(time_period)*&
                                        housing_size(housing_status_index)+max(financial_grid(financial_index),0.d0)
                                                                             
                                    if (years_left_on_mortgage_index<=1) then
                                    
                                        value_function_owner=large_negative_num
                                                                        
                                        call renters_problem(time_period,financial_index)
                                                                    
                                    else
                                    
                                        value_function_renter=large_negative_num
                                                    
                                        call owners_problem(time_period,financial_index)   
                                                                        
                                    end if
                                
                                    call which_is_better(time_period,financial_index)
                                                    
                                    if (do_internal_computations==1) then
                                    
                                        if (years_left_on_mortgage_index<=1) then
                                        
                                            if (optimal_household_can_consume_renter==1)  then
                                            
                                                run_internal_grid=1
                                                
                                            else
                                            
                                                run_internal_grid=0
                                                
                                            end if
                                            
                                        else
                                        
                                            if (optimal_household_can_consume_owner==1)  then
                                            
                                                run_internal_grid=1
                                                
                                            else
                                            
                                                run_internal_grid=0
                                                                                            
                                            end if
                                        
                                        end if
                                        
                                        if (run_internal_grid==1) then
                                    
                                            amortization_used_dist=&
                                                policy_function_years_of_amortization_index&
                                                (years_left_on_mortgage_index)%&
                                                vector_transition_integer(time_period,age_index,&
                                                financial_index,employment_index,&
                                                housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index)
                                    
                                            call internal_computations&
                                                (value_function(years_left_on_mortgage_index)%&
                                                vector_transition_real(time_period,age_index,financial_index,&
                                                employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index),&
                                                employment_grid(age_index,employment_index)*&
                                                labour_deterministic_efficiency(age_index),&
                                                policy_function_financial_index(years_left_on_mortgage_index)%&
                                                vector_transition_integer(time_period,age_index,financial_index,&
                                                employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index),&
                                                max(min(amortization_used_dist-1,1),0),&
                                                policy_function_space_lived_in_index(years_left_on_mortgage_index)%&
                                                vector_transition_integer(time_period,age_index,financial_index,&
                                                employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index),&
                                                policy_function_housing_status_index(years_left_on_mortgage_index)%&
                                                vector_transition_integer(time_period,age_index,financial_index,&
                                                employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index),&
                                                amortization_used_dist,&
                                                policy_function_years_holding_onto_asset(years_left_on_mortgage_index)%&
                                                vector_transition_integer(time_period,age_index,financial_index,&
                                                employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index),&
                                                financial_index,time_period,optimal_after_tax_wealth_net_of_expenses,&
                                                max_future_mortgage_index_1)

                                            value_function(years_left_on_mortgage_index)%&
                                                vector_transition_real(time_period,age_index,financial_index,&
                                                employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index)=&
                                                val_fun_internal
                                                
                                        end if
                                            
                                    end if
                                    
                                    if (optimal_after_tax_wealth_net_of_expenses>=0) then
                                                                                                            
                                        call check_budget_constraint_clears&
                                            (time_period,optimal_after_tax_wealth_net_of_expenses,financial_index)
                                                                                    
                                    end if
                                    
                                end do
                                    
                                !$omp end parallel do
                                                                                               
                            end do
                                                                        
                        end do
                        
                    end do
                
                end do
                
            end do
            
        end do
        
        if (solve_transition_dynamics==0) then
                
            loop_count=loop_count+1
            
        end if
         
        call cpu_time(value_function_iteration_stop_time)
        
        write(*,*) "time=", value_function_iteration_stop_time-value_function_iteration_start_time
        
        if ((solve_transition_dynamics==1) .AND. (is_counterfactual==1)) then
        
            write(*,*) "time_period=", time_period
            
        end if
    
    end subroutine
    
    subroutine renters_problem(time_period,financial_index)

        use Global_Vars
        
        implicit none
    
        integer :: time_period,financial_index
                                                                                                              
        call renter_to_renter_problem(time_period,financial_index)
        
        if (add_net_worth_bequest==1) then

            if (age_index>life_span) then
            
                value_function_renter_owner=large_negative_num
                
            else
            
                call renter_to_owner_problem(time_period,financial_index)

            end if
            
        else
                
            if (age_index==life_span) then
            
                value_function_renter_owner=large_negative_num
                
            else
            
                call renter_to_owner_problem(time_period,financial_index)
            
            end if
            
        end if           
                    
        if (value_function_renter_renter>value_function_renter_owner) then
                        
            value_function_renter=value_function_renter_renter
            
            optimal_renter_indeces(1)=optimal_renter_renter_indeces(1)
            optimal_renter_indeces(2)=optimal_renter_renter_indeces(2)
            optimal_renter_indeces(3)=optimal_renter_renter_indeces(3)    

            if (optimal_renter_indeces(2)>1) then

                write(*,*) "odddddd"

            end if
                       
            optimal_gross_wage_income_renter=optimal_gross_wage_income_renter_renter
            optimal_wage_income_tax_paid_renter=optimal_wage_income_tax_paid_renter_renter
            optimal_after_tax_wealth_net_of_expenses_renter=optimal_after_tax_wealth_net_of_expenses_renter_renter
            optimal_consumption_renter=optimal_consumption_renter_renter
            optimal_space_lived_in_renter=optimal_space_lived_in_renter_renter
            optimal_rent_subsidy_renter=optimal_rent_subsidy_renter_renter
            optimal_years_holding_onto_asset_renter=optimal_years_holding_onto_asset_renter_renter
            optimal_household_can_consume_renter=optimal_household_can_consume_renter_renter
                    
        else
        
            value_function_renter=value_function_renter_owner
            
            optimal_renter_indeces(1)=optimal_renter_owner_indeces(1)
            optimal_renter_indeces(2)=optimal_renter_owner_indeces(2)
            optimal_renter_indeces(3)=optimal_renter_owner_indeces(3)
                
            optimal_gross_wage_income_renter=optimal_gross_wage_income_renter_owner
            optimal_wage_income_tax_paid_renter=optimal_wage_income_tax_paid_renter_owner
            optimal_after_tax_wealth_net_of_expenses_renter=optimal_after_tax_wealth_net_of_expenses_renter_owner
            optimal_consumption_renter=optimal_consumption_renter_owner
            optimal_space_lived_in_renter=optimal_space_lived_in_renter_owner
            optimal_rent_subsidy_renter=0
            optimal_years_holding_onto_asset_renter=optimal_years_holding_onto_asset_renter_owner
            optimal_household_can_consume_renter=optimal_household_can_consume_renter_owner
        
        end if
                                        
    end subroutine
    
    subroutine renter_to_renter_problem(time_period,financial_index)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,financial_index,changing_homes,loop_over_rent_shocks,not_allowed_to_move_back_1,&
            choose_non_market_housing,choose_to_be_homeless,loop_over_current_house,&
            future_housing_status_index_to_use,lowest_rental_market_index,highest_rental_market_index
        real(8) :: after_tax_wealth_net_of_expenses_renter_renter
        
        largest_value=large_negative_num
        
        optimal_renter_renter_indeces(1)=negative_financial_grid_size
        optimal_renter_renter_indeces(2)=1
        optimal_renter_renter_indeces(3)=0
                 
        optimal_consumption_renter_renter=0
        optimal_after_tax_wealth_net_of_expenses_renter_renter=0
        optimal_gross_wage_income_renter_renter=0
        optimal_wage_income_tax_paid_renter_renter=0
        optimal_space_lived_in_renter_renter=0
        optimal_rent_subsidy_renter_renter=0
        optimal_years_holding_onto_asset_renter_renter=1          
        optimal_household_can_consume_renter_renter=0
        
        not_allowed_to_move_back_1=0
        
        if (age_index>=cannot_move_back_age) then
        
            not_allowed_to_move_back_1=include_moving_back*1
            
        end if
        
        loop_over_current_house=0
        
        if (random_assignment==1) then 
        
            ! if the household is in the controlled market, it loops also on its current house
            if ((years_left_on_mortgage_index==1) .AND. &
                (include_rent_decrease==1) .AND. (age_index>1)) then
        
                loop_over_current_house=1
                
            end if
        
        end if
                
        changing_homes=0
                
        do future_housing_status_index=can_be_homeless*lowest_housing_grid_point+&
            (1-can_be_homeless)*(1-not_allowed_to_move_back_1)*second_lowest_housing_grid_point,&
            largest_house_size_hhld_can_rent+loop_over_current_house
            
            future_housing_status_index_to_use=future_housing_status_index
            
            if (future_housing_status_index>largest_house_size_hhld_can_rent) then
            
                future_housing_status_index_to_use=housing_status_index
                
            end if
            
            choose_non_market_housing=0
            choose_to_be_homeless=0
            
            if ((not_allowed_to_move_back_1==1) .AND. &
               (future_housing_status_index_to_use==second_lowest_housing_grid_point)) then
                       
            else
            
                if ((future_housing_status_index_to_use<=second_lowest_housing_grid_point) .AND. &
                    (include_moving_back==1)) then
                    
                    choose_non_market_housing=1
                    
                    if ((future_housing_status_index_to_use==lowest_housing_grid_point) .AND. (include_homelessness==1)) then
                    
                        choose_to_be_homeless=1
                    
                    end if
                
                end if
                
                lowest_rental_market_index=1-two_rental_markets
                highest_rental_market_index=1
                
                if (choose_non_market_housing==1) then
                
                    highest_rental_market_index=lowest_rental_market_index
                    
                end if
                
                if (random_assignment==1) then
                
                    if (future_housing_status_index>largest_house_size_hhld_can_rent) then
                
                        lowest_rental_market_index=years_left_on_mortgage_index
                        highest_rental_market_index=years_left_on_mortgage_index
                    
                    else
                        
                        lowest_rental_market_index=rental_opportunity_index
                        highest_rental_market_index=rental_opportunity_index
                        
                    end if                       
                    
                end if
                
                do rental_market_index=highest_rental_market_index,lowest_rental_market_index,-1
                    
                    loop_over_rent_shocks=0
                    
                    if (choose_non_market_housing==0) then
                
                        if (rental_market_index==0) then
                        
                            loop_over_rent_shocks=1
                        
                        else
                        
                            if ((two_rental_markets==0) .AND. (rental_market_index==1) .AND. &
                                (include_shocks_to_free_market_rent==1)) then
                            
                                loop_over_rent_shocks=1
                                
                            end if
                        
                        end if

                    end if
                                        
                    years_in_rental=1

                    if (choose_non_market_housing==0) then
                                                    
                        if ((future_housing_status_index_to_use==housing_status_index) .AND. &
                            (years_left_on_mortgage_index==1) .AND. (rental_market_index==1) .AND. &
                            (include_rent_decrease==1) .AND. (age_index>1)) then
                            
                            years_in_rental=min(years_holding_onto_asset_index+1,max_tracking)
                                                
                        end if
                            
                    end if
                                                                                        
                    call calculate_after_tax_wealth&
                        (financial_index,negative_financial_grid_size,years_left_on_mortgage_index,rental_market_index,&
                        housing_status_index,future_housing_status_index_to_use,years_holding_onto_asset_index,&
                        years_in_rental,0,0,0,0,1,time_period)
                        ! owner_to_owner_move_1,owner_to_renter_1,renter_to_owner_1,owner_to_owner_stay_1,renter_to_renter_1
                                            
                    if ((future_housing_status_index_to_use==0) .AND. &
                        (after_tax_wealth_net_of_expenses<min_consumption_guaranteed)) then
                                        
                        rent_subsidy=min_consumption_guaranteed-after_tax_wealth_net_of_expenses
                            
                        after_tax_wealth_net_of_expenses_renter_renter=&
                            after_tax_wealth_net_of_expenses+rent_subsidy
                                                
                    else
                                
                        rent_subsidy=0
                        after_tax_wealth_net_of_expenses_renter_renter=after_tax_wealth_net_of_expenses
                        
                    end if
                    
                    do future_financial_index=negative_financial_grid_size,financial_grid_size
                                          
                        consumption_vector(future_financial_index)=(after_tax_wealth_net_of_expenses_renter_renter-&
                            financial_grid(future_financial_index))/(1+consumption_tax)
                                                                                                                                                         
                        if (consumption_vector(future_financial_index)>0) then
                                                                                          
                            flow_utility_vector(future_financial_index)=&
                                (housing_status_utility_function&
                                (future_housing_status_index_to_use,0,years_in_rental,rental_market_index)*&
                                (consumption_vector(future_financial_index)**((1-risk_aversion_consumption)*&
                                (1-relative_share_of_housing))))/(1-risk_aversion_consumption)
                            
                            flow_utility_vector(future_financial_index)=flow_utility_vector(future_financial_index)*&
                                scaled_household_size(age_index)
                                                                                
                            future_utility_vector=0
                                                    
                            if (add_net_worth_bequest>=1) then
                            
                                if ((add_net_worth_bequest==1) .AND. (age_index==life_span)) then
                                
                                    if (financial_grid(future_financial_index)>0) then
                                
                                        future_utility_vector(future_financial_index)=&
                                            future_utility_vector(future_financial_index)+&
                                            discount_factor_on_child*(financial_grid(future_financial_index)**&
                                            (1-risk_aversion_consumption))/(1-risk_aversion_consumption)
                                            
                                    else
                                    
                                        future_utility_vector(future_financial_index)=large_negative_num
                                    
                                    end if
                                
                                elseif (add_net_worth_bequest==2) then
                                                                                                           
                                    do future_employment_index=1,employment_grid_size
                                    
                                        do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                                                 
                                            future_utility_vector(future_financial_index)=&
                                                future_utility_vector(future_financial_index)+&
                                                discount_factor_on_child*(1-survival_probability(age_index))*(use_GKKOC_process*&
                                                intergenerational_employment_process(employment_index,future_employment_index)+&
                                                (1-use_GKKOC_process)*&
                                                unconditional_employment_probability(future_employment_index))*&
                                                probability_rental_opportunity(future_rental_opportunity_index)*&
                                                value_function_guess(rental_market_index)%vector_youngest_real&
                                                (future_financial_index,future_employment_index,&
                                                future_housing_status_index_to_use,1,future_rental_opportunity_index)
                                            
                                        end do
                                                                                                                                                                                                        
                                    end do

                                end if
                                                   
                            end if
                                                                                  
                            do future_employment_index=&
                                at_least_one_period_before_retirement*employment_index+&
                                (1-at_least_one_period_before_retirement)*1,&
                                at_least_one_period_before_retirement*employment_index+&
                                (1-at_least_one_period_before_retirement)*employment_grid_size
                                
                                do future_rent_shock_index=1,loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)
                                
                                    do future_rental_opportunity_index=0,random_assignment
                                                                                                                        
                                        future_utility_vector(future_financial_index)=&
                                            future_utility_vector(future_financial_index)+&
                                            (loop_over_rent_shocks*probability_of_rent_shock(future_rent_shock_index)+&
                                            (1-loop_over_rent_shocks))*discount_factor*survival_probability(age_index)*&
                                            employment_process(choose_to_be_homeless,age_index,&
                                            employment_index,future_employment_index)*&
                                            probability_rental_opportunity(future_rental_opportunity_index)*&
                                            value_function(rental_market_index)%vector_transition_real&
                                            (min(time_period+solve_transition_dynamics,transition_length),age_index+1,&
                                            future_financial_index,future_employment_index,future_housing_status_index_to_use,&
                                            loop_over_rent_shocks*future_rent_shock_index+&
                                            (1-loop_over_rent_shocks)*years_in_rental,future_rental_opportunity_index)
                                            
                                    end do
                                                                                                                    
                                end do
                                                                                                    
                            end do
                                                       
                            value_function_renter_renter=flow_utility_vector(future_financial_index)+&
                                future_utility_vector(future_financial_index)
                                              
                            if (value_function_renter_renter>largest_value) then
                                                                                                
                                largest_value=value_function_renter_renter
                                                            
                                optimal_renter_renter_indeces(1)=future_financial_index
                                
                                if ((future_housing_status_index_to_use<=second_lowest_housing_grid_point) .AND. &
                                    (include_moving_back==1))  then
                                
                                    optimal_renter_renter_indeces(2)=1-two_rental_markets
                                
                                else
                                                            
                                    optimal_renter_renter_indeces(2)=rental_market_index
                                                                        
                                end if
                                
                                optimal_renter_renter_indeces(3)=future_housing_status_index_to_use
                                
                                optimal_consumption_renter_renter=consumption_vector(optimal_renter_renter_indeces(1)) 
                                                  
                                optimal_after_tax_wealth_net_of_expenses_renter_renter=&
                                    after_tax_wealth_net_of_expenses_renter_renter
                                optimal_gross_wage_income_renter_renter=gross_wage_income
                                optimal_wage_income_tax_paid_renter_renter=wage_income_tax_paid
                                optimal_space_lived_in_renter_renter=future_housing_status_index_to_use
                                optimal_rent_subsidy_renter_renter=rent_subsidy
                                optimal_household_can_consume_renter_renter=1
                                optimal_years_holding_onto_asset_renter_renter=years_in_rental
                                                                                                                                                                                
                            end if
                                                                                                                                                                                                                                           
                        end if
                        
                    end do
                    
                end do

            end if
            
        end do
        
        value_function_renter_renter=largest_value
                    
    end subroutine
    
    subroutine renter_to_owner_problem(time_period,financial_index)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,financial_index,smallest_house,second_smallest_house
        integer :: max_future_mortgage_index,chooses_positive_mortgage,choose_to_be_homeless
        real(8) :: max_mortgage_value_allowed,value_of_inheritance
                
        smallest_house=0
        second_smallest_house=0
        choose_to_be_homeless=0
                
        largest_value=large_negative_num
        
        optimal_renter_owner_indeces(1)=1
        optimal_renter_owner_indeces(2)=max_amortization_years
        optimal_renter_owner_indeces(3)=index_of_smallest_house_hhld_can_buy
                 
        optimal_consumption_renter_owner=0
        optimal_after_tax_wealth_net_of_expenses_renter_owner=0
        optimal_gross_wage_income_renter_owner=0
        optimal_wage_income_tax_paid_renter_owner=0
        optimal_space_lived_in_renter_owner=index_of_smallest_house_hhld_can_buy
        optimal_years_holding_onto_asset_renter_owner=1
        optimal_household_can_consume_renter_owner=0
        
        future_housing_status_loop_1: do future_housing_status_index=index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
            
            if (can_get_mortgage==1) then
            
                if (housing_price(time_period)*housing_size(future_housing_status_index)<=&
                    fraction_of_average_house_price_data*((1-is_counterfactual)*&
                    model_average_owner_occupied_unit_size(time_period)*&
                    housing_price(time_period)+is_counterfactual*&
                    average_owner_occupied_house_size_calibrated*calibrated_stationary_housing_price)) then
                                
                    smallest_house=1
                    
                elseif (housing_price(time_period)*housing_size(future_housing_status_index)<=&
                    2*fraction_of_average_house_price_data*((1-is_counterfactual)*&
                    model_average_owner_occupied_unit_size(time_period)*&
                    housing_price(time_period)+is_counterfactual*&
                    average_owner_occupied_house_size_calibrated*calibrated_stationary_housing_price)) then 
                    
                    second_smallest_house=1
                    
                end if
                                
                if (in_retirement==1) then
                                
                    max_future_mortgage_index=negative_financial_grid_size
                        
                else
                
                    max_mortgage_value_allowed=&
                        max(housing_price(time_period)*housing_size(future_housing_status_index)*&
                        (real(1)-real(minimum_downpayment)+smallest_house*min_down_of_smallest+&
                        second_smallest_house*min_down_of_second_smallest),0.d0)
                
                    max_future_mortgage_index=min(max(minloc(max_mortgage_value_allowed+&
                        financial_grid(1:negative_financial_grid_size),1,mask=(max_mortgage_value_allowed+&
                        financial_grid(1:negative_financial_grid_size))>0),1),negative_financial_grid_size)
                    
                end if
                    
            else
            
                max_future_mortgage_index=negative_financial_grid_size
            
            end if
                        
            future_mortgage_loop_2: do future_mortgage_index=financial_grid_size,max_future_mortgage_index,-1
                
                if (future_mortgage_index<negative_financial_grid_size) then
                
                    chooses_positive_mortgage=1
                
                else
                
                    chooses_positive_mortgage=0
                
                end if
                                
                do future_years_of_amortization_index=2,max_amortization_years
                                                                                               
                    if (((total_household_wealth+min(financial_grid(financial_index),0.d0)*&
                        (1+mortgage_interest_rate(time_period))-(1+cost_of_buying)*housing_price(time_period)*&
                        housing_size(future_housing_status_index)-min(financial_grid(future_mortgage_index),0.d0))>0) .AND. &
                        (((-financial_grid(future_mortgage_index)/(housing_price(time_period)*&
                        housing_size(future_housing_status_index)))<=(1-minimum_downpayment+smallest_house*min_down_of_smallest+&
                        second_smallest_house*min_down_of_second_smallest)) .OR. &
                        (chooses_positive_mortgage==0))) then
                            
                        do space_lived_in_index=future_housing_status_index,future_housing_status_index
                                                            
                            space_lived_in_index_feasible=space_lived_in_index
                                                        
                            if ((-financial_grid(future_mortgage_index)/&
                                housing_price(time_period)*housing_size(future_housing_status_index))>0.8) then
                                                          
                                max_mortgage_payments_as_fraction_of_income=max_debt_to_income_ratio
                                    
                            else
                                            
                                max_mortgage_payments_as_fraction_of_income=max_debt_to_income_ratio_below_80
                                                
                            end if
                                
                            call calculate_after_tax_wealth&
                                (financial_index,future_mortgage_index,years_left_on_mortgage_index,&
                                (1-two_rental_markets),housing_status_index,future_housing_status_index,&
                                years_holding_onto_asset_index,years_holding_onto_asset_index,&
                                0,0,1,0,0,time_period)
                                ! owner_to_owner_move_1,owner_to_renter_1,renter_to_owner_1,owner_to_owner_stay_1,renter_to_renter_1
                                
                            if (chooses_positive_mortgage==1) then
                                
                                spending_on_housing=(-min(financial_grid(future_mortgage_index),0.d0)*&
                                    (mortgage_interest_rate(time_period)+average_fraction_of_mortgage_repaid))+&
                                    housing_price(time_period)*housing_size(future_housing_status_index)*&
                                    (fraction_of_property_taxes_in_GDS*property_tax(time_period)+&
                                    include_housing_depreciation_in_housing_costs*housing_depreciation_rate)
                                
                            else
                            
                                spending_on_housing=0
                                
                            end if
                                                                           
                            if (((spending_on_housing/(gross_wage_income+investment_income))<=&
                                max_mortgage_payments_as_fraction_of_income) .OR. &
                                (future_mortgage_index>=negative_financial_grid_size)) then
                                                                
                                consumption_vector=(after_tax_wealth_net_of_expenses-&
                                    financial_grid(future_mortgage_index))/(1+consumption_tax)
                                                                                        
                                if (consumption_vector(future_mortgage_index)>0) then
                                                                                      
                                    flow_utility_vector(future_mortgage_index)=&
                                        (housing_status_utility_function(space_lived_in_index_feasible,1,1,2)*&
                                        (consumption_vector(future_mortgage_index)**&
                                        ((1-risk_aversion_consumption)*(1-relative_share_of_housing))))/&
                                        (1-risk_aversion_consumption)
                                        
                                    flow_utility_vector(future_mortgage_index)=flow_utility_vector(future_mortgage_index)*&
                                        scaled_household_size(age_index)
                                                                                                                            
                                    future_utility_vector=0
                                                                                                                  
                                    if (add_net_worth_bequest>=1) then
                                    
                                        if ((add_net_worth_bequest==1) .AND. (age_index==life_span)) then
                                        
                                            value_of_inheritance=&
                                                housing_size(future_housing_status_index)*housing_price(time_period)+&
                                                financial_grid(future_mortgage_index)
                            
                                            if (value_of_inheritance>0) then
                                        
                                                future_utility_vector(future_mortgage_index)=&
                                                    future_utility_vector(future_mortgage_index)+&
                                                    discount_factor_on_child*(value_of_inheritance**&
                                                    (1-risk_aversion_consumption))/(1-risk_aversion_consumption)
                                                    
                                            else
                                            
                                                future_utility_vector(future_mortgage_index)=large_negative_num
                                            
                                            end if
                                            
                                        elseif (add_net_worth_bequest==2) then
                                                                                        
                                            do future_employment_index=1,employment_grid_size
                                            
                                                do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                                                                                                              
                                                    future_utility_vector(future_mortgage_index)=&
                                                        future_utility_vector(future_mortgage_index)+&
                                                        discount_factor_on_child*(1-survival_probability(age_index))*&
                                                        probability_rental_opportunity(future_rental_opportunity_index)*&
                                                        (use_GKKOC_process*intergenerational_employment_process&
                                                        (employment_index,future_employment_index)+&
                                                        (1-use_GKKOC_process)*unconditional_employment_probability&
                                                        (future_employment_index))*&
                                                        value_function_guess(future_years_of_amortization_index)%&
                                                        vector_youngest_real(future_mortgage_index,future_employment_index,&
                                                        future_housing_status_index,1,future_rental_opportunity_index)
                                                    
                                                end do
                                                                                                                                                                                                                                        
                                            end do
                                            
                                        end if
                                                                                                        
                                    end if
                                                                                    
                                    do future_employment_index=&
                                        at_least_one_period_before_retirement*employment_index+&
                                        (1-at_least_one_period_before_retirement)*1,&
                                        at_least_one_period_before_retirement*employment_index+&
                                        (1-at_least_one_period_before_retirement)*employment_grid_size
                                                                 
                                        do future_rental_opportunity_index=0,random_assignment
                                                                 
                                            future_utility_vector(future_mortgage_index)=&
                                                future_utility_vector(future_mortgage_index)+&
                                                discount_factor*survival_probability(age_index)*&
                                                probability_rental_opportunity(future_rental_opportunity_index)*&
                                                employment_process(choose_to_be_homeless,age_index,&
                                                employment_index,future_employment_index)*&
                                                value_function(future_years_of_amortization_index)%vector_transition_real&
                                                (min(time_period+solve_transition_dynamics,transition_length),&
                                                age_index+1,future_mortgage_index,future_employment_index,&
                                                future_housing_status_index,1,future_rental_opportunity_index)
                                                
                                        end do
                                                                                                                                                                                               
                                    end do
                                                                     
                                    value_function_renter_owner=flow_utility_vector(future_mortgage_index)+&
                                        future_utility_vector(future_mortgage_index)
                                                                                   
                                    if (value_function_renter_owner>largest_value) then
                                
                                        largest_value=value_function_renter_owner
                                                                          
                                        optimal_renter_owner_indeces(1)=future_mortgage_index
                                        optimal_renter_owner_indeces(2)=future_years_of_amortization_index
                                        optimal_renter_owner_indeces(3)=future_housing_status_index
                        
                                        optimal_consumption_renter_owner=consumption_vector(optimal_renter_owner_indeces(1))
                                        optimal_after_tax_wealth_net_of_expenses_renter_owner=after_tax_wealth_net_of_expenses
                                            
                                        optimal_gross_wage_income_renter_owner=gross_wage_income
                                        optimal_wage_income_tax_paid_renter_owner=wage_income_tax_paid
                                        optimal_space_lived_in_renter_owner=space_lived_in_index_feasible
                                        optimal_household_can_consume_renter_owner=1
                                        optimal_years_holding_onto_asset_renter_owner=1
                                                                                                                                                            
                                    end if
                                                                                    
                                end if
                                
                            end if
                                                                        
                        end do
                                            
                    end if
                    
                end do
                               
            end do future_mortgage_loop_2
            
        end do future_housing_status_loop_1
                    
        value_function_renter_owner=largest_value
        
    end subroutine
    
    subroutine owners_problem(time_period,financial_index)
    
        use Global_Vars
        
        implicit none
            
        integer :: time_period,financial_index
        
        if (add_net_worth_bequest==1) then
                            
            call owner_owner_stay_problem(time_period,financial_index)
            call owner_owner_move_problem(time_period,financial_index)
            
        else
                
            if (age_index==life_span) then
            
                value_function_owner_owner_stay=large_negative_num
                value_function_owner_owner_move=large_negative_num
                
            else
            
                call owner_owner_stay_problem(time_period,financial_index)
                call owner_owner_move_problem(time_period,financial_index)
            
            end if
            
        end if
                                                                  
        call owner_renter_problem(time_period,financial_index)
                                    
        if (value_function_owner_owner_stay>value_function_owner_owner_move) then
        
            if (value_function_owner_owner_stay>value_function_owner_renter) then
                    
                value_function_owner=value_function_owner_owner_stay
                                                                                     
                optimal_owner_indeces(1)=optimal_owner_owner_stay_indeces(1)
                optimal_owner_indeces(2)=optimal_owner_owner_stay_indeces(2)
                optimal_owner_indeces(3)=optimal_owner_owner_stay_indeces(3)

                optimal_consumption_owner=optimal_consumption_owner_owner_stay
                optimal_after_tax_wealth_net_of_expenses_owner=&
                    optimal_after_tax_wealth_net_of_expenses_owner_owner_stay
                optimal_gross_wage_income_owner=optimal_gross_wage_income_owner_owner_stay
                optimal_wage_income_tax_paid_owner=optimal_wage_income_tax_paid_owner_owner_stay
                optimal_space_lived_in_owner=optimal_space_lived_in_owner_owner_stay
                optimal_rent_subsidy_owner=0
                optimal_years_holding_onto_asset_owner=optimal_years_holding_onto_asset_owner_owner_stay
                optimal_household_can_consume_owner=optimal_household_can_consume_owner_owner_stay
                
            else
            
                value_function_owner=value_function_owner_renter
                                                                                     
                optimal_owner_indeces(1)=optimal_owner_renter_indeces(1)
                optimal_owner_indeces(2)=optimal_owner_renter_indeces(2)
                optimal_owner_indeces(3)=optimal_owner_renter_indeces(3)

                optimal_consumption_owner=optimal_consumption_owner_renter
                optimal_after_tax_wealth_net_of_expenses_owner=&
                    optimal_after_tax_wealth_net_of_expenses_owner_renter
                optimal_gross_wage_income_owner=optimal_gross_wage_income_owner_renter
                optimal_wage_income_tax_paid_owner=optimal_wage_income_tax_paid_owner_renter
                optimal_space_lived_in_owner=optimal_space_lived_in_owner_renter
                optimal_rent_subsidy_owner=optimal_rent_subsidy_owner_renter
                optimal_years_holding_onto_asset_owner=optimal_years_holding_onto_asset_owner_renter
                optimal_household_can_consume_owner=optimal_household_can_consume_owner_renter
                
            end if
            
        else
        
            if (value_function_owner_owner_move>value_function_owner_renter) then
                    
                value_function_owner=value_function_owner_owner_move
                                                                                     
                optimal_owner_indeces(1)=optimal_owner_owner_move_indeces(1)
                optimal_owner_indeces(2)=optimal_owner_owner_move_indeces(2)
                optimal_owner_indeces(3)=optimal_owner_owner_move_indeces(3)

                optimal_consumption_owner=optimal_consumption_owner_owner_move
                optimal_after_tax_wealth_net_of_expenses_owner=&
                    optimal_after_tax_wealth_net_of_expenses_owner_owner_move
                optimal_gross_wage_income_owner=optimal_gross_wage_income_owner_owner_move
                optimal_wage_income_tax_paid_owner=optimal_wage_income_tax_paid_owner_owner_move
                optimal_space_lived_in_owner=optimal_space_lived_in_owner_owner_move
                optimal_rent_subsidy_owner=0
                optimal_years_holding_onto_asset_owner=optimal_years_holding_onto_asset_owner_owner_move
                optimal_household_can_consume_owner=optimal_household_can_consume_owner_owner_move

            else
            
                value_function_owner=value_function_owner_renter
                                                                                     
                optimal_owner_indeces(1)=optimal_owner_renter_indeces(1)
                optimal_owner_indeces(2)=optimal_owner_renter_indeces(2)
                optimal_owner_indeces(3)=optimal_owner_renter_indeces(3)

                optimal_consumption_owner=optimal_consumption_owner_renter
                optimal_after_tax_wealth_net_of_expenses_owner=&
                    optimal_after_tax_wealth_net_of_expenses_owner_renter
                optimal_gross_wage_income_owner=optimal_gross_wage_income_owner_renter
                optimal_wage_income_tax_paid_owner=optimal_wage_income_tax_paid_owner_renter
                optimal_space_lived_in_owner=optimal_space_lived_in_owner_renter
                optimal_rent_subsidy_owner=optimal_rent_subsidy_owner_renter
                optimal_years_holding_onto_asset_owner=optimal_years_holding_onto_asset_owner_renter
                optimal_household_can_consume_owner=optimal_household_can_consume_owner_renter
                
            end if
            
        end if
                                
    end subroutine
    
    subroutine owner_owner_stay_problem(time_period,financial_index)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,financial_index,max_future_mortgage_index,smallest_house,second_smallest_house,&
            choose_to_be_homeless
        real(8) :: max_mortgage_value_allowed,value_of_inheritance
                                
        smallest_house=0
        second_smallest_house=0
        choose_to_be_homeless=0
                    
        largest_value=large_negative_num
                        
        optimal_owner_owner_stay_indeces(1)=1
        optimal_owner_owner_stay_indeces(2)=years_left_on_mortgage_index
        optimal_owner_owner_stay_indeces(3)=housing_status_index
                                    
        optimal_gross_wage_income_owner_owner_stay=0
        optimal_wage_income_tax_paid_owner_owner_stay=0
        optimal_after_tax_wealth_net_of_expenses_owner_owner_stay=0
        optimal_consumption_owner_owner_stay=0
        optimal_space_lived_in_owner_owner_stay=index_of_smallest_house_hhld_can_buy
        optimal_years_holding_onto_asset_owner_owner_stay=1
        optimal_household_can_consume_owner_owner_stay=0
                
        do space_lived_in_index=housing_status_index,housing_status_index
                            
            space_lived_in_index_feasible=space_lived_in_index
                                    
            if (can_get_mortgage==1) then
                                  
                if (housing_price(time_period)*housing_size(housing_status_index)<=&
                    fraction_of_average_house_price_data*((1-is_counterfactual)*&
                    model_average_owner_occupied_unit_size(time_period)*&
                    housing_price(time_period)+is_counterfactual*&
                    average_owner_occupied_house_size_calibrated*calibrated_stationary_housing_price)) then
                                
                    smallest_house=1
                    
                elseif (housing_price(time_period)*housing_size(housing_status_index)<=&
                    2*fraction_of_average_house_price_data*((1-is_counterfactual)*&
                    model_average_owner_occupied_unit_size(time_period)*&
                    housing_price(time_period)+is_counterfactual*&
                    average_owner_occupied_house_size_calibrated*calibrated_stationary_housing_price)) then 
                    
                    second_smallest_house=1
                    
                end if
                                
                if (in_retirement==1) then
                                        
                    max_mortgage_value_allowed=-min(financial_grid(financial_index),0.d0)*(1-fraction_to_repay)
                
                    max_future_mortgage_index=min(max(minloc(max_mortgage_value_allowed+&
                        financial_grid(1:negative_financial_grid_size),1,mask=(max_mortgage_value_allowed+&
                        financial_grid(1:negative_financial_grid_size))>=0),1),negative_financial_grid_size)
                                            
                else
                            
                    max_mortgage_value_allowed=housing_price(time_period)*&
                        housing_size(housing_status_index)*(real(1)-real(minimum_downpayment)+&
                        smallest_house*min_down_of_smallest+second_smallest_house*min_down_of_second_smallest)
                
                    max_future_mortgage_index=min(max(minloc(max_mortgage_value_allowed+&
                        financial_grid(1:negative_financial_grid_size),1,&
                        mask=(max_mortgage_value_allowed+financial_grid(1:negative_financial_grid_size))>=0),1),&
                        negative_financial_grid_size)
                        
                end if
                
            else
            
                max_future_mortgage_index=negative_financial_grid_size
            
            end if
                                                        
            do future_mortgage_index=financial_grid_size,max_future_mortgage_index,-1
                                            
                do future_years_of_amortization_index=2,max_amortization_years
                                
                    if ((-financial_grid(future_mortgage_index)/&
                        housing_price(time_period)*housing_size(housing_status_index))>0.8) then
                                                 
                        max_mortgage_payments_as_fraction_of_income=max_debt_to_income_ratio
                            
                    else
                                    
                        max_mortgage_payments_as_fraction_of_income=max_debt_to_income_ratio_below_80
                                        
                    end if
                                                     
                    call calculate_after_tax_wealth&
                        (financial_index,future_mortgage_index,years_left_on_mortgage_index,&
                        (1-two_rental_markets),housing_status_index,housing_status_index,&
                        years_holding_onto_asset_index,years_holding_onto_asset_index,&
                        0,0,0,1,0,time_period)
                        ! owner_to_owner_move_1,owner_to_renter_1,renter_to_owner_1,owner_to_owner_stay_1,renter_to_renter_1
                            
                    if (future_mortgage_index<negative_financial_grid_size) then
                    
                        if (financial_grid(future_mortgage_index)<(1-fraction_to_repay)*financial_grid(financial_index)) then
                                
                            spending_on_housing=(-min(financial_grid(future_mortgage_index),0.d0)*&
                                (mortgage_interest_rate(time_period)+average_fraction_of_mortgage_repaid))+&
                                housing_price(time_period)*housing_size(housing_status_index)*&
                                (fraction_of_property_taxes_in_GDS*property_tax(time_period)+&
                                include_housing_depreciation_in_housing_costs*housing_depreciation_rate)
                            
                        else
                        
                            spending_on_housing=0
                        
                        end if
                                                  
                    else
                    
                        spending_on_housing=0
                    
                    end if
                        
                    if (((spending_on_housing/(gross_wage_income+investment_income))<=&
                        max_mortgage_payments_as_fraction_of_income) .OR. &
                        (future_mortgage_index>=negative_financial_grid_size)) then
                        
                        consumption_vector(future_mortgage_index)=(after_tax_wealth_net_of_expenses-&
                            financial_grid(future_mortgage_index))/(1+consumption_tax)
                                                                   
                        if (consumption_vector(future_mortgage_index)>0) then
                                                                     
                            flow_utility_vector(future_mortgage_index)=(housing_status_utility_function&
                                (space_lived_in_index_feasible,1,min(years_holding_onto_asset_index+1,max_tracking),2)*&
                                (consumption_vector(future_mortgage_index)**&
                                ((1-risk_aversion_consumption)*(1-relative_share_of_housing))))/&
                                (1-risk_aversion_consumption)
                             
                            flow_utility_vector(future_mortgage_index)=flow_utility_vector(future_mortgage_index)*&
                                scaled_household_size(age_index)
                                                                                                                    
                            future_utility_vector=0
                                                                  
                            if (add_net_worth_bequest>=1) then
                            
                                if ((add_net_worth_bequest==1) .AND. (age_index==life_span)) then
                                        
                                    value_of_inheritance=&
                                        housing_size(housing_status_index)*housing_price(time_period)+&
                                        financial_grid(future_mortgage_index)
                    
                                    if (value_of_inheritance>0) then
                                
                                        future_utility_vector(future_mortgage_index)=&
                                            future_utility_vector(future_mortgage_index)+&
                                            discount_factor_on_child*(value_of_inheritance**&
                                            (1-risk_aversion_consumption))/(1-risk_aversion_consumption)
                                            
                                    else
                                    
                                        future_utility_vector(future_mortgage_index)=large_negative_num
                                    
                                    end if
                                    
                                elseif (add_net_worth_bequest==2) then
                                                                                
                                    do future_employment_index=1,employment_grid_size
                                    
                                        do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                               
                                            future_utility_vector(future_mortgage_index)=&
                                                future_utility_vector(future_mortgage_index)+&
                                                probability_rental_opportunity(future_rental_opportunity_index)*&
                                                discount_factor_on_child*(1-survival_probability(age_index))*&
                                                (use_GKKOC_process*intergenerational_employment_process&
                                                (employment_index,future_employment_index)+(1-use_GKKOC_process)*&
                                                unconditional_employment_probability(future_employment_index))*&
                                                value_function_guess(future_years_of_amortization_index)%&
                                                vector_youngest_real(future_mortgage_index,&
                                                future_employment_index,housing_status_index,1,&
                                                future_rental_opportunity_index)
                                                
                                        end do
                                                                                                                                                                                                                        
                                    end do
                                    
                                end if
                                                                                                
                            end if
                                                                            
                            do future_employment_index=&
                                at_least_one_period_before_retirement*employment_index+&
                                (1-at_least_one_period_before_retirement)*1,&
                                at_least_one_period_before_retirement*employment_index+&
                                (1-at_least_one_period_before_retirement)*employment_grid_size
                                
                                do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                                                                                                   
                                    future_utility_vector(future_mortgage_index)=&
                                        future_utility_vector(future_mortgage_index)+&
                                        discount_factor*survival_probability(age_index)*&
                                        probability_rental_opportunity(future_rental_opportunity_index)*&
                                        employment_process(choose_to_be_homeless,age_index,&
                                        employment_index,future_employment_index)*&
                                        value_function(future_years_of_amortization_index)%&
                                        vector_transition_real(min(time_period+solve_transition_dynamics,transition_length),&
                                        age_index+1,future_mortgage_index,future_employment_index,housing_status_index,1,&
                                        future_rental_opportunity_index)
                                        
                                end do
                                        
                            end do
                                                                
                            value_function_owner_owner_stay=&
                                flow_utility_vector(future_mortgage_index)+&
                                future_utility_vector(future_mortgage_index)
                                                                          
                            if (value_function_owner_owner_stay>largest_value) then
                        
                                largest_value=value_function_owner_owner_stay
                                                                      
                                optimal_owner_owner_stay_indeces(1)=future_mortgage_index
                                optimal_owner_owner_stay_indeces(2)=future_years_of_amortization_index
                                optimal_owner_owner_stay_indeces(3)=housing_status_index
                                optimal_consumption_owner_owner_stay=consumption_vector(optimal_owner_owner_stay_indeces(1))
                                optimal_after_tax_wealth_net_of_expenses_owner_owner_stay=after_tax_wealth_net_of_expenses
                                optimal_gross_wage_income_owner_owner_stay=gross_wage_income
                                optimal_wage_income_tax_paid_owner_owner_stay=wage_income_tax_paid
                                optimal_space_lived_in_owner_owner_stay=space_lived_in_index_feasible
                                optimal_household_can_consume_owner_owner_stay=1                                        
                                optimal_years_holding_onto_asset_owner_owner_stay=1
                                                                                                            
                            end if
                                                                            
                        end if
                        
                    end if
                                            
                end do
                
            end do
                                                     
        end do
                    
        value_function_owner_owner_stay=largest_value
        
    end subroutine
        
    subroutine owner_owner_move_problem(time_period,financial_index)
    
        use Global_Vars
        
        implicit none
            
        integer :: time_period,financial_index,smallest_house,second_smallest_house
        integer :: max_future_mortgage_index,chooses_positive_mortgage,choose_to_be_homeless
        real(8) :: max_mortgage_value_allowed,value_of_inheritance
        
        smallest_house=0
        second_smallest_house=0
        choose_to_be_homeless=0
        
        largest_value=large_negative_num
                        
        optimal_owner_owner_move_indeces(1)=1
        optimal_owner_owner_move_indeces(2)=max_amortization_years
        optimal_owner_owner_move_indeces(3)=index_of_smallest_house_hhld_can_buy
                                    
        optimal_consumption_owner_owner_move=0
        optimal_gross_wage_income_owner_owner_move=0
        optimal_wage_income_tax_paid_owner_owner_move=0
        optimal_after_tax_wealth_net_of_expenses_owner_owner_move=0
        optimal_space_lived_in_owner_owner_move=index_of_smallest_house_hhld_can_buy
        optimal_household_can_consume_owner_owner_move=0
                                
        future_housing_status_loop_1: do &
            future_housing_status_index=index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
            
            if ((future_housing_status_index==-1) .OR. (future_housing_status_index==housing_status_index)) then
                            
            else
            
                if (can_get_mortgage==1) then
                
                    if (housing_price(time_period)*housing_size(future_housing_status_index)<=&
                        fraction_of_average_house_price_data*((1-is_counterfactual)*&
                        model_average_owner_occupied_unit_size(time_period)*&
                        housing_price(time_period)+is_counterfactual*&
                        average_owner_occupied_house_size_calibrated*calibrated_stationary_housing_price)) then
                                
                        smallest_house=1
                        
                    elseif (housing_price(time_period)*housing_size(future_housing_status_index)<=&
                        2*fraction_of_average_house_price_data*((1-is_counterfactual)*&
                        model_average_owner_occupied_unit_size(time_period)*&
                        housing_price(time_period)+is_counterfactual*&
                        average_owner_occupied_house_size_calibrated*calibrated_stationary_housing_price)) then 
                        
                        second_smallest_house=1
                        
                    end if
                    
                    if (in_retirement==1) then
                                    
                        max_future_mortgage_index=negative_financial_grid_size
                            
                    else
                                                    
                        max_mortgage_value_allowed=max(housing_price(time_period)*&
                            housing_size(future_housing_status_index)*(real(1)-real(minimum_downpayment)+&
                            smallest_house*min_down_of_smallest+second_smallest_house*min_down_of_second_smallest),0.d0)
                    
                        max_future_mortgage_index=min(max(minloc(max_mortgage_value_allowed+&
                            financial_grid(1:negative_financial_grid_size),1,&
                            mask=(max_mortgage_value_allowed+financial_grid(1:negative_financial_grid_size))>=0),1),&
                            negative_financial_grid_size)
                        
                    end if
                        
                else
                
                    max_future_mortgage_index=negative_financial_grid_size
                
                end if
                                                    
                future_mortgage_loop_2: do future_mortgage_index=financial_grid_size,max_future_mortgage_index,-1
                    
                    if (future_mortgage_index<negative_financial_grid_size) then
                    
                        chooses_positive_mortgage=1
                    
                    else
                    
                        chooses_positive_mortgage=0
                    
                    end if
                                        
                    do future_years_of_amortization_index=2,max_amortization_years
                                                                                                  
                        if (((total_household_wealth+min(financial_grid(financial_index),0.d0)*&
                            (1+mortgage_interest_rate(time_period))-housing_price(time_period)*&
                            (housing_size(housing_status_index)*cost_of_selling+&
                            housing_size(future_housing_status_index)*cost_of_buying)-&
                            (housing_price(time_period)*housing_size(future_housing_status_index)-&
                            min(financial_grid(future_mortgage_index),0.d0)))>0) .AND. (((-financial_grid(future_mortgage_index)/&
                            (housing_price(time_period)*housing_size(future_housing_status_index)))<&
                            (1-minimum_downpayment+smallest_house*min_down_of_smallest+&
                            second_smallest_house*min_down_of_second_smallest)) .OR. (chooses_positive_mortgage==0))) then
                                
                            do space_lived_in_index=future_housing_status_index,future_housing_status_index
                                                                    
                                space_lived_in_index_feasible=space_lived_in_index
                                                                
                                if (-financial_grid(future_mortgage_index)/&
                                    (housing_price(time_period)*housing_size(future_housing_status_index))>0.8) then
                                                                
                                    max_mortgage_payments_as_fraction_of_income=max_debt_to_income_ratio
                                        
                                else
                                                
                                    max_mortgage_payments_as_fraction_of_income=max_debt_to_income_ratio_below_80
                                                    
                                end if
                                    
                                call calculate_after_tax_wealth&
                                    (financial_index,future_mortgage_index,years_left_on_mortgage_index,(1-two_rental_markets),&
                                    housing_status_index,future_housing_status_index,years_holding_onto_asset_index,1,&
                                    1,0,0,0,0,time_period)
                                    ! owner_to_owner_move_1,owner_to_renter_1,renter_to_owner_1,owner_to_owner_stay_1,renter_to_renter_1
                                    
                                if (chooses_positive_mortgage==1) then
                                                                                                                                                                            
                                    spending_on_housing=(-min(financial_grid(future_mortgage_index),0.d0)*&
                                        (mortgage_interest_rate(time_period)+average_fraction_of_mortgage_repaid))+&
                                        housing_price(time_period)*housing_size(future_housing_status_index)*&
                                        (fraction_of_property_taxes_in_GDS*property_tax(time_period)+&
                                        include_housing_depreciation_in_housing_costs*housing_depreciation_rate)
                                        
                                else
                                
                                    spending_on_housing=0
                                
                                end if
                                                                                
                                if (((spending_on_housing/(gross_wage_income+investment_income))<=&
                                    max_mortgage_payments_as_fraction_of_income) .OR. &
                                    (future_mortgage_index>=negative_financial_grid_size)) then
                                    
                                    consumption_vector(future_mortgage_index)=(after_tax_wealth_net_of_expenses-&
                                        financial_grid(future_mortgage_index))/(1+consumption_tax)
                                                                               
                                    if (consumption_vector(future_mortgage_index)>0) then
                                                                                
                                        flow_utility_vector(future_mortgage_index)=&   
                                            (housing_status_utility_function(space_lived_in_index_feasible,1,1,2)*&
                                            (consumption_vector(future_mortgage_index)**&
                                            ((1-risk_aversion_consumption)*(1-relative_share_of_housing))))/&
                                            (1-risk_aversion_consumption)
                                            
                                        flow_utility_vector(future_mortgage_index)=flow_utility_vector(future_mortgage_index)*&
                                            scaled_household_size(age_index)
                                                                                                                                
                                        future_utility_vector=0
                                        
                                        if (add_net_worth_bequest>=1) then
                                        
                                            if ((add_net_worth_bequest==1) .AND. (age_index==life_span)) then
                                        
                                                value_of_inheritance=&
                                                    housing_size(future_housing_status_index)*housing_price(time_period)+&
                                                    financial_grid(future_mortgage_index)
                                
                                                if (value_of_inheritance>0) then
                                            
                                                    future_utility_vector(future_mortgage_index)=&
                                                        future_utility_vector(future_mortgage_index)+&
                                                        discount_factor_on_child*(value_of_inheritance**&
                                                        (1-risk_aversion_consumption))/(1-risk_aversion_consumption)
                                                        
                                                else
                                                
                                                    future_utility_vector(future_mortgage_index)=large_negative_num
                                                
                                                end if
                                                
                                            elseif (add_net_worth_bequest==2) then
                                                                                            
                                                do future_employment_index=1,employment_grid_size
                                                
                                                    do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                                                                                                                      
                                                        future_utility_vector(future_mortgage_index)=&
                                                            future_utility_vector(future_mortgage_index)+&  
                                                            discount_factor_on_child*(1-survival_probability(age_index))*&
                                                            probability_rental_opportunity(future_rental_opportunity_index)*&
                                                            (use_GKKOC_process*intergenerational_employment_process&
                                                            (employment_index,future_employment_index)+(1-use_GKKOC_process)*&
                                                            unconditional_employment_probability(future_employment_index))*&
                                                            value_function_guess(future_years_of_amortization_index)%&
                                                            vector_youngest_real(future_mortgage_index,future_employment_index,&
                                                            future_housing_status_index,1,future_rental_opportunity_index)
                                                    
                                                    end do
                                                                                                                                                                                       
                                                end do
                                                
                                            end if
                                                                                                            
                                        end if
                                                                                        
                                        do future_employment_index=&
                                            at_least_one_period_before_retirement*employment_index+&
                                            (1-at_least_one_period_before_retirement)*1,&
                                            at_least_one_period_before_retirement*employment_index+&
                                            (1-at_least_one_period_before_retirement)*employment_grid_size
                                            
                                            do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                                                                                                                                                         
                                                future_utility_vector(future_mortgage_index)=&
                                                    future_utility_vector(future_mortgage_index)+&
                                                    discount_factor*survival_probability(age_index)*&
                                                    probability_rental_opportunity(future_rental_opportunity_index)*&
                                                    employment_process(choose_to_be_homeless,age_index,&
                                                    employment_index,future_employment_index)*&
                                                    value_function(future_years_of_amortization_index)%vector_transition_real&
                                                    (min(time_period+solve_transition_dynamics,transition_length),age_index+1,&
                                                    future_mortgage_index,future_employment_index,future_housing_status_index,1,&
                                                    future_rental_opportunity_index)
                                                    
                                            end do
                                            
                                        end do
                                                                            
                                        value_function_owner_owner_move=flow_utility_vector(future_mortgage_index)+&
                                            future_utility_vector(future_mortgage_index)
                                                                                  
                                        if (value_function_owner_owner_move>largest_value) then
                                    
                                            largest_value=value_function_owner_owner_move
                                                                              
                                            optimal_owner_owner_move_indeces(1)=future_mortgage_index
                                            optimal_owner_owner_move_indeces(2)=future_years_of_amortization_index
                                            optimal_owner_owner_move_indeces(3)=future_housing_status_index
                            
                                            optimal_consumption_owner_owner_move=&
                                                consumption_vector(optimal_owner_owner_move_indeces(1))
                                                
                                            optimal_after_tax_wealth_net_of_expenses_owner_owner_move=&
                                                after_tax_wealth_net_of_expenses
                                                
                                            optimal_gross_wage_income_owner_owner_move=gross_wage_income
                                            optimal_wage_income_tax_paid_owner_owner_move=wage_income_tax_paid
                                            optimal_space_lived_in_owner_owner_move=space_lived_in_index_feasible
                                            optimal_household_can_consume_owner_owner_move=1
                                            optimal_years_holding_onto_asset_owner_owner_move=1
                                                                                                      
                                        end if
                                                                                    
                                    end if
                                    
                                end if
                                
                            end do
                                                            
                        end if
                        
                    end do
                           
                end do future_mortgage_loop_2
                
            end if
    
        end do future_housing_status_loop_1
                    
        value_function_owner_owner_move=largest_value
                                                                
    end subroutine
    
    subroutine owner_renter_problem(time_period,financial_index)
    
        use Global_Vars
        
        implicit none
            
        integer :: time_period,financial_index,loop_over_rent_shocks,not_allowed_to_move_back_1,&
            choose_to_be_homeless,choose_non_market_housing,lowest_rental_market_index,highest_rental_market_index
        real(8) :: after_tax_wealth_net_of_expenses_owner_renter
                
        largest_value=large_negative_num
                        
        optimal_owner_renter_indeces(1)=negative_financial_grid_size
        optimal_owner_renter_indeces(2)=1
        optimal_owner_renter_indeces(3)=0
                                    
        optimal_consumption_owner_renter=0
        optimal_gross_wage_income_owner_renter=0
        optimal_wage_income_tax_paid_owner_renter=0
        optimal_after_tax_wealth_net_of_expenses_owner_renter=0
        optimal_after_tax_wealth_owner_renter=0
        optimal_space_lived_in_owner_renter=0
        optimal_rent_subsidy_owner_renter=0
        optimal_years_holding_onto_asset_owner_renter=1
        optimal_household_can_consume_owner_renter=0
        
        not_allowed_to_move_back_1=0
        
        if (age_index>=cannot_move_back_age) then
        
            not_allowed_to_move_back_1=include_moving_back*1
            
        end if
        
        do future_housing_status_index=can_be_homeless*lowest_housing_grid_point+&
            (1-can_be_homeless)*(1-not_allowed_to_move_back_1)*second_lowest_housing_grid_point,&
            largest_house_size_hhld_can_rent
            
            choose_to_be_homeless=0
            choose_non_market_housing=0
            
            if ((not_allowed_to_move_back_1==1) .AND. &
               (future_housing_status_index==second_lowest_housing_grid_point)) then
                       
            else
            
                if ((future_housing_status_index<=second_lowest_housing_grid_point) .AND. &
                    (include_moving_back==1)) then
                
                    choose_non_market_housing=1
                
                    if ((future_housing_status_index==lowest_housing_grid_point) .AND. (include_homelessness==1)) then
                    
                        choose_to_be_homeless=1
                        
                    end if
                    
                end if
                
                lowest_rental_market_index=1-two_rental_markets
                highest_rental_market_index=1
                
                if (choose_non_market_housing==1) then
                
                    highest_rental_market_index=lowest_rental_market_index
                    
                end if
                
                if (random_assignment==1) then
                                
                    lowest_rental_market_index=rental_opportunity_index
                    highest_rental_market_index=rental_opportunity_index
                    
                end if

                do rental_market_index=highest_rental_market_index,lowest_rental_market_index,-1
                
                    loop_over_rent_shocks=0

                    if (choose_non_market_housing==0) then
                
                        if (rental_market_index==0) then
                        
                            loop_over_rent_shocks=1
                        
                        else
                        
                            if ((two_rental_markets==0) .AND. (rental_market_index==1) .AND. &
                                (include_shocks_to_free_market_rent==1)) then
                            
                                loop_over_rent_shocks=1
                                
                            end if
                        
                        end if

                    end if
                
                    do future_financial_index=negative_financial_grid_size,financial_grid_size
                                
                        call calculate_after_tax_wealth&
                            (financial_index,future_financial_index,years_left_on_mortgage_index,rental_market_index,&
                            housing_status_index,future_housing_status_index,&
                            years_holding_onto_asset_index,years_holding_onto_asset_index,0,1,0,0,0,time_period)
                            ! owner_to_owner_move_1,owner_to_renter_1,renter_to_owner_1,owner_to_owner_stay_1,renter_to_renter_1
                                                                                         
                        if ((future_housing_status_index==0) .AND. &
                            (after_tax_wealth_net_of_expenses<min_consumption_guaranteed) .AND. &
                            (age_index>=1) .AND. (solve_transition_dynamics==1) .AND. &
                            (add_net_worth_bequest==0)) then
                            
                            rent_subsidy=0
                                                
                            after_tax_wealth_net_of_expenses_owner_renter=&
                                after_tax_wealth_net_of_expenses+rent_subsidy
                                                                               
                        else
                                            
                            rent_subsidy=0
                            after_tax_wealth_net_of_expenses_owner_renter=&
                                after_tax_wealth_net_of_expenses
                            
                        end if
                                                                                            
                        consumption_vector(future_financial_index)=(after_tax_wealth_net_of_expenses_owner_renter-&
                            financial_grid(future_financial_index))/(1+consumption_tax)
                                                                
                        if (consumption_vector(future_financial_index)>0) then
                                                                   
                            flow_utility_vector(future_financial_index)=&
                                (housing_status_utility_function(future_housing_status_index,0,1,1)*&
                                (consumption_vector(future_financial_index))**&
                                ((1-risk_aversion_consumption)*(1-relative_share_of_housing)))/(1-risk_aversion_consumption)
                                
                            flow_utility_vector(future_financial_index)=flow_utility_vector(future_financial_index)*&
                                scaled_household_size(age_index)
                                                                                                                    
                            future_utility_vector=0
                                   
                            if (add_net_worth_bequest>=1) then
                                    
                                if ((add_net_worth_bequest==1) .AND. (age_index==life_span)) then
                                   
                                   if (financial_grid(future_financial_index)>0) then
                
                                        future_utility_vector(future_financial_index)=&
                                            future_utility_vector(future_financial_index)+&                                                           
                                            discount_factor_on_child*(financial_grid(future_financial_index)**&
                                            (1-risk_aversion_consumption))/(1-risk_aversion_consumption)
                                            
                                    else
                                    
                                        future_utility_vector(future_financial_index)=large_negative_num
                                    
                                    end if
                                        
                                elseif (add_net_worth_bequest==2) then
                                                                                
                                    do future_employment_index=1,employment_grid_size
                                    
                                        do future_rental_opportunity_index=0,random_assignment
                                                                                                                                 
                                            future_utility_vector(future_financial_index)=&
                                                future_utility_vector(future_financial_index)+&   
                                                discount_factor_on_child*(1-survival_probability(age_index))*&
                                                probability_rental_opportunity(future_rental_opportunity_index)*&
                                                (use_GKKOC_process*intergenerational_employment_process&
                                                (employment_index,future_employment_index)+&
                                                (1-use_GKKOC_process)*unconditional_employment_probability&
                                                (future_employment_index))*&
                                                value_function_guess(rental_market_index)%vector_youngest_real&
                                                (future_financial_index,future_employment_index,future_housing_status_index,1,&
                                                future_rental_opportunity_index)
                                                
                                        end do
                                            
                                    end do
                                    
                                end if
                                                                                                
                            end if
                                                                            
                            do future_employment_index=&
                                at_least_one_period_before_retirement*employment_index+&
                                (1-at_least_one_period_before_retirement)*1,&
                                at_least_one_period_before_retirement*employment_index+&
                                (1-at_least_one_period_before_retirement)*employment_grid_size
                                
                                do future_rent_shock_index=1,loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)
                                
                                    do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                                       
                                        future_utility_vector(future_financial_index)=&
                                            future_utility_vector(future_financial_index)+&
                                            (loop_over_rent_shocks*probability_of_rent_shock(future_rent_shock_index)+&
                                            (1-loop_over_rent_shocks))*discount_factor*survival_probability(age_index)*&
                                            probability_rental_opportunity(future_rental_opportunity_index)*&
                                            employment_process(choose_to_be_homeless,age_index,&
                                            employment_index,future_employment_index)*&
                                            value_function(rental_market_index)%vector_transition_real&
                                            (min(time_period+solve_transition_dynamics,transition_length),&
                                            age_index+1,future_financial_index,future_employment_index,future_housing_status_index,&
                                            loop_over_rent_shocks*future_rent_shock_index+(1-loop_over_rent_shocks)*1,&
                                            future_rental_opportunity_index)
                                        
                                    end do    
                                    
                                end do
                                                                                                                                                                                        
                            end do
                                                                
                            value_function_owner_renter=flow_utility_vector(future_financial_index)+&
                                future_utility_vector(future_financial_index)
                                                                   
                            if (value_function_owner_renter>largest_value) then
                        
                                largest_value=value_function_owner_renter
                                                                     
                                optimal_owner_renter_indeces(1)=future_financial_index
                                
                                if ((future_housing_status_index<=second_lowest_housing_grid_point) .AND. &
                                    (include_moving_back==1)) then
                                
                                    optimal_renter_renter_indeces(2)=1-two_rental_markets
                                
                                else
                                
                                    optimal_owner_renter_indeces(2)=rental_market_index
                                    
                                end if
                                
                                optimal_owner_renter_indeces(3)=future_housing_status_index
                
                                optimal_consumption_owner_renter=consumption_vector(future_financial_index)
                                
                                optimal_after_tax_wealth_net_of_expenses_owner_renter=&
                                    after_tax_wealth_net_of_expenses_owner_renter
                                    
                                optimal_gross_wage_income_owner_renter=gross_wage_income
                                optimal_wage_income_tax_paid_owner_renter=wage_income_tax_paid
                                optimal_space_lived_in_owner_renter=future_housing_status_index
                                optimal_household_can_consume_owner_renter=1
                                optimal_rent_subsidy_owner_renter=rent_subsidy
                                optimal_years_holding_onto_asset_owner_renter=1
                                
                            end if
                                                                            
                        end if
                        
                    end do
                    
                end do
                
            end if
                                                     
        end do
        
        value_function_owner_renter=largest_value
                            
    end subroutine
    
    subroutine which_is_better(time_period,financial_index)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,financial_index,is_renter
        
        is_renter=1
        
        if (value_function_owner>value_function_renter) then
        
            is_renter=0
            
        end if
                                                                                                
        value_function(years_left_on_mortgage_index)%&
            vector_transition_real(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*value_function_renter+(1-is_renter)*value_function_owner
            
        optimal_after_tax_wealth_net_of_expenses=&
            is_renter*optimal_after_tax_wealth_net_of_expenses_renter+&
            (1-is_renter)*optimal_after_tax_wealth_net_of_expenses_owner
            
        policy_function_financial_index(years_left_on_mortgage_index)%&
            vector_transition_integer(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*optimal_renter_indeces(1)+(1-is_renter)*optimal_owner_indeces(1)
            
        policy_function_years_of_amortization_index(years_left_on_mortgage_index)%&
            vector_transition_integer(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*optimal_renter_indeces(2)+(1-is_renter)*optimal_owner_indeces(2)
                                        
        policy_function_housing_status_index(years_left_on_mortgage_index)%&
            vector_transition_integer(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*optimal_renter_indeces(3)+(1-is_renter)*optimal_owner_indeces(3)
                                
        policy_function_consumption(years_left_on_mortgage_index)%&
            vector_transition_real(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*optimal_consumption_renter+(1-is_renter)*optimal_consumption_owner
                    
        policy_function_space_lived_in_index(years_left_on_mortgage_index)%&
            vector_transition_integer(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*optimal_space_lived_in_renter+(1-is_renter)*optimal_space_lived_in_owner
        
        policy_function_space_lived_in_size(years_left_on_mortgage_index)%&
            vector_transition_real(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*housing_size(optimal_space_lived_in_renter)+(1-is_renter)*housing_size(optimal_space_lived_in_owner)
                            
        policy_function_wage_income_tax_paid(years_left_on_mortgage_index)%&
            vector_transition_real(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*optimal_wage_income_tax_paid_renter+(1-is_renter)*optimal_wage_income_tax_paid_owner
                                                        
        policy_function_rent_subsidy(years_left_on_mortgage_index)%&
            vector_transition_real(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*optimal_rent_subsidy_renter+(1-is_renter)*optimal_rent_subsidy_owner
            
        policy_function_years_holding_onto_asset(years_left_on_mortgage_index)%&
            vector_transition_integer(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
            is_renter*optimal_years_holding_onto_asset_renter+(1-is_renter)*optimal_years_holding_onto_asset_owner           
                                                                                                                           
    end subroutine
        
    subroutine calculate_after_tax_wealth&
        (financial_index_1,future_financial_index_1,years_left_on_mortgage_1,rental_market_1,&
        housing_status_index_1,future_housing_status_index_1,rent_shock_index_1,&
        rental_years_1,owner_to_owner_move_1,owner_to_renter_1,renter_to_owner_1,&
        owner_to_owner_stay_1,renter_to_renter_1,time_period)
            
        use Global_Vars
        
        implicit none
        
        integer, intent(in) :: financial_index_1,future_financial_index_1,years_left_on_mortgage_1,&
                    housing_status_index_1,future_housing_status_index_1,owner_to_owner_move_1,&
                    owner_to_renter_1,renter_to_owner_1,owner_to_owner_stay_1,renter_to_renter_1,time_period,&
                    rent_shock_index_1,rental_years_1,rental_market_1
                                    
        real(8) ::  cost_of_mortgage,cost_of_housing,lump_sum_transfer_given,prepayment_penalty_1,&
                    origination_cost_1,rent_shock_amount_1,fixed_cost_to_moving_1,inheritance_1
                    
        inheritance_1=0
        
        if ((age_index>=min_age_inheritance) .AND. (age_index<=max_age_inheritance) .AND. (add_net_worth_bequest==1))  then
        
            inheritance_1=employment_grid(age_index,employment_index)*proportion_inheritance
        
        end if
        
        rent_shock_amount_1=0

        if (housing_status_index_1>=third_lowest_housing_grid_point) then
                                    
            if ((years_left_on_mortgage_1==0) .AND. (age_index>1)) then
            
                rent_shock_amount_1=shock_to_rent(rent_shock_index_1)*&
                    rent(time_period,years_left_on_mortgage_1)*housing_size(housing_status_index_1)
                
            else
            
                if ((two_rental_markets==0) .AND. (years_left_on_mortgage_1==1) .AND. &
                    (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                    rent_shock_amount_1=shock_to_rent(rent_shock_index_1)*&
                        rent(time_period,years_left_on_mortgage_1)*housing_size(housing_status_index_1)
                     
                end if
                
            end if

        end if
                        
        gross_wage_income=in_retirement*model_ss_benefits(employment_index)+&
            (1-in_retirement)*(employment_grid(age_index,employment_index)*&
            labour_deterministic_efficiency(age_index))
                                                        
        total_household_working_wealth=max(financial_grid(financial_index_1),0.d0)
                                            
        investment_income=(total_household_working_wealth*risk_free_rate(time_period))
                        
        if (fraction_of_capital_income_taxable==0) then
                                                                                                  
            investment_income_tax_paid=investment_income_tax*&
                max(investment_income,0.d0)
                                
        else
        
            investment_income_tax_paid=0
            
        end if
                        
        wage_income_tax_paid=gross_wage_income+fraction_of_capital_income_taxable*investment_income-&
            (labor_income_tax_rate+increase_in_labour_income_tax)*&
            (gross_wage_income+fraction_of_capital_income_taxable*investment_income)**&
            curvature_of_labor_income_taxes
                                                          
        fixed_cost_to_moving_1=0
                              
        if (renter_to_renter_1==1) then
                
            prepayment_penalty_1=0
            
            origination_cost_1=0
        
            cost_of_housing=0
            
            if ((include_moving_back==1) .OR. (include_homelessness==1)) then
            
                if (future_housing_status_index_1<=second_lowest_housing_grid_point) then
                
                    rent_spending=0
                    
                else
                                                                                  
                    rent_spending=(rent(time_period,rental_market_1)/((1+rental_market_1*&
                        include_rent_decrease*rent_decrease_per_year)**(rental_years_1-1)))*&
                        housing_size(future_housing_status_index_1)
                        
                end if
                
            else
            
                rent_spending=(rent(time_period,rental_market_1)/((1+rental_market_1*&
                    include_rent_decrease*rent_decrease_per_year)**(rental_years_1-1)))*&
                    housing_size(future_housing_status_index_1)
            
            end if
                                                                                                                                       
            if (((future_housing_status_index_1 .NE. housing_status_index_1) .OR. &
                (rental_market_1 .NE. years_left_on_mortgage_index)) .AND. (age_index>1)) then
                        
                fixed_cost_to_moving_1=fixed_cost_to_moving*&
                    (is_counterfactual*average_labor_income_calibrated+&
                    (1-is_counterfactual)*model_average_labour_income(time_period))
                            
            end if
                                               
            property_taxes_paid=0
            
            housing_depreciation_expenditures=0
            
            cost_of_mortgage=0  
            
            if (give_lump_sum_to_renters==1) then
            
                lump_sum_transfer_given=lump_sum_transfer(time_period)
                
            else
                        
                lump_sum_transfer_given=0
                        
            end if
            
        elseif (renter_to_owner_1==1) then
        
            prepayment_penalty_1=0
            
            if (financial_grid(future_financial_index_1)<0) then
                                
!                origination_cost_1=mortgage_origination_cost*model_average_labour_income(time_period)
                origination_cost_1=0
                    
            else
            
                origination_cost_1=0
                    
            end if
                           
            cost_of_housing=(1+cost_of_buying)*housing_price(time_period)*housing_size(future_housing_status_index_1)

            if (age_index==life_span) then

                cost_of_housing=cost_of_housing+(housing_depreciation_rate+property_tax(time_period)+&
                    cost_of_selling)*housing_price(time_period)*housing_size(future_housing_status_index_1)

            end if
        
            rent_spending=0
                            
            property_taxes_paid=0
                                                                   
            housing_depreciation_expenditures=0
                                   
            cost_of_mortgage=0
                       
            if (give_lump_sum_to_owners==1) then
            
                lump_sum_transfer_given=lump_sum_transfer(time_period)
                
            else
                        
                lump_sum_transfer_given=0
                        
            end if
                                                                    
        elseif (owner_to_owner_stay_1==1) then
                
            if (financial_grid(financial_index_1)>0) then
                
                if (financial_grid(future_financial_index_1)<0) then
            
                    origination_cost_1=mortgage_origination_cost*model_average_labour_income(time_period)
                    
                else
                
                    origination_cost_1=0
                    
                end if
                
                prepayment_penalty_1=0
                    
            else
        
                if (financial_grid(future_financial_index_1)<(1-fraction_to_repay)*financial_grid(financial_index_1)) then
                
                    origination_cost_1=mortgage_origination_cost*model_average_labour_income(time_period)
                    prepayment_penalty_1=0
                
                else
                
                    origination_cost_1=0        
                    prepayment_penalty_1=penalty_months_of_interest*mortgage_interest_rate(time_period)*&
                        max(min(financial_grid(future_financial_index_1),0.d0)-financial_grid(financial_index_1)*&
                        (1-prepayment_penalty_starts_from),0.d0)
                                        
                end if
                                        
            end if
                                            
            cost_of_housing=housing_price(time_period)*housing_size(housing_status_index_1)

            if (age_index==life_span) then

                cost_of_housing=cost_of_housing+(housing_depreciation_rate+property_tax(time_period)+&
                    cost_of_selling)*housing_price(time_period)*housing_size(housing_status_index_1)

            end if
        
            rent_spending=0
                                            
            property_taxes_paid=property_tax(time_period-solve_transition_dynamics)*&
                housing_price(time_period-solve_transition_dynamics)*housing_size(housing_status_index_1)
                                                                                              
            housing_depreciation_expenditures=&
                housing_depreciation_rate*housing_price(time_period-solve_transition_dynamics)*housing_size(housing_status_index_1)
                                   
            cost_of_mortgage=-min(financial_grid(financial_index_1),0.d0)*(1+mortgage_interest_rate(time_period))
                            
            if (give_lump_sum_to_owners==1) then
            
                lump_sum_transfer_given=lump_sum_transfer(time_period)
                
            else
                        
                lump_sum_transfer_given=0
                        
            end if
                                                                                                                      
        elseif (owner_to_owner_move_1==1) then
            
            if (financial_grid(future_financial_index_1)<0) then
        
!                    origination_cost_1=mortgage_origination_cost*model_average_labour_income(time_period)
                origination_cost_1=0
            
            else
            
                origination_cost_1=0
                
            end if
        
            if (financial_grid(financial_index_1)>0) then
            
                prepayment_penalty_1=0
                    
            else
                        
                prepayment_penalty_1=penalty_months_of_interest*mortgage_interest_rate(time_period)*&
                    (-min(financial_grid(financial_index_1),0.d0))*(1-prepayment_penalty_starts_from)
                                             
            end if
                                 
            rent_spending=0
                                                                        
            property_taxes_paid=property_tax(time_period-solve_transition_dynamics)*&
                housing_price(time_period-solve_transition_dynamics)*housing_size(housing_status_index_1)
                   
            cost_of_housing=(1+cost_of_buying)*housing_price(time_period)*&
                housing_size(future_housing_status_index_1)+cost_of_selling*(housing_price(time_period)*&
                housing_size(housing_status_index_1))

            if (age_index==life_span) then

                cost_of_housing=cost_of_housing+(housing_depreciation_rate+property_tax(time_period)+&
                    cost_of_selling)*housing_price(time_period)*housing_size(future_housing_status_index_1)

            end if
                                                                               
            housing_depreciation_expenditures=&
                housing_depreciation_rate*housing_price(time_period-solve_transition_dynamics)*housing_size(housing_status_index_1)
                                            
            cost_of_mortgage=-min(financial_grid(financial_index_1),0.d0)*(1+mortgage_interest_rate(time_period))
                     
            if (give_lump_sum_to_owners==1) then
            
                lump_sum_transfer_given=lump_sum_transfer(time_period)
                
            else
                        
                lump_sum_transfer_given=0
                        
            end if
                                    
        elseif (owner_to_renter_1==1) then
        
            origination_cost_1=0
                            
            prepayment_penalty_1=penalty_months_of_interest*mortgage_interest_rate(time_period)*&
                (-min(financial_grid(financial_index_1),0.d0))*(1-prepayment_penalty_starts_from)
                
            if ((include_moving_back==1) .OR. (include_homelessness==1)) then
            
                if (future_housing_status_index_1<=second_lowest_housing_grid_point) then
                
                    rent_spending=0
                
                else
                                                               
                    rent_spending=rent(time_period,rental_market_1)*housing_size(future_housing_status_index_1)
                    
                end if
                
            else
            
                rent_spending=rent(time_period,rental_market_1)*housing_size(future_housing_status_index_1)
            
            end if
                                        
            property_taxes_paid=property_tax(time_period-solve_transition_dynamics)*&
                housing_price(time_period-solve_transition_dynamics)*housing_size(housing_status_index_1)
                                             
            cost_of_housing=cost_of_selling*housing_price(time_period)*housing_size(housing_status_index_1)
                                    
            housing_depreciation_expenditures=&
                housing_depreciation_rate*housing_price(time_period-solve_transition_dynamics)*housing_size(housing_status_index_1)
                        
            cost_of_mortgage=-min(financial_grid(financial_index_1),0.d0)*(1+mortgage_interest_rate(time_period))
                
            if (give_lump_sum_to_renters==1) then
            
                lump_sum_transfer_given=lump_sum_transfer(time_period)
                
            else
                        
                lump_sum_transfer_given=0
                        
            end if
                                                                                    
        end if
                                                                                
        after_tax_wealth_net_of_expenses=gross_wage_income-wage_income_tax_paid+&
            investment_income-investment_income_tax_paid+total_household_wealth-& 
            property_taxes_paid-housing_depreciation_expenditures-rent_spending-cost_of_mortgage-&
            cost_of_housing+lump_sum_transfer_given-origination_cost_1-prepayment_penalty_1-&
            rent_shock_amount_1-fixed_cost_to_moving_1+inheritance_1
                        
    end subroutine
        
    subroutine internal_computations&
        (value_function_1,gross_wage_income_1,fin_index_1,ownership_status_index_1,&
        space_lived_in_index_1,housing_status_index_1,years_of_amortization_1,&
        years_holding_onto_asset_index_1,financial_index,time_period,&
        after_tax_wealth_net_of_expenses_1,max_future_mortgage_index_2)
    
        use Global_Vars
        
        implicit none
        
        integer :: financial_index,fin_index_1,ownership_status_index_1,can_purchase_new_house,years_of_amortization_1,&
            housing_status_index_1,space_lived_in_index_1,time_period,smallest_house,second_smallest_house,&
            max_future_mortgage_index_2,owner_to_owner_stay_2,check_GDS_2,loop_over_rent_shocks,&
            years_holding_onto_asset_index_1,choose_to_be_homeless,not_allowed_to_move_back_1
            
        real(8) :: value_function_1,after_tax_wealth_net_of_expenses_1,spending_on_housing_1,&
            max_mortgage_payments_as_fraction_of_income_1,gross_wage_income_1,value_of_inheritance
        
        not_allowed_to_move_back_1=0
        
        if (age_index>=cannot_move_back_age) then
        
            not_allowed_to_move_back_1=include_moving_back*1
            
        end if
        
        loop_over_rent_shocks=0

        if (housing_status_index_1>=third_lowest_housing_grid_point) then
            
            if ((years_of_amortization_1==0)) then
                    
                loop_over_rent_shocks=1
            
            else
            
                if ((two_rental_markets==0) .AND. (years_of_amortization_1==1) .AND. &
                    (include_shocks_to_free_market_rent==1)) then
                
                    loop_over_rent_shocks=1
                                
                end if
            
            end if

        end if
            
        owner_to_owner_stay_2=0
            
        if ((ownership_status_index_1==1) .AND. (years_left_on_mortgage_index==2) .AND. &
            (housing_status_index_1==housing_status_index)) then
        
            owner_to_owner_stay_2=1
                    
        end if
                            
        consumption_vector=(after_tax_wealth_net_of_expenses_1-financial_grid)/(1+consumption_tax)
                                    
        consumption_internal_optimal_1=consumption_vector(fin_index_1)
                        
        space_lived_in_internal=housing_size(space_lived_in_index_1)
        
        choose_to_be_homeless=0
        
        if ((space_lived_in_index_1==lowest_housing_grid_point) .AND. (include_homelessness==1)) then
        
            choose_to_be_homeless=1
        
        end if
                                                                                                                               
        frac_mov_optimal_1(years_left_on_mortgage_index)%&
            vector_transition_real(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=0
                        
        fin_index_frac_mov_to_optimal(years_left_on_mortgage_index)%&
            vector_transition_integer(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=fin_index_1
                    
        if (in_retirement==1) then
    
            if (age_index<life_span) then
        
                if (owner_to_owner_stay_2==1) then
            
                    dis_prev_index_fin=financial_grid(fin_index_1)-financial_grid(max(fin_index_1-1,&
                        (ownership_status_index_1*max_future_mortgage_index_2+&
                        (1-ownership_status_index_1)*negative_financial_grid_size)))
                        
                else
                
                    dis_prev_index_fin=financial_grid(fin_index_1)-&
                        financial_grid(max(fin_index_1-1,negative_financial_grid_size))
                
                end if
                
            else
            
                dis_prev_index_fin=financial_grid(fin_index_1)-financial_grid(max(fin_index_1-1,negative_financial_grid_size))
            
            end if
            
        else
        
            dis_prev_index_fin=financial_grid(fin_index_1)-financial_grid(max(fin_index_1-1,&
                can_get_mortgage*(ownership_status_index_1*1+(1-ownership_status_index_1)*negative_financial_grid_size)+&
                (1-can_get_mortgage)*negative_financial_grid_size))
        
        end if
                                                                 
        dis_next_index_fin=financial_grid(min(fin_index_1+1,financial_grid_size))-financial_grid(fin_index_1)
                                                        
        slope_of_financial_internal=(dis_prev_index_fin+dis_next_index_fin)/(fin_num_of_internal-1)
                                                                            
        internal_loop_1: do future_financial_index=1,fin_num_of_internal-2
                        
            if (in_retirement==1) then
    
                if (age_index<life_span) then
            
                    if (owner_to_owner_stay_2==1) then
                
                        financial_internal=financial_grid(max(fin_index_1-1,max_future_mortgage_index_2))+&
                            slope_of_financial_internal*future_financial_index
                            
                    else
                    
                        financial_internal=financial_grid(max(fin_index_1-1,negative_financial_grid_size))+&
                            slope_of_financial_internal*future_financial_index
                    
                    end if
                    
                else
                
                    financial_internal=financial_grid(max(fin_index_1-1,negative_financial_grid_size))+&
                        slope_of_financial_internal*future_financial_index
                
                end if
            
            else
            
                financial_internal=financial_grid(max(fin_index_1-1,&
                    can_get_mortgage*(ownership_status_index_1*1+(1-ownership_status_index_1)*negative_financial_grid_size)+&
                    (1-can_get_mortgage)*negative_financial_grid_size))+&
                    slope_of_financial_internal*future_financial_index
            
            end if                     
                                                                                        
            can_purchase_new_house=1
            
            if ((financial_internal<financial_grid(fin_index_1)) .AND. (ownership_status_index_1==1) .AND. &
               (financial_internal<0)) then
                        
                smallest_house=0
                second_smallest_house=0

                if (housing_price(time_period)*housing_size(housing_status_index_1)<=&
                    fraction_of_average_house_price_data*((1-is_counterfactual)*&
                    model_average_owner_occupied_unit_size(time_period)*housing_price(time_period)+is_counterfactual*&
                    average_owner_occupied_house_size_calibrated*calibrated_stationary_housing_price)) then
                            
                    smallest_house=1
                    
                elseif (housing_price(time_period)*housing_size(housing_status_index_1)<=&
                    2*fraction_of_average_house_price_data*((1-is_counterfactual)*&
                    model_average_owner_occupied_unit_size(time_period)*housing_price(time_period)+is_counterfactual*&
                    average_owner_occupied_house_size_calibrated*calibrated_stationary_housing_price)) then 
                    
                    second_smallest_house=1
                    
                end if
            
                if ((-financial_internal/(housing_price(time_period)*housing_size(housing_status_index_1))<&
                    (1-minimum_downpayment+smallest_house*min_down_of_smallest+second_smallest_house+&
                    min_down_of_second_smallest))) then
                    
                    if (-financial_internal/(housing_price(time_period)*housing_size(housing_status_index_1))>0.8) then
                                                   
                        max_mortgage_payments_as_fraction_of_income_1=max_debt_to_income_ratio
                            
                    else
                                    
                        max_mortgage_payments_as_fraction_of_income_1=max_debt_to_income_ratio_below_80
                                        
                    end if
                    
                    check_GDS_2=1
                    
                    if (owner_to_owner_stay_2==1) then
                    
                        if (financial_internal>financial_grid(financial_index)*(1-fraction_to_repay)) then
                        
                            check_GDS_2=0
                            
                        end if                               
                    
                    end if
                    
                    if (check_GDS_2==1) then
                                
                        spending_on_housing_1=&
                            (-min(financial_internal,0.d0)*(mortgage_interest_rate(time_period)+&
                            average_fraction_of_mortgage_repaid))+&
                            housing_price(time_period)*housing_size(housing_status_index_1)*&
                            (fraction_of_property_taxes_in_GDS*property_tax(time_period)+&
                            include_housing_depreciation_in_housing_costs*housing_depreciation_rate)
                            
                    else
                    
                        spending_on_housing_1=0
                    
                    end if
                                                                                                
                    if ((spending_on_housing_1/(gross_wage_income_1+&
                        risk_free_rate(time_period)*max(financial_internal,0.d0)))<=&
                        max_mortgage_payments_as_fraction_of_income_1) then
                        
                        can_purchase_new_house=1
                        
                    else
                    
                        can_purchase_new_house=0
                    
                    end if
                    
                else
                
                    can_purchase_new_house=0
                    
                end if
            
            end if
            
            if (can_purchase_new_house==1) then
                                
                consumption_internal_optimal_1=(after_tax_wealth_net_of_expenses_1-financial_internal)/(1+consumption_tax)
                                        
                if (consumption_internal_optimal_1>0) then
                                                                                
                    dis_internal_from_optimal_fin=financial_internal-financial_grid(fin_index_1)
                                                
                    if (dis_internal_from_optimal_fin<0) then
                            
                        if (in_retirement==1) then

                            if (age_index<life_span) then
                        
                                if (owner_to_owner_stay_2==1) then
                            
                                    fin_index_frac_mov_to=max(fin_index_1-1,max_future_mortgage_index_2)
                                        
                                else
                                
                                    fin_index_frac_mov_to=max(fin_index_1-1,negative_financial_grid_size)
                                
                                end if
                                
                            else
                            
                                fin_index_frac_mov_to=max(fin_index_1-1,negative_financial_grid_size)
                            
                            end if
                        
                        else
                        
                            fin_index_frac_mov_to=max(fin_index_1-1,can_get_mortgage*(ownership_status_index_1*1+&
                                (1-ownership_status_index_1)*negative_financial_grid_size)+&
                                (1-can_get_mortgage)*negative_financial_grid_size)
                        
                        end if
                    
                        frac_mov_to_another_cell_1=abs(dis_internal_from_optimal_fin)/dis_prev_index_fin
                                                                                             
                    ! Considering a financial choice above the optimal one found
                    elseif (dis_internal_from_optimal_fin>0) then
                    
                        fin_index_frac_mov_to=min(fin_index_1+1,financial_grid_size)
                                                
                        frac_mov_to_another_cell_1=abs(dis_internal_from_optimal_fin)/dis_next_index_fin
                                                                                                                                                      
                    ! This means we are considering the same financial choice as the optimal choice found
                    else
                                                                        
                        frac_mov_to_another_cell_1=0
                                                            
                        fin_index_frac_mov_to=fin_index_1
                                                                                                            
                    end if
                    
                    financial_index_copy=fin_index_1
                                    
                    financial_index_copy_1=fin_index_frac_mov_to
                                                                
                    flow_utility_internal=(housing_status_utility_function&
                        (space_lived_in_index_1,ownership_status_index_1,&
                        years_holding_onto_asset_index_1,years_of_amortization_1)*&
                        (consumption_internal_optimal_1)**((1-risk_aversion_consumption)*&
                        (1-relative_share_of_housing)))/(1-risk_aversion_consumption)
                                                    
                    flow_utility_internal=flow_utility_internal*scaled_household_size(age_index)
                                                                                                
                    future_utility_internal=0
                                        
                    if (add_net_worth_bequest>=1) then
                       
                        if ((add_net_worth_bequest==1) .AND. (age_index==life_span)) then
                        
                            value_of_inheritance=&
                                ownership_status_index_1*housing_size(housing_status_index_1)*&
                                housing_price(time_period)+financial_internal
                       
                            if (value_of_inheritance>0) then
                            
                                future_utility_internal=future_utility_internal+discount_factor_on_child*&
                                    (value_of_inheritance**(1-risk_aversion_consumption))/(1-risk_aversion_consumption)
                            
                            else
        
                                future_utility_internal=large_negative_num
                                    
                            end if
                                
                        elseif (add_net_worth_bequest==2) then
                                                                                
                            do future_employment_index=1,employment_grid_size
                            
                                do future_rental_opportunity_index=0,random_assignment
                                                                                                                               
                                    future_utility_internal=future_utility_internal+discount_factor_on_child*&
                                        (1-survival_probability(age_index))*&
                                        probability_rental_opportunity(future_rental_opportunity_index)*&
                                        (use_GKKOC_process*intergenerational_employment_process&
                                        (employment_index,future_employment_index)+(1-use_GKKOC_process)*&
                                        unconditional_employment_probability(future_employment_index))*&
                                        (frac_mov_to_another_cell_1*value_function_guess(years_of_amortization_1)%&
                                        vector_youngest_real(financial_index_copy_1,future_employment_index,&
                                        housing_status_index_1,1,future_rental_opportunity_index)+(1-frac_mov_to_another_cell_1)*&
                                        value_function_guess(years_of_amortization_1)%vector_youngest_real&
                                        (financial_index_copy,future_employment_index,housing_status_index_1,1,&
                                        future_rental_opportunity_index))
                                        
                                end do
                                                                                                                                                                                
                            end do
                            
                        end if
                                                                                   
                    end if
                                                                           
                    do future_employment_index=&
                        at_least_one_period_before_retirement*employment_index+&
                        (1-at_least_one_period_before_retirement)*1,&
                        at_least_one_period_before_retirement*employment_index+&
                        (1-at_least_one_period_before_retirement)*employment_grid_size
                        
                        do future_rent_shock_index=1,loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)
                        
                            do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                                                                           
                                future_utility_internal=future_utility_internal+discount_factor*&
                                    probability_rental_opportunity(future_rental_opportunity_index)*&
                                    survival_probability(age_index)*employment_process(choose_to_be_homeless,age_index,&
                                    employment_index,future_employment_index)*&
                                    (loop_over_rent_shocks*probability_of_rent_shock(future_rent_shock_index)+&
                                    (1-loop_over_rent_shocks))*(frac_mov_to_another_cell_1*value_function(years_of_amortization_1)%&
                                    vector_transition_real(min(time_period+solve_transition_dynamics,transition_length),&
                                    age_index+1,financial_index_copy_1,future_employment_index,housing_status_index_1,&
                                    loop_over_rent_shocks*future_rent_shock_index+(1-loop_over_rent_shocks)*&
                                    ((1-ownership_status_index_1)*years_holding_onto_asset_index_1+&
                                    ownership_status_index_1),future_rental_opportunity_index)+(1-frac_mov_to_another_cell_1)*&
                                    value_function(years_of_amortization_1)%vector_transition_real&
                                    (min(time_period+solve_transition_dynamics,transition_length),age_index+1,financial_index_copy,&
                                    future_employment_index,housing_status_index_1,loop_over_rent_shocks*future_rent_shock_index+&
                                    (1-loop_over_rent_shocks)*((1-ownership_status_index_1)*years_holding_onto_asset_index_1+&
                                    ownership_status_index_1),future_rental_opportunity_index))
                                    
                            end do
                                
                        end do
                                                                                                                                        
                    end do
                                                                            
                    val_fun_internal=flow_utility_internal+future_utility_internal
                                                
                    if (val_fun_internal>value_function_1) then
                                    
                        value_function_1=val_fun_internal
                                                                        
                        frac_mov_optimal_1(years_left_on_mortgage_index)%&
                            vector_transition_real(time_period,age_index,financial_index,&
                            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
                            frac_mov_to_another_cell_1
                            
                        fin_index_frac_mov_to_optimal(years_left_on_mortgage_index)%&
                            vector_transition_integer(time_period,age_index,financial_index,&
                            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
                            fin_index_frac_mov_to
                    
                        policy_function_consumption(years_left_on_mortgage_index)%&
                            vector_transition_real(time_period,age_index,financial_index,&
                            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
                            consumption_internal_optimal_1
                                                                                                                                                                                                                                                                                      
                    end if
                                                
                end if
                                                                            
            end if
                                                
        end do internal_loop_1
            
        val_fun_internal=value_function_1
                                                    
    end subroutine
    
    subroutine check_budget_constraint_clears(time_period,after_tax_wealth_net_of_expenses_1,financial_index)
        
        use Global_Vars
        
        implicit none
        
        integer :: financial_index
        integer :: time_period,fin_index_frac_used,financial_index_used,amortization_used
        real(8) :: after_tax_wealth_net_of_expenses_1,frac_used,consumption_used

        frac_used=frac_mov_optimal_1(years_left_on_mortgage_index)%vector_transition_real&
            (time_period,age_index,financial_index,employment_index,housing_status_index,years_holding_onto_asset_index,&
            rental_opportunity_index)
    
        fin_index_frac_used=fin_index_frac_mov_to_optimal(years_left_on_mortgage_index)%vector_transition_integer&
            (time_period,age_index,financial_index,employment_index,housing_status_index,years_holding_onto_asset_index,&
            rental_opportunity_index)

        financial_index_used=policy_function_financial_index(years_left_on_mortgage_index)%vector_transition_integer&
            (time_period,age_index,financial_index,employment_index,housing_status_index,years_holding_onto_asset_index,&
            rental_opportunity_index)

        consumption_used=policy_function_consumption(years_left_on_mortgage_index)%vector_transition_real&
            (time_period,age_index,financial_index,employment_index,housing_status_index,years_holding_onto_asset_index,&
            rental_opportunity_index)

        amortization_used=policy_function_years_of_amortization_index(years_left_on_mortgage_index)%&
            vector_transition_integer(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)                                                
                                                                                                            
        x=after_tax_wealth_net_of_expenses_1-(frac_used*financial_grid(fin_index_frac_used)+&
            (1-frac_used)*financial_grid(max(financial_index_used,1))+(1+consumption_tax)*consumption_used)
                                                                                                      
        if ((abs(x)>5*10.d0**(-8)) .AND. (min_consumption_guaranteed>0) .AND. &
           (after_tax_wealth_net_of_expenses_1>0))  then
                         
            write(*,*) "error budget does not clear"
            write(*,*) "x=", x
                
            write(*,*) "financial_index=", financial_index_used
                                    
            write(*,*) "age_index=", age_index
                
            write(*,*) "years_left_on_mortgage_index=", &
                years_left_on_mortgage_index    
                
            write(*,*) "amortization_used=", amortization_used
                                                
            write(*,*) "financial_index=", financial_index
                
            write(*,*) "fin_index_frac_used=", fin_index_frac_used
                            
            write(*,*) "frac_used=", frac_used
            
            write(*,*) "consumption+savings=",&
                frac_used*(financial_grid(fin_index_frac_used)-&
                can_get_mortgage*abs(min(financial_grid(financial_index_used),0.d0)))+&
                (1-frac_used)*(financial_grid(financial_index_used)-&
                can_get_mortgage*abs(min(financial_grid(financial_index_used),0.d0)))+&
                (1+consumption_tax)*consumption_used
                
            write(*,*) "after_tax_wealth_net_of_expenses_1=", after_tax_wealth_net_of_expenses_1
                                
            write(*,*) "years_left_on_mortgage_index=", years_left_on_mortgage_index
        
            write(*,*) "rent=", rent(time_period,years_left_on_mortgage_index)
                                                    
        end if
            
        if ((value_function(years_left_on_mortgage_index)%&
            vector_transition_real(time_period,age_index,financial_index,&
            employment_index,housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)==&
            large_negative_num) .AND. (after_tax_wealth_net_of_expenses_1>0) .AND. &
            (years_left_on_mortgage_index<=1) .AND. (age_index<life_span)) then
            
            if (min_consumption_guaranteed>0) then
            
                write(*,*) "error value function"
                write(*,*) "after_tax_wealth_net_of_expenses_1=", after_tax_wealth_net_of_expenses_1
                write(*,*) "age_index=", age_index
                write(*,*) "employment_index=", employment_index
                write(*,*) "financial_index=", financial_index
                write(*,*) "income minnus rent=", employment_grid(1,1)-rent(1,1)*smallest_house_size
                write(*,*) "disutility_of_landlords=", disutility_of_landlords
                write(*,*) "rental_management_costs=", rental_management_costs
                
            end if
                                                           
            negative_consumption=1
                        
        end if
    
    end subroutine
    
    subroutine initialize_feasible_distribution(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,has_house,financial_index,loop_over_rent_shocks,years_holding_onto_asset_index_used,&
            years_left_on_mortgage_index_used
        character*1 :: string_used
                        
        do i=1-two_rental_markets,2
                
            distribution_guess(i)%vector_transition_real(time_period,:,:,:,:,:,:)=0
                                
        end do
        
        value_of_initial_assets=0
        
        if (include_exogenous_endowment==1) then
                    
            relative_endowments(7)=1.3853
            relative_endowments(6)=0.8520
            relative_endowments(5)=0.2609
            relative_endowments(4)=0.1964
            relative_endowments(3)=0.1338
            relative_endowments(2)=0.1319
            relative_endowments(1)=0.1319

            fraction_with_positive_wealth(7)=0.7690
            fraction_with_positive_wealth(6)=0.6654
            fraction_with_positive_wealth(5)=0.5946
            fraction_with_positive_wealth(4)=0.6482
            fraction_with_positive_wealth(3)=0.5171
            fraction_with_positive_wealth(2)=0.4779
            fraction_with_positive_wealth(1)=0.4779
                                    
            max_initial_endowment=is_counterfactual*average_labor_income_calibrated+&
                (1-is_counterfactual)*model_average_labour_income(time_period)

            if (is_counterfactual==0) then

                initial_endowment=negative_financial_grid_size

            end if
            
            do employment_index=1,employment_grid_size

                if (is_counterfactual==0) then
            
                    if (fraction_with_positive_wealth(employment_index)>0) then
                
                        initial_endowment(employment_index)=&
                            minloc(financial_grid(:)-relative_endowments(employment_index)*max_initial_endowment,1,&
                            mask=financial_grid(:)-relative_endowments(employment_index)*max_initial_endowment>0)
                                                                            
                    else
                    
                        initial_endowment(employment_index)=negative_financial_grid_size
                            
                    end if

                end if
                
                do future_employment_index=1,employment_grid_size
                
                    do future_rental_opportunity_index=0,random_assignment
                    
                        distribution_guess(1)%vector_transition_real&
                            (time_period,1,initial_endowment(employment_index),employment_index,0,1,&
                            future_rental_opportunity_index)=probability_rental_opportunity(future_rental_opportunity_index)*&
                            fraction_with_positive_wealth(employment_index)*&
                            (use_GKKOC_process*intergenerational_employment_process(future_employment_index,employment_index)+&
                            (1-use_GKKOC_process)*unconditional_employment_probability(future_employment_index))
                            
                        distribution_guess(1)%vector_transition_real&
                            (time_period,1,negative_financial_grid_size,employment_index,0,1,future_rental_opportunity_index)=&
                            probability_rental_opportunity(future_rental_opportunity_index)*&
                            (1-fraction_with_positive_wealth(employment_index))*&
                            (use_GKKOC_process*intergenerational_employment_process(future_employment_index,employment_index)+&
                            (1-use_GKKOC_process)*unconditional_employment_probability(future_employment_index))
                            
                    end do
                                                                        
                end do
                
                do future_rental_opportunity_index=0,random_assignment
                    
                    value_of_initial_assets=value_of_initial_assets+&
                        distribution_guess(1)%vector_transition_real&
                        (time_period,1,initial_endowment(employment_index),&
                        employment_index,0,1,future_rental_opportunity_index)*&
                        financial_grid(initial_endowment(employment_index))
                        
                end do
                                       
            end do

            write(*,*) "initial_endowment=", initial_endowment  
            write(*,*) "value_of_initial_assets=", value_of_initial_assets
                        
        else

            fraction_with_positive_wealth=0
            
            do future_rental_opportunity_index=0,random_assignment
            
                distribution_guess(1)%vector_transition_real&
                    (time_period,1,negative_financial_grid_size,1,0,1,future_rental_opportunity_index)=&
                    probability_rental_opportunity(future_rental_opportunity_index)
                    
            end do
                            
        end if
                
        do age_index=2,life_span
                    
            do employment_index=1,employment_grid_size
            
                do future_rental_opportunity_index=0,random_assignment
                                
                    if ((include_exogenous_endowment==1) .AND. (age_index==2)) then
                                                
                        distribution_guess(1)%vector_transition_real&
                            (time_period,age_index,negative_financial_grid_size,&
                            employment_index,0,1,future_rental_opportunity_index)=&
                            survival_probability(age_index-1)*distribution_guess(1)%vector_transition_real&
                            (time_period,age_index-1,initial_endowment(employment_index),employment_index,0,1,&
                            future_rental_opportunity_index)

                    end if
                    
                    distribution_guess(1)%vector_transition_real&
                        (time_period,age_index,negative_financial_grid_size,1,0,1,future_rental_opportunity_index)=&
                        survival_probability(age_index-1)*distribution_guess(1)%vector_transition_real&
                        (time_period,age_index-1,negative_financial_grid_size,1,0,1,future_rental_opportunity_index)
                         
                end do
                         
            end do
                                                                  
        end do
        
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
        
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_guess(i)%vector_transition_real(time_period,:,:,:,:,:,:))
                
        end do
        
        write(*,*) "sum(distribution_guess)=", sum_of_distribution
        
        if ((use_old_distribution==1) .AND. (two_rental_markets==benchmark_has_two_rental_markets))  then
        
            do i=1-two_rental_markets,2
            
                write(string_used,'(i1)') i
        
                open(unit=12,file="distribution_stationary"//trim(string_used)//trim(old_results)//".dat",action='read')
                    read(12,'(F30.20)') distribution_guess(i)%vector_transition_real(1,:,:,:,:,:,:)
                close(12)
                            
                if (hold_distribution_fixed==1) then
                
                    distribution_stationary(i)%vector_transition_real(1,:,:,:,:,:,:)=&
                        distribution_guess(i)%vector_transition_real(1,:,:,:,:,:,:)
                
                end if
                
            end do
            
        end if
        
        if ((solve_transition_dynamics==1) .AND. (max_tracking==1))  then
                        
            do i=1-two_rental_markets,2
            
                distribution_guess(i)%vector_transition_real(0,:,:,:,:,:,:)=0
                
            end do
                
            do age_index=1,life_span
            
                do rental_opportunity_index=0,random_assignment
        
                    do years_left_on_mortgage_index=(1-benchmark_has_two_rental_markets),max_amortization_years
                                
                        if (years_left_on_mortgage_index>1) then
                            
                            has_house=1
                            
                        else
                        
                            has_house=0
                            
                        end if
                        
                        loop_over_rent_shocks=0
                    
                        if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                            
                            loop_over_rent_shocks=1
                            
                        else
                        
                            if ((benchmark_has_two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                                (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                            
                                loop_over_rent_shocks=1
                                                            
                            end if
                            
                        end if
                        
                        years_left_on_mortgage_index_used=max(years_left_on_mortgage_index,1-two_rental_markets)
                                                        
                        do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                            (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*&
                            min(age_index,max_tracking_benchmark))
                            
                            years_holding_onto_asset_index_used=min(years_holding_onto_asset_index,max_tracking)
                                                
                            do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                                                           
                                do employment_index=1,employment_grid_size
                                                    
                                    do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                        has_house*index_of_smallest_house_hhld_can_buy,housing_status_grid_size-1
                                            
                                        distribution_guess(years_left_on_mortgage_index_used)%vector_transition_real&
                                            (0,age_index,financial_index,employment_index,housing_status_index,&
                                            years_holding_onto_asset_index_used,rental_opportunity_index)=&
                                            distribution_guess(years_left_on_mortgage_index_used)%&
                                            vector_transition_real(0,age_index,financial_index,&
                                            employment_index,housing_status_index,years_holding_onto_asset_index_used,&
                                            rental_opportunity_index)+distribution_to_fetch(years_left_on_mortgage_index)%&
                                            vector_transition_real(1,age_index,financial_index,employment_index,&
                                            housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)
                                        
                                    end do
                                    
                                end do
                                
                            end do
                            
                        end do
                    
                    end do
                    
                end do
                
            end do
            
            sum_of_distribution=0
        
            do i=1-two_rental_markets,2
            
                sum_of_distribution=sum_of_distribution+sum(distribution_guess(i)%vector_transition_real(0,:,:,:,:,:,:))
                    
                distribution_stationary(i)%vector_transition_real(0,:,:,:,:,:,:)=&
                    distribution_guess(i)%vector_transition_real(0,:,:,:,:,:,:)
                    
            end do
                        
            write(*,*) "sum_of_distribution 1=", sum_of_distribution
                        
        end if
        
    end subroutine
    
    subroutine converge_to_stationary_equilibrium(time_period)
    
        use Global_Vars
        !$ use omp_lib
               
        implicit none
        
        integer :: financial_index,has_house,time_period
        integer :: number_of_threads_used,loop_only_over_oldest,loop_over_rent_shocks,amortization_used
        real(8) :: sum_distribution_implied,sum_distribution_guess
        
        if ((have_probablistic_death==0) .AND. (add_net_worth_bequest==0)) then
            
            loop_only_over_oldest=1
            
        else
        
            loop_only_over_oldest=1
            
        end if
        
        if (solve_transition_dynamics>=0) then
        
!            !$omp parallel
!            !$omp master
!            !$ total_number_of_threads=omp_get_num_threads()
!!            write(*,*) "total_number_of_threads=", total_number_of_threads
!            !$omp end master
!            !$omp end parallel
                    
        end if
        
        if ((is_counterfactual==1) .AND. (solve_transition_dynamics==1)) then
        
            write(*,*) "time_period=", time_period
        
        end if
        
        distance_distribution=1
                
        loop_count_dist=0
               
        do while (distance_distribution>stopping_rule_stationary_distribution)
        
            do i=1-two_rental_markets,2
                
                distribution_implied_private(i)%vector_cpu_real(:,:,:,:,:,:,:)=0
                distribution_implied(i)%vector_transition_real(:,:,:,:,:,:,:)=0
                
            end do
            
             if (solve_transition_dynamics>=0) then
             
                number_of_threads_used=max_CPUs

                CALL OMP_SET_NUM_THREADS(number_of_threads_used)
        
                !$omp parallel
                !$omp master
                !$ total_number_of_threads=omp_get_num_threads()
                !$omp end master
                !$omp end parallel
                    
            end if
            
            do m=1,2
                
                do age_index=1,life_span-1
                    
                    if (age_index>=(life_span-retirement_span)) then
                    
                        at_least_one_period_before_retirement=1
                    
                    else
                    
                        at_least_one_period_before_retirement=0
                    
                    end if
                                        
                    if (age_index==1) then
                
                        newborn_household=1
                        
                    else
                
                        newborn_household=0
                    
                    end if
                    
                    do rental_opportunity_index=0,random_assignment
                
                        do years_left_on_mortgage_index=newborn_household+(1-newborn_household)*(1-two_rental_markets),&
                            newborn_household*1+(1-newborn_household)*max_amortization_years
                                                
                            if (years_left_on_mortgage_index>1) then
                            
                                has_house=1
                                
                            else
                            
                                has_house=0
                                
                            end if
                            
                            loop_over_rent_shocks=0
                            
                            if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                            
                                loop_over_rent_shocks=1
                            
                            else
                            
                                if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                                    (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                                    loop_over_rent_shocks=1
                                                                    
                                end if
                            
                            end if
                            
                            do years_holding_onto_asset_index=1,newborn_household*1+&
                                (1-newborn_household)*(has_house+(1-has_house)*&
                                (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking)))
                                                                 
                                !$omp parallel do default(none) &
                                !$omp& shared(&
                                !$omp& distribution_implied_private,distribution_guess,&
                                !$omp& frac_mov_optimal_1,fin_index_frac_mov_to_optimal,&
                                !$omp& policy_function_financial_index,policy_function_years_of_amortization_index,&
                                !$omp& policy_function_housing_status_index,policy_function_years_holding_onto_asset,&
                                !$omp& m,time_period,has_mortgage,newborn_household,loop_over_rent_shocks,&
                                !$omp& at_least_one_period_before_retirement,survival_probability,employment_process,&
                                !$omp& age_index,has_house,years_left_on_mortgage_index,years_holding_onto_asset_index,&
                                !$omp& probability_of_rent_shock,rental_opportunity_index,probability_rental_opportunity)
                                                                    
                                do financial_index=newborn_household*negative_financial_grid_size+&
                                     (1-newborn_household)*(has_house*1+(1-has_house)*negative_financial_grid_size),&
                                     include_exogenous_endowment*financial_grid_size+(1-include_exogenous_endowment)*&
                                     (newborn_household*negative_financial_grid_size+(1-newborn_household)*financial_grid_size)
                                
            !                        thread_num=0
                                    !$ thread_num=omp_get_thread_num()
                                    
                                    do employment_index=1,employment_grid_size
                                                        
                                        do housing_status_index=(1-newborn_household)*&
                                            ((1-has_house)*lowest_housing_grid_point+&
                                            has_house*index_of_smallest_house_hhld_can_buy),&
                                            (1-newborn_household)*(housing_status_grid_size-1)
                                                                                                                                                        
                                            if (policy_function_years_of_amortization_index(years_left_on_mortgage_index)%&
                                                vector_transition_integer(time_period,age_index,financial_index,&
                                                employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index)>1) then
                                                
                                                choose_to_own=1
                                                
                                            else
                                            
                                                choose_to_own=0
                                                
                                            end if
                                            
                                            loop_over_future_rent_shocks=0
                                            
                                            if (policy_function_years_of_amortization_index&
                                                (years_left_on_mortgage_index)%vector_transition_integer&
                                                (time_period,age_index,financial_index,employment_index,&
                                                housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index)==0) then
                                                        
                                                loop_over_future_rent_shocks=1
                                                
                                            else
                                            
                                                if ((two_rental_markets==0) .AND. &
                                                    (policy_function_years_of_amortization_index&
                                                    (years_left_on_mortgage_index)%vector_transition_integer&
                                                    (time_period,age_index,financial_index,employment_index,&
                                                    housing_status_index,years_holding_onto_asset_index,&
                                                    rental_opportunity_index)==1)) then
                                        
                                                    loop_over_future_rent_shocks=1
                                                                                                  
                                                end if
                                                
                                            end if
                                     
                                            do future_employment_index=&
                                                at_least_one_period_before_retirement*employment_index+&
                                                (1-at_least_one_period_before_retirement)*1,&
                                                at_least_one_period_before_retirement*employment_index+&
                                                (1-at_least_one_period_before_retirement)*employment_grid_size
                                                
                                                do future_rent_shock_index=1,&
                                                    loop_over_future_rent_shocks*rent_shock_grid_size+&
                                                    (1-loop_over_future_rent_shocks)
                                                    
                                                    do future_rental_opportunity_index=0,random_assignment
                                                            
                                                        if (m==1) then
                                                        
                                                            financial_index_copy=policy_function_financial_index&
                                                                (years_left_on_mortgage_index)%vector_transition_integer&
                                                                (time_period,age_index,financial_index,employment_index,&
                                                                housing_status_index,years_holding_onto_asset_index,&
                                                                rental_opportunity_index)
                                                                
                                                            fraction_copy=(1-frac_mov_optimal_1(years_left_on_mortgage_index)%&
                                                                vector_transition_real(time_period,age_index,&
                                                                financial_index,employment_index,&
                                                                housing_status_index,years_holding_onto_asset_index,&
                                                                rental_opportunity_index))
                                                                                                                
                                                        elseif (m==2) then
                                                        
                                                            financial_index_copy=fin_index_frac_mov_to_optimal&
                                                                (years_left_on_mortgage_index)%vector_transition_integer&
                                                                (time_period,age_index,financial_index,employment_index,&
                                                                housing_status_index,years_holding_onto_asset_index,&
                                                                rental_opportunity_index)
                                                                
                                                            fraction_copy=frac_mov_optimal_1(years_left_on_mortgage_index)%&
                                                                vector_transition_real(time_period,&
                                                                age_index,financial_index,employment_index,&
                                                                housing_status_index,years_holding_onto_asset_index,&
                                                                rental_opportunity_index)
                                                                                                                                                                                                                                                                       
                                                        end if

                                                        years_holding_onto_asset_dist_used=&
                                                            policy_function_years_holding_onto_asset&
                                                            (years_left_on_mortgage_index)%&
                                                            vector_transition_integer(time_period,age_index,financial_index,&
                                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                            rental_opportunity_index)

                                                        amortization_used_dist=policy_function_years_of_amortization_index&
                                                            (years_left_on_mortgage_index)%vector_transition_integer&
                                                            (min(time_period,transition_length),&
                                                            age_index,financial_index,employment_index,&
                                                            housing_status_index,years_holding_onto_asset_index,&
                                                            rental_opportunity_index)

                                                        choose_to_be_homeless_1=0

                                                        housing_status_index_dist_used=&
                                                            policy_function_housing_status_index(years_left_on_mortgage_index)%&
                                                            vector_transition_integer(time_period,age_index,financial_index,&
                                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                            rental_opportunity_index)
                                                            
                                                        if ((housing_status_index_dist_used==lowest_housing_grid_point) .AND. &
                                                           (include_homelessness==1)) then
                                                        
                                                            choose_to_be_homeless_1=1
                                                            
                                                        end if
                                                                                                                                                                 
                                                        distribution_implied_private(amortization_used_dist)%vector_cpu_real&
                                                            (thread_num+1,age_index+1,financial_index_copy,&
                                                            future_employment_index,housing_status_index_dist_used,&
                                                            loop_over_future_rent_shocks*future_rent_shock_index+&
                                                            (1-loop_over_future_rent_shocks)*years_holding_onto_asset_dist_used,&
                                                            future_rental_opportunity_index)=&
                                                            probability_rental_opportunity(future_rental_opportunity_index)*&
                                                            (loop_over_future_rent_shocks*&
                                                            probability_of_rent_shock(future_rent_shock_index)+&
                                                            (1-loop_over_future_rent_shocks))*&
                                                            fraction_copy*survival_probability(age_index)*&
                                                            employment_process(choose_to_be_homeless_1,age_index,&
                                                            employment_index,future_employment_index)*&
                                                            distribution_guess(years_left_on_mortgage_index)%&
                                                            vector_transition_real&
                                                            (time_period-solve_transition_dynamics,age_index,financial_index,&
                                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                            rental_opportunity_index)+&
                                                            distribution_implied_private(amortization_used_dist)%&
                                                            vector_cpu_real(thread_num+1,age_index+1,financial_index_copy,&
                                                            future_employment_index,housing_status_index_dist_used,&
                                                            loop_over_future_rent_shocks*&
                                                            future_rent_shock_index+(1-loop_over_future_rent_shocks)*&
                                                            years_holding_onto_asset_dist_used,future_rental_opportunity_index)
                                                                                                                                                                  
                                                    end do
                                                
                                                end do
                                                
                                            end do
                                                                                    
                                        end do
                                                                                            
                                    end do
                                    
                                end do
                                
                                !$omp end parallel do
                                                            
                            end do
                    
                        end do
                    
                    end do
                                                    
                end do
            
            end do
            
            if (solve_transition_dynamics>=0) then
            
                number_of_threads_used=max_CPUs

                CALL OMP_SET_NUM_THREADS(number_of_threads_used)
        
                !$omp parallel
                !$omp master
                !$ total_number_of_threads=omp_get_num_threads()
                !$omp end master
                !$omp end parallel
                    
            end if
            
            do m=1,2
            
                do age_index=loop_only_over_oldest*life_span+(1-loop_only_over_oldest)*1,life_span
                                                        
                    if (age_index==1) then
                
                        newborn_household=1
                                            
                    else
                
                        newborn_household=0
                    
                    end if
                    
                    do rental_opportunity_index=0,random_assignment
                                                        
                        do years_left_on_mortgage_index=newborn_household+(1-newborn_household)*(1-two_rental_markets),&
                            newborn_household*1+(1-newborn_household)*max_amortization_years
                                                
                            if (years_left_on_mortgage_index>1) then
                            
                                has_house=1
                                
                            else
                            
                                has_house=0
                                
                            end if
                            
                            loop_over_rent_shocks=0
                            
                            if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                            
                                loop_over_rent_shocks=1
                            
                            else
                            
                                if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                                    (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                        
                                    loop_over_rent_shocks=1
                                                                    
                                end if
                            
                            end if
                                                    
                            do years_holding_onto_asset_index=1,&
                                newborn_household*1+(1-newborn_household)*(has_house+(1-has_house)*&
                                (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking)))
                                                                  
                                !$omp parallel do default(none) &
                                !$omp& shared(distribution_implied_private,distribution_guess,&
                                !$omp& frac_mov_optimal_1,fin_index_frac_mov_to_optimal,initial_endowment,&
                                !$omp& policy_function_financial_index,policy_function_years_of_amortization_index,&
                                !$omp& policy_function_housing_status_index,policy_function_years_holding_onto_asset,&
                                !$omp& m,time_period,has_mortgage,at_least_one_period_before_retirement,&
                                !$omp& has_house,survival_probability,intergenerational_employment_process,&
                                !$omp& unconditional_employment_probability,newborn_household,&
                                !$omp& age_index,years_left_on_mortgage_index,years_holding_onto_asset_index,loop_over_rent_shocks,&
                                !$omp& fraction_with_positive_wealth,rental_opportunity_index,probability_rental_opportunity)
                                                                                    
                                do financial_index=newborn_household*negative_financial_grid_size+&
                                    (1-newborn_household)*(has_house*1+(1-has_house)*negative_financial_grid_size),&
                                    include_exogenous_endowment*financial_grid_size+(1-include_exogenous_endowment)*&
                                    (newborn_household*negative_financial_grid_size+(1-newborn_household)*financial_grid_size)
                                
            !                        thread_num=0
                                    !$ thread_num=omp_get_thread_num()
            
                                   do employment_index=1,employment_grid_size
                                                            
                                        do housing_status_index=(1-newborn_household)*((1-has_house)*lowest_housing_grid_point+&
                                            has_house*index_of_smallest_house_hhld_can_buy),&
                                            (1-newborn_household)*(housing_status_grid_size-1)
                                                                                        
                                            if (policy_function_years_of_amortization_index(years_left_on_mortgage_index)%&
                                                vector_transition_integer(time_period,age_index,financial_index,employment_index,&
                                                housing_status_index,years_holding_onto_asset_index,&
                                                rental_opportunity_index)>1) then
                                                
                                                choose_to_own=1
                                                
                                            else
                                            
                                                choose_to_own=0
                                                
                                            end if
                                                                                                  
                                            do future_employment_index=1,employment_grid_size
                                            
                                                do future_rental_opportunity_index=0,random_assignment
                                                                                                                                                                        
                                                    if (m==1) then
                                                            
                                                        financial_index_copy=policy_function_financial_index&
                                                            (years_left_on_mortgage_index)%&
                                                            vector_transition_integer(time_period,age_index,financial_index,&
                                                            employment_index,housing_status_index,&
                                                            years_holding_onto_asset_index,rental_opportunity_index)
                                                            
                                                        fraction_copy=(1-frac_mov_optimal_1(years_left_on_mortgage_index)%&
                                                            vector_transition_real(time_period,age_index,&
                                                            financial_index,employment_index,housing_status_index,&
                                                            years_holding_onto_asset_index,rental_opportunity_index))
                                                        
                                                    elseif (m==2) then
                                                    
                                                        financial_index_copy=fin_index_frac_mov_to_optimal&
                                                            (years_left_on_mortgage_index)%&
                                                            vector_transition_integer(time_period,age_index,financial_index,&
                                                            employment_index,housing_status_index,&
                                                            years_holding_onto_asset_index,rental_opportunity_index)
                                                            
                                                        fraction_copy=frac_mov_optimal_1(years_left_on_mortgage_index)%&
                                                            vector_transition_real(time_period,age_index,&
                                                            financial_index,employment_index,housing_status_index,&
                                                            years_holding_onto_asset_index,rental_opportunity_index)
                                                    
                                                    end if
                                                    
                                                    if (include_exogenous_endowment==1) then
                                                                                                                                                                    
                                                        distribution_implied_private(1)%vector_cpu_real(thread_num+1,1,&
                                                            initial_endowment(future_employment_index),future_employment_index,0,1,&
                                                            future_rental_opportunity_index)=&
                                                            probability_rental_opportunity(future_rental_opportunity_index)*&
                                                            fraction_copy*(1-survival_probability(age_index))*&
                                                            fraction_with_positive_wealth(employment_index)*&
                                                            (use_GKKOC_process*intergenerational_employment_process&
                                                            (employment_index,future_employment_index)+&
                                                            (1-use_GKKOC_process)*unconditional_employment_probability&
                                                            (future_employment_index))*&
                                                            distribution_guess(years_left_on_mortgage_index)%vector_transition_real&
                                                            (time_period-solve_transition_dynamics,age_index,financial_index,&
                                                            employment_index,housing_status_index,years_holding_onto_asset_index,&
                                                            rental_opportunity_index)+&
                                                            distribution_implied_private(1)%vector_cpu_real&
                                                            (thread_num+1,1,initial_endowment(future_employment_index),&
                                                            future_employment_index,0,1,future_rental_opportunity_index)

                                                    end if
                                                                                                                
                                                    distribution_implied_private(1)%vector_cpu_real(thread_num+1,1,&
                                                        negative_financial_grid_size,future_employment_index,0,1,&
                                                        future_rental_opportunity_index)=&
                                                        probability_rental_opportunity(future_rental_opportunity_index)*&
                                                        fraction_copy*(1-survival_probability(age_index))*&
                                                        (1-fraction_with_positive_wealth(employment_index))*&
                                                        (use_GKKOC_process*intergenerational_employment_process&
                                                        (employment_index,future_employment_index)+&
                                                        (1-use_GKKOC_process)*unconditional_employment_probability&
                                                        (future_employment_index))*&
                                                        distribution_guess(years_left_on_mortgage_index)%vector_transition_real&
                                                        (time_period-solve_transition_dynamics,age_index,&
                                                        financial_index,employment_index,&
                                                        housing_status_index,years_holding_onto_asset_index,&
                                                        rental_opportunity_index)+distribution_implied_private(1)%vector_cpu_real&
                                                        (thread_num+1,1,negative_financial_grid_size,future_employment_index,0,1,&
                                                        future_rental_opportunity_index)
                                                                                                               
                                                end do
                                                                                               
                                            end do
                                            
                                        end do
                                        
                                    end do
                                    
                                end do
                                
                                !$omp end parallel do 
                                                            
                            end do
                    
                        end do
                                
                    end do
                                
                end do
            
            end do
            
            if (solve_transition_dynamics>=0) then
            
                number_of_threads_used=max_CPUs

                CALL OMP_SET_NUM_THREADS(number_of_threads_used)
        
                !$omp parallel
                !$omp master
                !$ total_number_of_threads=omp_get_num_threads()
                !$omp end master
                !$omp end parallel
                    
            end if
                        
            do age_index=1,life_span
            
                if (age_index==1) then
                
                    newborn_household=1
                    
                else
                
                    newborn_household=0
                    
                end if
                
                do rental_opportunity_index=0,random_assignment
                                                      
                    do years_left_on_mortgage_index=newborn_household+(1-newborn_household)*(1-two_rental_markets),&
                        newborn_household*1+(1-newborn_household)*max_amortization_years
                    
                        if (years_left_on_mortgage_index>1) then
                        
                            has_house=1
                            
                        else
                        
                            has_house=0
                            
                        end if
                        
                        loop_over_rent_shocks=0
                        
                        if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                            loop_over_rent_shocks=1
                        
                        else
                        
                            if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                                (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                    
                                loop_over_rent_shocks=1
                                                                
                            end if
                        
                        end if
                        
                        do years_holding_onto_asset_index=1,&
                            newborn_household*1+(1-newborn_household)*(has_house+(1-has_house)*&
                            (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking)))
                            
                            !$omp parallel do default(none) &
                            !$omp& shared(&
                            !$omp& distribution_implied_private,distribution_implied,&
                            !$omp& total_number_of_threads,time_period,age_index,&
                            !$omp& has_mortgage,has_house,years_left_on_mortgage_index,&
                            !$omp& years_holding_onto_asset_index,newborn_household,loop_over_rent_shocks,rental_opportunity_index)
                                                                                
                            do financial_index=newborn_household*negative_financial_grid_size+&
                                (1-newborn_household)*(has_house*1+(1-has_house)*negative_financial_grid_size),&
                                include_exogenous_endowment*financial_grid_size+(1-include_exogenous_endowment)*&
                                (newborn_household*negative_financial_grid_size+(1-newborn_household)*financial_grid_size)
                            
                                do employment_index=1,employment_grid_size
                                                    
                                    do housing_status_index=(1-newborn_household)*&
                                        ((1-has_house)*lowest_housing_grid_point+&
                                        has_house*index_of_smallest_house_hhld_can_buy),&
                                        (1-newborn_household)*(housing_status_grid_size-1)
                                                                                                
                                        do thread_index=1,total_number_of_threads ! 1
                                        
                                            distribution_implied(years_left_on_mortgage_index)%vector_transition_real&
                                                (time_period,age_index,financial_index,employment_index,&
                                                housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)=&
                                                distribution_implied(years_left_on_mortgage_index)%vector_transition_real&
                                                (time_period,age_index,financial_index,employment_index,&
                                                housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)+&
                                                distribution_implied_private(years_left_on_mortgage_index)%vector_cpu_real&
                                                (thread_index,age_index,financial_index,employment_index,&
                                                housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)
                                                                                            
                                        end do
                                                                                
                                    end do
                                    
                                end do
                                                            
                            end do
                            
                            !$omp end parallel do
                            
                        end do
                        
                    end do
                                        
                end do
                                
            end do
            
            value_of_initial_assets=0

            if (include_exogenous_endowment==1) then
            
                do employment_index=1,employment_grid_size
                
                    do rental_opportunity_index=0,random_assignment
                                                
                        value_of_initial_assets=value_of_initial_assets+&
                            distribution_implied(1)%vector_transition_real(thread_num+1,1,&
                            initial_endowment(employment_index),employment_index,0,1,rental_opportunity_index)*&
                            financial_grid(initial_endowment(employment_index))
                            
                    end do
                                                                                          
                end do

            end if
            
            if (solve_transition_dynamics==0) then
            
                distance_distribution=0
            
                do i=1-two_rental_markets,2
            
                    if (maxval(abs(distribution_implied(i)%vector_transition_real(time_period,:,:,:,:,:,:)-&
                        distribution_guess(i)%vector_transition_real(time_period,:,:,:,:,:,:)))>distance_distribution) then
                        
                        distance_distribution=&
                            maxval(abs(distribution_implied(i)%vector_transition_real(time_period,:,:,:,:,:,:)-&
                            distribution_guess(i)%vector_transition_real(time_period,:,:,:,:,:,:)))
                            
                    end if
                        
                end do
                    
            else
            
                distance_distribution=0
                    
            end if
                        
            do i=1-two_rental_markets,2
            
                if (minval(distribution_implied(i)%vector_transition_real(:,:,:,:,:,:,:))<0) then
                
                    write(*,*) "minval distribution_implied=", minval(distribution_implied(i)%vector_transition_real(:,:,:,:,:,:,:))            
                
                end if
                
            end do
            
            sum_distribution_implied=0
            sum_distribution_guess=0
                                    
            do i=1-two_rental_markets,2
            
                sum_distribution_implied=sum_distribution_implied+&
                    sum(distribution_implied(i)%vector_transition_real(time_period,:,:,:,:,:,:))
            
                sum_distribution_guess=sum_distribution_guess+&
                    sum(distribution_guess(i)%vector_transition_real(time_period-solve_transition_dynamics,:,:,:,:,:,:))
            
            end do
                        
            if (abs(sum_distribution_guess-sum_distribution_implied)>10.d0**(-10)) then
                                
                write(*,*) "sum(distribution_guess(",time_period,")=", sum_distribution_guess
                                                            
                write(*,*) "sum(distribution_implied(",time_period,")=", sum_distribution_implied
                    
            end if
                                    
            if (solve_transition_dynamics==0) then
                
                write(*,*) "distribution distance=", distance_distribution
                
            end if
                
            do i=1-two_rental_markets,2
                
                distribution_guess(i)%vector_transition_real(time_period,:,:,:,:,:,:)=&
                    distribution_implied(i)%vector_transition_real(time_period,:,:,:,:,:,:)
                    
            end do
                        
            loop_count_dist=loop_count_dist+1
        
        end do
        
        do i=1-two_rental_markets,2
               
            distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:)=&
                distribution_implied(i)%vector_transition_real(time_period,:,:,:,:,:,:)

        end do

    end subroutine
        
    subroutine function_of_utility_advantage_from_homeowning(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,years_holding_onto_asset_index_to_use
        real(8) :: alpha_1
        
        if (random_assignment==0) then
        
            probability_rental_opportunity(0)=1
            
        else
        
            probability_rental_opportunity(0)=1-set_rental_opportunity_probability
            probability_rental_opportunity(0+random_assignment)=set_rental_opportunity_probability
            
        end if
        
        total_constrcution_profits(time_period)=0
        
        if ((is_counterfactual==1) .AND. (solve_transition_dynamics==0)) then
           
            housing_stock(time_period)=(1/housing_depreciation_rate)*((housing_price(time_period))/&
                (c_1_calibrated))**(empirical_housing_supplpy_elasticity)
                
            housing_investment(time_period)=(housing_price(time_period)/&
                c_1_calibrated)**(empirical_housing_supplpy_elasticity)
                                                                     
        elseif ((is_counterfactual==1) .AND. (solve_transition_dynamics==1)) then
            
            housing_investment(time_period)=(housing_price(time_period)/&
                c_1_calibrated)**(empirical_housing_supplpy_elasticity)
                                    
        elseif ((is_counterfactual==0)) then
                                    
            c_1=(housing_price(time_period))/((housing_depreciation_rate)*&
                total_housing_demand(time_period))**(1/empirical_housing_supplpy_elasticity)
                                                            
            housing_stock(time_period)=(1/housing_depreciation_rate)*&
                (housing_price(time_period)/c_1)**(empirical_housing_supplpy_elasticity)
                                                
            housing_investment(time_period)=(housing_price(time_period)/c_1)**&
                (empirical_housing_supplpy_elasticity)
            
            housing_investment_value(time_period)=housing_price(time_period)*housing_investment(time_period)
                
            cutoff_housing_investment_bisection(1)=0
            cutoff_housing_investment_bisection(2)=housing_investment(time_period)
            
            distance_cutoff_housing_investment=1
            
            do while (distance_cutoff_housing_investment>stopping_rule_cutoff_housing_investment)
            
                cutoff_housing_investment=&
                    (cutoff_housing_investment_bisection(1)+cutoff_housing_investment_bisection(2))/2
            
                c_min=c_1*(cutoff_housing_investment)**(1/empirical_housing_supplpy_elasticity)
                                
                construction_company_profits(time_period)=0
                construction_company_profits(time_period)=construction_company_profits(time_period)+&
                    (housing_price(time_period)-c_min)*cutoff_housing_investment
                construction_company_profits(time_period)=construction_company_profits(time_period)+&
                    housing_price(time_period)*(housing_investment(time_period)-cutoff_housing_investment)-&
                    ((c_1*(housing_investment(time_period))**((1/empirical_housing_supplpy_elasticity)+1))/&
                    ((1/empirical_housing_supplpy_elasticity)+1)-(c_1*(cutoff_housing_investment)**&
                    ((1/empirical_housing_supplpy_elasticity)+1))/((1/empirical_housing_supplpy_elasticity)+1))
                            
                land_value=construction_company_profits(time_period)
                        
                land_to_housing_stock=land_value/housing_investment_value(time_period)
                
                distance_cutoff_housing_investment=abs(land_to_housing_stock-empirical_land_to_housing_stock)
                
                if (land_to_housing_stock<empirical_land_to_housing_stock) then
        
                    cutoff_housing_investment_bisection(2)=cutoff_housing_investment
        
                else
            
                    cutoff_housing_investment_bisection(1)=cutoff_housing_investment
                
                end if
                
            
            end do
                                        
            total_constrcution_profits(time_period)=total_constrcution_profits(time_period)+&
                construction_company_profits(time_period)
                
            distance_housing_market(time_period)=&
                abs(total_housing_demand(time_period)-housing_stock(time_period))
                                    
        end if
        
        total_constrcution_profits(time_period)=housing_price(time_period)*housing_investment(time_period)-&
            (((1-is_counterfactual)*c_1+is_counterfactual*c_1_calibrated)*&
            (housing_investment(time_period))**((1/empirical_housing_supplpy_elasticity)+1))/&
            ((1/empirical_housing_supplpy_elasticity)+1)
                
        if ((loop_count==0) .AND. (solve_transition_dynamics==0)) then
        
            write(*,*) "total_constrcution_profits(time_period)=", total_constrcution_profits(time_period)
            write(*,*) "c_1=", c_1
            
        end if
        
        if (is_counterfactual==0) then
        
            if (calibrate_to_rent_control==0) then
            
                if (loop_count<1) then
                                        
                    rent(time_period,1-two_rental_markets)=rent_to_housing_price*housing_price(time_period)
                    model_average_rent(time_period,1-two_rental_markets)=rent(time_period,1-two_rental_markets)*&
                        (1+free_market_rent_increase_due_to_shock)
                        
                end if
                
            end if
            
            if (infinite_elasticity_of_rental_supply==1) then
                                                
                rental_management_costs=&
                    model_average_rent(time_period,1-two_rental_markets)+housing_price(time_period)*&
                    ((1-housing_depreciation_rate-property_tax(time_period))/(1+risk_free_rate(time_period))-1)
                                            
            else
            
                rental_management_costs=&
                    (model_average_rent(1,1-two_rental_markets)-housing_price(time_period)*&
                    (risk_free_rate(time_period)+housing_depreciation_rate+property_tax(time_period)+&
                    property_tax_on_rental_housing)/(1+risk_free_rate(time_period)))/&
                    ((sum(total_rental_demand(time_period,:))**(disutility_of_landlords-1))*disutility_of_landlords)
                             
            end if
                        
        elseif (is_counterfactual==1) then
        
            if (infinite_elasticity_of_rental_supply==1) then
            
                rent(time_period,1-two_rental_markets)=max(rental_management_costs_calibrated+&
                    housing_price(time_period)-housing_price(time_period-solve_transition_dynamics)*&
                    (1-housing_depreciation_rate-property_tax(time_period-solve_transition_dynamics))/&
                    (1+risk_free_rate(time_period)),0.d0)
            
            else
                
!                    model_average_rent(time_period,1)=&
!                        weight_on_housing_price*&
!                        (disutility_of_landlords*rental_management_costs_calibrated*&
!                        sum(total_rental_demand(time_period,:))**&
!                        (disutility_of_landlords-1)+&
!                        housing_price(time_period)-(housing_price&
!                        (min(time_period+solve_transition_dynamics*1,transition_length))*&
!                        (1-housing_depreciation_rate-&
!                        property_tax(min(time_period+&
!                        solve_transition_dynamics*1,transition_length))-&
!                        property_tax_on_rental_housing)/&
!                        (1+risk_free_rate(time_period))))+&
!                        (1-weight_on_housing_price)*&
!                        model_average_rent(time_period,1)
                                      
            end if
                                                                
        end if
        
        total_rental_company_profits(time_period)=0
                
        if (infinite_elasticity_of_rental_supply==1) then
        
        
        else
        
            do i=1-two_rental_markets,1
        
                total_rental_company_profits(time_period)=total_rental_company_profits(time_period)+&
                    total_rental_demand(time_period,i)*model_average_rent(time_period,i)
                    
            end do
                            
            total_rental_company_profits(time_period)=total_rental_company_profits(time_period)-&
                (housing_price(time_period)*sum(total_rental_demand(time_period,:))-&
                housing_price(time_period-solve_transition_dynamics)*&
                sum(total_rental_demand(time_period-solve_transition_dynamics,:))*&
                (1-property_tax(time_period-solve_transition_dynamics)-housing_depreciation_rate)+&
                (is_counterfactual*rental_management_costs_calibrated+&
                (1-is_counterfactual)*rental_management_costs)*&
                sum(total_rental_demand(time_period,:))**disutility_of_landlords)
        
        end if
                             
        if (solve_transition_dynamics==0) then
                        
            alpha_1=1/(1+risk_free_rate(time_period))
            
            total_profits(time_period)=total_constrcution_profits(time_period)/(1-alpha_1)+&
                total_rental_company_profits(time_period)/(1-alpha_1)
                            
        end if
        
!        write(*,*) "total_profits(time_period)=", total_profits(time_period)
!        write(*,*) "total_constrcution_profits(time_period)/(1-alpha_1)=", total_constrcution_profits(time_period)/(1-alpha_1)
!        write(*,*) "total_rental_company_profits(time_period)/(1-alpha_1)=", total_rental_company_profits(time_period)/(1-alpha_1)
!        write(*,*) "risk_free_rate(time_period)=", risk_free_rate(time_period)
                                   
        if (loop_count>=0) then
                                
            housing_size(0)=smallest_house_size
                    
            housing_size(housing_status_grid_size-1)=largest_house_size
            
            if (housing_status_grid_size>2) then
                                                   
                do i=1,housing_status_grid_size-1
        
                    housing_status_grid_distance(i)=&
                        i*((housing_size(housing_status_grid_size-1)-housing_size(0))**&
                        (real(1)/housing_status_grid_expander))/(real(housing_status_grid_size)-real(1))
    
                end do
                
                housing_size=housing_size(0)+housing_status_grid_distance**(housing_status_grid_expander)
                                
            end if
            
            if ((include_moving_back==1) .AND. (include_homelessness==1)) then
            
                housing_size(lowest_housing_grid_point)=size_homeless*housing_size(0)
                housing_size(second_lowest_housing_grid_point)=size_move_back*housing_size(0)
            
            end if
            
            if ((include_moving_back==0) .AND. (include_homelessness==1)) then
            
                housing_size(lowest_housing_grid_point)=size_homeless*housing_size(0)
            
            end if
            
            open(unit=3,file="housing_status_grid_points"//trim(file_number)//".txt",action="write",status="replace")
                do i=lowest_housing_grid_point,housing_status_grid_size-1
                    write(3,*) housing_size(i)
                end do
            close(3)
        
        end if
        
        if (solve_transition_dynamics==0) then
        
            write(*,*) "size_of_smallest_house_hhld_can_buy=", size_of_smallest_house_hhld_can_buy
            
        end if
                
        if ((loop_count<1) .AND. (solve_transition_dynamics==0)) then
        
            write(*,*) "housing_size(index_of_smallest_house_hhld_can_buy)=", & 
                housing_size(index_of_smallest_house_hhld_can_buy)
            write(*,*) "index_of_smallest_house_hhld_can_buy=", index_of_smallest_house_hhld_can_buy
            write(*,*) "smallest_house_size=", smallest_house_size
            write(*,*) "housing_status_grid_expander=", housing_status_grid_expander
        
        end if
        
        do housing_status_index=lowest_housing_grid_point,(housing_status_grid_size-1)
                    
            do years_holding_onto_asset_index=1,max_tracking+1
            
                years_holding_onto_asset_index_to_use=years_holding_onto_asset_index
            
                if (random_assignment==1) then
                
                    years_holding_onto_asset_index_to_use=min(max_years_random,years_holding_onto_asset_index)
                    
                end if
            
                do rental_market_index=1-two_rental_markets,1
                                        
                    housing_status_utility_function(housing_status_index,0,&
                        years_holding_onto_asset_index,rental_market_index)=&
                        (housing_size(housing_status_index)/&
                        ((1+rental_market_index*include_rent_decrease*&
                        include_rental_depreciation*deterioration_controlled_units)**&
                        (years_holding_onto_asset_index_to_use-1)))**&
                        ((1-risk_aversion_housing)*relative_share_of_housing)
                                                                            
                end do
                    
                housing_status_utility_function(housing_status_index,1,years_holding_onto_asset_index,2)=&
                    (housing_size(housing_status_index)*(1+advantage_to_owning))**&
                    ((1-risk_aversion_housing)*relative_share_of_housing)
                                                        
                if (loop_count<1) then
            
                    do i=0,1
                                    
!                        write(*,*) "utility from housing size", housing_size(housing_status_index), " gives utility of ", &
!                            housing_status_utility_function(housing_status_index,i,years_holding_onto_asset_index,1)
!                        write(*,*) "housing_status_index=", housing_status_index
!                        write(*,*) "ownership_status_index=", i
                    
                    end do
            
                end if
                                
            end do
            
        end do
                    
    end subroutine
    
    subroutine initialize_variables_and_grids()
    
        use Global_Vars
        
        implicit none
        
        real(8) :: persistence_fixed_employment_newborn=0.5
        real(8) :: sd_fixed_employment_newborn=0.309 ! 0.309
        real(8) :: persistence_employment_surviving=0.9 ! 0.4489**(real(1)/real(5)) (Ours but not as good since it is personal income) ! 0.9 (GKKOC)
        real(8) :: sd_employment_surviving=0.14
        real(8) :: sd_epsilon_log_employment_productivity_surviving=0.42 ! from Census personal=0.71, household=0.32
        real(8) :: s_deviation_top=3
        real(8) :: s_deviation_bottom=3
        real(8), dimension(life_span) :: sum_1
               
        if (loop_count<1) then
        
            if (use_GKKOC_process==1) then
                
                call GKKOC_intergenerational_employment_process(1,sd_epsilon_log_employment_productivity_surviving)
                
            else
            
                call KKK_process(persistence_fixed_employment_newborn,sd_fixed_employment_newborn,&
                    persistence_employment_surviving,sd_employment_surviving,s_deviation_top,&
                    s_deviation_bottom,employment_grid,unconditional_employment_probability,&
                    employment_process)
                                                
                call get_probabilities_KKK_process(persistence_fixed_employment_newborn,&
                    sd_fixed_employment_newborn,persistence_employment_surviving,&
                    sd_employment_surviving,employment_grid,unconditional_employment_probability,&
                    employment_process)
                 
                do j=1,life_span
                 
                    sum_1(j)=sum(employment_process(max(include_homelessness,0),j,1,2:employment_grid_size))
                    
                end do
                
                do j=1,life_span
                    
                    employment_process(max(include_homelessness,0),j,1,1)=&
                        employment_process(max(include_homelessness,0),j,1,1)*1.2
                    
                end do
                
                do j=1,life_span
                    
                    do i=2,employment_grid_size                
                    
                        employment_process(max(include_homelessness,0),j,1,i)=&
                            (1-employment_process(max(include_homelessness,0),j,1,1))*&
                            (employment_process(max(include_homelessness,0),j,1,i)/sum_1(j))
                    
                    end do
                    
                end do
                                    
                employment_grid=exp(employment_grid)
                
                do age_index=(life_span-retirement_span),life_span
        
                    do employment_index=1,employment_grid_size
                    
                        do future_employment_index=1,employment_grid_size
                        
                            if (future_employment_index==employment_index) then
                            
                                employment_process(0:include_homelessness,age_index,employment_index,future_employment_index)=1
                            
                            else
                            
                                employment_process(0:include_homelessness,age_index,employment_index,future_employment_index)=0
                            
                            end if
                            
                        end do
                        
                    end do
                    
                end do
                
                call set_deterministic_efficiency()
                call set_rent_shocks()
                
!                write(*,*) "employment_grid=", employment_grid
!                write(*,*) "unconditional_employment_probability=", unconditional_employment_probability
!                write(*,*) "employment_process=", employment_process(10,1,:)
!                write(*,*) "employment_grid=", employment_grid(1,:)
!                write(*,*) "employment_grid=", employment_grid(life_span-retirement_span,:)
            
            end if
            
        end if
                        
        do i=1,life_span
        
            scaled_household_size(i)=(sqrt(actual_household_size(i)))**risk_aversion_consumption
            
            if (loop_count<1) then
            
!                write(*,*) "scaled_household_size(i)=", scaled_household_size(i)
                
            end if
            
            survival_probability(i)=1
        
            do j=(i+1)-(1),min(i,life_span)
        
                survival_probability(i)=survival_probability(i)*actual_survival_probability(j)
                
            end do
                
        end do
        
        if (life_span<=60) then
        
            survival_probability(life_span)=0
            
            if (loop_count==0) then
            
!                write(*,*) "survival_probability=", survival_probability
                
            end if
        
        end if            
        
        if (have_probablistic_death==0) then
        
            survival_probability=1
            survival_probability(life_span)=0
            
            if (loop_count==0) then
            
!                write(*,*) "survival_probability=", survival_probability
                
            end if
            
        end if
                
        if (loop_count<1) then
                                    
            do age_index=1,life_span
                
                if (solve_transition_dynamics==0) then
            
!                    write(*,*) "survival_probability(",age_index,")=", survival_probability(age_index)
                    
                end if
                    
            end do

        end if
                
        cohort_population(1)=1
        
        total_population=cohort_population(1)
            
        do i=2,life_span
        
            cohort_population(i)=cohort_population(i-1)*survival_probability(i-1)
            
            total_population=total_population+cohort_population(i)
            
        end do
        
        if ((solve_transition_dynamics==0) .AND. &
           (loop_count<1)) then
                
            write(*,*) "total population=", total_population
        
        end if
         
        stopping_rule_wealth_dist(1)=0.01
        stopping_rule_wealth_dist(2)=0.1
        stopping_rule_wealth_dist(3)=0.2
        stopping_rule_wealth_dist(4)=0.5
        
    end subroutine  
    
    subroutine KKK_process(rho_fixed,sigma_fixed,rho_shock,sigma_shock,s_deviation_top,s_deviation_bottom,&
        z_grid_by_age,unconditional_probability_distribution,transition_prob_by_age)
        
        use Global_Vars
        
        implicit none
        
        real(8), intent(in) :: s_deviation_top,s_deviation_bottom
        real(8), intent(in) :: rho_fixed,rho_shock,sigma_fixed,sigma_shock
        real(8) :: variance_1,variance_eta,variance_fix
        real(8), dimension(life_span-retirement_span) :: variance_by_age
        real(8), dimension(life_span,employment_grid_size), intent(out) :: z_grid_by_age
        real(8), dimension(0:include_homelessness,life_span,&
            employment_grid_size,employment_grid_size), intent(out) :: transition_prob_by_age
        real(8), dimension(life_span-retirement_span,employment_grid_size) :: probability_distribution
        real(8), dimension(employment_grid_size), intent(out) :: unconditional_probability_distribution
        real(8), dimension(life_span-retirement_span) :: width
        real(8) :: temp_upper,temp_lower
                
        variance_1=sigma_fixed**2
        variance_eta=sigma_shock**2
        variance_fix=variance_1/(1-rho_fixed**(2))
        
        variance_by_age(1)=variance_eta+variance_fix
        
        do i=2,life_span-retirement_span
        
            variance_by_age(i)=(rho_shock**2)*variance_by_age(i-1)+variance_eta
            
        end do
                
        do i=1,life_span-retirement_span
        
            z_grid_by_age(i,1)=-s_deviation_bottom*sqrt(variance_by_age(i))
            z_grid_by_age(i,employment_grid_size)=s_deviation_top*sqrt(variance_by_age(i))
            
            width(i)=(z_grid_by_age(i,employment_grid_size)-z_grid_by_age(i,1))/(employment_grid_size-1)
            
            do j=2,employment_grid_size-1
            
                z_grid_by_age(i,j)=z_grid_by_age(i,1)+width(i)*(j-1)
                
            end do
                        
        end do
        
        do i=1,life_span-retirement_span-1
        
            do j=1,employment_grid_size
            
                transition_prob_by_age(0,i,j,1)=&
                    (1+erf(((z_grid_by_age(i+1,1)+0.5*width(i+1)-rho_shock*z_grid_by_age(i,j))/sqrt(variance_eta))/sqrt(2.d0)))/2
                
                do k=2,employment_grid_size-1
               
                    temp_upper=(1+erf(((z_grid_by_age(i+1,k)+0.5*width(i+1)-rho_shock*z_grid_by_age(i,j))/&
                        sqrt(variance_eta))/sqrt(2.d0)))/2                    
                    temp_lower=(1+erf(((z_grid_by_age(i+1,k)-0.5*width(i+1)-rho_shock*z_grid_by_age(i,j))/&
                        sqrt(variance_eta))/sqrt(2.d0)))/2
                    
                    transition_prob_by_age(0,i,j,k)=temp_upper-temp_lower
                    
                end do
                
                transition_prob_by_age(0,i,j,employment_grid_size)=&
                    1-(1+erf(((z_grid_by_age(i+1,employment_grid_size)-0.5*width(i+1)-rho_shock*z_grid_by_age(i,j))/&
                    sqrt(variance_eta))/sqrt(2.d0)))/2
                
                transition_prob_by_age(0,i,j,:)=transition_prob_by_age(0,i,j,:)/sum(transition_prob_by_age(0,i,j,:))
                    
            end do
            
        end do
                
        transition_prob_by_age(0,life_span-retirement_span,:,:)=0
        
        do i=1,employment_grid_size
                
            transition_prob_by_age(0,life_span-retirement_span,i,i)=1
            
        end do
        
        do j=life_span-retirement_span+1,life_span
        
            transition_prob_by_age(0,j,:,:)=transition_prob_by_age(0,life_span-retirement_span,:,:)
            
        end do
                
        probability_distribution(1,1)=(1+erf(((z_grid_by_age(1,1)+0.5*width(1))/sqrt(variance_by_age(1)))/sqrt(2.d0)))/2
        
        do j=2,employment_grid_size-1
        
            temp_upper=(1+erf(((z_grid_by_age(1,j)+0.5*width(1))/sqrt(variance_by_age(1)))/sqrt(2.d0)))/2
            temp_lower=(1+erf(((z_grid_by_age(1,j)-0.5*width(1))/sqrt(variance_by_age(1)))/sqrt(2.d0)))/2
            probability_distribution(1,j)=temp_upper-temp_lower
            
        end do
        
        transition_prob_by_age(include_homelessness,:,:,:)=transition_prob_by_age(0,:,:,:)
        
        probability_distribution(1,employment_grid_size)=&
            1-(1+erf(((z_grid_by_age(1,employment_grid_size)-&
            0.5*width(1))/sqrt(variance_by_age(1)))/sqrt(2.d0)))/2
            
        probability_distribution(1,:)=probability_distribution(1,:)/sum(probability_distribution(1,:))
        
        unconditional_employment_probability=probability_distribution(1,:)
                
    end subroutine
    
    subroutine get_probabilities_KKK_process(rho_fixed,sigma_fixed,rho_shock,sigma_shock,&
        z_grid_by_age,unconditional_probability_distribution,transition_prob_by_age)
        
        use Global_Vars
        
        implicit none
        
        real(8), intent(in) :: rho_fixed,rho_shock,sigma_fixed,sigma_shock
        real(8) :: variance_1,variance_eta,variance_fix
        real(8), dimension(life_span-retirement_span) :: variance_by_age
        real(8), dimension(life_span,employment_grid_size), intent(in) :: z_grid_by_age
        real(8), dimension(0:include_homelessness,life_span,&
            employment_grid_size,employment_grid_size), intent(out) :: transition_prob_by_age
        real(8), dimension(life_span-retirement_span,employment_grid_size) :: probability_distribution
        real(8), dimension(employment_grid_size), intent(out) :: unconditional_probability_distribution
        real(8), dimension(life_span-retirement_span) :: width
        real(8) :: temp_upper,temp_lower
                
        variance_1=sigma_fixed**2
        variance_eta=sigma_shock**2
        variance_fix=variance_1/(1-rho_fixed**(2))
        
        variance_by_age(1)=variance_eta+variance_fix
        
        do i=2,life_span-retirement_span
        
            variance_by_age(i)=(rho_shock**2)*variance_by_age(i-1)+variance_eta
            
        end do
                        
        do i=1,life_span-retirement_span-1
        
            do j=1,employment_grid_size
            
                transition_prob_by_age(0,i,j,1)=&
                    (1+erf(((z_grid_by_age(i+1,1)+0.5*width(i+1)-rho_shock*z_grid_by_age(i,j))/sqrt(variance_eta))/sqrt(2.d0)))/2
                
                do k=2,employment_grid_size-1
               
                    temp_upper=(1+erf(((z_grid_by_age(i+1,k)+0.5*width(i+1)-rho_shock*z_grid_by_age(i,j))/&
                        sqrt(variance_eta))/sqrt(2.d0)))/2                    
                    temp_lower=(1+erf(((z_grid_by_age(i+1,k)-0.5*width(i+1)-rho_shock*z_grid_by_age(i,j))/&
                        sqrt(variance_eta))/sqrt(2.d0)))/2
                    
                    transition_prob_by_age(0,i,j,k)=temp_upper-temp_lower
                    
                end do
                
                transition_prob_by_age(0,i,j,employment_grid_size)=&
                    1-(1+erf(((z_grid_by_age(i+1,employment_grid_size)-0.5*width(i+1)-rho_shock*z_grid_by_age(i,j))/&
                    sqrt(variance_eta))/sqrt(2.d0)))/2
                
                transition_prob_by_age(0,i,j,:)=transition_prob_by_age(0,i,j,:)/sum(transition_prob_by_age(0,i,j,:))
                    
            end do
            
        end do
                
        transition_prob_by_age(0,life_span-retirement_span,:,:)=0
        
        do i=1,employment_grid_size
                
            transition_prob_by_age(0,life_span-retirement_span,i,i)=1
            
        end do
        
        do j=life_span-retirement_span+1,life_span
        
            transition_prob_by_age(0,j,:,:)=&
                transition_prob_by_age(0,life_span-retirement_span,:,:)
            
        end do
        
        transition_prob_by_age(include_homelessness,:,:,:)=transition_prob_by_age(0,:,:,:)
                
        probability_distribution(1,1)=(1+erf(((z_grid_by_age(1,1)+0.5*width(1))/sqrt(variance_by_age(1)))/sqrt(2.d0)))/2
        
        do j=2,employment_grid_size-1
        
            temp_upper=(1+erf(((z_grid_by_age(1,j)+0.5*width(1))/sqrt(variance_by_age(1)))/sqrt(2.d0)))/2
            temp_lower=(1+erf(((z_grid_by_age(1,j)-0.5*width(1))/sqrt(variance_by_age(1)))/sqrt(2.d0)))/2
            probability_distribution(1,j)=temp_upper-temp_lower
            
        end do
        
        probability_distribution(1,employment_grid_size)=&
            1-(1+erf(((z_grid_by_age(1,employment_grid_size)-&
            0.5*width(1))/sqrt(variance_by_age(1)))/sqrt(2.d0)))/2
            
        probability_distribution(1,:)=probability_distribution(1,:)/sum(probability_distribution(1,:))
        
        unconditional_employment_probability=probability_distribution(1,:)
                
    end subroutine
        
    subroutine GKKOC_intergenerational_employment_process(time_period,sd_epsilon_log_employment_productivity_surviving)
    
        use Global_Vars
        
        implicit none
        
        real(8) :: persistency_log_employment_productivity=0.199 
        real(8) :: sd_epsilon_log_employment_productivity=1.09 
        real(8) :: persistency_log_employment_productivity_surviving=0.4489**(real(1)/real(5))  ! personal=0.4489, household=0.62
        real(8) :: sd_log_employment_productivity,log_employment_productivity_lower_bound,&
            log_employment_productivity_upper_bound,sd_epsilon_log_employment_productivity_surviving
        real(8) :: log_employment_productivity_deviation_down=3,log_employment_productivity_deviation_up=3
        integer :: log_employment_productivity_index,future_log_employment_productivity_index
        integer, parameter :: unconditional_probability_at_birth=1
        integer :: time_period
        real(8), dimension(employment_grid_size) :: sum_of_distribution_employment_all
                 
        if (loop_count<1) then
        
            write(*,*) "persistency_log_employment_productivity_surviving=", persistency_log_employment_productivity_surviving
            write(*,*) "sd_epsilon_log_employment_productivity_surviving=", sd_epsilon_log_employment_productivity_surviving
            
        end if
        
        sd_log_employment_productivity=sd_epsilon_log_employment_productivity/&
            sqrt(1-(persistency_log_employment_productivity)**2)
    
        log_employment_productivity_lower_bound=-log_employment_productivity_deviation_down*sd_log_employment_productivity
        log_employment_productivity_upper_bound=+log_employment_productivity_deviation_up*sd_log_employment_productivity
        
        do log_employment_productivity_index=1,employment_grid_size
        
            log_employment_productivity_grid(1,log_employment_productivity_index)=&
                log_employment_productivity_lower_bound+((log_employment_productivity_upper_bound-&
                log_employment_productivity_lower_bound)/(employment_grid_size-1))*(log_employment_productivity_index-1)
        
        end do
        
        do log_employment_productivity_index=1,employment_grid_size-1
        
            log_employment_productivity_grid_mid_point(1,log_employment_productivity_index)=&
                (log_employment_productivity_grid(1,log_employment_productivity_index+1)+&
                log_employment_productivity_grid(1,log_employment_productivity_index))/2
        
        end do
        
        if (unconditional_probability_at_birth==0) then
        
            do log_employment_productivity_index=1,employment_grid_size
            
                do future_log_employment_productivity_index=2,employment_grid_size-1
                
                    cut_off_1=(log_employment_productivity_grid_mid_point&
                        (1,future_log_employment_productivity_index)-persistency_log_employment_productivity*&
                        log_employment_productivity_grid(1,log_employment_productivity_index))/&
                        sd_epsilon_log_employment_productivity

                    cut_off_2=(log_employment_productivity_grid_mid_point&
                        (1,future_log_employment_productivity_index-1)-persistency_log_employment_productivity*&
                        log_employment_productivity_grid(1,log_employment_productivity_index))/&
                        sd_epsilon_log_employment_productivity
               
                    intergenerational_employment_process(log_employment_productivity_index,&
                        future_log_employment_productivity_index)=&
                        (1+ERF(cut_off_1/SQRT(2.d0)))/2-(1+ERF(cut_off_2/SQRT(2.d0)))/2
                            
                end do
            
            end do
                
            do log_employment_productivity_index=1,employment_grid_size
            
                cut_off_1=(log_employment_productivity_grid_mid_point(1,1)-&
                    persistency_log_employment_productivity*log_employment_productivity_grid&
                    (1,log_employment_productivity_index))/sd_epsilon_log_employment_productivity
        
                cut_off_2=(log_employment_productivity_grid_mid_point&
                    (1,employment_grid_size-1)-persistency_log_employment_productivity*&
                    log_employment_productivity_grid(1,log_employment_productivity_index))/&
                    sd_epsilon_log_employment_productivity

                intergenerational_employment_process(log_employment_productivity_index,1)=(1+ERF(cut_off_1/SQRT(2.d0)))/2
                    
                intergenerational_employment_process(log_employment_productivity_index,employment_grid_size)=&
                    1-(1+ERF(cut_off_2/SQRT(2.d0)))/2
                    
            end do
            
        end if
        
        if (loop_count>1) then
                
            sum_of_distribution=0
                    
            do i=1-two_rental_markets,2
                    
                sum_of_distribution=sum_of_distribution+&
                    sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
                        
            end do
        
            sum_of_distribution_employment_all(:)=0
            
            do employment_index=1,employment_grid_size
            
                do i=1-two_rental_markets,2
                                                
                    sum_of_distribution_employment_all(employment_index)=sum_of_distribution_employment_all(employment_index)+&
                        sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,employment_index,:,:,:))
                                            
                end do
                
            end do
            
            sum_of_distribution_employment_all(:)=&
                sum_of_distribution_employment_all(:)/sum_of_distribution
        
        end if
        
        if ((unconditional_probability_at_birth==1) .AND. (use_old_results==0)) then
        
            if (loop_count>1) then
            
                do log_employment_productivity_index=1,employment_grid_size-1
                
                    intergenerational_employment_process(:,log_employment_productivity_index)=&
                        sum_of_distribution_employment_all(log_employment_productivity_index)
                
                end do
            
            else
            
                intergenerational_employment_process(:,1)=1.0999904411638338E-002
                intergenerational_employment_process(:,2)=7.5309138226917191E-002
                intergenerational_employment_process(:,3)=0.23860571736643943
                intergenerational_employment_process(:,4)=0.35017047999010775
                intergenerational_employment_process(:,5)=0.23860571736642602
                intergenerational_employment_process(:,6)=7.5309138226907962E-002
                         
            end if
            
            intergenerational_employment_process(:,employment_grid_size)=&
                1-sum(intergenerational_employment_process(1,1:employment_grid_size-1))
            
        end if        
            
        if (loop_count<100) then
        
            if (unconditional_probability_at_birth==1) then
                    
                write(*,*) "intergenerational_employment_process(",1,")=", intergenerational_employment_process(1,:)
                write(*,*) "sum(intergenerational_employment_process(",1,":)=", &
                    sum(intergenerational_employment_process(1,:))
                            
            else
            
                if (loop_count<1) then
            
                    do log_employment_productivity_index=1,employment_grid_size
                
                        write(*,*) "intergenerational_employment_process(",log_employment_productivity_index,")=", &
                            intergenerational_employment_process(log_employment_productivity_index,:)
                        
                    end do
                    
                    do log_employment_productivity_index=1,employment_grid_size
                                
                        write(*,*) "sum(intergenerational_employment_process(",log_employment_productivity_index,":)=", &
                            sum(intergenerational_employment_process(log_employment_productivity_index,:))
                                        
                    end do
                    
                end if
                
            end if
                
        end if
        
        sd_log_employment_productivity=sd_epsilon_log_employment_productivity_surviving/&
            sqrt(1-(persistency_log_employment_productivity_surviving)**2)
    
        log_employment_productivity_lower_bound=-log_employment_productivity_deviation_down*sd_log_employment_productivity
        log_employment_productivity_upper_bound=+log_employment_productivity_deviation_up*sd_log_employment_productivity
                  
        do log_employment_productivity_index=1,employment_grid_size
        
            log_employment_productivity_grid(1,log_employment_productivity_index)=&
                log_employment_productivity_lower_bound+((log_employment_productivity_upper_bound-&
                log_employment_productivity_lower_bound)/(employment_grid_size-1))*(log_employment_productivity_index-1)
        
        end do
        
        do log_employment_productivity_index=1,employment_grid_size-1
        
            log_employment_productivity_grid_mid_point(1,log_employment_productivity_index)=&
                (log_employment_productivity_grid(1,log_employment_productivity_index+1)+&
                log_employment_productivity_grid(1,log_employment_productivity_index))/2
        
        end do
        
        do log_employment_productivity_index=1,employment_grid_size
        
            do future_log_employment_productivity_index=2,employment_grid_size-1
            
                cut_off_1=(log_employment_productivity_grid_mid_point&
                    (1,future_log_employment_productivity_index)-persistency_log_employment_productivity_surviving*&
                    log_employment_productivity_grid(1,log_employment_productivity_index))/&
                    sd_epsilon_log_employment_productivity_surviving
                
                cut_off_2=(log_employment_productivity_grid_mid_point&
                    (1,future_log_employment_productivity_index-1)-persistency_log_employment_productivity_surviving*&
                    log_employment_productivity_grid(1,log_employment_productivity_index))/&
                    sd_epsilon_log_employment_productivity_surviving
                    
                employment_process(0,:,&
                    log_employment_productivity_index,future_log_employment_productivity_index)=&
                    (1+ERF(cut_off_1/SQRT(2.d0)))/2-(1+ERF(cut_off_2/SQRT(2.d0)))/2
                        
            end do
        
        end do
                            
        do log_employment_productivity_index=1,employment_grid_size
        
            cut_off_1=(log_employment_productivity_grid_mid_point(1,1)-&
                persistency_log_employment_productivity_surviving*&
                log_employment_productivity_grid(1,log_employment_productivity_index))/&
                sd_epsilon_log_employment_productivity_surviving
        
            cut_off_2=(log_employment_productivity_grid_mid_point&
                (1,employment_grid_size-1)-persistency_log_employment_productivity_surviving*&
                log_employment_productivity_grid(1,log_employment_productivity_index))/&
                sd_epsilon_log_employment_productivity_surviving
     
            employment_process(0,:,log_employment_productivity_index,1)=&
                (1+ERF(cut_off_1/SQRT(2.d0)))/2
                
            employment_process(0,:,log_employment_productivity_index,employment_grid_size)=&
                1-(1+ERF(cut_off_2/SQRT(2.d0)))/2
                
        end do
                    
        if (loop_count<1) then
            
            do log_employment_productivity_index=1,employment_grid_size
                        
                write(*,*) "employment_process(",1,log_employment_productivity_index,")=", &
                    employment_process(0,1,log_employment_productivity_index,:)
                            
            end do
            
            do log_employment_productivity_index=1,employment_grid_size
                        
                write(*,*) "sum(employment_process(",1,log_employment_productivity_index,":)=", &
                    sum(employment_process(0,1,log_employment_productivity_index,:))
                                
            end do
                
        end if
                
        do age_index=(life_span-retirement_span),life_span
        
            do employment_index=1,employment_grid_size
            
                do future_employment_index=1,employment_grid_size
                
                    if (future_employment_index==employment_index) then
                    
                        employment_process(0,age_index,employment_index,future_employment_index)=1
                    
                    else
                    
                        employment_process(0,age_index,employment_index,future_employment_index)=0
                    
                    end if
                    
                end do
                
            end do
            
        end do
            
        log_employment_productivity_grid=log_employment_productivity_grid
        
        do age_index=1,life_span
            
            employment_grid(age_index,:)=exp(log_employment_productivity_grid(1,:))
            
        end do
        
        do age_index=1,life_span-retirement_span
        
            do employment_index=1,employment_grid_size
                
                do future_employment_index=1,employment_grid_size
                
                    if (future_employment_index==employment_index) then
                    
                        employment_process(0,age_index,employment_index,future_employment_index)=1
                    
                    else
                    
                        employment_process(0,age_index,employment_index,future_employment_index)=0
                    
                    end if
                    
                end do
                
            end do
            
        end do
        
        employment_process(include_homelessness,:,:,:)=employment_process(0,:,:,:)
                    
        if (loop_count<1) then
                
            do i=1,employment_grid_size
    
                write(*,*) "employment_index", i, "=", employment_grid(1,i)
        
            end do
               
        end if        
        
        if (loop_count<1) then
        
            write(*,*) "sd_epsilon_log_employment_productivity_surviving=",sd_epsilon_log_employment_productivity_surviving
            write(*,*) "persistency_log_employment_productivity_surviving=", persistency_log_employment_productivity_surviving
            write(*,*) "sd_epsilon_log_employment_productivity=",sd_epsilon_log_employment_productivity
            write(*,*) "persistency_log_employment_productivity=", persistency_log_employment_productivity
                
        end if
        
        call set_deterministic_efficiency()
        call set_rent_shocks()
        
    end subroutine
    
    subroutine set_deterministic_efficiency()
    
        use Global_Vars
        
        implicit none
        
        labour_deterministic_efficiency(1)=1
                                                                        
        do age_index=2,(life_span-retirement_span)
        
            if (age_index<=life_span-retirement_span) then
        
                labour_deterministic_efficiency(age_index)=&
                    life_cycle_component_a+life_cycle_component_b*age_index+life_cycle_component_c*(age_index)**2
                          
            end if
                
            if (loop_count<1) then
            
                if (solve_transition_dynamics==0) then
                            
!                    write(*,*) "labour efficiency=", labour_deterministic_efficiency(age_index)
!                    write(*,*) "age_index=", age_index
                    
                end if
                
            end if
            
        end do
            
        labour_deterministic_efficiency((life_span-retirement_span)+1:life_span)=0
        
    end subroutine
        
    subroutine set_rent_shocks()
    
        use Global_Vars
        
        implicit none
        
        if (include_shocks_to_free_market_rent==1) then
        
            if (loop_count<1) then
        
                probability_of_rent_shock(1)=0
                probability_of_rent_shock(2)=0.1600
                probability_of_rent_shock(3)=0.1200
                probability_of_rent_shock(5)=0.0800
                probability_of_rent_shock(6)=0.1600
                probability_of_rent_shock(7)=0.0400
                
                probability_of_rent_shock(4)=1-(sum(probability_of_rent_shock(1:3))+sum(probability_of_rent_shock(5:7)))
                
                shock_to_rent(1)=0
                shock_to_rent(2)=-0.1227
                shock_to_rent(3)=-0.0607
                shock_to_rent(5)=0.0624
                shock_to_rent(6)=0.1284
                shock_to_rent(7)=0.2589
                
                shock_to_rent(4)=0
                
                free_market_rent_increase_due_to_shock=0
                
                do i=1,rent_shock_grid_size
                
                    free_market_rent_increase_due_to_shock=free_market_rent_increase_due_to_shock+&
                        shock_to_rent(i)*probability_of_rent_shock(i)     
                
                end do
                
            end if
                        
        else
        
            probability_of_rent_shock(1)=1
            shock_to_rent(1)=free_market_rent_increase_due_to_shock 
            
        end if
        
        if (loop_count<1) then
        
            write(*,*) "probability_of_rent_shock=", probability_of_rent_shock
            write(*,*) "shock_to_rent=", shock_to_rent
            write(*,*) "rent_shock_grid_size=", rent_shock_grid_size
        
        end if
                
    end subroutine
    
    subroutine set_smallest_house_size()
    
        use Global_Vars
        
        implicit none
        
        real(8) :: net_income_lowest
        real(8), dimension(1+benchmark_has_two_rental_markets) :: rent_upload
                
        net_income_lowest=labor_income_tax_rate*employment_grid(1,1)**(curvature_of_labor_income_taxes)
        
        if (include_shocks_to_free_market_rent==1) then
        
            smallest_house_size=net_income_lowest*smallest_rental_relative_to_lowest_income/&
                (free_market_rent_to_house_price*calibrated_stationary_housing_price*&
                (1+shock_to_rent(rent_shock_grid_size)))
                
            write(*,*) "smallest_house_size*free_market_rent_to_house_price*&
                housing_price(1)*(1+shock_to_rent(7)/net_income_lowest=", &
                (smallest_house_size*free_market_rent_to_house_price*calibrated_stationary_housing_price*&
                (1+shock_to_rent(rent_shock_grid_size)))/net_income_lowest
                
            write(*,*) "smallest_house_size=", smallest_house_size
                
        else
            
            if (re_do_simulation_with_fixed_parameters==0) then
            
                if (loop_count<0) then
            
                    smallest_house_size=net_income_lowest*smallest_rental_relative_to_lowest_income/&
                        (free_market_rent_to_house_price*calibrated_stationary_housing_price*&
                        (1+shock_to_rent(rent_shock_grid_size)))
                        
                    write(*,*) "smallest_house_size*free_market_rent_to_house_price*&
                        housing_price(1)/net_income_lowest=", &
                        (smallest_house_size*0.78045899780246208)/net_income_lowest   
                        
                else
                
                    if (housing_status_grid_size>8) then
                
                        smallest_house_size=net_income_lowest*smallest_rental_relative_to_lowest_income/rent(1,1)
                            
                        write(*,*) "smallest_house_size=", smallest_house_size
                        write(*,*) "smallest_house_size*rent(1)/net_income_lowest=", &
                            (smallest_house_size*rent(1,1))/net_income_lowest    
                            
                    else
                    
                        smallest_house_size=net_income_lowest*smallest_rental_relative_to_lowest_income/rent(1,1-two_rental_markets)
                            
                        write(*,*) "smallest_house_size=", smallest_house_size
                        write(*,*) "smallest_house_size*rent(1-two_rental_markets)/net_income_lowest=", &
                            (smallest_house_size*rent(1,1-two_rental_markets))/net_income_lowest   
                    
                    end if
                
                end if
                
            else
            
                if (is_counterfactual==0) then
                
                    if (housing_status_grid_size>8) then
            
                        smallest_house_size=net_income_lowest*smallest_rental_relative_to_lowest_income/rent(1,1)
                        
                        write(*,*) "smallest_house_size=", smallest_house_size
                        write(*,*) "smallest_house_size*rent(1,1)/net_income_lowest=", &
                            (smallest_house_size*rent(1,1))/net_income_lowest   
                            
                    else
                    
                        smallest_house_size=net_income_lowest*smallest_rental_relative_to_lowest_income/rent(1,1-two_rental_markets)
                        
                        write(*,*) "smallest_house_size=", smallest_house_size
                        write(*,*) "smallest_house_size*rent(1,1-two_rental_markets)/net_income_lowest=", &
                            (smallest_house_size*rent(1,1-two_rental_markets))/net_income_lowest   
                            
                    end if
                        
                else
                
                    if (housing_status_grid_size>8) then

                        smallest_house_size=net_income_lowest*smallest_rental_relative_to_lowest_income/rent_calibrated(1)
                        
                        write(*,*) "smallest_house_size=", smallest_house_size
                        write(*,*) "smallest_house_size*rent(1,1)/net_income_lowest=", &
                            (smallest_house_size*rent_calibrated(1))/net_income_lowest   
                            
                    else
                    
                        open(unit=23,file="rent"//trim(old_results)//".dat",action='read')
                            read(23,'(F15.8)') rent_upload(1)
                        close(23)
                                            
                        smallest_house_size=net_income_lowest*smallest_rental_relative_to_lowest_income/rent_upload(1)
                        
                        write(*,*) "smallest_house_size=", smallest_house_size
                        write(*,*) "smallest_house_size*rent_upload(1,1)/net_income_lowest=", &
                            (smallest_house_size*rent_upload(1))/net_income_lowest
                    
                    end if
                
                end if
                    
            end if
            
        end if
        
    end subroutine
    
    SUBROUTINE rouwenhorst(rho,sigma_eps,mu_eps,n,zgrid,P)

        ! rouwenhorst.f90
        !
        ! [zgrid, P] = rouwenhorst(rho, sigma_eps, n)
        !
        ! rho is the 1st order autocorrelation
        ! sigma_eps is the standard deviation of the error term
        ! n is the number of points in the discrete approximation
        !
        ! see "Finite State Markov-chain Approximations to Highly Persistent
        ! Processes"

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: n
        DOUBLE PRECISION, INTENT(IN) :: rho,sigma_eps,mu_eps
        DOUBLE PRECISION, DIMENSION(n,n), INTENT(OUT) :: P
        DOUBLE PRECISION, DIMENSION(n), INTENT(OUT) :: zgrid

        INTEGER :: i, status
        DOUBLE PRECISION :: q, nu
        DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: P11, P12, P21, P22
        EXTERNAL :: linspace

        q = (rho+1.d0)/2.d0
        nu = SQRT((DBLE(n)-1.d0)/(1.d0-rho**2.d0)) * sigma_eps
        P = 0.d0

        P(1,1) = q
        P(1,2) = 1 - q
        P(2,1) = 1 - q
        P(2,2) = q

        DO i=2,n-1
!        WRITE(*,*) i
        ALLOCATE(P11(i+1,i+1), P12(i+1,i+1), P21(i+1,i+1), P22(i+1,i+1), STAT=status)
        
        P11 = 0.d0
        P12 = 0.d0
        P21 = 0.d0
        P22 = 0.d0
        
        P11(1:i,1:i) = P(1:i,1:i)
        P12(1:i,2:i+1) = P(1:i,1:i)
        P21(2:i+1,1:i) = P(1:i,1:i)
        P22(2:i+1,2:i+1) = P(1:i,1:i)
        
!        WRITE(*,*) 'Assigned partial matrices'   

        P(1:i+1,1:i+1) = q*P11 + (1-q)*P12 + (1-q)*P21 + q*P22

        P(2:i,:) = P(2:i,:)/2.d0

        DEALLOCATE(P11,P12,P21,P22, STAT = status)
        END DO

!        WRITE(*,*) 'About to call linspace'
        CALL linspace(mu_eps/(1.d0-rho)-nu,mu_eps/(1.d0-rho)+nu,n,zgrid)

    END SUBROUTINE
    
    SUBROUTINE linspace(d1,d2,n,grid)

        IMPLICIT NONE

        INTEGER, INTENT(IN) :: n
        DOUBLE PRECISION, INTENT(IN) :: d1, d2
        DOUBLE PRECISION, DIMENSION(n), INTENT(OUT) :: grid

        INTEGER :: indxi
        
        grid(1) = d1
        DO indxi= 0,n-2
        grid(indxi+1) = d1+(DBLE(indxi)*(d2-d1))/DBLE(n-1)
        END DO
        grid(n) = d2

    END SUBROUTINE
        
    subroutine set_SS_benefits(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        real(8), dimension(employment_grid_size) :: sum_of_distribution_employment
        
        sum_of_distribution_youngest=0
        sum_of_distribution_employment(:)=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution_youngest=sum_of_distribution_youngest+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,1,:,:,:,:,:))
                
            do employment_index=1,employment_grid_size
            
                sum_of_distribution_employment(employment_index)=sum_of_distribution_employment(employment_index)+&
                    sum(distribution_stationary(i)%vector_transition_real(time_period,1,:,employment_index,:,:,:))
            
            end do
                
        end do
                                   
        model_ss_benefits=0
                                    
        do employment_index=1,employment_grid_size
        
            probability_came_from_past_employment_index_1=0

            model_lifetime_income_conditional_on_last_period_shock(employment_index)=0
            
            probability_came_from_past_employment_index_1(employment_index)=1
    
            do age_index=(life_span-retirement_span),1,-1
            
                probability_came_from_past_employment_index_2=0
                    
                do past_employment_index_1=1,employment_grid_size
                                                                
                    model_lifetime_income_conditional_on_last_period_shock(employment_index)=&
                        model_lifetime_income_conditional_on_last_period_shock(employment_index)+&
                        probability_came_from_past_employment_index_1(past_employment_index_1)*&
                        employment_grid(age_index,past_employment_index_1)*labour_deterministic_efficiency(age_index)
                            
                    do past_employment_index_2=1,employment_grid_size
                        
                        probability_came_from_past_employment_index_2(past_employment_index_2)=&
                            probability_came_from_past_employment_index_2(past_employment_index_2)+&
                            probability_came_from_past_employment_index_1(past_employment_index_1)*&
                            employment_process(0,max(age_index-1,1),past_employment_index_2,past_employment_index_1)/&
                            sum(employment_process(0,max(age_index-1,1),:,past_employment_index_1))
                                                        
                    end do
                    
                end do
                                        
                probability_came_from_past_employment_index_1=&
                    probability_came_from_past_employment_index_2
                                            
            end do
            
            model_lifetime_income_conditional_on_last_period_shock(employment_index)=&
                model_lifetime_income_conditional_on_last_period_shock(employment_index)/&
                (life_span-retirement_span)
            
        end do
                          
        model_average_lifetime_income_conditional_on_last_period_shock=0
            
        do employment_index=1,employment_grid_size
                
            if (loop_count<1) then
                
                model_average_lifetime_income_conditional_on_last_period_shock=&
                    model_average_lifetime_income_conditional_on_last_period_shock+&
                    model_lifetime_income_conditional_on_last_period_shock(employment_index)
                    
            else
            
                model_average_lifetime_income_conditional_on_last_period_shock=&
                    model_average_lifetime_income_conditional_on_last_period_shock+&
                    model_lifetime_income_conditional_on_last_period_shock(employment_index)*&
                    sum_of_distribution_employment(employment_index)/sum_of_distribution_youngest
            
            end if
                                  
        end do
                                           
        do employment_index=1,employment_grid_size
        
            model_ss_benefits(employment_index)=&
                (0.376-0.21*log(min(model_lifetime_income_conditional_on_last_period_shock(employment_index)/&
                model_average_labour_income(time_period),1.5)))
                                               
        end do
                            
        do employment_index=1,employment_grid_size
                                            
            model_ss_benefits(employment_index)=model_ss_benefits(employment_index)*&
                model_lifetime_income_conditional_on_last_period_shock(employment_index)
                
            model_ss_benefits(employment_index)=&
                max(model_ss_benefits(employment_index),model_ss_benefits(max(employment_index-1,1)))
                                                                                                                        
        end do
            
!        write(*,*) "model_ss_benefits(:)=", model_ss_benefits(:)
                    
    end subroutine
    
    subroutine initialize_financial_grid(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        
        financial_grid(1)=-10.050318826513271
                         
        financial_grid(negative_financial_grid_size)=0.d0
        
        financial_grid_expander_negative=housing_status_grid_expander
        
        do i=2,negative_financial_grid_size-1
        
            financial_grid_distance_negative(negative_financial_grid_size+1-i)=&
                (real(i)-real(1))*((1-is_counterfactual)*housing_price(time_period)+&
                is_counterfactual*calibrated_stationary_housing_price)**&
                ((real(1)/real(financial_grid_expander_negative)))*&
                (-financial_grid(1)/((1-is_counterfactual)*housing_price(time_period)+&
                is_counterfactual*calibrated_stationary_housing_price))**&
                (real(1)/real(financial_grid_expander_negative))/&
                (real(negative_financial_grid_size)-real(1))
    
        end do
        
        financial_grid(financial_grid_size)=max_times_labour_income_saved*employment_grid(1,employment_grid_size)
            
        do i=negative_financial_grid_size+1,financial_grid_size
        
            financial_grid_distance_positive(i)=&
                (real(i)-real(negative_financial_grid_size))*&
                ((financial_grid(financial_grid_size))**&
                (real(1)/real(financial_grid_expander_positive+1)))/&
                (real(positive_financial_grid_size))
    
        end do
        
        financial_grid(2:negative_financial_grid_size)=&            
            -financial_grid_distance_negative(2:negative_financial_grid_size)**&
            (real(financial_grid_expander_negative))
            
        financial_grid(negative_financial_grid_size+1:financial_grid_size)=&
            financial_grid_distance_positive(negative_financial_grid_size+1:financial_grid_size)**&
            (real(financial_grid_expander_positive))
            
        open(unit=3,file="financial_grid_points"//trim(file_number)//".txt",action="write",status="replace")
        do i=1,financial_grid_size
            write(3,*) financial_grid(i)/employment_grid(1,employment_grid_size)
        end do
        close(3)
        open(unit=3,file="financial_grid"//trim(file_number)//".txt",action="write",status="replace")
        do i=1,financial_grid_size
            write(3,*) financial_grid(i)
        end do
        close(3)

    end subroutine
    
    subroutine compute_rental_management_costs(time_period,disutility_of_landlords_3,rental_management_costs_3)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        real(8), intent(inout) :: rental_management_costs_3
        real(8), intent(in) :: disutility_of_landlords_3
    
        rental_management_costs_3=(model_average_rent(1,1-two_rental_markets)-housing_price(time_period)*&
            (risk_free_rate(time_period)+housing_depreciation_rate+property_tax(time_period)+&
            property_tax_on_rental_housing)/(1+risk_free_rate(time_period)))/&
            ((sum(total_rental_demand(time_period,:))**(disutility_of_landlords_3-1))*disutility_of_landlords_3)
                        
    end subroutine
    
    subroutine compute_rental_housing_supply&
        (disutility_of_landlords_3,rental_management_costs_3,rental_housing_supply_3,rent_increase_1)
    
        use Global_Vars
        
        implicit none
        
        real(8), intent(in) :: rental_management_costs_3,rent_increase_1
        real(8), intent(in) :: disutility_of_landlords_3
        real(8), intent(inout) :: rental_housing_supply_3
            
        rental_housing_supply_3=max(((model_average_rent(1,1-two_rental_markets)*rent_increase_1-&
            housing_price(1)+(1/(1+risk_free_rate(1)))*(housing_price(1)*&
            (1-housing_depreciation_rate-property_tax(1)-property_tax_on_rental_housing)))/&
            (rental_management_costs_3*disutility_of_landlords_3)),0.d0)**(1/(disutility_of_landlords_3-1))
                                    
    end subroutine
    
    subroutine solve_elasticity_of_rental_housing(time_period,disutility_of_landlords_solved)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        integer, parameter :: dimensions_of_problem=2
        real(8), dimension(dimensions_of_problem+1) :: rental_housing_supply_1
        real(8), dimension(dimensions_of_problem+1) :: disutility_of_landlords_1,rental_management_costs_1
        real(8), dimension(dimensions_of_problem+1) :: elasticity_1
        real(8), intent(out) :: disutility_of_landlords_solved
        real(8) :: stopping_rule_elasticity=10.d0**(-4),rent_increase_1
        
        disutility_of_landlords_1(1)=1.1
        disutility_of_landlords_1(2)=70
                                
        do i=1,dimensions_of_problem
        
            call compute_rental_management_costs(time_period,disutility_of_landlords_1(i),rental_management_costs_1(i))
            
            rent_increase_1=1.01
            
            call compute_rental_housing_supply(disutility_of_landlords_1(i),&
                rental_management_costs_1(i),rental_housing_supply_1(i),rent_increase_1)
                
            elasticity_1(i)=rental_housing_supply_1(i)
            
            rent_increase_1=1
                
            call compute_rental_housing_supply(disutility_of_landlords_1(i),&
                rental_management_costs_1(i),rental_housing_supply_1(i),rent_increase_1)
                
            elasticity_1(i)=100*(elasticity_1(i)/rental_housing_supply_1(i)-1)
                            
        end do
        
        elasticity_1(3)=0
                
        do while (abs(elasticity_1(3)-elasticity_of_rental_supply_data)>stopping_rule_elasticity)
                
            disutility_of_landlords_1(3)=sum(disutility_of_landlords_1(1:dimensions_of_problem))/dimensions_of_problem
            
            call compute_rental_management_costs(time_period,disutility_of_landlords_1(3),rental_management_costs_1(3))
            
            rent_increase_1=1.01
            
            call compute_rental_housing_supply(disutility_of_landlords_1(3),&
                rental_management_costs_1(3),rental_housing_supply_1(3),rent_increase_1)
                
            elasticity_1(3)=rental_housing_supply_1(3)
            
            rent_increase_1=1
                
            call compute_rental_housing_supply(disutility_of_landlords_1(3),&
                rental_management_costs_1(3),rental_housing_supply_1(3),rent_increase_1)
                
            elasticity_1(3)=100*(elasticity_1(3)/rental_housing_supply_1(3)-1)
            
            if (elasticity_1(3)>elasticity_of_rental_supply_data) then
            
                disutility_of_landlords_1(1)=disutility_of_landlords_1(3)
                
            else
            
                disutility_of_landlords_1(dimensions_of_problem)=disutility_of_landlords_1(3)
                
            end if
                        
        end do       
        
        disutility_of_landlords_solved=disutility_of_landlords_1(1)
    
    end subroutine
    
    subroutine converge_on_proportion_inheritance(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        real(8) :: distribution_used,distance_1,stopping_rule_1,bequest_1
        
        stopping_rule_1=10.d0**(-8)
        
        distance_1=1
        
        do while (distance_1>stopping_rule_1)
        
            value_of_bequests=0    
            
            do years_left_on_mortgage_index=1-two_rental_markets,max_amortization_years
            
                do age_index=min_age_inheritance,max_age_inheritance
                
                    do employment_index=1,employment_grid_size
                                            
                        distribution_used=&
                            sum(distribution_stationary(years_left_on_mortgage_index)%vector_transition_real&
                            (time_period,age_index,:,employment_index,:,:,:))
                                                                            
                        bequest_1=employment_grid(age_index,employment_index)*proportion_inheritance                                                   
                        
                        value_of_bequests=value_of_bequests+bequest_1*distribution_used
                                                
                    end do
                
                end do
                
            end do
            
            distance_1=abs(model_bequests(time_period)-(value_of_initial_assets+value_of_bequests))
            
            proportion_inheritance=proportion_inheritance+proportion_inheritance_adjuster*&
                (model_bequests(time_period)-(value_of_initial_assets+value_of_bequests))
                            
        end do
    
    end subroutine
        
    subroutine calculate_aggregate_consumption(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: financial_index,has_house,loop_over_rent_shocks,amortization_used,financial_index_used
        integer :: time_period,time_period_used,housing_status_index_used,years_holding_onto_asset_used,is_end_of_life
        real(8) :: consumption_used,distribution_mass_used,lived_in_size_used
                
        time_period_used=time_period-solve_transition_dynamics
        
        aggregate_housing_consumption(time_period)=0
        aggregate_housing_transaction_costs(time_period)=0
        aggregate_consumption(time_period)=0
        aggregate_mortgage_consumption(time_period)=0
        aggregate_rent_spending(time_period)=0
        aggregate_origination_costs(time_period)=0
        aggregate_prepayment_costs(time_period)=0
        aggregate_moving_costs(time_period)=0
        
        do age_index=1,life_span
            
            if (age_index==1) then
            
                newborn_household=1
                
            else
            
                newborn_household=0
                
            end if
            
            is_end_of_life=0
            
            if (age_index==life_span) then
            
                is_end_of_life=1
                
            end if
            
            do rental_opportunity_index=0,random_assignment
                                        
                do years_left_on_mortgage_index=newborn_household+(1-newborn_household)*(1-two_rental_markets),&
                    newborn_household+(1-newborn_household)*max_amortization_years
                                
                    if (years_left_on_mortgage_index>1) then
                    
                        has_house=1
                        
                    else
                    
                        has_house=0
                    
                    end if
                    
                    loop_over_rent_shocks=0
                    
                    if ((years_left_on_mortgage_index==0) .AND. (age_index>1)) then
                        
                        loop_over_rent_shocks=1
                        
                    else
                    
                        if ((two_rental_markets==0) .AND. (years_left_on_mortgage_index==1) .AND. &
                            (age_index>1) .AND. (include_shocks_to_free_market_rent==1)) then
                
                            loop_over_rent_shocks=1
                                                    
                        end if
                        
                    end if
                                    
                    do years_holding_onto_asset_index=1,has_house+(1-has_house)*&
                        (loop_over_rent_shocks*rent_shock_grid_size+(1-loop_over_rent_shocks)*min(age_index,max_tracking))
                                                                                  
                        do financial_index=has_house*1+(1-has_house)*negative_financial_grid_size,financial_grid_size
                        
                            do employment_index=1,employment_grid_size
                                                
                                do housing_status_index=(1-has_house)*lowest_housing_grid_point+&
                                    has_house*index_of_smallest_house_hhld_can_buy,(housing_status_grid_size-1)
                                                                            
                                    lived_in_size_used=policy_function_space_lived_in_size(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                                      
                                    amortization_used=policy_function_years_of_amortization_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)

                                    years_holding_onto_asset_used=&
                                        policy_function_years_holding_onto_asset(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)

                                    financial_index_used=policy_function_financial_index(years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,employment_index,&
                                        housing_status_index,years_holding_onto_asset_index,rental_opportunity_index)

                                    housing_status_index_used=policy_function_housing_status_index&
                                        (years_left_on_mortgage_index)%&
                                        vector_transition_integer(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)

                                    consumption_used=policy_function_consumption(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                    
                                    distribution_mass_used=distribution_stationary(years_left_on_mortgage_index)%&
                                        vector_transition_real(time_period_used,age_index,financial_index,&
                                        employment_index,housing_status_index,years_holding_onto_asset_index,&
                                        rental_opportunity_index)
                                        
                                    aggregate_consumption(time_period)=aggregate_consumption(time_period)+&
                                        consumption_used*distribution_mass_used
                                                                            
                                    if (years_left_on_mortgage_index<=1) then
                                                                            
                                        if ((financial_grid(financial_index_used)<0) .AND. &
                                            (amortization_used==2)) then
                                            
    !                                            aggregate_origination_costs(time_period)=aggregate_origination_costs(time_period)+&
    !                                                mortgage_origination_cost*model_average_labour_income(time_period)*&
    !                                                distribution_mass_used
                                                
                                        end if
                                                                                         
                                    end if
                                    
                                    if ((years_left_on_mortgage_index<=1) .AND. (amortization_used<=1) .AND. (age_index>1)) then
                                    
                                        if ((housing_status_index_used .NE. housing_status_index) .OR. &
                                           (amortization_used .NE. years_left_on_mortgage_index)) then
                                           
                                            aggregate_moving_costs(time_period)=aggregate_moving_costs(time_period)+&
                                                fixed_cost_to_moving*(is_counterfactual*average_labor_income_calibrated+&
                                                (1-is_counterfactual)*model_average_labour_income(time_period))*&
                                                distribution_mass_used
                                        
                                        end if
                                    
                                    end if
                                                                            
                                    if (years_left_on_mortgage_index==2) then
                                                                
                                        if ((amortization_used==2) .AND. &
                                            (housing_status_index_used .NE. housing_status_index)) then
                                            
                                            if (financial_grid(financial_index_used)<0) then
                                            
    !                                                aggregate_origination_costs(time_period)=aggregate_origination_costs(time_period)+&
    !                                                    mortgage_origination_cost*model_average_labour_income(time_period)*&
    !                                                    distribution_mass_used
                                                    
                                            end if
                                            
                                            if (financial_grid(financial_index)>0) then
                                                                                            
                                            else
                                                
                                                aggregate_prepayment_costs(time_period)=&
                                                    aggregate_prepayment_costs(time_period)+&
                                                    penalty_months_of_interest*mortgage_interest_rate(time_period)*&
                                                    (-min(financial_grid(financial_index),0.d0)*&
                                                    (1-prepayment_penalty_starts_from))*distribution_mass_used
                                                    
                                            end if
                                                
                                        end if
                                        
                                        if ((amortization_used==2) .AND. (housing_status_index_used==housing_status_index)) then
                                            
                                            if (financial_grid(financial_index)>0) then
                                            
                                                if (financial_grid(financial_index_used)<0) then
                                                
                                                    aggregate_origination_costs(time_period)=&
                                                        aggregate_origination_costs(time_period)+&
                                                        mortgage_origination_cost*model_average_labour_income(time_period)*&
                                                        distribution_mass_used

                                                end if
                                                    
                                            else
                                            
                                                if (financial_grid(financial_index_used)<&
                                                    (1-fraction_to_repay)*financial_grid(financial_index))  then
                                                
                                                    aggregate_origination_costs(time_period)=&
                                                        aggregate_origination_costs(time_period)+&
                                                        mortgage_origination_cost*model_average_labour_income(time_period)*&
                                                        distribution_mass_used
                                                    
                                                else
                                                
                                                    aggregate_prepayment_costs(time_period)=&
                                                        aggregate_prepayment_costs(time_period)+&
                                                        penalty_months_of_interest*mortgage_interest_rate(time_period)*&
                                                        max((min(financial_grid(financial_index_used),0.d0)-&
                                                        financial_grid(financial_index)*&
                                                        (1-prepayment_penalty_starts_from)),0.d0)*&
                                                        distribution_mass_used
                                                        
                                                end if
                                                                                                
                                            end if
                                                                                                          
                                        end if
                                        
                                        if (amortization_used<=1) then
                                                                                                          
                                            aggregate_prepayment_costs(time_period)=&
                                                aggregate_prepayment_costs(time_period)+&
                                                penalty_months_of_interest*mortgage_interest_rate(time_period)*&
                                                (-min(financial_grid(financial_index),0.d0)*&
                                                (1-prepayment_penalty_starts_from))*distribution_mass_used
                                                                                                                                                        
                                        end if
                                                                                        
                                        aggregate_mortgage_consumption(time_period)=&
                                            aggregate_mortgage_consumption(time_period)+&
                                            (-min(financial_grid(financial_index),0.d0))*&
                                            mortgage_interest_rate(time_period)*distribution_mass_used
                                                                                                       
                                    end if
                                                                    
                                    if (amortization_used>1) then
                                                                            
                                        aggregate_housing_consumption(time_period)=&
                                            aggregate_housing_consumption(time_period)+&
                                            housing_depreciation_rate*housing_price(time_period)*&
                                            housing_size(housing_status_index_used)*distribution_mass_used
                                                                                        
                                    end if
                                                                          
                                    if ((amortization_used<=1) .AND. &
                                        (housing_status_index_used>=third_lowest_housing_grid_point)) then
                                    
                                        if (include_rent_decrease==0) then
                                
                                            aggregate_rent_spending(time_period)=aggregate_rent_spending(time_period)+&
                                                lived_in_size_used*(rent(time_period,amortization_used)*&
                                                (1+(1-is_end_of_life)*free_market_rent_increase_due_to_shock)/&
                                                (1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                (years_holding_onto_asset_used-1))*distribution_mass_used
                                                
                                        else
                                        
                                            aggregate_rent_spending(time_period)=aggregate_rent_spending(time_period)+&
                                                lived_in_size_used*(rent(time_period,amortization_used)*&
                                                (1+(1-amortization_used)*(1-is_end_of_life)*&
                                                free_market_rent_increase_due_to_shock)/&
                                                ((1+amortization_used*include_rent_decrease*rent_decrease_per_year)**&
                                                (years_holding_onto_asset_used-1)))*distribution_mass_used
                                                
                                        end if
                                                                                
                                    end if
                                                                                                         
                                    if ((years_left_on_mortgage_index<=1) .AND. (amortization_used>=2)) then
                                    
                                        if ((age_index>1) .AND. (age_index<life_span)) then
                                        
                                            aggregate_housing_transaction_costs(time_period)=&
                                                aggregate_housing_transaction_costs(time_period)+&
                                                cost_of_buying*housing_price(time_period)*&
                                                housing_size(housing_status_index_used)*distribution_mass_used
                                            
                                        elseif (age_index==life_span) then
                                        
                                            aggregate_housing_transaction_costs(time_period)=&
                                                aggregate_housing_transaction_costs(time_period)+&
                                                ((cost_of_selling+cost_of_buying)*housing_price(time_period)*&
                                                housing_size(housing_status_index_used))*distribution_mass_used
                                                
                                        end if

                                    end if
                                                                                
                                    if ((years_left_on_mortgage_index>=2) .AND. (amortization_used<=1)) then
                                                                                    
                                        if ((age_index>1) .AND. (age_index<=life_span)) then
                                        
                                            aggregate_housing_transaction_costs(time_period)=&
                                                aggregate_housing_transaction_costs(time_period)+&
                                                cost_of_selling*housing_price(time_period)*&
                                                housing_size(housing_status_index)*distribution_mass_used
                                                                  
                                        end if
                                                                                        
                                    end if
                                                                          
                                    if ((years_left_on_mortgage_index>=2) .AND. (amortization_used>=2) .AND. &
                                        (housing_status_index_used .NE. housing_status_index)) then
                                                                                                                
                                        if ((age_index>1) .AND. (age_index<life_span)) then
                                                                            
                                            aggregate_housing_transaction_costs(time_period)=&
                                                aggregate_housing_transaction_costs(time_period)+&
                                                (cost_of_selling*housing_price(time_period)*housing_size(housing_status_index)+&
                                                cost_of_buying*housing_price(time_period)*&
                                                housing_size(housing_status_index_used))*distribution_mass_used
                                                
                                        elseif (age_index==life_span) then
                                        
                                            aggregate_housing_transaction_costs(time_period)=&
                                                aggregate_housing_transaction_costs(time_period)+&
                                                (cost_of_selling*housing_price(time_period)*housing_size(housing_status_index)+&
                                                (cost_of_selling+cost_of_buying)*housing_price(time_period)*&
                                                housing_size(housing_status_index_used))*distribution_mass_used
                                                                             
                                        end if
                                        
                                    end if
                                    
                                    if ((years_left_on_mortgage_index>=2) .AND. (amortization_used>=2) .AND. &
                                        (housing_status_index_used==housing_status_index)) then
                                                                                                                
                                        if (age_index==life_span) then
                                        
                                            aggregate_housing_transaction_costs(time_period)=&
                                                aggregate_housing_transaction_costs(time_period)+&
                                                cost_of_selling*housing_price(time_period)*&
                                                housing_size(housing_status_index)*distribution_mass_used
                                                                             
                                        end if
                                        
                                    end if
                                    
                                end do
                            
                            end do
                            
                        end do
                                                                                
                    end do
                    
                end do
                                                                    
            end do
                            
        end do
                                                                    
        write(*,*) "aggregate_consumption", time_period, "=", aggregate_consumption(time_period)
        write(*,*) "aggregate_housing_consumption=", time_period, "=", aggregate_housing_consumption(time_period)
        write(*,*) "aggregate_mortgage_consumption=", time_period, "=", aggregate_mortgage_consumption(time_period)
        write(*,*) "aggregate_housing_transaction_costs=", aggregate_housing_transaction_costs(time_period)
        write(*,*) "aggregate_origination_costs=", aggregate_origination_costs(time_period)
        write(*,*) "aggregate_prepayment_costs=", aggregate_prepayment_costs(time_period)
        write(*,*) "aggregate_moving_costs=", aggregate_moving_costs(time_period)
        write(*,*) "aggregate_rent_spending=", aggregate_rent_spending(time_period)
        
        write(*,*) "total_expendifure=", &
            aggregate_consumption(time_period)+aggregate_housing_consumption(time_period)+&
            aggregate_mortgage_consumption(time_period)+aggregate_housing_transaction_costs(time_period)+&
            aggregate_origination_costs(time_period)+aggregate_prepayment_costs(time_period)+&
            aggregate_moving_costs(time_period)+net_govt_revenues_1(time_period)+&
            aggregate_rent_spending(time_period)-housing_price(time_period)*&
            sum(total_rental_demand(time_period-solve_transition_dynamics,:))*property_tax(time_period-solve_transition_dynamics)
        write(*,*) "model_total_income=", model_output(time_period)+model_total_investment_income(time_period)
        write(*,*) "model_total_income-model_total_expenditure=", &
            model_output(time_period)+model_total_investment_income(time_period)-&
            (aggregate_consumption(time_period)+aggregate_housing_consumption(time_period)+&
            aggregate_mortgage_consumption(time_period)+aggregate_housing_transaction_costs(time_period)+&
            aggregate_origination_costs(time_period)+aggregate_prepayment_costs(time_period)+&
            aggregate_moving_costs(time_period)+net_govt_revenues_1(time_period)+&
            aggregate_rent_spending(time_period)-housing_price(time_period)*&
            sum(total_rental_demand(time_period-solve_transition_dynamics,:))*property_tax(time_period-solve_transition_dynamics))
                                   
    end subroutine
    
    subroutine update_tax_system(time_period,which_tax_system_to_solve)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period,which_tax_system_to_solve,loop_count_iterations
        real(8) :: lower_bound,upper_bound,distance_lower_bound,distance_upper_bound
        real(8) :: previous_income_tax_increase,previous_lump_sum_transfer,previous_property_tax,net_govt_revenues_previous
        real(8) :: weight=solve_transition_dynamics*0.7+(1-solve_transition_dynamics)*0.9
        real(8) :: weight_property_tax=solve_transition_dynamics*0.7+(1-solve_transition_dynamics)*0.5
        integer :: equal_weights=1
        
        sum_of_distribution=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period-solve_transition_dynamics,:,:,:,:,:,:))
            
        end do
                
        if (loop_count>=aftr_how_mny_lps_start_update) then
                
            loop_count_iterations=0
            
            if (which_tax_system_to_solve==0) then
                            
                net_govt_revenues_previous=net_govt_revenues(time_period)+govt_lump_sum_transfers(time_period)
                
                if (solve_transition_dynamics==0) then
        
                    write(*,*) "using slicing method 2"
                    write(*,*) "lump_sum_transfer"
                    write(*,*) "net_govt_revenue_constraint=", net_govt_revenue_constraint
                    
                end if
                    
                previous_lump_sum_transfer=lump_sum_transfer(time_period)
        
                if (net_govt_revenues(time_period)>net_govt_revenue_constraint) then
            
                    lower_bound=lump_sum_transfer(time_period)
                
                    distance_lower_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                    
                    lump_sum_transfer(time_period)=lower_bound+0.02
                                        
                    net_govt_revenues(time_period)=net_govt_revenues_previous-&
                        lump_sum_transfer(time_period)*sum_of_distribution
                    
                    loop_count_iterations=loop_count_iterations+1
                
                    do while (net_govt_revenues(time_period)>net_govt_revenue_constraint)
                
                        lump_sum_transfer(time_period)=lump_sum_transfer(time_period)+0.02
                    
                        net_govt_revenues(time_period)=net_govt_revenues_previous-&
                            lump_sum_transfer(time_period)*sum_of_distribution
                    
                        loop_count_iterations=loop_count_iterations+1
                        
!                        write(*,*) "lump_sum_transfer=", lump_sum_transfer
!                        write(*,*) "net_govt_revenues(time_period)=", net_govt_revenues(time_period)
!                        write(*,*) "net_govt_revenue_constraint=", net_govt_revenue_constraint
                                                        
                    end do
                                    
                    distance_upper_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                
                    upper_bound=lump_sum_transfer(time_period)
                
                    lump_sum_transfer(time_period)=equal_weights*(lower_bound+upper_bound)/2+(1-equal_weights)*&
                        ((distance_lower_bound/(distance_lower_bound+distance_upper_bound))*lower_bound+&
                        (distance_upper_bound/(distance_lower_bound+distance_upper_bound))*upper_bound)
                
                    do while (abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)>10.d0**(-4))
                
                        net_govt_revenues(time_period)=net_govt_revenues_previous-&
                            lump_sum_transfer(time_period)*sum_of_distribution
                    
                        loop_count_iterations=loop_count_iterations+1
            
                        if (net_govt_revenues(time_period)>net_govt_revenue_constraint) then
                        
                            lower_bound=lump_sum_transfer(time_period)
                        
                            distance_lower_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                       
                        else
                    
                            upper_bound=lump_sum_transfer(time_period)
                            
                            distance_upper_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                        
                        end if
                    
                        lump_sum_transfer(time_period)=equal_weights*(lower_bound+upper_bound)/2+(1-equal_weights)*&
                            ((distance_lower_bound/(distance_lower_bound+distance_upper_bound))*lower_bound+&
                            (distance_upper_bound/(distance_lower_bound+distance_upper_bound))*upper_bound)
                            
!                        write(*,*) "lump_sum_transfer=", lump_sum_transfer
                        
                    end do
                                    
                else
            
                    upper_bound=lump_sum_transfer(time_period)
                
                    distance_upper_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                                
                    lump_sum_transfer(time_period)=upper_bound-0.02
                    
                    net_govt_revenues(time_period)=net_govt_revenues_previous-&
                        lump_sum_transfer(time_period)*sum_of_distribution
                
                    loop_count_iterations=loop_count_iterations+1
                    
                    do while (net_govt_revenues(time_period)<net_govt_revenue_constraint)
                                    
                        lump_sum_transfer(time_period)=lump_sum_transfer(time_period)-0.02
                        
                        net_govt_revenues(time_period)=net_govt_revenues_previous-&
                            lump_sum_transfer(time_period)*sum_of_distribution
                    
                        loop_count_iterations=loop_count_iterations+1
                        
!                        write(*,*) "lump_sum_transfer=", lump_sum_transfer
!                        write(*,*) "net_govt_revenues(time_period)-net_govt_revenue_constraint=", &
!                            net_govt_revenues(time_period)-net_govt_revenue_constraint
                                
                    end do
                
                    distance_lower_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                
                    lower_bound=lump_sum_transfer(time_period)
                
                    write(*,*) "start to slice"
                
                    lump_sum_transfer(time_period)=equal_weights*(lower_bound+upper_bound)/2+(1-equal_weights)*&
                        ((distance_lower_bound/(distance_lower_bound+distance_upper_bound))*lower_bound+&
                        (distance_upper_bound/(distance_lower_bound+distance_upper_bound))*upper_bound)
                
                    do while (abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)>10.d0**(-4))
                    
                        net_govt_revenues(time_period)=net_govt_revenues_previous-&
                            lump_sum_transfer(time_period)*sum_of_distribution
                    
                        loop_count_iterations=loop_count_iterations+1
                
                        if (net_govt_revenues(time_period)>net_govt_revenue_constraint) then
                    
                            lower_bound=lump_sum_transfer(time_period)
                        
                            distance_lower_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                        
                        else
                    
                            upper_bound=lump_sum_transfer(time_period)
                        
                            distance_upper_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                            
                        end if
                    
                        lump_sum_transfer(time_period)=equal_weights*(lower_bound+upper_bound)/2+(1-equal_weights)*&
                            ((distance_lower_bound/(distance_lower_bound+distance_upper_bound))*lower_bound+&
                            (distance_upper_bound/(distance_lower_bound+distance_upper_bound))*upper_bound)
                            
!                        write(*,*) "lump_sum_transfer=", lump_sum_transfer
                    
                    end do
                                
                end if
                
                lump_sum_transfer(time_period)=weight*previous_lump_sum_transfer+&
                    (1-weight)*lump_sum_transfer(time_period)
                
                net_govt_revenues(time_period)=net_govt_revenues_previous-&
                    lump_sum_transfer(time_period)*sum_of_distribution
            
                write(*,*) "sliced net_govt_revenues=", net_govt_revenues(time_period)
                write(*,*) "lump_sum_transfer=", lump_sum_transfer(time_period)
                            
            elseif (which_tax_system_to_solve==1) then
            
                call govt_revenues_calc(time_period)
        
                write(*,*) "using slicing method 2"
                write(*,*) "income_tax"
    
                previous_income_tax_increase=increase_in_labour_income_tax
        
                if (net_govt_revenues(time_period)<net_govt_revenue_constraint) then
            
                    lower_bound=increase_in_labour_income_tax
                
                    distance_lower_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                    
                    increase_in_labour_income_tax=lower_bound-0.002
                                        
                    call govt_revenues_calc(time_period)
                
                    loop_count_iterations=loop_count_iterations+1
                
                    do while (net_govt_revenues(time_period)<net_govt_revenue_constraint)
                
                        increase_in_labour_income_tax=increase_in_labour_income_tax-0.002
                    
                        call govt_revenues_calc(time_period)
                    
                        loop_count_iterations=loop_count_iterations+1
                        
!                        write(*,*) "increase_in_labour_income_tax=", increase_in_labour_income_tax
!                        write(*,*) "net_govt_revenues(time_period)=", &
!                            net_govt_revenues(time_period)
!                        write(*,*) "net_govt_revenue_constraint=", net_govt_revenue_constraint
                                                        
                    end do
                                    
                    distance_upper_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                
                    upper_bound=increase_in_labour_income_tax
                
                    increase_in_labour_income_tax=equal_weights*(lower_bound+upper_bound)/2+(1-equal_weights)*&
                        ((distance_lower_bound/(distance_lower_bound+distance_upper_bound))*lower_bound+&
                        (distance_upper_bound/(distance_lower_bound+distance_upper_bound))*upper_bound)
                
                    do while (abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)>10.d0**(-4))
                
                        call govt_revenues_calc(time_period)
                    
                        loop_count_iterations=loop_count_iterations+1
            
                        if (net_govt_revenues(time_period)<net_govt_revenue_constraint) then
                        
                            lower_bound=increase_in_labour_income_tax
                        
                            distance_lower_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                       
                        else
                    
                            upper_bound=increase_in_labour_income_tax
                            
                            distance_upper_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                        
                        end if
                    
                        increase_in_labour_income_tax=equal_weights*(lower_bound+upper_bound)/2+(1-equal_weights)*&
                            ((distance_lower_bound/(distance_lower_bound+distance_upper_bound))*lower_bound+&
                            (distance_upper_bound/(distance_lower_bound+distance_upper_bound))*upper_bound)
                            
!                        write(*,*) "increase_in_labour_income_tax=", increase_in_labour_income_tax
                        
                    end do
                                    
                else
            
                    upper_bound=increase_in_labour_income_tax
                
                    distance_upper_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                                
                    increase_in_labour_income_tax=upper_bound+0.002
                    
                    call govt_revenues_calc(time_period)
                
                    loop_count_iterations=loop_count_iterations+1
                    
                    do while (net_govt_revenues(time_period)>net_govt_revenue_constraint)
                                    
                        increase_in_labour_income_tax=increase_in_labour_income_tax+0.002
                        
                        ! Computes net govt revenues
                        call govt_revenues_calc(time_period)
                    
                        loop_count_iterations=loop_count_iterations+1
                        
!                        write(*,*) "increase_in_labour_income_tax=", increase_in_labour_income_tax
!                        write(*,*) "net_govt_revenues(time_period)-net_govt_revenue_constraint=", &
!                            net_govt_revenues(time_period)-net_govt_revenue_constraint
                                
                    end do
                
                    distance_lower_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                
                    lower_bound=increase_in_labour_income_tax
                
                    write(*,*) "start to slice"
                
                    increase_in_labour_income_tax=equal_weights*(lower_bound+upper_bound)/2+&
                        (1-equal_weights)*((distance_lower_bound/(distance_lower_bound+distance_upper_bound))*lower_bound+&
                        (distance_upper_bound/(distance_lower_bound+distance_upper_bound))*upper_bound)
                
                    do while (abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)>10.d0**(-4))
                    
                        call govt_revenues_calc(time_period)
                    
                        loop_count_iterations=loop_count_iterations+1
                
                        if (net_govt_revenues(time_period)<net_govt_revenue_constraint) then
                    
                            lower_bound=increase_in_labour_income_tax
                        
                            distance_lower_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                        
                        else
                    
                            upper_bound=increase_in_labour_income_tax
                        
                            distance_upper_bound=abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)
                            
                        end if
                    
                        increase_in_labour_income_tax=equal_weights*(lower_bound+upper_bound)/2+&
                            (1-equal_weights)*((distance_lower_bound/(distance_lower_bound+distance_upper_bound))*lower_bound+&
                            (distance_upper_bound/(distance_lower_bound+distance_upper_bound))*upper_bound)
                            
!                        write(*,*) "increase_in_labour_income_tax=", &
!                            increase_in_labour_income_tax
                    
                    end do
                                
                end if
                
                increase_in_labour_income_tax=weight*previous_income_tax_increase+&
                    (1-weight)*increase_in_labour_income_tax
                
                call govt_revenues_calc(time_period)
            
                write(*,*) "slicing net_govt_revenues=", net_govt_revenues(time_period)
                write(*,*) "increase_in_labour_income_tax=", increase_in_labour_income_tax
                         
            elseif (which_tax_system_to_solve==2) then
                                            
                if (solve_transition_dynamics==0) then
        
                    write(*,*) "using slicing method 2"
                    write(*,*) "property_tax"
                    write(*,*) "net_govt_revenue_constraint=", net_govt_revenue_constraint
                    
                end if
                
                previous_property_tax=property_tax(time_period)
                                                                                            
                loop_count_iterations=0
                                            
                if (net_govt_revenues(time_period)<net_govt_revenue_constraint) then
                    
                    lower_bound=property_tax(time_period)  
                                                                                                                            
                    do while (net_govt_revenues(time_period)<net_govt_revenue_constraint)
                
                        property_tax(time_period)=property_tax(time_period)+0.0000001
                    
                        net_govt_revenues(time_period)=&
                            property_tax(time_period)*housing_price(time_period)*&
                            total_housing_demand(time_period-solve_transition_dynamics)
                    
                        loop_count_iterations=loop_count_iterations+1
                        
                        if (solve_transition_dynamics==0) then
                        
                            write(*,*) "property_tax=", property_tax
!                            write(*,*) "net_govt_revenues(time_period)  111=", net_govt_revenues(time_period)
                            
                        end if
                                                        
                    end do
                                                    
!                    upper_bound=property_tax(time_period)
!                
!                    property_tax(time_period)=(lower_bound+upper_bound)/2
!                                        
!                    net_govt_revenues(time_period)=&
!                        property_tax(time_period)*housing_price(time_period)*&
!                        total_housing_demand(time_period-solve_transition_dynamics)
!                        
!                    write(*,*) "start to slice below"
!                    
!                    loop_count_iterations=0
!                
!                    do while (abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)>10.d0**(-3))
!                    
!                        net_govt_revenues(time_period)=&
!                            property_tax(time_period)*housing_price(time_period)*&
!                            total_housing_demand(time_period-solve_transition_dynamics)
!                    
!                        loop_count_iterations=loop_count_iterations+1
!            
!                        if (net_govt_revenues(time_period)<net_govt_revenue_constraint) then
!                        
!                            lower_bound=property_tax(time_period)
!                                               
!                        else
!                    
!                            upper_bound=property_tax(time_period)
!                                                    
!                        end if
!                    
!                        property_tax(time_period)=(lower_bound+upper_bound)/2
!                            
!                        if (solve_transition_dynamics==0) then
!                            
!                            write(*,*) "property_tax=", property_tax(time_period)
!                            write(*,*) "net_govt_revenues(time_period) 222=", net_govt_revenues(time_period)
!                            
!                        end if
!                        
!                    end do
                                    
                else
            
                    upper_bound=property_tax(time_period)
                                                                    
                    property_tax(time_period)=upper_bound
                                    
                    loop_count_iterations=0
                    
                    do while (net_govt_revenues(time_period)>net_govt_revenue_constraint)
                                    
                        property_tax(time_period)=property_tax(time_period)-0.0000001
                        
                        net_govt_revenues(time_period)=&
                            property_tax(time_period)*housing_price(time_period)*&
                            total_housing_demand(time_period-solve_transition_dynamics)
                    
                        loop_count_iterations=loop_count_iterations+1
                        
                        if (solve_transition_dynamics==0) then
                        
                            write(*,*) "property_tax=", property_tax
!                            write(*,*) "net_govt_revenues(time_period) 333=", net_govt_revenues(time_period)
                            
                        end if
                                
                    end do
                                
!                    lower_bound=property_tax(time_period)
!                
!                    write(*,*) "start to slice above"
!                
!                    property_tax(time_period)=(lower_bound+upper_bound)/2                    
!                    
!                    net_govt_revenues(time_period)=&
!                        property_tax(time_period)*housing_price(time_period)*&
!                        total_housing_demand(time_period-solve_transition_dynamics)
!                        
!                    loop_count_iterations=0
!                
!                    do while (abs(net_govt_revenues(time_period)-net_govt_revenue_constraint)>10.d0**(-3))
!                    
!                        net_govt_revenues(time_period)=&
!                            property_tax(time_period)*housing_price(time_period)*&
!                            total_housing_demand(time_period-solve_transition_dynamics)
!                    
!                        loop_count_iterations=loop_count_iterations+1
!                
!                        if (net_govt_revenues(time_period)<net_govt_revenue_constraint) then
!                    
!                            lower_bound=property_tax(time_period)
!                                                
!                        else
!                    
!                            upper_bound=property_tax(time_period)
!                                                    
!                        end if
!                    
!                        property_tax(time_period)=(lower_bound+upper_bound)/2
!                            
!                        if (solve_transition_dynamics==0) then
!                            
!                            write(*,*) "property_tax=", property_tax(time_period)
!                            write(*,*) "net_govt_revenues(time_period) 444=", net_govt_revenues(time_period)
!                            
!                        end if
!                    
!                    end do
                                
                end if
                
                property_tax(time_period)=weight_property_tax*previous_property_tax+&
                    (1-weight_property_tax)*property_tax(time_period)
                
                net_govt_revenues(time_period)=&
                    property_tax(time_period)*housing_price(time_period)*&
                    total_housing_demand(time_period-solve_transition_dynamics)
                    
                if (solve_transition_dynamics==0) then
            
                    write(*,*) "sliced net_govt_revenues=", net_govt_revenues(time_period)
                    write(*,*) "net_govt_revenues(time_period)-net_govt_revenue_constraint=", &
                        net_govt_revenues(time_period)-net_govt_revenue_constraint
                        
                end if
                    
                write(*,*) "property_tax=", property_tax(time_period)
                                                                                                       
            end if
            
        end if
    
    end subroutine
    
    subroutine save_vars_and_functions(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        
        character*1 :: string_used
        
        write(*,*) "saving vars and functions"
                
        do i=1-two_rental_markets,2
        
            write(string_used,'(i1)') i
            
            open(unit=1,file="value_function"//trim(string_used)//trim(file_number)//".dat",status='replace')
                write(1,'(F40.18)') value_function(i)%vector_transition_real(:,:,:,:,:,:,:)
            close(1)
        
            open(unit=2,file="distribution_stationary"//trim(string_used)//trim(file_number)//".dat",status='replace')
                write(2,'(F30.20)') distribution_stationary(i)%vector_transition_real(:,:,:,:,:,:,:)
            close(2)
            
            open(unit=2,file="policy_function_space_lived_in_size"//trim(string_used)//trim(file_number)//".dat",status='replace')
                write(2,'(F25.20)') policy_function_space_lived_in_size(i)%vector_transition_real(:,:,:,:,:,:,:)
            close(2)
                        
        end do
                               
        open(unit=18,file="average_labor_income"//trim(file_number)//".dat",status='replace')
            write(18,'(F15.12)') model_average_labour_income(time_period)
        close(18)
        
        open(unit=19,file="model_ss_benefits"//trim(file_number)//".dat",status='replace')
            do i=1,employment_grid_size
                write(19,'(F15.12)') model_ss_benefits(i)
            end do
        close(19)
        
        open(unit=20,file="c_1"//trim(file_number)//".dat",status='replace')
            write(20,'(F15.12)') c_1
        close(20)
        
        open(unit=21,file="c_min"//trim(file_number)//".dat",status='replace')
            write(21,'(F15.12)') c_min
        close(21)
        
        open(unit=22,file="housing_price"//trim(file_number)//".dat",status='replace')
            write(22,'(F15.12)') housing_price(time_period)
        close(22)
        
        open(unit=23,file="rent"//trim(file_number)//".dat",status='replace')
            write(23,'(F15.14)') rent(time_period,:)
        close(23)
        
        open(unit=23,file="model_average_rent"//trim(file_number)//".dat",status='replace')
            write(23,'(F15.14)') model_average_rent(time_period,:)
        close(23)
        
        open(unit=23,file="lump_sum_transfer"//trim(file_number)//".dat",status='replace')
            write(23,'(F15.14)') lump_sum_transfer(time_period)
        close(23)
        
        open(unit=23,file="property_tax"//trim(file_number)//".dat",status='replace')
            write(23,'(F15.14)') property_tax(time_period)
        close(23)
        
        open(unit=23,file="proportion_inheritance"//trim(file_number)//".dat",status='replace')
            write(23,'(F15.14)') proportion_inheritance
        close(23)
        
        open(unit=24,file="housing_stock"//trim(file_number)//".dat",status='replace')
            write(24,'(F15.12)') housing_stock(time_period)
        close(24)
        
        open(unit=24,file="free_market_rent_increase_due_to_shock"//trim(file_number)//".dat",status='replace')
            write(24,'(F15.14)') free_market_rent_increase_due_to_shock
        close(24)
        
        open(unit=25,file="total_rental_supply"//trim(file_number)//".dat",status='replace')
            write(25,'(F15.12)') total_rental_supply(time_period,:)
        close(25)
        
        open(unit=26,file="net_govt_revenues"//trim(file_number)//".dat",status='replace')
            write(26,'(F15.12)') net_govt_revenues(time_period)
        close(26)
        
        open(unit=27,file="rental_management_costs"//trim(file_number)//".dat",status='replace')
            write(27,'(F50.49)') rental_management_costs
        close(27)
        
        open(unit=28,file="cutoff_housing_investment"//trim(file_number)//".dat",status='replace')
            write(28,'(F15.13)') cutoff_housing_investment
        close(28)
        
        open(unit=35,file="max_initial_endowment"//trim(file_number)//".dat",status='replace')
            write(35,'(F15.12)') max_initial_endowment
        close(35)

        open(unit=35,file="initial_endowment"//trim(file_number)//".dat",status='replace')
            write(35,'(I3)') initial_endowment
        close(35)
                
        open(unit=30,file="discount_factor"//trim(file_number)//".dat",status='replace')
            write(30,'(F15.14)') discount_factor
        close(30)
        
        open(unit=30,file="discount_factor_on_child"//trim(file_number)//".dat",status='replace')
            write(30,'(F15.11)') discount_factor_on_child
        close(30)
        
        open(unit=31,file="advantage_to_owning"//trim(file_number)//".dat",status='replace')
            write(31,'(F15.14)') advantage_to_owning
        close(31)
        
        open(unit=32,file="disutility_of_landlords"//trim(file_number)//".dat",status='replace')
            write(32,'(F15.13)') disutility_of_landlords
        close(32)
        
        open(unit=33,file="relative_share_of_housing"//trim(file_number)//".dat",status='replace')
            write(33,'(F15.14)') relative_share_of_housing
        close(33)
        
        open(unit=22,file="labor_income_tax_rate"//trim(file_number)//".dat",status='replace')
            write(22,'(F15.14)') labor_income_tax_rate
        close(22)
        
        open(unit=35,file="smallest_house_size"//trim(file_number)//".dat",status='replace')
            write(35,'(F15.14)') smallest_house_size
        close(35)
        
        open(unit=35,file="size_move_back"//trim(file_number)//".dat",status='replace')
            write(35,'(F15.14)') size_move_back
        close(35)
        
        open(unit=35,file="smallest_rental_relative_to_lowest_income"//trim(file_number)//".dat",status='replace')
            write(35,'(F15.13)') smallest_rental_relative_to_lowest_income
        close(35)
                
        open(unit=36,file="size_of_smallest_house_hhld_can_buy"//trim(file_number)//".dat",status='replace')
            write(36,'(F15.14)') size_of_smallest_house_hhld_can_buy
        close(36)
                
        open(unit=37,file="model_average_owner_occupied_unit_size"//trim(file_number)//".dat",status='replace')
            write(37,'(F15.14)') model_average_owner_occupied_unit_size(time_period)
        close(37)
                
        open(unit=39,file="total_rental_demand"//trim(file_number)//".dat",status='replace')
            write(39,'(F15.12)') total_rental_demand(time_period,:)
        close(39)

        open(unit=39,file="risk_free_rate"//trim(file_number)//".dat",status='replace')
            write(39,'(F15.14)') risk_free_rate(time_period)
        close(39)

        open(unit=39,file="mortgage_interest_rate"//trim(file_number)//".dat",status='replace')
            write(39,'(F15.14)') mortgage_interest_rate(time_period)
        close(39)
        
        open(unit=39,file="smallest_financial_chosen_in_equilibrium"//trim(file_number)//".dat",status='replace')
            write(39,'(F15.11)') financial_grid(smallest_financial_chosen_in_equilibrium)
        close(39)
        
        open(unit=39,file="largest_financial_chosen_in_equilibrium"//trim(file_number)//".dat",status='replace')
            write(39,'(F15.11)') financial_grid(largest_financial_chosen_in_equilibrium)
        close(39)
        
        open(unit=40,file="housing_status_grid_expander"//trim(file_number)//".dat",status='replace')
            write(40,'(F15.13)') housing_status_grid_expander
        close(40)
        
        open(unit=40,file="total_profits"//trim(file_number)//".dat",status='replace')
            write(40,'(F15.11)') total_profits
        close(40)
        
        open(unit=40,file="intergenerational_employment_process"//trim(file_number)//".dat",status='replace')
            write(40,'(F20.19)') intergenerational_employment_process
        close(40)
        
        write(*,*) "finished saving vars and functions"
                
    end subroutine
    
    subroutine upload_vars(time_period)
    
        use Global_Vars
        
        implicit none
        
        integer :: time_period
        real(8), dimension(1+benchmark_has_two_rental_markets) :: rent_upload
        real(8), dimension(2+benchmark_has_two_rental_markets) :: model_average_rent_upload
        real(8), dimension(0:transition_length) :: rent_path_to_upload,house_price_path_to_upload
        
        write(*,*) "upload vars"

        open(unit=26,file="net_govt_revenues"//trim(old_results)//".dat",action='read')
            read(26,'(F15.12)') net_govt_revenue_constraint
        close(26)
                
        open(unit=19,file="model_ss_benefits"//trim(old_results)//".dat",action='read')
            read(19,'(F15.12)') model_ss_benefits
        close(19)
        
        open(unit=20,file="c_1"//trim(old_results)//".dat",action='read')
            read(20,'(F15.12)') c_1
        close(20)
        
        c_1_calibrated=c_1
        
        open(unit=21,file="c_min"//trim(old_results)//".dat",action='read')
            read(21,'(F15.12)') c_min
        close(21)
        
        c_min_calibrated=c_min
                        
        open(unit=22,file="housing_price"//trim(old_results)//".dat",action='read')
            read(22,'(F15.12)') housing_price(time_period)
        close(22)
        
        calibrated_stationary_housing_price=housing_price(time_period)
        
        property_tax(1-solve_transition_dynamics)=property_tax_benchmark
        
        open(unit=22,file="property_tax"//trim(long_run_simulation)//".dat",action='read')
            read(22,'(F15.14)') property_tax(transition_length)
        close(22)
        
        open(unit=22,file="proportion_inheritance"//trim(long_run_simulation)//".dat",action='read')
            read(22,'(F15.14)') proportion_inheritance
        close(22)
        
        if (two_rental_markets==1) then
        
            if (upload_random_assignment_vars==1) then
            
                open(unit=23,file="rent"//trim(benchmark_simulation)//".dat",action='read')
                    read(23,'(F15.14)') rent(time_period,:)
                close(23)
            
            else
        
                open(unit=23,file="rent"//trim(old_results)//".dat",action='read')
                    read(23,'(F15.14)') rent(time_period,:)
                close(23)
                                            
            end if
            
            rent_calibrated(:)=rent(time_period,:)
            
        else
        
            open(unit=23,file="rent"//trim(old_results)//".dat",action='read')
                read(23,'(F15.14)') rent_upload(:)
            close(23)
                        
            if (include_rent_decrease==0) then
                                    
                rent(time_period-solve_transition_dynamics,1)=rent_upload(1)
                rent_calibrated(1)=rent_upload(1)
                
            else
            
                rent(time_period-solve_transition_dynamics,1)=rent_upload(benchmark_has_two_rental_markets+1)
                rent_calibrated(1)=rent_upload(benchmark_has_two_rental_markets+1)
                
            end if
                    
        end if
        
        if (two_rental_markets==1) then
                   
            open(unit=23,file="model_average_rent"//trim(old_results)//".dat",action='read')
                read(23,'(F15.14)') model_average_rent(time_period,:)
            close(23)

            average_rent_calibrated(:)=model_average_rent(time_period,:)
            
        else
        
            open(unit=23,file="model_average_rent"//trim(old_results)//".dat",action='read')
                read(23,'(F15.14)') model_average_rent_upload(1:3)
            close(23)
            
            model_average_rent(time_period,1:2)=model_average_rent_upload(1:2)
            average_rent_calibrated(1:2)=model_average_rent_upload(1:2)
            
            if (include_rent_decrease==0) then
                        
                rent_calibrated(1)=average_rent_calibrated(1)
                
            end if
                    
        end if
            
        open(unit=24,file="housing_stock"//trim(old_results)//".dat",action='read')
            read(24,'(F15.12)') housing_stock(time_period-solve_transition_dynamics)
        close(24)
        
        open(unit=24,file="free_market_rent_increase_due_to_shock"//trim(old_results)//".dat",action='read')
            read(24,'(F15.14)') free_market_rent_increase_due_to_shock
        close(24)
        
        calibrated_housing_stock=housing_stock(time_period-solve_transition_dynamics)
        
        do i=1-solve_transition_dynamics,transition_length
        
            housing_stock(i)=calibrated_housing_stock
            
        end do
                        
        open(unit=26,file="net_govt_revenues"//trim(old_results)//".dat",action='read')
            read(26,'(F15.13)') net_govt_revenues(time_period)
        close(26)
        
        open(unit=27,file="rental_management_costs"//trim(old_results)//".dat",action='read')
            read(27,'(F50.49)') rental_management_costs
        close(27)
        
        rental_management_costs_calibrated=rental_management_costs
        
        open(unit=28,file="cutoff_housing_investment"//trim(old_results)//".dat",action='read')
            read(28,'(F15.13)') cutoff_housing_investment
        close(28)
        
        open(unit=35,file="max_initial_endowment"//trim(old_results)//".dat",action='read')
            read(35,'(F15.12)') max_initial_endowment
        close(35)

        open(unit=35,file="initial_endowment"//trim(old_results)//".dat",action='read')
            read(35,'(I3)') initial_endowment
        close(35)
        
        cutoff_housing_investment_calibrated=cutoff_housing_investment
        
        open(unit=30,file="discount_factor"//trim(old_results)//".dat",action='read')
            read(30,'(F15.14)') discount_factor
        close(30)
        
        open(unit=30,file="discount_factor_on_child"//trim(old_results)//".dat",action='read')
            read(30,'(F15.11)') discount_factor_on_child
        close(30)
        
        open(unit=30,file="labor_income_tax_rate"//trim(old_results)//".dat",action='read')
            read(30,'(F15.14)') labor_income_tax_rate
        close(30)
        
        open(unit=31,file="advantage_to_owning"//trim(old_results)//".dat",action='read')
            read(31,'(F15.14)') advantage_to_owning
        close(31)
        
        open(unit=32,file="disutility_of_landlords"//trim(old_results)//".dat",action='read')
            read(32,'(F15.12)') disutility_of_landlords
        close(32)
        
        open(unit=33,file="relative_share_of_housing"//trim(old_results)//".dat",action='read')
            read(33,'(F15.14)') relative_share_of_housing
        close(33)
        
        open(unit=37,file="model_average_owner_occupied_unit_size"//trim(old_results)//".dat",action='read')
            read(37,'(F15.14)') model_average_owner_occupied_unit_size(time_period)
        close(37)
        
        average_owner_occupied_house_size_calibrated=model_average_owner_occupied_unit_size(time_period)


        open(unit=35,file="smallest_house_size"//trim(old_results)//".dat",action='read')
            read(35,'(F15.14)') smallest_house_size
        close(35)
        
        open(unit=35,file="smallest_rental_relative_to_lowest_income"//trim(old_results)//".dat",action='read')
            read(35,'(F15.13)') smallest_rental_relative_to_lowest_income
        close(35)
        
        open(unit=35,file="size_move_back"//trim(old_results)//".dat",action='read')
            read(35,'(F15.14)') size_move_back
        close(35)
                
        open(unit=36,file="size_of_smallest_house_hhld_can_buy"//trim(old_results)//".dat",action='read')
            read(36,'(F15.14)') size_of_smallest_house_hhld_can_buy
        close(36)
        
        size_of_smallest_house_hhld_can_buy=size_of_smallest_house_hhld_can_buy-0.01
                
        if (two_rental_markets==0) then
        
            open(unit=39,file="total_rental_supply"//trim(old_results)//".dat",action='read')
                read(39,'(F15.12)') total_rental_supply_upload(:)
            close(39)
            
        else
        
            open(unit=39,file="total_rental_supply"//trim(old_results)//".dat",action='read')
                read(39,'(F15.12)') total_rental_supply(time_period-solve_transition_dynamics,:)
            close(39)
            
        end if
        
        total_rental_supply_calibrated(:)=total_rental_supply(time_period-solve_transition_dynamics,:)
        
        if (two_rental_markets==0) then
        
            do i=1-solve_transition_dynamics,transition_length
        
                total_rental_supply(i,:)=sum(total_rental_supply_upload)
            
            end do
            
        end if
        
        if (two_rental_markets==0) then
        
            open(unit=39,file="total_rental_demand"//trim(old_results)//".dat",action='read')
                read(39,'(F15.12)') total_rental_supply_upload(:)
            close(39)
            
        else
        
            open(unit=39,file="total_rental_demand"//trim(old_results)//".dat",action='read')
                read(39,'(F15.12)') total_rental_demand(time_period-solve_transition_dynamics,:)
            close(39)
            
        end if
        
        if (two_rental_markets==0) then
        
            do i=1-solve_transition_dynamics,transition_length
        
                total_rental_demand(i,:)=sum(total_rental_supply_upload)
            
            end do
            
        end if
                        
        open(unit=40,file="housing_stock"//trim(old_results)//".dat",action='read')
            read(40,'(F15.12)') total_housing_demand(time_period-solve_transition_dynamics)
        close(40)
        
        if (two_rental_markets==0) then
        
            do i=1-solve_transition_dynamics,transition_length
        
                total_housing_demand(i)=total_housing_demand(time_period-solve_transition_dynamics)
            
            end do
            
        end if
    
        open(unit=39,file="risk_free_rate"//trim(old_results)//".dat",action='read')
            read(39,'(F15.14)') risk_free_rate(time_period)
        close(39)
        
        open(unit=39,file="mortgage_interest_rate"//trim(old_results)//".dat",action='read')
            read(39,'(F15.14)') mortgage_interest_rate(time_period)
        close(39)

        open(unit=18,file="average_labor_income"//trim(old_results)//".dat",action='read')
            read(18,'(F15.12)') model_average_labour_income(time_period)
        close(18)
        
        open(unit=39,file="smallest_financial_chosen_in_equilibrium"//trim(old_results)//".dat",action='read')
            read(39,'(F15.11)') smallest_financial_chosen_in_equilibrium_calibrated
        close(39)

        open(unit=18,file="largest_financial_chosen_in_equilibrium"//trim(old_results)//".dat",action='read')
            read(18,'(F15.11)') largest_financial_chosen_in_equilibrium_calibrated
        close(18)
        
        open(unit=19,file="housing_status_grid_expander"//trim(old_results)//".dat",action='read')
            read(19,'(F15.13)') housing_status_grid_expander
        close(19)
        
        open(unit=20,file="intergenerational_employment_process"//trim(old_results)//".dat",action='read')
            read(20,'(F20.19)') intergenerational_employment_process
        close(20)
        
        average_labor_income_calibrated=model_average_labour_income(time_period)
                
        if (solve_transition_dynamics==1) then
        
            open(unit=38,file="total_rental_supply"//trim(long_run_simulation)//".dat",action='read')
                read(38,'(F15.13)') total_rental_supply(transition_length,:)
            close(38)
                        
            open(unit=39,file="total_rental_demand"//trim(long_run_simulation)//".dat",action='read')
                read(39,'(F15.13)') total_rental_demand(transition_length,:)
            close(39)
                        
            if (use_old_transition_results==1) then
            
!                open(unit=22,file="total_rental_demand_in_transition"//trim(old_transition_resuls)//".txt",action='read')
!                    read(22,'(F15.8)') total_rental_demand
!                close(22)
                                    
            end if
            
            open(unit=40,file="housing_stock"//trim(long_run_simulation)//".dat",action='read')
                read(40,'(F15.12)') total_housing_demand(transition_length)
            close(40)

            housing_stock(transition_length)=total_housing_demand(transition_length)

            open(unit=39,file="risk_free_rate"//trim(long_run_simulation)//".dat",action='read')
                read(39,'(F15.14)') risk_free_rate(transition_length)
            close(39)
            
            open(unit=22,file="housing_price"//trim(long_run_simulation)//".dat",action='read')
                read(22,'(F15.12)') housing_price(transition_length)
            close(22)
            
            if (use_old_transition_results==1) then
            
!                open(unit=22,file="housing_price_in_transition"//trim(old_transition_resuls)//".txt",action='read')
!                    read(22,'(F15.8)') house_price_path_to_upload
!                close(22)
!                
!                housing_price(1-solve_transition_dynamics:transition_length-1)=&
!                    house_price_path_to_upload(1-solve_transition_dynamics:transition_length-1)
                
                if (transition_length==50) then
                                        
    !                 housing_price(1:transition_length)=&
    !                      (/13.041107,13.114704,13.126625,13.126839,13.127742,13.127653,13.127916,13.127622,13.127761,&
    !                        13.127910,13.127709,13.127834,13.127861,13.127859,13.127664,13.127770,13.127851,13.127974,&
    !                        13.127783,13.127539,13.127744,13.127885,13.127873,13.127689,13.128012,13.127921,13.127932,&
    !                        13.127871,13.127711,13.127718,13.127428,13.127425,13.127548,13.127466,13.128055,13.127601,&
    !                        13.127728,13.127881,13.127757,13.127724,13.127594,13.127795,13.128068,13.127644,13.127847,&
    !                        13.127597,13.127908,13.127263,13.126763,13.126554,13.123998,13.118515,13.116499,13.115342,&
    !                        13.114380,13.113955,13.113886,13.113934,13.114014,13.114203,13.114305,13.114377,13.114285,&
    !                        13.114007,13.113740,13.113662,13.113587,13.113590,13.113571,13.113582,13.113064,13.113107,&
    !                        13.112103,13.108264,13.106005/)
                        
                end if
                
            end if

            long_run_stationary_housing_price=housing_price(transition_length)
                            
            open(unit=23,file="rent"//trim(long_run_simulation)//".dat",action='read')
                read(23,'(F15.14)') rent(transition_length,:)
            close(23)
            
            if (use_old_transition_results==1) then
            
!                open(unit=23,file="rent_in_transition"//trim(old_transition_resuls)//".txt",action='read')
!                    read(23,'(F15.8)') rent_path_to_upload(0:transition_length)
!                close(23)
!                
!                rent(1-solve_transition_dynamics:transition_length-1,1)=&
!                    rent_path_to_upload(1-solve_transition_dynamics:transition_length-1)
                            
                if (transition_length==50) then
                
    !                rent(1:transition_length,1)=&
    !                  (/0.726564480,0.736556720,0.738740420,0.739224540,0.740293850,0.740113940,0.740118140,0.739830050,&
    !                  0.739761090,0.739524080,0.738892860,0.739161740,0.739055400,0.738924460,0.739024880,0.739048080,&
    !                  0.739090530,0.739573890,0.739594100,0.739935210,0.740069190,0.740091760,0.740094520,0.740350990,&
    !                  0.740055590,0.740278620,0.740169140,0.740213630,0.740252000,0.740306260,0.740135580,0.739957410,&
    !                  0.739834640,0.739392250,0.739245250,0.738874220,0.738592750,0.738359330,0.738234460,0.738071790,&
    !                  0.737718710,0.737473560,0.737231170,0.736898430,0.736700620,0.736631060,0.736479130,0.736369110,&
    !                  0.736216990,0.736181560,0.736263150,0.736253550,0.736356410,0.736494780,0.736557380,0.736561750,&
    !                  0.736526840,0.736565640,0.736572270,0.736559430,0.736563360,0.736548790,0.736546600,0.736556940,&
    !                  0.736529630,0.736493160,0.736457650,0.736553330,0.736516010,0.736658030,0.736328580,0.736100620,&
    !                  0.736094260,0.737016750,0.736271080/)
                        
                end if
                        
            end if
        
            long_run_rent(:)=rent(transition_length,:)
                            
            open(unit=24,file="housing_stock"//trim(long_run_simulation)//".dat",action='read')
                read(24,'(F15.12)') housing_stock(transition_length)
            close(24)
            
            long_run_housing_stock=housing_stock(transition_length)
                        
            open(unit=23,file="lump_sum_transfer"//trim(long_run_simulation)//".dat",action='read')
                read(23,'(F15.14)') lump_sum_transfer(transition_length)
            close(23)       
            
            if (use_old_transition_results==1) then
            
!                open(unit=23,file="lump_sum_transfer_in_transition"//trim(old_transition_resuls)//".txt",action='read')
!                    read(23,'(F15.8)') lump_sum_transfer(1-solve_transition_dynamics:transition_length)
!                close(23)
                
                if (transition_length==50) then
                    
!                    lump_sum_transfer(1:transition_length)=&
!                        (/0.000208590,0.000523710,0.000616230,0.000665130,0.000680170,0.000674900,0.000692730,0.000673240,&
!                        0.000690790,0.000705100,0.000666710,0.000692830,0.000713110,0.000707740,0.000676330,0.000719680,&
!                        0.000718150,0.000740360,0.000703390,0.000680840,0.000731710,0.000727320,0.000753260,0.000731800,&
!                        0.000770550,0.000758910,0.000769430,0.000775350,0.000738170,0.000755190,0.000736920,0.000733410,&
!                        0.000764890,0.000752370,0.000814300,0.000750750,0.000773230,0.000799150,0.000767870,0.000772980,&
!                        0.000756820,0.000813730,0.000839190,0.000758200,0.000810360,0.000790320,0.000822710,0.000818260,&
!                        0.000801750,0.000857220,0.000824620,0.000775520,0.000749900,0.000741570,0.000727480,0.000722300,&
!                        0.000720840,0.000720660,0.000720940,0.000721000,0.000722140,0.000721670,0.000723700,0.000720850,&
!                        0.000722810,0.000729920,0.000744430,0.000733090,0.000739210,0.000737460,0.000740690,0.000739090,&
!                        0.000733970,0.000711430,0.000708480/)
                          
                end if
                
            end if
                    
        end if
        
        write(*,*) "finished uploading vars"
                
    end subroutine
    
    subroutine write_outputs(time_period)
    
        use Global_Vars  
        
        implicit none
        
        integer :: time_period
        real(8), dimension(employment_grid_size) :: sum_of_distribution_employment_all,&
            sum_of_distribution_employment,sum_of_youngest_employment
        real(8) :: sum_of_retired,sum_of_non_retired,sum_at_top_financial,sum_of_youngest
        character*1 :: string_used
        character*1 :: string_used_1,string_used_1_2
        character*2 :: string_used_2
        character*3 :: string_used_3
        
        sum_of_distribution=0
        sum_of_retired=0
        sum_of_non_retired=0
        sum_at_top_financial=0
        sum_of_youngest=0
        sum_of_youngest_employment=0
        
        do i=1-two_rental_markets,2
                
            sum_of_distribution=sum_of_distribution+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,:,:,:,:))
                
            sum_of_non_retired=sum_of_non_retired+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,1:life_span-retirement_span,:,:,:,:,:)) 
                
            sum_of_retired=sum_of_retired+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,life_span-retirement_span+1:life_span,:,:,:,:,:)) 
                
            sum_at_top_financial=sum_at_top_financial+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,:,financial_grid_size,:,:,:,:)) 
                
            sum_of_youngest=sum_of_youngest+&
                sum(distribution_stationary(i)%vector_transition_real(time_period,1,:,:,:,:,:)) 
            
        end do
        
        sum_of_distribution_employment(:)=0
        sum_of_distribution_employment_all(:)=0
        
        do employment_index=1,employment_grid_size
        
            do i=1-two_rental_markets,2
                    
                sum_of_distribution_employment(employment_index)=sum_of_distribution_employment(employment_index)+&
                    sum(distribution_stationary(i)%vector_transition_real&
                    (time_period,1:life_span-retirement_span,:,employment_index,:,:,:))
                    
                sum_of_distribution_employment_all(employment_index)=sum_of_distribution_employment_all(employment_index)+&
                    sum(distribution_stationary(i)%vector_transition_real(time_period,:,:,employment_index,:,:,:))
                    
                sum_of_youngest_employment(employment_index)=sum_of_youngest_employment(employment_index)+&
                    sum(distribution_stationary(i)%vector_transition_real&
                    (time_period,1,:,employment_index,:,:,:))
                
            end do
            
        end do
        
        open(unit=2,file="welfare_tax_code_output"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period

            write(2,*) "period", j
            write(2,*) "hours to finish simulaion", (simulation_stop_time-simulation_start_time)/3600
            write(2,*) "is it counter factual", is_counterfactual
            write(2,*) "two_rental_markets", two_rental_markets
            write(2,*) "max_tracking", max_tracking
            write(2,*) "partial_equilibrium",partial_equilibrium
            write(2,*) "adjust_prices", adjust_house_prices
            write(2,*) "adjust_rents", adjust_rents
            write(2,*) "adjust_taxes", adjust_taxes
            write(2,*) "adjust_bequests", adjust_bequests
            write(2,*) "max_debt_to_income_ratio", max_debt_to_income_ratio
            write(2,*) "max_debt_to_income_ratio_below_80", max_debt_to_income_ratio_below_80
            write(2,*) "tax_system_type", tax_system_type
            write(2,*) "fraction_of_property_taxes_in_GDS", fraction_of_property_taxes_in_GDS
            write(2,*) "include_housing_depreciation_in_housing_costs", include_housing_depreciation_in_housing_costs
            write(2,*) "have_probablistic_death", have_probablistic_death
            write(2,*) "share_of_free_market_rent_calibrated", share_of_free_market_rent_calibrated
            write(2,*) "share_of_free_market_rent", share_of_free_market_rent
            write(2,*) "infinite_elasticity_of_rental_supply", infinite_elasticity_of_rental_supply
            write(2,*) "other_rental_supply_elasticity", other_rental_supply_elasticity
            write(2,*) "rent_control_premium_data", rent_control_premium_data
            write(2,*) "target_30_perc", target_30_perc
            write(2,*) "total_rental_demand(j,1)/sum(total_rental_demand(j,:))", &
                total_rental_demand(j,1)/sum(total_rental_demand(j,:))
            write(2,*) "check_tax_incidence", check_tax_incidence
            write(2,*) "min_consumption_guaranteed", min_consumption_guaranteed
            write(2,*) "re_do_simulation_with_fixed_parameters", re_do_simulation_with_fixed_parameters
            write(2,*) "calibrate_only_rental_management_costs", calibrate_only_rental_management_costs
            write(2,*) "financial_grid_expander_negative", financial_grid_expander_negative
            write(2,*) "financial_grid_expander_positive", financial_grid_expander_positive
            write(2,*) "housing_status_grid_expander", housing_status_grid_expander
            write(2,*) "do_internal_computations", do_internal_computations
            write(2,*) "file number", file_number
            write(2,*) "benchmark_simulation", benchmark_simulation
            write(2,*) "long_run_simulation", long_run_simulation
            write(2,*) "loop_count", loop_count
            write(2,*) "smallest_rental_relative_to_lowest_income", smallest_rental_relative_to_lowest_income
            write(2,*) "elasticity_of_rental_supply_data", elasticity_of_rental_supply_data
            write(2,*) "risk_free_rate", risk_free_rate(j)
            write(2,*) "prepayment_penalty_starts_from", prepayment_penalty_starts_from
            write(2,*) "average_fraction_of_mortgage_repaid", average_fraction_of_mortgage_repaid
            write(2,*) "fraction_to_repay", fraction_to_repay
            write(2,*) "mortgage_origination_cost", mortgage_origination_cost
            write(2,*) "penalty_months_of_interest", penalty_months_of_interest
            write(2,*) "FRED_housing_services_to_GDP", FRED_housing_services_to_GDP
            write(2,*) "SCF_homeownership_rate", SCF_homeownership_rate
            write(2,*) "renters_spend_over_X_data", renters_spend_over_X_data
            write(2,*) "SCF_mortgage_to_gross_housing_wealth", SCF_mortgage_to_gross_housing_wealth
            write(2,*) "SFS_households_with_mortgage", SFS_households_with_mortgage
            write(2,*) "aftr_how_mny_lps_start_update", aftr_how_mny_lps_start_update
            write(2,*) "negative_consumption", negative_consumption
            write(2,*) "weight_on_housing_price", weight_on_housing_price
            write(2,*) "average_welfare_of_youngest_cohort", average_welfare_of_youngest_cohort(j)
            write(2,*) "frac_in_support_youngest", frac_in_support_youngest(j)
            write(2,*) "average_welfare_of_retired", average_welfare_of_retired(j)
            write(2,*) "frac_in_support_retired", frac_in_support_retired(j)
            write(2,*) "average_welfare_of_all_cohorts", average_welfare_of_all_cohorts(j)
            write(2,*) "frac_in_support", frac_in_support(j)
            write(2,*) "frac_in_support_homeowners", frac_in_support_homeowners(j)
            write(2,*) "frac_in_support_renters", frac_in_support_renters(j)
            write(2,*) "housing_price", housing_price(j)
            write(2,*) "housing_price/calibrated_stationary_housing_price", &
                housing_price(j)/calibrated_stationary_housing_price
            write(2,*) "calibrated_stationary_housing_price", calibrated_stationary_housing_price
            write(2,*) "long_run_stationary_housing_price", long_run_stationary_housing_price
            write(2,*) "model_homeownership_rate", model_homeownership_rate(j)
            write(2,*) "model_years_in_same_rental", model_years_in_same_rental(j,:)
            do i=1-two_rental_markets,1
                write(2,*) "total_rental_demand", total_rental_demand(j,i)
                write(2,*) "total_rental_supply", total_rental_supply(j,i)
                write(2,*) "rent_to_housing_price", rent_to_housing_price
                write(2,*) "model_average_rent", model_average_rent(j,i)
            end do
            write(2,*) "total_rental_supply", sum(total_rental_supply(j,:))
            do i=1-two_rental_markets,1
                write(2,*) "rent", rent(j,i)
                write(2,*) "rent_calibrated", rent_calibrated(i)
                write(2,*) "rent/rent_calibrated", rent(j,i)/rent_calibrated(i) 
            end do
            do i=1-two_rental_markets,2
                write(2,*) "model_average_rent", model_average_rent(j,i)
            end do            
            write(2,*) "rent_premium", rent(j,1)/model_average_rent(j,1-two_rental_markets)
            do i=1-two_rental_markets,1           
                write(2,*) "model_average_income_by_market", model_average_income_by_market(j,i)
            end do
            write(2,*) "model_average_income_by_market ratio", &
                model_average_income_by_market(j,1-two_rental_markets)/model_average_income_by_market(j,1)
            do i=1-two_rental_markets,1           
                write(2,*) "model_average_age_by_market", model_average_age_by_market(j,i)
            end do
            write(2,*) "max_years_random", max_years_random
            write(2,*) "include_shocks_to_free_market_rent", include_shocks_to_free_market_rent
            write(2,*) "rent_shock_grid_size", rent_shock_grid_size
            write(2,*) "shock_to_rent", shock_to_rent
            write(2,*) "probability_of_rent_shock", probability_of_rent_shock
            write(2,*) "model_average_rent(:)/average_rent_calibrated(:)", model_average_rent(j,:)/average_rent_calibrated(:)
            write(2,*) "rent_decrease_per_year", rent_decrease_per_year
            write(2,*) "share_of_free_market_rent", share_of_free_market_rent
            write(2,*) "proportion_inheritance", proportion_inheritance
            write(2,*) "share_of_free_market_rent_calibrated", share_of_free_market_rent_calibrated
            write(2,*) "rent/housing_price", model_average_rent(j,2)/housing_price(j)
            write(2,*) "model_fraction_of_property_tax_paid_by_renters", model_fraction_of_property_tax_paid_by_renters(j)
            write(2,*) "model_fraction_living_with_parents", model_fraction_living_with_parents(j)
            write(2,*) "model_fraction_living_with_parents_above_max_age", &
                model_fraction_living_with_parents_above_max_age(j)
            write(2,*) "model_fraction_homeless (%)", 100*model_fraction_homeless(j)
            write(2,*) "model_fraction_homeless_not_lowest_employment", model_fraction_homeless_not_lowest_employment(j)
            write(2,*) "c_1", c_1
            write(2,*) "c_1_calibrated", c_1_calibrated
            write(2,*) "c_min", c_min
            write(2,*) "c_min_calibrated", c_min_calibrated
            write(2,*) "rental_management_costs", rental_management_costs
            write(2,*) "rental_management_costs_calibrated", rental_management_costs_calibrated
            write(2,*) "rental_rental_management_costs*disutility_of_landlords*total_rental_supply(j)**&
                (disutility_of_landlords-1)/rent(j)", &
                (rental_management_costs*disutility_of_landlords*&
                sum(total_rental_supply(j,1-two_rental_markets:1))**(disutility_of_landlords-1))/&
                model_average_rent(j,2)
            write(2,*) "cutoff_housing_investment", cutoff_housing_investment
            write(2,*) "housing_investment", housing_investment(j)
            write(2,*) "cutoff_housing_investment_calibrated", cutoff_housing_investment_calibrated
            write(2,*) "housing_stock", housing_stock(j)
            write(2,*) "calibrated_housing_stock", calibrated_housing_stock 
            write(2,*) "long_run_housing_stock", long_run_housing_stock
            write(2,*) "total_housing_demand", total_housing_demand(j)
            write(2,*) "housing grid size", housing_status_grid_size
            write(2,*) "smallest_house_size", smallest_house_size
            write(2,*) "size_move_back", size_move_back
            write(2,*) "size_homeless", size_homeless
            write(2,*) "largest_house_size", largest_house_size
            write(2,*) "index_of_smallest_house_hhld_can_buy", index_of_smallest_house_hhld_can_buy
            write(2,*) "housing_size(index_of_smallest_house_hhld_can_buy)", &
                housing_size(index_of_smallest_house_hhld_can_buy)
            write(2,*) "largest_house_size_hhld_can_rent", largest_house_size_hhld_can_rent
            write(2,*) "total_housing_demand", total_housing_demand(j)
            write(2,*) "empirical housing_stock_elasticity", empirical_housing_supplpy_elasticity
            write(2,*) "total_constrcution_profits", total_constrcution_profits(j)
            write(2,*) "total_rental_company_profits", total_rental_company_profits(j)
            write(2,*) "model_homeownership rate youngest cohort", model_homeownership_rate_young(j)
            do i=1,life_span
                write(2,*) "labour_deterministic_efficiency(",i,")", labour_deterministic_efficiency(i)
            end do
            write(2,*) "model_total_income", &
                model_output(j)+model_total_investment_income(j)
            write(2,*) "model_total_expenditures", &
                (aggregate_consumption(j)+aggregate_housing_consumption(j)+&
                aggregate_mortgage_consumption(j)+aggregate_housing_transaction_costs(j)+&
                aggregate_origination_costs(j)+aggregate_prepayment_costs(j)+aggregate_moving_costs(j)+net_govt_revenues_1(j)+&
                aggregate_rent_spending(j)-housing_price(j)*&
                sum(total_rental_demand(j-solve_transition_dynamics,:))*property_tax(j-solve_transition_dynamics))
            write(2,*) "model_mortgages", model_mortgages(j)
            write(2,*) "total income-total expenditures", &
                abs(model_output(j)+model_total_investment_income(j)-&
                (aggregate_consumption(j)+aggregate_housing_consumption(j)+&
                aggregate_mortgage_consumption(j)+aggregate_housing_transaction_costs(j)+&
                aggregate_origination_costs(j)+aggregate_prepayment_costs(j)+aggregate_moving_costs(j)+net_govt_revenues_1(j)+&
                aggregate_rent_spending(j)-housing_price(j)*&
                sum(total_rental_demand(j-solve_transition_dynamics,:))*property_tax(j-solve_transition_dynamics)))
            write(2,*) "total income-total expenditures", &
                abs(model_output(j)+model_total_investment_income(j)-&
                (aggregate_consumption(j)+aggregate_housing_consumption(j)+&
                aggregate_mortgage_consumption(j)+aggregate_housing_transaction_costs(j)+&
                aggregate_origination_costs(j)+aggregate_prepayment_costs(j)+aggregate_moving_costs(j)+net_govt_revenues_1(j)+&
                aggregate_rent_spending(j)-housing_price(j)*&
                sum(total_rental_demand(j-solve_transition_dynamics,:))*property_tax(j-solve_transition_dynamics)))
            write(2,*) "aggregate_consumption", aggregate_consumption(j)
            write(2,*) "aggregate_housing_consumption", aggregate_housing_consumption(j)
            write(2,*) "(total_housing_demand(j)-sum(total_rental_demand(j,:)))*housing_depreciation_rate*housing_price(j)", &
                (total_housing_demand(j)-sum(total_rental_demand(j,:)))*housing_depreciation_rate*housing_price(j)
            write(2,*) "aggregate_mortgage_consumption", aggregate_mortgage_consumption(j)
            write(2,*) "mortgage_interest_rate*model_mortgages", mortgage_interest_rate(j)*model_mortgages(j)
            write(2,*) "aggregate_housing_transaction_costs", aggregate_housing_transaction_costs(j)
            write(2,*) "aggregate_origination_costs", aggregate_origination_costs(j)
            write(2,*) "aggregate_prepayment_costs", aggregate_prepayment_costs(j)
            write(2,*) "(aggregate_origination_costs(j)+aggregate_prepayment_costs(j))/model_GDP_incl_imput_rent(j)", &
                (aggregate_origination_costs(j)+aggregate_prepayment_costs(j))/model_GDP_incl_imput_rent(j)
            write(2,*) "net_govt_revenues", net_govt_revenues(j)
            write(2,*) "net_govt_revenues_1", net_govt_revenues_1(j)
            write(2,*) "housing_price(j)*sum(total_rental_demand(j,:))*property_tax(j)", &
                housing_price(j-solve_transition_dynamics)*sum(total_rental_demand(j-solve_transition_dynamics,:))*&
                property_tax(j-solve_transition_dynamics)
            write(2,*) "aggregate_rent_spending", aggregate_rent_spending(j)
            write(2,*) "aggregate_rent_spending", &
                aggregate_rent_spending(j)
            write(2,*) "output", model_output(j)
            write(2,*) "GDP", model_GDP(j)
            write(2,*) "model_total_investment_income", model_total_investment_income(j)
            write(2,*) "model total_wages", model_total_efficiency_units(j)
            write(2,*) "wage to output", model_total_efficiency_units(j)/model_output(j)
            write(2,*) "model_GDP_incl_imput_rent", model_GDP_incl_imput_rent(j)
            write(2,*) "total_rent_paid/total_renters_income", (sum(total_rent_paid(j,:))/total_renters_income(j))
            do i=1,employment_grid_size
                write(2,*) "distribution_stationary(",j,":,:,",i,")", &
                    sum_of_distribution_employment(i)/sum_of_non_retired
            end do  
            do i=1,employment_grid_size
                write(2,*) "distribution_stationary_all(",j,":,:,",i,")", &
                    sum_of_distribution_employment_all(i)/sum_of_distribution
            end do  
            do i=1,employment_grid_size
                write(2,*) "distribution_stationary_youngest(",j,"1,:,",i,")", &
                    sum_of_youngest_employment(i)/sum_of_youngest
            end do  
            write(2,*) "model_housing_value_per_capita", model_housing_value_per_capita(j)
            write(2,*) "model_average_net_wealth", model_average_net_wealth(j)
            write(2,*) "total financial wealth", model_financial(j)
            write(2,*) "total net housing wealth", model_net_housing_wealth(j)
            write(2,*) "total mortgage debt", model_mortgages(j)
            write(2,*) "total net wealth", model_total_net_wealth(j)
            write(2,*) "model_total_workers", model_total_workers(j)
            write(2,*) "model_average_labour_income_tax", model_average_labour_income_tax(j)
            write(2,*) "fraction of hhlds who make it to the highest net wealth", sum_at_top_financial/sum_of_distribution
            write(2,*) "consumption tax", consumption_tax
            write(2,*) "investment_income_tax", investment_income_tax
            write(2,*) "property_tax", property_tax(j)
            write(2,*) "net_govt_revenue_constraint", net_govt_revenue_constraint
            write(2,*) "net housing wealth to total wealth", model_net_housing_wealth(j)/model_total_net_wealth(j)
            write(2,*) "NPV of gross govt revenues", total_govt_revenues(j)
            write(2,*) "NPV of net govt revenues", net_govt_revenues(j)
            write(2,*) "% of GDP raised in net govt revenues", 100*(net_govt_revenues(j)/model_GDP(j))
            write(2,*) "% of GDP raised in gross govt revenues", 100*(total_govt_revenues(j)/model_GDP(j))
            write(2,*) "% of gross govt revenues from labour income tax", &
                100*(govt_revenues_from_labour_income(j)/total_govt_revenues(j))
            write(2,*) "% of gross govt revenues from consumption tax", &
                100*(govt_revenues_from_consumption(j)/total_govt_revenues(j))
            write(2,*) "% of gross govt revenues from investment income tax", &
                100*(govt_revenues_from_investment_income(j)/total_govt_revenues(j))
            write(2,*) "% of gross govt lump sum transfers", 100*(govt_lump_sum_transfers(j)/total_govt_revenues(j))
            write(2,*) "% of gross govt_rent_subsidies", &
                100*(govt_rent_subsidies(j)/total_govt_revenues(j))
            write(2,*) "% of gross govt revenues spent on social security", &
                100*(govt_spending_on_ss(j)/total_govt_revenues(j))
            write(2,*) "govt_revenues_from_labour_income", govt_revenues_from_labour_income(j)
            write(2,*) "govt_revenues_from_consumption", govt_revenues_from_consumption(j)
            write(2,*) "govt_revenues_from_investment_income", govt_revenues_from_investment_income(j)
            write(2,*) "govt_revenues_from_property_tax", govt_revenues_from_property_tax(j)
            write(2,*) "govt_spending_homelessness", govt_spending_homelessness(j)
            write(2,*) "property_tax(j)*(total_housing_demand(j)-total_rental_demand(j))*housing_price(j)", & 
                property_tax(j-solve_transition_dynamics)*&
                (total_housing_demand(j-solve_transition_dynamics)-&
                sum(total_rental_demand(j-solve_transition_dynamics,:)))*&
                housing_price(j-solve_transition_dynamics)
            write(2,*) "property_tax(j)*total_rental_demand(j)*housing_price(j)", & 
                property_tax(j-solve_transition_dynamics)*&
                sum(total_rental_demand(j-solve_transition_dynamics,:))*&
                &housing_price(j-solve_transition_dynamics)
            write(2,*) "govt_rent_subsidies", govt_rent_subsidies(j)
            write(2,*) "govt_lump_sum_transfers", govt_lump_sum_transfers(j)
            write(2,*) "model_bequests", model_bequests(j)
            write(2,*) "model_bequests/model_net_wealth", model_bequests(j)/model_total_net_wealth(j)
            write(2,*) "risk_free_rate", risk_free_rate(j)
            write(2,*) "model average labour income", model_average_labour_income(j)
            write(2,*) "model_average_rent_to_income_ratio", model_average_rent_to_income_ratio(j)
            write(2,*) "model_ss_benefits", model_ss_benefits
            do i=1,employment_grid_size
                write(2,*) "model_ss_benefits", model_ss_benefits(i)
            end do                    
            do i=1,employment_grid_size
                write(2,*) "employment_grid(",i,")", employment_grid(1,i)
            end do
            write(2,*) "lump_sum_transfer", lump_sum_transfer
            write(2,*) "property_tax_on_rental_housing", property_tax_on_rental_housing
            write(2,*) "lump_sum_transfer/average_income", lump_sum_transfer/model_average_labour_income(j)
            write(2,*) "increase_in_labour_income_tax", increase_in_labour_income_tax
            write(2,*) "advantage_to_owning", advantage_to_owning
            write(2,*) "discount factor", discount_factor
            write(2,*) "labor_income_tax_rate", labor_income_tax_rate
            write(2,*) "curvature_of_labor_income_taxes", curvature_of_labor_income_taxes
            write(2,*) "disutility_of_landlords", disutility_of_landlords
            write(2,*) "relative_share_of_housing", relative_share_of_housing
            write(2,*) "himmelberg_etal_rent_to_price", himmelberg_etal_rent_to_price
            write(2,*) "free_market_rent_to_house_price", free_market_rent_to_house_price
            write(2,*) "model_average_rental_unit_size(j)",model_average_rental_unit_size(j)
            do i=1-two_rental_markets,1
                write(2,*) "model_average_rental_unit_size_by_market", i,"", model_average_rental_unit_size_by_market(j,i)
            end do
            write(2,*) "model_average_rental_unit_size_by_market ratio", &
                model_average_rental_unit_size_by_market(j,1-two_rental_markets)/model_average_rental_unit_size_by_market(j,1)
            do i=1-two_rental_markets,1
                write(2,*) "model_average_rent_spending_by_market", i,"", model_average_rent_spending_by_market(j,i)
            end do
            write(2,*) "model_average_owner_occupied_unit_size(j)",model_average_owner_occupied_unit_size(j)
            write(2,*) "model_average_rental_unit_size(j)/model_average_owner_occupied_unit_size(j)", &
                model_average_rental_unit_size(j)/model_average_owner_occupied_unit_size(j)
            write(2,*) "housing_price(j)*model_average_owner_occupied_unit_size(j)&
                /(model_GDP_incl_imput_rent(j)/total_population)", &
                housing_price(j)*model_average_owner_occupied_unit_size(j)/(model_GDP_incl_imput_rent(j)/total_population)
            write(2,*) "housing_price(j)*model_average_owner_occupied_unit_size(j)/model_average_labour_income(j)", &
                housing_price(j)*model_average_owner_occupied_unit_size(j)/model_average_labour_income(j)
            write(2,*) "largest_housing_size_consumed_in_equilibrium", largest_housing_size_consumed_in_equilibrium
            write(2,*) "largest_financial_chosen_in_equilibrium", largest_financial_chosen_in_equilibrium
            write(2,*) "smallest_financial_chosen_in_equilibrium", smallest_financial_chosen_in_equilibrium
            write(2,*) "fraction_with_largest_housing_size_consumed_in_eqilibrium", &
                fraction_with_largest_housing_size_consumed_in_eqilibrium
            write(2,*) "fraction_with_largest_years_holding_onto_asset_in_eqilibrium", &
                fraction_with_largest_years_holding_onto_asset_in_eqilibrium
            write(2,*) "fraction_with_smallest_financial_in_eqilibrium", fraction_with_smallest_financial_in_eqilibrium
            write(2,*) "largest income grid", max_times_labour_income_saved
            write(2,*) "financial grid points", financial_grid_size
            write(2,*) "financial grid points internal", fin_num_of_internal
            write(2,*) "multiples of income", max_times_labour_income_saved
            write(2,*) "set_rental_opportunity_probability", set_rental_opportunity_probability
            write(2,*) "probability_rental_opportunity", probability_rental_opportunity
            write(2,*) "random_assignment", random_assignment
            write(2,*) "housing depreciation rate", housing_depreciation_rate
            write(2,*) "deterioration_controlled_units", deterioration_controlled_units
            write(2,*) "model_wealth_of_owners", model_wealth_of_owners(j)
            write(2,*) "model_wealth_of_renters", model_wealth_of_renters(j)
            write(2,*) "housing depreciation expenditures", housing_price(j)*housing_depreciation_rate
            write(2,*) "model_fraction_of_homeowners_with_mortgage", model_fraction_of_homeowners_with_mortgage(j)
            write(2,*) "model fraction of households with mortgage", &
                model_fraction_of_homeowners_with_mortgage(j)*model_homeownership_rate(j)
            write(2,*) "model_fraction_of_homeowners_with_mortgage_over_80_perc", &
                model_fraction_of_homeowners_with_mortgage_over_80_perc(j)
            write(2,*) "fraction of households with mortgage ober 80 perc", &
                model_fraction_of_homeowners_with_mortgage_over_80_perc(j)*&
                model_fraction_of_homeowners_with_mortgage(j)*model_homeownership_rate(j)
            write(2,*) "total population", total_population
            write(2,*) "fraction of retired households in popualtoin", sum_of_retired/sum_of_distribution
            write(2,*) "mortgage_interest_rate", mortgage_interest_rate(j)
            write(2,*) "total efficiency units", model_total_efficiency_units(j)
            write(2,*) "model financial wealth to output", model_financial(j)/model_output(j)
            write(2,*) "model net housing wealth to output", model_net_housing_wealth(j)/model_output(j)
            write(2,*) "model total net wealth to output", model_total_net_wealth(j)/model_output(j)
            write(2,*) "model_fraction_cannot_consume_market_housing(j)", &
                model_fraction_cannot_consume_market_housing(j)
            write(2,*) "model_net_housing_wealth/model_total_net_wealth", &
                model_net_housing_wealth(j)/model_total_net_wealth(j)
            write(2,*) "model mortgage debt to gross housing wealth", model_mortgages(j)/model_gross_housing_wealth(j)
            write(2,*) "model_gross_housing_wealth", model_gross_housing_wealth(j)
            write(2,*) "model_fraction_living_in_free", 1-model_fraction_living_in_controlled(j)
            write(2,*) "life span", life_span
            write(2,*) "retirement_span", retirement_span
            write(2,*) "wealth to GDP", model_total_net_wealth(j)/model_GDP(j)
            write(2,*) "mortgage_interest_rate(j)*model_mortgages(j)", mortgage_interest_rate(j)*model_mortgages(j)
            write(2,*) "financial wealth to GDP", model_financial(j)/model_GDP(j)
            write(2,*) "net housing wealth to GDP", model_net_housing_wealth(j)/model_GDP(j)
            write(2,*) "mass_by_employment", mass_by_employment
            write(2,*) "model_total_net_wealth/model_GDP_incl_imput_rent", &
                model_total_net_wealth(j)/model_GDP_incl_imput_rent(j)
            write(2,*) "model total net wealth to GDP with imputed rents", &
                model_total_net_wealth(j)/model_GDP_incl_imput_rent(j)
            write(2,*) "model total net wealth to output", model_total_net_wealth(j)/model_output(j)
            write(2,*) "gross housing wealth to GDP", model_gross_housing_wealth(j)/model_GDP(j)
            write(2,*) "minimum_downpayment", minimum_downpayment
            write(2,*) "risk_aversion_consumption", risk_aversion_consumption
            write(2,*) "risk_aversion_housing", risk_aversion_housing
            write(2,*) "permanent labour_deterministic_efficiency slope", life_cycle_component_a
            do i=1,employment_grid_size
                write(2,*) "employment_grid(",i,")", employment_grid(1,i)
            end do
            write(2,*) "stopping_rule_stationary_distribution", stopping_rule_stationary_distribution
            write(2,*) "stopping_rule_cutoff_housing_investment", stopping_rule_cutoff_housing_investment
            write(2,*) "stopping_rule_value_function", stopping_rule_value_function
            write(2,*) "stopping_rule_housing_market", stopping_rule_housing_market
            write(2,*) "stopping_rule_rental_market", stopping_rule_rental_market
            write(2,*) "distance_value_function", distance_value_function
            write(2,*) "distance_housing_market", distance_housing_market(j)
            write(2,*) "distance_rental_market", distance_rental_market(j) 
            do i=1,number_of_quintiles-1
                write(2,*) "income_cutoffs_of_quintiles(i)", income_cutoffs_of_quintiles(i)
            end do
            do i=1,number_of_quintiles
                write(2,*) "quintile", i ,"", mass_of_quintile(i)/(include_retirees_in_income_cdf*sum_of_distribution+&
                    (1-include_retirees_in_income_cdf)*sum_of_non_retired)
                write(2,*) "mass_of_quintile", i ,"", mass_of_quintile(i)
                write(2,*) "tenure_by_income_quintile", sum(tenure_by_income_quintile(j,i,:))
            end do
            do i=1,employment_grid_size
                write(2,*) "employment_process(",1,i,")", employment_process(0,1,i,:)
            end do
            do i=1,employment_grid_size
                write(2,*) "sum(employment_process(",1,i,":)", sum(employment_process(0,1,i,:))
            end do            
            do i=1,employment_grid_size
                write(2,*) "intergenerational_employment_process(",i,")", intergenerational_employment_process(i,:)
            end do
            do i=1,employment_grid_size
                write(2,*) "sum(intergenerational_employment_process(",i,":)", sum(intergenerational_employment_process(i,:))
            end do
            do i=1,wealth_brackets
                write(2,*) "model_wealth_of_top_x_perc(",i,")/model_net_wealth",&
                    model_wealth_of_top_x_perc(i)/model_total_net_wealth(j)
                write(2,*) "stopping_rule_wealth_dist(",i,")", stopping_rule_wealth_dist(i)
            end do
            write(2,*) "gini_coefficient", gini_coefficient(j)
            write(2,*) "gini_coefficient_income", gini_coefficient_income(j)
            write(2,*) "model_average_financial", model_average_financial(j)
            write(2,*) "model_mass_with_positive_financial", model_mass_with_positive_financial(j)
            write(2,*) "total_profits", total_profits(j)
            do i=1,number_of_quintiles
                write(2,*) "welfare_of_quintile ",i," ", welfare_by_income_quintile_youngest(j,i)
            end do
            do i=1,101
                if (i<=10) then
                    write(string_used_1,'(i1)') (i-1)
                    write(2,*) "rent_to_income_cdf_"//trim(string_used_1), rent_to_income_cdf(j,i)
                elseif (i<=100) then
                    write(string_used_2,'(i2)') (i-1)
                    write(2,*) "rent_to_income_cdf_"//trim(string_used_2), rent_to_income_cdf(j,i)
                else
                    write(string_used_3,'(i3)') (i-1)
                    write(2,*) "rent_to_income_cdf_"//trim(string_used_3), rent_to_income_cdf(j,i)
                end if
            end do                  
            do i=1,12        
                if (i<10) then
                    write(string_used_1,'(i1)') i
                    write(2,*) "LTV_age_group"//trim(string_used_1), model_LTV_by_age_group(j,i)
                else
                    write(string_used_2,'(i2)') i
                    write(2,*) "LTV_age_group"//trim(string_used_2), model_LTV_by_age_group(j,i)
                end if                
            end do   
            do age_index=1,life_span
                if (age_index<10) then
                    write(string_used_1,'(i1)') age_index              
                    write(2,*) "homeownership_age"//trim(string_used_1), homeownership_by_age(j,age_index)
                else
                    write(string_used_2,'(i2)') age_index                  
                    write(2,*) "homeownership_age"//trim(string_used_2), homeownership_by_age(j,age_index)
                end if
            end do
            do age_index=1,life_span
                if (age_index<10) then
                    write(string_used_1,'(i1)') age_index                  
                    write(2,*) "homelessness_age"//trim(string_used_1), homelessness_by_age(j,age_index)
                else
                    write(string_used_2,'(i2)') age_index                   
                    write(2,*) "homelessness_age"//trim(string_used_2), homelessness_by_age(j,age_index)
                end if
            end do
            do employment_index=1,employment_grid_size
                write(string_used_1,'(i1)') employment_index
                write(2,*) "welfare_youngest_employment_index"//trim(string_used_1), &
                    welfare_by_employment_youngest(j,employment_index)
            end do
            do employment_index=1,employment_grid_size
                write(string_used_1,'(i1)') employment_index
                write(2,*) "welfare_employment_index"//trim(string_used_1), welfare_by_employment(j,employment_index)
            end do
            do age_index=1,life_span
                if (age_index<10) then
                    write(string_used_1,'(i1)') age_index                
                    write(2,*) "support"//trim(string_used_1), frac_in_support_by_age(j,age_index)
                else
                    write(string_used_2,'(i2)') age_index  
                    write(2,*) "support"//trim(string_used_2), frac_in_support_by_age(j,age_index)
                end if
            end do
            do age_index=1,life_span
                if (age_index<10) then
                    write(string_used_1,'(i1)') age_index                    
                    write(2,*) "welfare_age"//trim(string_used_1), welfare_by_age(j,age_index)
                else
                    write(string_used_2,'(i2)') age_index   
                    write(2,*) "welfare_age"//trim(string_used_2), welfare_by_age(j,age_index)
                end if                        
            end do
            write(2,*) "tenure by income quintile"
            do i=1-two_rental_markets,3  
                write(string_used_1,'(i1)') i
                do k=1,number_of_quintiles  
                    write(string_used_1_2,'(i1)') k
                    write(2,*) "market_"//trim(string_used_1)//"_quintile_"//trim(string_used_1_2),&
                        100*(tenure_by_income_quintile(j,k,i)/mass_of_quintile(k))
                end do
                
            end do
        end do
        
        close(2)
        
        open(unit=2,file="rent_to_income_cdf"//trim(file_number)//".txt",action="write",status="replace")
            
        do j=1,time_period
              
            do i=1,101
                write(2,*) rent_to_income_cdf(j,i)
            end do 
            
        end do
            
        close(2)
        
        open(unit=2,file="homelessness_by_age"//trim(file_number)//".txt",action="write",status="replace")
              
        do j=1,time_period
        
            do i=1,life_span
                write(2,*) homelessness_by_age(j,i)
            end do 
            
        end do
            
        close(2)
                
        open(unit=2,file="welfare_by_income_quintile_youngest"//trim(file_number)//".txt",action="write",status="replace")
                    
        do j=1,time_period
                    
            do i=1,number_of_quintiles
                write(2,*) welfare_by_income_quintile_youngest(j,i)
            end do 
            
        end do
            
        close(2)
            
        open(unit=2,file="model_share_in_controlled_market_by_age"//trim(file_number)//".txt",action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                                   
                write(2,*) model_share_in_controlled_market_by_age(j,age_index)

            end do
            
        end do
        
        close(2)
        
        open(unit=2,file="model_share_in_free_market_by_age"//trim(file_number)//".txt",action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                                   
                write(2,*) model_share_in_free_market_by_age(j,age_index)

            end do
            
        end do
        
        close(2)
        
        open(unit=2,file="average_age_of_homeowner_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do employment_index=1,employment_grid_size
                                   
                write(2,*) average_age_of_homeowner_by_employment(j,employment_index)

            end do
            
        end do
        
        close(2)
        
        open(unit=2,file="model_share_in_controlled_market_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do employment_index=1,employment_grid_size
                                   
                write(2,*) model_share_in_controlled_market_by_employment(j,employment_index)

            end do
            
        end do
        
        close(2)
        
        open(unit=3,file="mortgage_to_house_value_ratio_by_age"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                            
                write(3,*) mortgage_to_house_value_ratio_of_owners_by_age(j,age_index)
                        
            end do
            
        end do
        
        close(3)
        
        open(unit=3,file="model_share_in_market_by_age_and_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
        
            do i=1,1-two_rental_markets,-1
                    
                do age_index=1,life_span
                                
                    do employment_index=1,employment_grid_size
                                    
                        write(3,*) model_share_in_market_by_age_and_employment(j,age_index,employment_index,i)
                    
                    end do
                
                end do
                        
            end do
            
        end do
        
        close(3)
        
        open(unit=3,file="model_share_with_parents_by_age_and_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
        
            do i=1,1-two_rental_markets,-1
                    
                do age_index=1,life_span
                                
                    do employment_index=1,employment_grid_size
                                    
                        write(3,*) model_share_with_parents_by_age_and_employment(j,age_index,employment_index,i)
                    
                    end do
                
                end do
                        
            end do
            
        end do
        
        close(3)
                
        open(unit=3,file="model_LTV_by_age_group"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do i=1,12
        
                write(3,*) model_LTV_by_age_group(j,i)
        
            end do
            
        end do
        
        close(3)
            
        open(unit=3,file="percent_with_mortgage_by_age"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                            
                write(3,*) mass_mortgage_to_house_value_ratio_of_owners_by_age&
                    (j,age_index)
                        
            end do
            
        end do
        
        close(3)
            
        open(unit=3,file="average_wealth_by_age_renter"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                            
                write(3,*) wealth_by_age_renter(j,age_index)
                        
            end do
            
        end do
        
        close(3)
        
        open(unit=3,file="income_by_age_renter"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                            
                write(3,*) income_by_age_renter(j,age_index)
                        
            end do
            
        end do
        
        close(3)
        
        open(unit=3,file="consumption_by_age_renter"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                            
                write(3,*) consumption_by_age_renter(j,age_index)
                        
            end do
            
        end do
        
        close(3)
        
        open(unit=3,file="consumption_by_age"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                            
                write(3,*) consumption_by_age(j,age_index)
                        
            end do
            
        end do
        
        close(3)
        
        open(unit=3,file="housing_consumption_by_age_renter"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do j=1,time_period
                    
            do age_index=1,life_span
                            
                write(3,*) housing_consumption_by_age_renter(j,age_index)
                        
            end do
            
        end do
        
        close(3)
                
        open(unit=3,file="average_rent_by_age"//trim(file_number)//".txt",&
            action="write",status="replace")
                    
        do age_index=1,life_span
                            
            write(3,*) average_rent_by_age(age_index)
                        
        end do
        
        close(3)
                
        open(unit=3,file="welfare_by_age"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
            
            do age_index=1,life_span
                            
                write(3,*) welfare_by_age(j,age_index)
                        
            end do
                
        end do
            
        close(3)
        
        open(unit=3,file="welfare_by_employment_and_age"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
        
            do employment_index=1,employment_grid_size
            
                do age_index=1,life_span
                                
                    write(3,*) welfare_by_employment_and_age(j,employment_index,age_index)
                       
                end do
                
            end do
                
        end do
            
        close(3)

        open(unit=4,file="welfare_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) welfare_by_employment(j,employment_index)
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="average_tenure_duration_of_renter_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) average_tenure_duration_of_renter_by_employment(j,employment_index)
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="welfare_by_employment_youngest"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) welfare_by_employment_youngest(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="welfare_by_employment_youngest_old_dist"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) welfare_by_employment_youngest_old_dist(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="MID_share_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) MID_share_by_employment(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="wealth_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) wealth_by_employment(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="fraction_with_mortgage"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) 100*fraction_with_mortgage(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="fraction_renters"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) 100*fraction_renters_by_employment(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        do i=1,number_of_thresholds
        
            write(string_used,'(i1)') i
        
            open(unit=4,file="model_rent_burden_by_age"//trim(string_used)//trim(file_number)//".txt",&
                action="write",status="replace")
            
            do j=time_period,time_period
                        
                do age_index=1,life_span
                            
                    write(4,*) 100*model_rent_burden_by_age(j,age_index,i)
                       
                end do
                    
            end do
                
            close(4)
            
        end do
                
        open(unit=4,file="average_age_of_homeowner"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) average_age_of_homeowner(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="average_income"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) average_income(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="average_house_inheritance"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) average_house_inheritance(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="average_house_non_inheritance"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) average_house_non_inheritance(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="fraction_with_inheritance"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) mass_inheritance(j,employment_index)/(mass_inheritance(j,employment_index)+&
                    mass_non_inheritance(j,employment_index))
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="average_housing_costs"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) average_housing_costs(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
        
        open(unit=4,file="average_consumption"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                        
                write(4,*) average_consumption(j,employment_index)
                   
            end do
                
        end do
            
        close(4)
                
        open(unit=4,file="welfare_by_employment_and_tenure"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
            
                do i=1,3
                        
                    write(4,*) welfare_by_employment_and_tenure(j,employment_index,i)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="frac_in_support_by_age"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do age_index=1,life_span
                                    
                write(4,*) frac_in_support_by_age(j,age_index)
                                           
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="wealth_by_employment_and_tenure"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
        
            do i=1,3
                    
                do employment_index=1,employment_grid_size
                                    
                    write(4,*) wealth_by_employment_and_tenure(j,employment_index,i)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="consumption_by_employment_and_tenure"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
            
                do i=1,3
                        
                    write(4,*) consumption_by_employment_and_tenure(j,employment_index,i)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="housing_size_by_employment_and_tenure"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
            
            do i=1,3
                    
                do employment_index=1,employment_grid_size
                                    
                    write(4,*) housing_size_by_employment_and_tenure(j,employment_index,i)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="mortgag_by_employment_and_tenure"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
            
                do i=1,3
                        
                    write(4,*) mortgage_by_employment_and_tenure(j,employment_index,i)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="mortgag_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
                                    
                write(4,*) mortgage_by_employment(j,employment_index)
                                           
            end do
                
        end do
        
        close(4)
                    
        open(unit=4,file="support_by_employment_and_tenure"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
        
            do i=1-two_rental_markets,3
                    
                do employment_index=1,employment_grid_size
                                    
                    write(4,*) 100*frac_in_support_by_tenure_and_employment(j,employment_index,i)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="welfare_by_employment_and_age_renter"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                            
            do employment_index=1,employment_grid_size
            
                do age_index=1,int(real(life_span)/real(10))
                                        
                    write(4,*) welfare_by_employment_tenure_and_age(j,employment_index,1,age_index)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="welfare_by_employment_and_age_owner_w_m"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                            
            do employment_index=1,employment_grid_size
            
                do age_index=1,int(real(life_span)/real(10))
                                        
                    write(4,*) welfare_by_employment_tenure_and_age(j,employment_index,2,age_index)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
        
        open(unit=4,file="welfare_by_employment_and_age_owner_wo_m"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                            
            do employment_index=1,employment_grid_size
            
                do age_index=1,int(real(life_span)/real(10))
                                        
                    write(4,*) welfare_by_employment_tenure_and_age(j,employment_index,3,age_index)
                        
                end do
                   
            end do
                
        end do
        
        close(4)
            
        open(unit=4,file="percentage_by_tenure"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                    
            do employment_index=1,employment_grid_size
            
                do i=1,3
                        
                    write(4,*) 100*(mass_by_employment_and_tenure(j,employment_index,i)/&
                        mass_by_employment(j,employment_index))
                        
                end do
                   
            end do
                
        end do    
            
        close(4)
        
        open(unit=8,file="homeownership_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                
            do employment_index=1,employment_grid_size
                            
                write(8,*) homeownership_by_employment(j,employment_index)
                        
            end do
                
        end do
            
        close(8)
        
        sum_of_distribution_employment(:)=0
        
        do employment_index=1,employment_grid_size
        
            do i=1-two_rental_markets,2
                    
                sum_of_distribution_employment(employment_index)=&
                    sum_of_distribution_employment(employment_index)+&
                    sum(distribution_stationary(i)%vector_transition_real&
                    (time_period,:,:,employment_index,:,:,:))
                
            end do
            
        end do
        
        open(unit=8,file="tenure_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
        
            do i=1-two_rental_markets,3
                
                do employment_index=1,employment_grid_size
                            
                    write(8,*) 100*tenure_by_employment(j,employment_index,i)/&
                        sum_of_distribution_employment(employment_index)
                
                end do
                
            end do
                
        end do
            
        close(8)
        
        open(unit=8,file="tenure_by_income_quintile"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
        
            do i=1-two_rental_markets,3
                
                do k=1,number_of_quintiles
                            
                    write(8,*) 100*(tenure_by_income_quintile(j,k,i)/mass_of_quintile(k))
                
                end do
                
            end do
                
        end do
            
        close(8)
        
        open(unit=9,file="housing_size_by_employment"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                                
            do employment_index=1,employment_grid_size
                
                write(9,*) housing_size_by_employment(j,employment_index)
                                    
            end do
                
        end do
            
        close(9)
        
        open(unit=9,file="housing_size_by_employment_and_age"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                                
            do employment_index=1,employment_grid_size
            
                do age_index=1,life_span
                
                    write(9,*) housing_size_by_employment_and_age(j,employment_index,age_index)
                        
                end do
                                    
            end do
                
        end do
            
        close(9)
        
        open(unit=9,file="consumption_by_employment_and_age"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                                
            do employment_index=1,employment_grid_size
            
                do age_index=1,life_span
                
                    write(9,*) consumption_by_employment_and_age(j,employment_index,age_index)
                        
                end do
                                    
            end do
                
        end do
            
        close(9)
        
        open(unit=9,file="wealth_by_employment_and_age"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                                
            do employment_index=1,employment_grid_size
            
                do age_index=1,life_span
                
                    write(9,*) wealth_by_employment_and_age(j,employment_index,age_index)
                        
                end do
                                    
            end do
                
        end do
            
        close(9)
        
        open(unit=10,file="homeownership_by_age"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                        
            do age_index=1,life_span
                                            
                write(10,*) homeownership_by_age(j,age_index)
                    
            end do
                
        end do
            
        close(10)
        
        open(unit=11,file="housing_size_by_age"//trim(file_number)//".txt",&
            action="write",status="replace")
        
        do j=time_period,time_period
                        
            do age_index=1,life_span
                            
                write(11,*) housing_size_by_age(j,age_index)
                                        
            end do
                
        end do
            
        close(11)
                                                    
    end subroutine
    
    subroutine allocate_matrix()
    
        use Global_Vars
        
        implicit none
        
        integer :: is_owner
                
        do i=1-two_rental_markets,2
        
            if (i==2) then
            
                is_owner=1
                
            else
            
                is_owner=0
                
            end if
        
            allocate(value_function(i)%&
                vector_transition_real(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                    
            allocate(policy_function_wage_income_tax_paid(i)%&
                vector_transition_real(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(policy_function_consumption(i)%&
                vector_transition_real(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(policy_function_space_lived_in_size(i)%&
                vector_transition_real(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                                                
            allocate(policy_function_rent_subsidy(i)%&
                vector_transition_real(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(frac_mov_optimal_1(i)%&
                vector_transition_real(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                       
            allocate(distribution_guess(i)%&
                vector_transition_real((1-solve_transition_dynamics):transition_length,&
                life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(distribution_implied(i)%&
                vector_transition_real(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(distribution_stationary(i)%&
                vector_transition_real((1-solve_transition_dynamics):transition_length,life_span+1,&
                is_owner*1+(1-is_owner)*negative_financial_grid_size:financial_grid_size,employment_grid_size,&
                (1-is_owner)*lowest_housing_grid_point+is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
            
            allocate(policy_function_financial_index(i)%&
                vector_transition_integer(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                       
            allocate(policy_function_housing_status_index(i)%&
                vector_transition_integer(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(policy_function_space_lived_in_index(i)%&
                vector_transition_integer(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(policy_function_years_of_amortization_index(i)%&
                vector_transition_integer(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(policy_function_years_holding_onto_asset(i)%&
                vector_transition_integer(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                        
            allocate(fin_index_frac_mov_to_optimal(i)%&
                vector_transition_integer(transition_length,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                       
            allocate(distribution_implied_private(i)%&
                vector_cpu_real(max_CPUs,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
                                    
            allocate(value_function_guess(i)%&
                vector_youngest_real(is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking,0:random_assignment))
            
        end do
        
        do i=1-benchmark_has_two_rental_markets,2
        
            if (i==2) then
            
                is_owner=1
                
            else
            
                is_owner=0
                
            end if
        
            allocate(value_function_benchmark(i)%vector_real(life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking_benchmark,0:benchmark_has_random_assignment))
                                        
            allocate(distribution_stationary_benchmark(i)%&
                vector_real(life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                is_owner*index_of_smallest_house_hhld_can_buy:&
                housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking_benchmark,0:benchmark_has_random_assignment))
                
        end do
        
        if (solve_transition_dynamics==1) then
        
            do i=1-benchmark_has_two_rental_markets,2
            
                if (i==2) then
            
                    is_owner=1
                    
                else
                
                    is_owner=0
                    
                end if
            
                allocate(distribution_to_fetch(i)%vector_transition_real&
                    (1,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                    financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                    is_owner*index_of_smallest_house_hhld_can_buy:&
                    housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking_benchmark,0:benchmark_has_random_assignment))
                    
                allocate(policy_function_space_lived_in_size_to_fetch(i)%vector_transition_real&
                    (1,life_span+1,is_owner*1+(1-is_owner)*negative_financial_grid_size:&
                    financial_grid_size,employment_grid_size,(1-is_owner)*lowest_housing_grid_point+&
                    is_owner*index_of_smallest_house_hhld_can_buy:&
                    housing_status_grid_size-1,is_owner*1+(1-is_owner)*max_tracking_benchmark,0:benchmark_has_random_assignment))
                        
            end do
            
        end if
                    
    end subroutine
    
    subroutine cleanup_matrix()
        
        use Global_Vars
        
        implicit none
                
        do i=1-two_rental_markets,2
                    
            if (allocated(value_function(i)%vector_transition_real)) then
            
                deallocate(value_function(i)%vector_transition_real)
                
            end if
            
            if (allocated(policy_function_wage_income_tax_paid(i)%vector_transition_real)) then
            
                deallocate(policy_function_wage_income_tax_paid(i)%vector_transition_real)
                
            end if
                           
            if (allocated(policy_function_consumption(i)%vector_transition_real)) then
            
                deallocate(policy_function_consumption(i)%vector_transition_real)
                
            end if
                      
            if (allocated(policy_function_space_lived_in_size(i)%vector_transition_real)) then
            
                deallocate(policy_function_space_lived_in_size(i)%vector_transition_real)
                
            end if
                        
            if (allocated(policy_function_rent_subsidy(i)%vector_transition_real)) then
            
                deallocate(policy_function_rent_subsidy(i)%vector_transition_real)
                
            end if
            
            if (allocated(frac_mov_optimal_1(i)%vector_transition_real)) then
            
                deallocate(frac_mov_optimal_1(i)%vector_transition_real)
                
            end if
            
            if (allocated(distribution_guess(i)%vector_transition_real)) then
            
                deallocate(distribution_guess(i)%vector_transition_real)
                
            end if
            
            if (allocated(distribution_implied(i)%vector_transition_real)) then
            
                deallocate(distribution_implied(i)%vector_transition_real)
                
            end if
            
            if (allocated(distribution_stationary(i)%vector_transition_real)) then
            
                deallocate(distribution_stationary(i)%vector_transition_real)
                
            end if
            
            if (allocated(policy_function_financial_index(i)%vector_transition_integer)) then
            
                deallocate(policy_function_financial_index(i)%vector_transition_integer)
                
            end if
            
            if (allocated(policy_function_housing_status_index(i)%vector_transition_integer)) then
            
                deallocate(policy_function_housing_status_index(i)%vector_transition_integer)
                
            end if
                        
            if (allocated(policy_function_space_lived_in_index(i)%vector_transition_integer)) then
            
                deallocate(policy_function_space_lived_in_index(i)%vector_transition_integer)
                
            end if
            
            if (allocated(policy_function_years_of_amortization_index(i)%vector_transition_integer)) then
            
                deallocate(policy_function_years_of_amortization_index(i)%vector_transition_integer)
                
            end if
                        
            if (allocated(policy_function_years_holding_onto_asset(i)%vector_transition_integer)) then
            
                deallocate(policy_function_years_holding_onto_asset(i)%vector_transition_integer)
                
            end if
                      
            if (allocated(fin_index_frac_mov_to_optimal(i)%vector_transition_integer)) then
            
                deallocate(fin_index_frac_mov_to_optimal(i)%vector_transition_integer)
                
            end if
            
            if (allocated(distribution_implied_private(i)%vector_cpu_real)) then
            
                deallocate(distribution_implied_private(i)%vector_cpu_real)
                
            end if
                        
            if (allocated(value_function_guess(i)%vector_youngest_real)) then
            
                deallocate(value_function_guess(i)%vector_youngest_real)
                
            end if
                                                   
        end do
        
        do i=1-benchmark_has_two_rental_markets,2
        
            if (allocated(value_function_benchmark(i)%vector_real)) then
                
                deallocate(value_function_benchmark(i)%vector_real)
                
            end if
            
            if (allocated(distribution_stationary_benchmark(i)%vector_real)) then
            
                deallocate(distribution_stationary_benchmark(i)%vector_real)
                
            end if
            
        end do
        
        if (solve_transition_dynamics==1) then
        
            do i=1-benchmark_has_two_rental_markets,2
            
                if (allocated(distribution_to_fetch(i)%vector_transition_real)) then
                
                    deallocate(distribution_to_fetch(i)%vector_transition_real)
                    
                end if
                
                if (allocated(policy_function_space_lived_in_size_to_fetch(i)%vector_transition_real)) then
                
                    deallocate(policy_function_space_lived_in_size_to_fetch(i)%vector_transition_real)
                    
                end if
            
            end do
            
        end if

    end subroutine
    
    subroutine calibrating_model()
    
        use Global_Vars
        
        implicit none
        
        integer :: largest_value_youngest,bequest_from_childs_utility
        logical :: file_exists        
        real(8) :: mass_at_top_financial
                                
        simulation_ended=0
                                        
        call cpu_time(simulation_start_time)
        
        lump_sum_transfer(1)=0
        
        model_average_owner_occupied_unit_size(1)=average_owner_occupied_house_size_calibrated

        loop_count=0                      
        
        call initialize_variables_and_grids()
                    
        if (use_old_results==1) then
        
            call upload_vars(1)
            
        else
        
            housing_price(1)=calibrated_stationary_housing_price
        
            if ((1-two_rental_markets)==0) then

                rent(1,max(0,(1-two_rental_markets)))=free_market_rent_to_house_price*housing_price(1)
                rent(1,1)=rent(1,max(0,(1-two_rental_markets)))*1.14
                
            else
            
                rent(1,1)=rent_to_housing_price*housing_price(1)

            end if
                                          
            risk_free_rate(1)=calibrated_risk_free_rate
            mortgage_interest_rate(1)=risk_free_rate(1)+mortgage_interest_gap
            
            model_average_labour_income(1)=average_labor_income_calibrated
            total_housing_demand(1)=calibrated_housing_stock
            
            total_rental_demand(1,:)=total_rental_supply_calibrated(:)
            total_rental_supply(1,:)=total_rental_supply_calibrated(:)
            
        end if
        
        call function_of_utility_advantage_from_homeowning(1)
        
        call allocate_matrix()
                            
        call initialize_financial_grid(1)
                               
        call initialize_feasible_distribution(1)
        call set_SS_benefits(1)
        
        call function_of_utility_advantage_from_homeowning(1)
        call initialize_financial_grid(1)
                
        distance_value_function=1
        distance_discount_factor=1
        distance_discount_factor_on_child=1
        distance_advantage_to_owning=1
        distance_relative_share_of_housing=1
        distance_rental_management_costs=1
        distance_housing_status_grid_expander=1
        distance_rent_to_price=1
        distance_smallest_house_size=1
        distance_rental_market=1
        distance_fixed_cost_to_moving=1
        distance_proportion_inheritance=1
        distance_size_move_back=1
        
        do i=1-two_rental_markets,2
        
            value_function_guess(i)%vector_youngest_real(:,:,:,:,:)=0
            value_function(i)%vector_transition_real(1,:,:,:,:,:,:)=0
            
        end do
                                                          
        write(*,*) "discount_factor=", discount_factor
        write(*,*) "labor_income_tax_rate=", labor_income_tax_rate
        write(*,*) "curvature_of_labor_income_taxes=", curvature_of_labor_income_taxes
        write(*,*) "rent_to_housing_price=", rent_to_housing_price  
        write(*,*) "max_amortization_years=", max_amortization_years
        write(*,*) "model_average_owner_occupied_unit_size(1)=", model_average_owner_occupied_unit_size(1)
        write(*,*) "infinite_elasticity_of_rental_supply=", infinite_elasticity_of_rental_supply
        write(*,*) "calibration"
        write(*,*) "advantage_to_owning=", advantage_to_owning
                
        if (use_old_results==0) then
        
            if (two_rental_markets==1) then
            
                rent(1,max(0,(1-two_rental_markets)))=0.76332952171358215/(1+free_market_rent_increase_due_to_shock)
                model_average_rent(1,max(0,(1-two_rental_markets)))=rent(1,max(0,(1-two_rental_markets)))*&
                    (1+free_market_rent_increase_due_to_shock)
                rent(1,1)=model_average_rent(1,max(0,(1-two_rental_markets)))*&
                    (1+rent_control_premium_data)/(1+free_market_rent_increase_due_to_shock)
                model_average_rent(1,1:2)=0.68798997590735012
                    
            else
                
                rent(1,1)=0.76332952171358215
                model_average_rent(1,1)=0.75482414805510600
                                
            end if
            
        end if
        
        call set_smallest_house_size()
        call function_of_utility_advantage_from_homeowning(1)

        INQUIRE(FILE="total_rental_demand"//trim(long_run_simulation)//".dat", EXIST=file_exists)
                
        if (file_exists) then
        
            open(unit=39,file="total_rental_demand"//trim(long_run_simulation)//".dat",action='read')
                read(39,'(F15.12)') total_rental_demand(1,:)
            close(39)
                        
            call solve_elasticity_of_rental_housing(1,disutility_of_landlords)
            
            rental_management_costs=&
                (model_average_rent(1,1-two_rental_markets)-housing_price(1)*&
                (risk_free_rate(1)+housing_depreciation_rate+property_tax(1)+&
                property_tax_on_rental_housing)/(1+risk_free_rate(1)))/((sum(total_rental_demand(1,:))**&
                (disutility_of_landlords-1))*disutility_of_landlords)
        
            open(unit=27,file="rental_management_costs"//trim(file_number)//".dat",status='replace')
                write(27,'(F30.29)') rental_management_costs
            close(27)
            
            open(unit=27,file="disutility_of_landlords"//trim(file_number)//".dat",status='replace')
                write(27,'(F15.12)') disutility_of_landlords
            close(27)
                                    
        end if
                
        write(*,*) "rent=", rent(1,:)
        write(*,*) "model_average_rent=", model_average_rent(1,:)
        write(*,*) "rental_management_costs=", rental_management_costs
        write(*,*) "rent_decrease_per_year=", rent_decrease_per_year
        write(*,*) "share_of_free_market_rent=", share_of_free_market_rent
        write(*,*) "share_of_free_market_rent_calibrated=", share_of_free_market_rent_calibrated
        write(*,*) "free_market_rent_to_house_price=", free_market_rent_to_house_price        
        write(*,*) "property_tax=", property_tax
        write(*,*) "max_tracking=", max_tracking
        write(*,*) "two_rental_markets=", two_rental_markets
        write(*,*) "max_debt_to_income_ratio=", max_debt_to_income_ratio
        write(*,*) "max_debt_to_income_ratio_below_80=", max_debt_to_income_ratio_below_80
        write(*,*) "rent(1,1)/rent(1,1-two_rental_markets)=", rent(1,1)/rent(1,1-two_rental_markets)
        write(*,*) "free_market_rent_increase_due_to_shock=", free_market_rent_increase_due_to_shock
        write(*,*) "housing_size=", housing_size
        
        write(*,*) "file_number=", file_number
        write(*,*) "old_results=", old_results
        write(*,*) "benchmark_simulation=", benchmark_simulation
        write(*,*) "probability_rental_opportunity=", probability_rental_opportunity
        
        open(unit=24,file="free_market_rent_increase_due_to_shock"//trim(file_number)//".dat",status='replace')
            write(24,'(F15.14)') free_market_rent_increase_due_to_shock
        close(24)
                                        
        do while((distance_rental_market(1)>stopping_rule_rental_market) .OR. &
            (distance_discount_factor>stopping_rule_discount_factor) .OR. &
            (distance_discount_factor_on_child>stopping_rule_discount_factor_on_child) .OR. &
            (distance_advantage_to_owning>stopping_rule_advantage_to_owning) .OR. &
            (distance_relative_share_of_housing>stopping_rule_relative_share_of_housing) .OR. &
            (distance_rental_management_costs>stopping_rule_rental_management_costs) .OR. &
            (distance_housing_status_grid_expander>stopping_rule_housing_status_grid_expander) .OR. &
            (distance_rent_to_price>stopping_rule_rent_to_price) .OR. &
            (distance_smallest_house_size>stopping_rule_smallest_house_size) .OR. &
            (distance_fixed_cost_to_moving>stopping_rule_fixed_cost_to_moving) .OR. &
            (distance_proportion_inheritance>stopping_rule_proportion_inheritance) .OR. & 
            (distance_size_move_back>stopping_rule_size_move_back) .OR. &
            (distance_size_homeless>stopping_rule_size_homeless) .OR. &
            (loop_count<((1-use_old_results)*1+use_old_results*1)))
                        
            largest_value_function_value=0
            bequest_from_childs_utility=0
            
            if (add_net_worth_bequest==2) then
            
                distance_value_function=1
                bequest_from_childs_utility=1

            else
            
                call solve_value_function_and_policy_functions(1)
            
            end if
                        
            do while (bequest_from_childs_utility*distance_value_function>&
                (stopping_rule_value_function*(1+abs(largest_value_function_value))))
                
                call solve_value_function_and_policy_functions(1)
                                        
                largest_value_youngest=1-two_rental_markets
                
                distance_value_function=0
                
                do i=1-two_rental_markets,2
                                        
                    if (maxval(abs(value_function_guess(i)%vector_youngest_real(:,:,:,:,:)-&
                        value_function(i)%vector_transition_real(1,1,:,:,:,:,:)))>&
                        distance_value_function) then
                    
                        distance_value_function=&
                            maxval(abs(value_function_guess(i)%vector_youngest_real(:,:,:,:,:)-&
                        value_function(i)%vector_transition_real(1,1,:,:,:,:,:)))
                            
                        largest_value_youngest=i
                        
                    end if
                
                end do
                
                distance_value_function_indeces_youngest=&
                    maxloc(abs(value_function_guess(largest_value_youngest)%vector_youngest_real(:,:,:,:,:)-&
                    value_function(largest_value_youngest)%vector_transition_real(1,1,:,:,:,:,:)),&
                    mask=abs(value_function_guess(largest_value_youngest)%vector_youngest_real(:,:,:,:,:)-&
                    value_function(largest_value_youngest)%vector_transition_real(1,1,:,:,:,:,:))>=0)
                
                largest_value_function_value=&
                    value_function(largest_value_youngest)%vector_transition_real&
                    (1,1,distance_value_function_indeces_youngest(1),&
                    distance_value_function_indeces_youngest(2),&
                    distance_value_function_indeces_youngest(3),&
                    distance_value_function_indeces_youngest(4),&
                    distance_value_function_indeces_youngest(5))
                    
                do i=1-two_rental_markets,2
                                                           
                    value_function_guess(i)%vector_youngest_real(:,:,:,:,:)=&
                        value_function(i)%vector_transition_real(1,1,:,:,:,:,:)
                        
                end do
                                    
                write(*,*) "distance_value_function=", distance_value_function
                                                          
            end do
            
            if (loop_count<=1) then
            
                stopping_rule_stationary_distribution=10.d0**(-8)
                
            else
            
                stopping_rule_stationary_distribution=10.d0**(-8)
            
            end if
                        
            call converge_to_stationary_equilibrium(1)
                        
            call calculate_gini_coefficient_income(1)
            call calculate_sd_log_earnings(1)
            call stationary_equilibrium_statistics(1)
            call stationary_equilibirum_homeownership_rate(1)
            call govt_revenues_calc(1)
            call calculate_aggregate_consumption(1)
            call calculate_welfare(1)
            call calculate_household_housing_spending(1)
            call calculate_welfare_by_characteristics(1)
            call calculate_homeownership_by_characteristics(1)
            
            if ((loop_count>aftr_how_mny_lps_start_update) .AND. (add_net_worth_bequest==1)) then
            
                previous_proportion_inheritance=proportion_inheritance
                
                call converge_on_proportion_inheritance(1)
                
                distance_proportion_inheritance=abs(previous_proportion_inheritance-proportion_inheritance)
                
                proportion_inheritance=proportion_inheritance_weight*proportion_inheritance+&
                    (1-proportion_inheritance_weight)*previous_proportion_inheritance
                                        
            else
            
                distance_proportion_inheritance=0
            
            end if
             
            if (re_do_simulation_with_fixed_parameters==1) then
            
                distance_discount_factor=0
                distance_discount_factor_on_child=0
                distance_housing_status_grid_expander=0
                distance_rent_to_price=0
                distance_smallest_house_size=0
                distance_advantage_to_owning=0
                distance_relative_share_of_housing=0
                distance_rental_management_costs=0
                distance_fixed_cost_to_moving=0
                distance_size_move_back=0
                distance_size_homeless=0
                        
            else
            
                if (loop_count>aftr_how_mny_lps_start_update+1) then
                
                    write(*,*) "discount_factor=", discount_factor
                
                    if (target_households_with_mortgage==0) then
                                                            
                        discount_factor=min(max(discount_factor+discount_factor_adjuster*&
                            ((model_mortgages(1)/model_gross_housing_wealth(1))-&
                            SCF_mortgage_to_gross_housing_wealth),0.d0),1.3)  ! Standard
                    
                        distance_discount_factor=abs(((model_mortgages(1)/model_gross_housing_wealth(1))-&
                            SCF_mortgage_to_gross_housing_wealth))
                                        
                    else
                                                                    
                        discount_factor=min(max(discount_factor+discount_factor_adjuster*&
                            (model_fraction_of_homeowners_with_mortgage(1)-&
                            SFS_households_with_mortgage),0.d0),1.3)  ! Standard
                    
                        distance_discount_factor=abs(model_fraction_of_homeowners_with_mortgage(1)-&
                            SFS_households_with_mortgage)
                    
                    end if
                                        
                    write(*,*) "discount_factor=", discount_factor
                    write(*,*) "distance_discount_factor=", distance_discount_factor
                    write(*,*) "model_total_net_wealth(1)/model_GDP_incl_imput_rent(1)=", &
                        model_total_net_wealth(1)/model_GDP_incl_imput_rent(1)
                    write(*,*) "model_fraction_of_homeowners_with_mortgage(1)=", &
                        model_fraction_of_homeowners_with_mortgage(1)
                    write(*,*) "(model_mortgages(1)/model_gross_housing_wealth(1))=", &
                        (model_mortgages(1)/model_gross_housing_wealth(1))
                        
                    if (add_net_worth_bequest==1) then
                    
                        write(*,*) "discount_factor_on_child=", discount_factor_on_child
                    
                        if (target_bequests==0) then
                                            
                            discount_factor_on_child=min(max(discount_factor_on_child-discount_factor_on_child_adjuster*&
                                (model_homeownership_rate_retired(1)-homeownership_rate_retired_data),0.d0),1000.d0) 
                                
                            distance_discount_factor_on_child=&
                                abs(model_homeownership_rate_retired(1)-homeownership_rate_retired_data)
                                
                            write(*,*) "model_homeownership_rate_retired=", model_homeownership_rate_retired(1)
                            
                        elseif (target_bequests==1) then
                        
                            discount_factor_on_child=min(max(discount_factor_on_child-discount_factor_on_child_adjuster*&
                                (model_bequests(1)/model_total_net_wealth(1)-bequests_to_net_wealth_data),0.d0),1000.d0) 
                                
                            distance_discount_factor_on_child=&
                                abs(model_bequests(1)/model_total_net_wealth(1)-bequests_to_net_wealth_data)
                                
                            write(*,*) "model_bequests(1)/model_total_net_wealth(1)=", &
                                model_bequests(1)/model_total_net_wealth(1)
                        
                        end if
                        
                        write(*,*) "discount_factor_on_child=", discount_factor_on_child
                            
                    else
                    
                        distance_discount_factor_on_child=0
                            
                    end if
                                         
                    if (advantage_to_owning_adjuster>0) then
                    
                        write(*,*) "advantage_to_owning=", advantage_to_owning
                    
                        advantage_to_owning=min(max(advantage_to_owning-advantage_to_owning_adjuster*&
                            (model_homeownership_rate(1)-SCF_homeownership_rate),0.d0),10000.d0) 
                                                                    
                        distance_advantage_to_owning=abs(model_homeownership_rate(1)-SCF_homeownership_rate)
                        
                        write(*,*) "advantage_to_owning=", advantage_to_owning
                        write(*,*) "distance_advantage_to_owning=", distance_advantage_to_owning
                        write(*,*) "model_homeownership_rate(1)=", model_homeownership_rate(1)  
                        write(*,*) "advantage_to_owning_adjuster=", advantage_to_owning_adjuster
                        
                        distance_rent_to_price=0
                        
                    else
                        
                        distance_advantage_to_owning=0
                        distance_rent_to_price=0
                            
                    end if
                        
                    write(*,*) "relative_share_of_housing=", relative_share_of_housing 
                                        
                    relative_share_of_housing=&
                        min(max(relative_share_of_housing-relative_share_of_housing_adjuster*&
                        (model_average_housing_cost_to_income_ratio(1)-FRED_housing_services_to_GDP),0.01),0.9) 
                                                                 
                    distance_relative_share_of_housing=&
                        abs(model_average_housing_cost_to_income_ratio(1)-FRED_housing_services_to_GDP)
                                
                    write(*,*) "model_average_housing_cost_to_income_ratio=", &
                        model_average_housing_cost_to_income_ratio(1)
                                                 
                    write(*,*) "relative_share_of_housing=", relative_share_of_housing 
                    write(*,*) "distance_relative_share_of_housing=", distance_relative_share_of_housing
                    
                    distance_rental_management_costs=0
                                            
                    write(*,*) "housing_status_grid_expander=", housing_status_grid_expander
                        
                    if (housing_status_grid_expander_adjuster>0) then
                    
                        housing_status_grid_expander=&
                            min(max(housing_status_grid_expander-housing_status_grid_expander_adjuster*&
                            (model_homeownership_rate(1)-SCF_homeownership_rate),0.0),5.d0) 
                                                                     
                        distance_housing_status_grid_expander=&
                            abs(model_homeownership_rate(1)-SCF_homeownership_rate)
                            
                        distance_rent_to_price=0
                                                
                    else
                    
                        distance_housing_status_grid_expander=0
                        distance_rent_to_price=0
                        
                    end if
                                        
                    if ((calibrate_size_of_smallest_house_hhld_can_buy==0) .AND. &
                        (advantage_to_owning_adjuster==0) .AND. (two_rental_markets==0)) then
                        
                        write(*,*) "rent(1,1)/housing_price=", rent(1,1)/housing_price(1)
                        
                        distance_rent_to_price=abs(model_homeownership_rate(1)-SCF_homeownership_rate)
                            
                        rent(1,1)=rent(1,1)-rent_to_price_adjuster*&
                            (model_homeownership_rate(1)-SCF_homeownership_rate)
                        
                        write(*,*) "rent(1,1)/housing_price=", rent(1,1)/housing_price(1)
                        write(*,*) "distance_rent_to_price=", distance_advantage_to_owning
                        write(*,*) "model_homeownership_rate(1)=", model_homeownership_rate(1)
                        
                    else
                    
!                        write(*,*) "rent(1,1)=", rent(1,1)
!                        write(*,*) "model_average_rent=", model_average_rent(1,1)
!                        write(*,*) "model_average_rent/housing_price=", model_average_rent(1,1)/housing_price(1)
!                    
!                        rent(1,1)=rent(1,1)-rent_to_price_adjuster*&
!                            (model_average_rent(1,1)/housing_price(1)-rent_to_housing_price)
!                            
!                        write(*,*) "rent(1,1)=", rent(1,1)
!                            
                    end if
                    
                    write(*,*) "size_of_smallest_house_hhld_can_buy=", size_of_smallest_house_hhld_can_buy
                    write(*,*) "housing_status_grid_expander=", housing_status_grid_expander
                    write(*,*) "housing_size(index_of_smallest_house_hhld_can_buy)=", &
                        housing_size(index_of_smallest_house_hhld_can_buy)
                    write(*,*) "model_fraction_of_homeowners_with_mortgage(1)=", model_fraction_of_homeowners_with_mortgage(1)
                                        
                    write(*,*) "smallest_rental_relative_to_lowest_income=", smallest_rental_relative_to_lowest_income                    
                    
                    if (calibrate_smallest_house_size==1) then
                                        
                        smallest_rental_relative_to_lowest_income=&
                            min(max(smallest_rental_relative_to_lowest_income-smallest_house_size_adjuster*&
                            (mass_of_renters_at_housing_spending(1,target_30_perc*1+(1-target_30_perc)*2)-&
                            renters_spend_over_X_data),0.d0),10.d0) 
                                                                     
                        distance_smallest_house_size=abs(mass_of_renters_at_housing_spending&
                            (1,target_30_perc*1+(1-target_30_perc)*2)-renters_spend_over_X_data)
                                                         
                    else
                    
                        distance_smallest_house_size=0
                    
                    end if
                    
                    write(*,*) "smallest_rental_relative_to_lowest_income=", smallest_rental_relative_to_lowest_income
                    write(*,*) "mass_of_renters_at_housing_spending=", &
                        mass_of_renters_at_housing_spending(1,target_30_perc*1+(1-target_30_perc)*2)
                        
                    write(*,*) "labor_income_tax_rate=", labor_income_tax_rate
                                            
                    labor_income_tax_rate=max(min(labor_income_tax_rate+labor_income_tax_rate_adjuster*&
                        (model_average_labour_income_tax(1)-average_labor_income_tax_data),2.d0),0.d0)
                        
                    write(*,*) "labor_income_tax_rate=", labor_income_tax_rate
                                                                                
                    if (fixed_cost_to_moving_adjuster>0)  then
                    
                        write(*,*) "model_years_in_same_rental=", model_years_in_same_rental(1,2)
                        write(*,*) "fixed_cost_to_moving=", fixed_cost_to_moving
                                
                        fixed_cost_to_moving=max(min(fixed_cost_to_moving-fixed_cost_to_moving_adjuster*&
                            (model_years_in_same_rental(1,2)-average_years_in_same_rental_data),1.d0),0.d0)
                            
                        distance_fixed_cost_to_moving=abs(model_years_in_same_rental(1,2)-average_years_in_same_rental_data)
                            
                        write(*,*) "fixed_cost_to_moving=", fixed_cost_to_moving
                        write(*,*) "distance_fixed_cost_to_moving=", distance_fixed_cost_to_moving
                    
                    else
                    
                        distance_fixed_cost_to_moving=0
                    
                    end if
                    
                    if (include_moving_back==1) then
                    
                        write(*,*) "model_fraction_living_with_parents(1)=", model_fraction_living_with_parents(1)
                        write(*,*) "size_move_back=", size_move_back
                    
                        size_move_back=max(min(size_move_back-size_move_back_adjuster*&
                            (model_fraction_living_with_parents(1)-fraction_living_with_parents_data),1.d0),0.d0)
                            
                        distance_size_move_back=&
                            abs(model_fraction_living_with_parents(1)-fraction_living_with_parents_data)
                            
                        write(*,*) "size_move_back=", size_move_back
                    
                    else
                    
                        distance_size_move_back=0
                    
                    end if
                    
                    if (include_homelessness==1) then
                    
                        write(*,*) "model_fraction_homeless(1)=", model_fraction_homeless(1)
                        write(*,*) "size_homeless=", size_homeless
                    
                        size_homeless=max(min(size_homeless-size_homeless_adjuster*&
                            (model_fraction_homeless(1)-fraction_homeless_data),1.d0),0.d0)
                            
                        distance_size_homeless=&
                            abs(model_fraction_homeless(1)-fraction_homeless_data)
                            
                        write(*,*) "size_homeless=", size_homeless
                    
                    else
                    
                        distance_size_homeless=0
                    
                    end if
            
                end if
                                        
            end if
            
            if ((calibrate_only_rental_management_costs==1) .AND. &
                (re_do_simulation_with_fixed_parameters==1)) then
                
                write(*,*) "rental_management_costs=", rental_management_costs
                                            
                rental_management_costs=&
                    min(max(rental_management_costs-rental_management_costs_adjuster*&
                    (total_rental_demand(1,1)-total_rental_supply(1,1)),0.d0),2.d0) 
                                                                     
                distance_rental_management_costs=abs(total_rental_demand(1,1)-total_rental_supply(1,1))
                    
                write(*,*) "rental_management_costs=", rental_management_costs
                
            elseif ((calibrate_only_rental_management_costs==0) .AND. &
                   (re_do_simulation_with_fixed_parameters==1)) then
                    
                distance_rental_management_costs=0
            
            end if
                                                             
            distance_rental_market(1)=maxval(abs(total_rental_demand(1,:)-total_rental_supply(1,:)))
                    
            write(*,*) "rent(1)=", rent(1,:)
            write(*,*) "rental_supply(1)=", total_rental_supply(1,:)
            write(*,*) "rental_demand(1)=", total_rental_demand(1,:)
            write(*,*) "distance_rental_market(1)=", distance_rental_market(1)
                                                                                           
            if (two_rental_markets==0) then
                    
                write(*,*) "one market"
    
                rental_market_adjuster=1
                
                if (re_do_simulation_with_fixed_parameters==5) then
            
                    distance_rental_market(1)=(abs(himmelberg_etal_rent_to_price-&
                        (model_average_rent(1,2)/housing_price(1))))
                        
                    rent(1,1)=rent(1,1)+rental_market_adjuster*(himmelberg_etal_rent_to_price-&
                        (model_average_rent(1,2)/housing_price(1)))
                        
                else
                
                    distance_rental_market(1)=0
                    
                end if
                                            
            else
            
                if (re_do_simulation_with_fixed_parameters==0) then
                                        
                    rental_market_adjuster=0.005
                                    
                    write(*,*) "two markets - adjusting free market rents"
                    
!                    if (loop_count>1) then
                
                        distance_rental_market(1)=abs((1-fraction_in_free_market_data)-&
                            model_fraction_living_in_controlled(1))
                            
                        write(*,*) "rent(1,1)=", rent(1,1)
                            
                        rent(1,1)=rent(1,1)-&
                            rental_market_adjuster*((1-fraction_in_free_market_data)-&
                            model_fraction_living_in_controlled(1))
                            
                        write(*,*) "rent(1,1)=", rent(1,1)
                        write(*,*) "model_fraction_living_in_controlled(1)=", model_fraction_living_in_controlled(1)
                            
!                    end if
                    
!                    distance_rental_market(1)=0
                
                else
                
!                    distance_rental_market(1)=0
                    
                end if
            
            end if
                            
            write(*,*) "rent(1,:)=", rent(1,:)
            write(*,*) "model_years_in_same_rental=", model_years_in_same_rental(1,:)
            write(*,*) "rental_market_adjuster=", rental_market_adjuster
            do i=1-two_rental_markets,1
                write(*,*) "model_average_income_by_market=", model_average_income_by_market(1,i)
            end do
            write(*,*) "model_average_income_by_market(0)/model_average_income_by_market(1)=", &
                model_average_income_by_market(1,1-two_rental_markets)/model_average_income_by_market(1,1)
            do i=1-two_rental_markets,1
                write(*,*) "model_average_age_by_market=", model_average_age_by_market(1,i)
            end do
            write(*,*) "model_average_rent(1,:)=", model_average_rent(1,:) 
            write(*,*) "model_average_rent(1,:)/housing_price(1)=", model_average_rent(1,:)/housing_price(1) 
            write(*,*) "total_rental_demand(1,1)/sum(total_rental_supply(1,:))=", &
                total_rental_demand(1,1)/sum(total_rental_demand(1,:))
            write(*,*) "model_mortgages(1)/model_gross_housing_wealth(1)=", model_mortgages(1)/model_gross_housing_wealth(1)
            write(*,*) "model_average_rental_unit_size(1)=", model_average_rental_unit_size(1)
            write(*,*) "model_average_owner_occupied_unit_size(1)=", model_average_owner_occupied_unit_size(1)
            write(*,*) "model_average_rental_unit_size(1)/model_average_owner_occupied_unit_size(1)=", &
                model_average_rental_unit_size(1)/model_average_owner_occupied_unit_size(1)
            write(*,*) "model_fraction_of_homeowners_with_mortgage(1)=", model_fraction_of_homeowners_with_mortgage(1)
                                
            write(*,*) "model_homeownership_rate(1)=", model_homeownership_rate(1)
                                                        
            call cpu_time(simulation_stop_time)
            
            write(*,*) "re_do_simulation_with_fixed_parameters=", re_do_simulation_with_fixed_parameters
            write(*,*) "do_internal_computations=", do_internal_computations
            write(*,*) "relative_share_of_housing=", relative_share_of_housing
            write(*,*) "advantage_to_owning=", advantage_to_owning
                
            write(*,*) "discount_factor=", discount_factor
            
            if (add_net_worth_bequest==1) then
            
                write(*,*) "discount_factor_on_child=", discount_factor_on_child
            
            end if
            
            write(*,*) "disutility_of_landlords=", disutility_of_landlords
                                 
            write(*,*) "housing_status_grid_expander=", housing_status_grid_expander
                                
            mass_at_top_financial=0
            
            do i=1-two_rental_markets,2
                                    
                mass_at_top_financial=mass_at_top_financial+&
                    sum(distribution_stationary(i)%vector_transition_real(1,:,financial_grid_size,:,:,:,:)) 
                    
            end do
                        
            write(*,*) "labor_income_tax_rate=", labor_income_tax_rate
            write(*,*) "curvature_of_labor_income_taxes=", curvature_of_labor_income_taxes
            write(*,*) "deterioration_controlled_units=", deterioration_controlled_units
            write(*,*) "smallest_house_size=", smallest_house_size
            write(*,*) "smallest_rental_relative_to_lowest_income=", smallest_rental_relative_to_lowest_income
            write(*,*) "size_homeless=", size_homeless
            write(*,*) "size_move_back=", size_move_back
            write(*,*) "housing_size(index_of_smallest_house_hhld_can_buy)=", &
                housing_size(index_of_smallest_house_hhld_can_buy)
            write(*,*) "largest_financial_chosen_in_equilibrium=", largest_financial_chosen_in_equilibrium
            write(*,*) "largest_housing_size_consumed_in_equilibrium=", largest_housing_size_consumed_in_equilibrium
            write(*,*) "largest_years_holding_onto_asset_in_equilibrium=", largest_years_holding_onto_asset_in_equilibrium
            write(*,*) "smallest_financial_chosen_in_equilibrium=", smallest_financial_chosen_in_equilibrium
            write(*,*) "financial_grid(smallest_financial_chosen_in_equilibrium)=", &
                financial_grid(smallest_financial_chosen_in_equilibrium)
            write(*,*) "fraction of hhlds who make it to the highest net wealth=", mass_at_top_financial/sum_of_distribution
            write(*,*) "fraction_with_largest_housing_size_consumed_in_eqilibrium=", &
                fraction_with_largest_housing_size_consumed_in_eqilibrium
            write(*,*) "fraction_with_largest_years_holding_onto_asset_in_eqilibrium=", &
                fraction_with_largest_years_holding_onto_asset_in_eqilibrium
            write(*,*) "fraction_with_smallest_financial_in_eqilibrium=", &
                fraction_with_smallest_financial_in_eqilibrium
                
            write(*,*) "model_fraction_of_homeowners_with_mortgage_over_80_perc=", &
                model_fraction_of_homeowners_with_mortgage_over_80_perc(1)
            write(*,*) "rent(1)/housing_price(1)=", rent(1,:)/housing_price(1)
            write(*,*) "sum(total_rent_paid(1,:))/total_renters_income(1)=", 100*(sum(total_rent_paid(1,:))/total_renters_income(1))
            
            do i=1-two_rental_markets,1
                write(*,*) "model_average_rental_unit_size_by_market", i,"=", model_average_rental_unit_size_by_market(1,i)
            end do
            
            write(*,*) "ratio_model_average_rental_unit_size_by_market=", &
                model_average_rental_unit_size_by_market(1,1-two_rental_markets)/model_average_rental_unit_size_by_market(1,1)
                                                                            
            call initialize_variables_and_grids()
            call initialize_financial_grid(1)
            call set_smallest_house_size()
            call function_of_utility_advantage_from_homeowning(1)
            call govt_revenues_calc(1)
            call set_SS_benefits(1)
            
            write(*,*) "solve_elasticity_of_rental_housing"
                        
            call solve_elasticity_of_rental_housing(1,disutility_of_landlords)
                                                                                             
            write(*,*) "loop_count=", loop_count
            
            write(*,*) "rental_management_costs=", rental_management_costs
                            
            if (govt_rent_subsidies(1)>0) then
                write(*,*) "govt_rent_subsidies=", govt_rent_subsidies(1)
            end if
                        
            write(*,*) "model average labour income=", model_average_labour_income(1)
            write(*,*) "model_average_labour_income_tax=", model_average_labour_income_tax(1)
            
            if (model_bequests(1)>0) then
            
                write(*,*) "model_bequests=", model_bequests(1)
                
            end if
                        
            if (add_net_worth_bequest==1) then
            
                write(*,*) "model_bequests/model_net_wealth=", model_bequests(1)/model_total_net_wealth(1)
                
            end if
                        
            write(*,*) "two_rental_markets=", two_rental_markets
            write(*,*) "rent_decrease_per_year=", rent_decrease_per_year            
            write(*,*) "model_homeownership_rate_retired=", model_homeownership_rate_retired(1)
            write(*,*) "model_fraction_living_in_free=", 1-model_fraction_living_in_controlled(1)
            write(*,*) "model_fraction_living_with_parents=", model_fraction_living_with_parents(1)
            write(*,*) "model_fraction_homeless (%)=", 100*model_fraction_homeless(1)
            write(*,*) "proportion_inheritance=", proportion_inheritance
            write(*,*) "distance_proportion_inheritance=", distance_proportion_inheritance        
            write(*,*) "rent(1,1)/rent(1,1-two_rental_markets)=", rent(1,1)/rent(1,1-two_rental_markets)
            write(*,*) "file_number=", file_number
            write(*,*) "total_housing_spending=", total_housing_spending(1)
            write(*,*) "model_average_rent_to_income_ratio", model_average_rent_to_income_ratio(1)
            write(*,*) "model_average_housing_cost_to_income_ratio=", model_average_housing_cost_to_income_ratio(1)
                                                                                                     
        end do
        
        simulation_ended=1
        
        call calculate_homeownership_by_characteristics(1)
        
        write(*,*) "calculating inequality"
        
        if (use_old_results>=0) then
        
            call stationary_equilibrium_wealth_distribution(1)
            call calculate_gini_coefficient_wealth(1)
            
        end if
        
        call save_vars_and_functions(1)
        
        write(*,*) "simulation ended"
                                        
    end subroutine
    
    subroutine long_run_counter_factual()
    
        use Global_Vars
        
        implicit none
        
        real(8) :: previous_distance
        character*1 :: string_used
        integer :: bequest_from_childs_utility
        real(8), dimension(1+benchmark_has_two_rental_markets) :: rent_upload
        real(8), dimension(2+benchmark_has_two_rental_markets) :: average_rent_upload
        
        integer :: largest_value_youngest,end_loop
                
        simulation_ended=0
                                    
        call cpu_time(simulation_start_time)
                        
        if (use_old_results==1) then

            call upload_vars(transition_length)
            
        else
                                
        end if

        increase_in_labour_income_tax=0
        
        loop_count=0

        call function_of_utility_advantage_from_homeowning(transition_length)
        call allocate_matrix()
                        
        call initialize_variables_and_grids()
        call set_SS_benefits(transition_length)        
        call initialize_feasible_distribution(transition_length)
        
        call function_of_utility_advantage_from_homeowning(transition_length)
        call initialize_financial_grid(transition_length)
                               
        distance_rental_market(transition_length)=1
        distance_value_function=1
        distance_housing_market(transition_length)=1
          
        do i=1-two_rental_markets,2
          
            value_function_guess(i)%vector_youngest_real(:,:,:,:,:)=0
            value_function(i)%vector_transition_real(transition_length,:,:,:,:,:,:)=0
            
        end do
                       
        if (two_rental_markets==1) then
                    
            rent(transition_length,max(0,(1-two_rental_markets)))=rent_calibrated(1-two_rental_markets)
            model_average_rent(1,max(0,(1-two_rental_markets)))=rent(1,max(0,(1-two_rental_markets)))
            rent(transition_length,1)=(1-random_assignment)*rent_calibrated(1)+random_assignment*average_rent_calibrated(1)
            model_average_rent(transition_length,:)=average_rent_calibrated(:)            
                                            
        else
                    
            if (adjust_rents==1) then
            
                open(unit=22,file="rent"//trim(long_run_simulation)//".dat",action='read')
                    read(22,'(F15.8)') rent_upload(:)
                close(22)
                
                open(unit=22,file="model_average_rent"//trim(long_run_simulation)//".dat",action='read')
                    read(22,'(F15.8)') average_rent_upload(:)
                close(22)
                
                if (include_rent_decrease==1) then
        
                    rent(transition_length,1)=rent_upload(2)
                    model_average_rent(transition_length,1:2)=average_rent_upload(2)
                    
                else
                
                    rent(transition_length,1)=rent_upload(1)
                    model_average_rent(transition_length,1:2)=average_rent_upload(1)
                
                end if
                                                        
            else
            
                rent(transition_length,1)=rent_calibrated(1-two_rental_markets)  
                model_average_rent(transition_length,1:2)=rent_calibrated(1)
            
            end if
            
        end if
        
        if (random_assignment==1) then
        
!             max_years_random=&
!                int(log(model_average_rent(transition_length,max(0,(1-two_rental_markets)))/&
!                model_average_rent(transition_length,1))/log(1+deterioration_controlled_units))+1
!                
!            call function_of_utility_advantage_from_homeowning(transition_length)
        
        end if
        
        if (adjust_house_prices==1) then
        
            open(unit=22,file="housing_price"//trim(long_run_simulation)//".dat",action='read')
                read(22,'(F15.8)') housing_price(1)
            close(22)
        
        end if
        
        if (adjust_taxes==1) then
        
            open(unit=22,file="property_tax"//trim(long_run_simulation)//".dat",action='read')
                read(22,'(F15.8)') property_tax(1)
            close(22)
        
        end if
        
        if (adjust_bequests==1) then
        
            open(unit=22,file="proportion_inheritance"//trim(long_run_simulation)//".dat",action='read')
                read(22,'(F15.8)') proportion_inheritance
            close(22)
        
        end if
        
        if (check_tax_incidence==1) then
        
            housing_price(1)=calibrated_stationary_housing_price*1
            rent(1,1)=rent_calibrated(1)*1.021
            model_average_rent(transition_length,:)=average_rent_calibrated(1)*1.02
            
            lump_sum_transfer=0
            
        end if
        
        if (remove_controlled_market==1) then

            rent(1,1)=100
            model_average_rent(1,1)=100

        end if

        if (remove_free_market==1) then

            rent(1,1-two_rental_markets)=100
            model_average_rent(1,1-two_rental_markets)=100

        end if
                                        
        write(*,*) "total_rental_supply_calibrated=", total_rental_supply_calibrated
        write(*,*) "calibrated_housing_stock=", calibrated_housing_stock
        write(*,*) "cutoff_housing_investment_calibrated=",cutoff_housing_investment_calibrated
        write(*,*) "calibrated_risk_free_rate=", calibrated_risk_free_rate
        write(*,*) "average_labor_income_calibrated=", average_labor_income_calibrated
        write(*,*) "re_do_simulation_with_fixed_parameters=", re_do_simulation_with_fixed_parameters
        write(*,*) "property_tax(transition_length)=", property_tax(transition_length)
        write(*,*) "rental_management_costs=", rental_management_costs
        write(*,*) "mortgage_interest_rate=", mortgage_interest_rate(transition_length)
        write(*,*) "infinite_elasticity_of_rental_supply=", infinite_elasticity_of_rental_supply
        write(*,*) "rent/rent_calibrated=", rent(transition_length,:)/rent_calibrated(:)
        write(*,*) "property_tax_on_rental_housing=", property_tax_on_rental_housing
        write(*,*) "check_tax_incidence=", check_tax_incidence
        write(*,*) "stopping_rule_govt_budget=", stopping_rule_govt_budget
        write(*,*) "stopping_rule_housing_market=", stopping_rule_housing_market
        write(*,*) "stopping_rule_rental_market=", stopping_rule_rental_market
        write(*,*) "increase_in_labour_income_tax=", increase_in_labour_income_tax
        write(*,*) "housing_price=", housing_price(1)
        write(*,*) "housing_price/calibrated_stationary_housing_price=", &
            housing_price(transition_length)/calibrated_stationary_housing_price
        write(*,*) "average_owner_occupied_house_size_calibrated=", average_owner_occupied_house_size_calibrated
        write(*,*) "model_ss_benefits=", model_ss_benefits
        write(*,*) "discount_factor=", discount_factor
        write(*,*) "relative_share_of_housing=", relative_share_of_housing
        write(*,*) "advantage_to_owning=", advantage_to_owning
        write(*,*) "discount_factor_on_child=", discount_factor_on_child
        write(*,*) "deterioration_controlled_units=", deterioration_controlled_units
        write(*,*) "rental_management_costs_calibrated=", rental_management_costs_calibrated
        write(*,*) "disutility_of_landlords=", disutility_of_landlords
        write(*,*) "labor_income_tax_rate=", labor_income_tax_rate
        write(*,*) "housing_status_grid_expander=", housing_status_grid_expander
        write(*,*) "proportion_inheritance=", proportion_inheritance
        write(*,*) "model_average_rent=", model_average_rent(1,:)
        write(*,*) "average_rent_calibrated=", average_rent_calibrated(:)
        write(*,*) "rent=", rent(1,:)
        write(*,*) "counter_factual"
        write(*,*) "share_of_free_market_rent=", share_of_free_market_rent
        write(*,*) "share_of_free_market_rent_calibrated=", share_of_free_market_rent_calibrated
        write(*,*) "rent_decrease_per_year=", rent_decrease_per_year
        write(*,*) "rent_calibrated(1-two_rental_markets)=", rent_calibrated(1-two_rental_markets)
        write(*,*) "rent_calibrated(1)=", rent_calibrated(1)
        write(*,*) "max_tracking=", max_tracking
        write(*,*) "two_rental_markets=", two_rental_markets   
        write(*,*) "do_internal_computations=", do_internal_computations
        write(*,*) "disutility_of_landlords=", disutility_of_landlords
        write(*,*) "free_market_rent_increase_due_to_shock=", free_market_rent_increase_due_to_shock
        write(*,*) "housing_size(index_of_smallest_house_hhld_can_buy)=", &
            housing_size(index_of_smallest_house_hhld_can_buy)
        write(*,*) "old_results=", old_results
        write(*,*) "file_number=", file_number
        write(*,*) "long_run_simulation=", long_run_simulation
        write(*,*) "partial_equilibrium=", partial_equilibrium
        write(*,*) "adjust_bequests=", adjust_bequests
        write(*,*) "adjust_house_prices=", adjust_house_prices
        write(*,*) "adjust_rents=", adjust_rents
        write(*,*) "adjust_taxes=", adjust_taxes
        write(*,*) "include_rental_depreciation=", include_rental_depreciation 
        write(*,*) "set_rental_opportunity_probability=", set_rental_opportunity_probability
        write(*,*) "max_years_random=", max_years_random
                                                  
        lump_sum_transfer(transition_length)=0
        previous_distance=0
        end_loop=0
                                     
        do while ((distance_rental_market(transition_length)>stopping_rule_rental_market) .OR. &
            (distance_housing_market(transition_length)>stopping_rule_housing_market) .OR. &
            (distance_govt_budget>stopping_rule_govt_budget) .OR. &
            (distance_proportion_inheritance>stopping_rule_proportion_inheritance) .OR. &
            (loop_count<aftr_how_mny_lps_start_update))
                    
            largest_value_function_value=0
            bequest_from_childs_utility=0
            
            if (add_net_worth_bequest==2) then
            
                distance_value_function=1
                bequest_from_childs_utility=1

            else
            
                if (skip_vf_and_dist==0) then

                    call solve_value_function_and_policy_functions(transition_length)
                    
                end if
            
            end if
            
            do while (bequest_from_childs_utility*distance_value_function>&
                (stopping_rule_value_function*(1+abs(largest_value_function_value))))
                
                call solve_value_function_and_policy_functions(transition_length)
                
                largest_value_youngest=1-two_rental_markets
                
                distance_value_function=0
                
                do i=1-two_rental_markets,2
                                        
                    if (maxval(abs(value_function_guess(i)%vector_youngest_real(:,:,:,:,:)-&
                        value_function(i)%vector_transition_real(1,1,:,:,:,:,:)))>&
                        distance_value_function) then
                    
                        distance_value_function=&
                            maxval(abs(value_function_guess(i)%vector_youngest_real(:,:,:,:,:)-&
                        value_function(i)%vector_transition_real(1,1,:,:,:,:,:)))
                            
                        largest_value_youngest=i
                        
                    end if
                
                end do
                
                distance_value_function_indeces_youngest=&
                    maxloc(abs(value_function_guess(largest_value_youngest)%vector_youngest_real(:,:,:,:,:)-&
                    value_function(largest_value_youngest)%vector_transition_real(1,1,:,:,:,:,:)),&
                    mask=abs(value_function_guess(largest_value_youngest)%vector_youngest_real(:,:,:,:,:)-&
                    value_function(largest_value_youngest)%vector_transition_real(1,1,:,:,:,:,:))>=0)
                
                largest_value_function_value=&
                    value_function(largest_value_youngest)%vector_transition_real&
                    (1,1,distance_value_function_indeces_youngest(1),&
                    distance_value_function_indeces_youngest(2),&
                    distance_value_function_indeces_youngest(3),&
                    distance_value_function_indeces_youngest(4),&
                    distance_value_function_indeces_youngest(5))
                    
                do i=1-two_rental_markets,2
                                                           
                    value_function_guess(i)%vector_youngest_real(:,:,:,:,:)=&
                        value_function(i)%vector_transition_real(1,1,:,:,:,:,:)
                        
                end do
                    
                write(*,*) "distance_value_function=", distance_value_function
                                    
            end do
            
            if (hold_distribution_fixed==0) then
            
                if (skip_vf_and_dist==0) then
                
!                    if (random_assignment==1) then
!                    
!                        fraction_with_positive_wealth=0
!            
!                        do future_rental_opportunity_index=0,random_assignment
!                        
!                            distribution_guess(1)%vector_transition_real&
!                                (1,1,negative_financial_grid_size,1,0,1,future_rental_opportunity_index)=&
!                                probability_rental_opportunity(future_rental_opportunity_index)
!                                
!                        end do
!                        
!                    end if
        
                    call converge_to_stationary_equilibrium(transition_length)
                    
                end if
                
            end if

            if ((loop_count>aftr_how_mny_lps_start_update+1) .AND. (add_net_worth_bequest==1) .AND. &
                (adjust_bequests==1)) then
            
                previous_proportion_inheritance=proportion_inheritance
                
                call converge_on_proportion_inheritance(1)
                
                distance_proportion_inheritance=abs(previous_proportion_inheritance-proportion_inheritance)
                
                proportion_inheritance=proportion_inheritance_weight*proportion_inheritance+&
                    (1-proportion_inheritance_weight)*previous_proportion_inheritance
                                        
            else
            
                if (partial_equilibrium==1) then
                
                    distance_proportion_inheritance=0
                
                else

                    if (loop_count>aftr_how_mny_lps_start_update+1) then
                
                        distance_proportion_inheritance=0

                    else

                        distance_proportion_inheritance=1

                    end if
                    
                end if
                
            end if
            
            if (skip_vf_and_dist==1) then
            
                do i=1-two_rental_markets,max_amortization_years
        
                    write(string_used,'(i1)') i
                    
                    open(unit=9,file="value_function"//trim(string_used)//trim(long_run_simulation)//".dat",action='read')
                        read(9,'(F40.18)') value_function(i)%vector_transition_real(:,:,:,:,:,:,:)
                    close(9)
                    
                    open(unit=10,file="distribution_stationary"//trim(string_used)//trim(long_run_simulation)//".dat",action='read')
                        read(10,'(F30.20)') distribution_stationary(i)%vector_transition_real(:,:,:,:,:,:,:)
                    close(10)
                    
                end do
            
            end if
            
            if (skip_vf_and_dist==0) then
            
                call stationary_equilibrium_statistics(transition_length)            
                call stationary_equilibirum_homeownership_rate(transition_length)
                call govt_revenues_calc(transition_length)
                call calculate_welfare_by_characteristics(transition_length)
                call calculate_homeownership_by_characteristics(transition_length)
                call calculate_aggregate_consumption(transition_length)
                
            end if
                             
            write(*,*) "rent(transition_length,:)=", rent(transition_length,:)
            
            if ((adjust_rents==1) .AND. (loop_count>aftr_how_mny_lps_start_update+1) .AND. (random_assignment==0)) then
                    
                if (two_rental_markets==0) then
                
                    rental_market_adjuster=0.003
                    
                    distance_rental_market(transition_length)=&
                        (1-partial_equilibrium)*abs(total_rental_demand(transition_length,1)-&
                        total_rental_supply(transition_length,1))
                    
                    rent(transition_length,1)=(rent(transition_length,1)+&
                        (1-partial_equilibrium)*rental_market_adjuster*&
                        (total_rental_demand(transition_length,1)-&
                        total_rental_supply(transition_length,1)))
                                                        
                else
                
                    if (remove_controlled_market==1) then

                        distance_rental_market(transition_length)=&
                            (1-partial_equilibrium)*(abs(total_rental_demand(transition_length,1-two_rental_markets)-&
                            total_rental_supply(transition_length,1-two_rental_markets)))
                            
                    end if

                    if (remove_free_market==1) then

                        distance_rental_market(transition_length)=&
                            (1-partial_equilibrium)*(abs(total_rental_demand(transition_length,1)-&
                            total_rental_supply(transition_length,1)))

                    end if

                    if ((remove_free_market .NE. 1) .AND. (remove_controlled_market .NE. 1)) then
                    
                        distance_rental_market(transition_length)=&
                            (1-partial_equilibrium)*&
                            abs(total_rental_demand(transition_length,1)-total_rental_supply(transition_length,1))
                                                                                    
                        distance_rental_market(transition_length)=&
                            (1-partial_equilibrium)*max(distance_rental_market(transition_length),&
                            abs(sum(total_rental_demand(transition_length,:))-&
                            sum(total_rental_supply(transition_length,:))))
                    
                    end if
                        
                    rental_market_adjuster=0.0005
                    
                    if (remove_controlled_market==1) then
                    
                        rent(transition_length,1-two_rental_markets)=&
                            (rent(transition_length,1-two_rental_markets)+&
                            (1-partial_equilibrium)*rental_market_adjuster*&
                            (total_rental_demand(transition_length,1-two_rental_markets)-&
                            total_rental_supply(transition_length,1-two_rental_markets)))

                    end if

                    if (remove_free_market==1) then

                        rent(transition_length,1)=(rent(transition_length,1)+&
                            (1-partial_equilibrium)*rental_market_adjuster*&
                            (total_rental_demand(transition_length,1)-&
                            total_rental_supply(transition_length,1)))

                    end if

                    if ((remove_free_market .NE. 1) .AND. (remove_controlled_market .NE. 1)) then

                        rent(transition_length,1)=(rent(transition_length,1)+&
                            (1-partial_equilibrium)*rental_market_adjuster*&
                            (total_rental_demand(transition_length,1)-&
                            total_rental_supply(transition_length,1)))
                        
                        rent(transition_length,1-two_rental_markets)=&
                            (rent(transition_length,1-two_rental_markets)+&
                            (1-partial_equilibrium)*rental_market_adjuster*&
                            (sum(total_rental_demand(transition_length,:))-&
                            sum(total_rental_supply(transition_length,:))))

                    end if   
                        
                    previous_distance=total_rental_demand(transition_length,1)-&
                        total_rental_supply(transition_length,1)
                        
                end if
                
            elseif (adjust_rents==0) then
            
                distance_rental_market(transition_length)=0
                
            end if

            if ((adjust_rents==1) .AND. (loop_count>aftr_how_mny_lps_start_update+1) .AND. (random_assignment==1)) then
            
                rental_market_adjuster=0.01

                ! Adjusting free market rent
                rent(transition_length,1-two_rental_markets)=&
                    (rent(transition_length,1-two_rental_markets)+(1-partial_equilibrium)*rental_market_adjuster*&
                    (sum(total_rental_demand(transition_length,:))-sum(total_rental_supply(transition_length,:))))

                write(*,*) "set_rental_opportunity_probability=", set_rental_opportunity_probability

                ! Adjusting probability of rental opportunity so that demand equals to supply in controlled market
                set_rental_opportunity_probability=set_rental_opportunity_probability-&
                    (1-partial_equilibrium)*set_rental_opportunity_probability_adjuster*&
                    (total_rental_demand(transition_length,1)-total_rental_supply(transition_length,1))

                write(*,*) "set_rental_opportunity_probability=", set_rental_opportunity_probability
                
                distance_rental_market(1)=abs(sum(total_rental_demand(transition_length,:))-&
                    sum(total_rental_supply(transition_length,:)))
                    
                distance_rental_market(1)=max(distance_rental_market(1),&
                    abs(total_rental_demand(transition_length,1)-total_rental_supply(transition_length,1)))

            end if    
                    
            write(*,*) "rent(transition_length,:)=", rent(transition_length,:)
            do i=1-two_rental_markets,1
                write(*,*) "model_average_income_by_market=", model_average_income_by_market(transition_length,i)
            end do
            do i=1-two_rental_markets,1
                write(*,*) "model_average_age_by_market=", model_average_age_by_market(transition_length,i)
            end do
            write(*,*) "rental_market_adjuster=", rental_market_adjuster
            write(*,*) "distance_rental_market(transition_length)=", distance_rental_market(transition_length)
            write(*,*) "total_rental_supply(:)=", total_rental_supply(transition_length,:)
            write(*,*) "total_rental_demand(:)=", total_rental_demand(transition_length,:)

            if (two_rental_markets==1) then

                write(*,*) "sum(total_rental_supply(transition_length,:))=", sum(total_rental_supply(transition_length,:))
                write(*,*) "sum(total_rental_demand(transition_length,:))=", sum(total_rental_demand(transition_length,:))

            end if

            write(*,*) "rent(:)/rent_calibrated(:)=", rent(transition_length,:)/rent_calibrated(:)
            write(*,*) "model_average_rent(transition_length,:)=", model_average_rent(transition_length,:)
            write(*,*) "model_average_rent(transition_length,:)/average_rent_calibrated(:)=", &
                model_average_rent(transition_length,:)/average_rent_calibrated(:)                     
            write(*,*) "loop_count=", loop_count
            write(*,*) "adjust_rents=", adjust_rents
            write(*,*) "adjust_house_prices=", adjust_house_prices
            write(*,*) "housing_price(transition_length)=", housing_price(transition_length)
            
            distance_housing_market(transition_length)=adjust_house_prices*&
                (1-partial_equilibrium)*abs(housing_price(transition_length)-&
                (c_1_calibrated)*(housing_depreciation_rate*total_housing_demand(transition_length))**&
                (1/empirical_housing_supplpy_elasticity))
                
            weight_on_housing_price=(1-check_tax_incidence)*0.02

            if ((adjust_house_prices==1) .AND. (loop_count>aftr_how_mny_lps_start_update+1)) then                                         

                housing_price(transition_length)=&
                    (1-partial_equilibrium)*((1-weight_on_housing_price)*housing_price(transition_length)+&
                    weight_on_housing_price*(c_1_calibrated)*(housing_depreciation_rate*total_housing_demand(transition_length))**&
                    (1/empirical_housing_supplpy_elasticity))+partial_equilibrium*housing_price(transition_length)

            else

                housing_price(transition_length)=calibrated_stationary_housing_price

            end if
                
            write(*,*) "housing_price(transition_length)=", housing_price(transition_length)
            write(*,*) "housing_price/calibrated_stationary_housing_price=", &
                housing_price(transition_length)/calibrated_stationary_housing_price
            write(*,*) "housing_stock=", housing_stock(transition_length)
            write(*,*) "total_housing_demand=", total_housing_demand(transition_length)
            write(*,*) "distance_housing_market=", distance_housing_market(transition_length)
            
            if (skip_vf_and_dist==0) then
                    
                call govt_revenues_calc(transition_length)
                call calculate_household_housing_spending(transition_length)
                
            end if
            
            call calculate_welfare(transition_length)    
            
            if (compute_support==1) then
            
                call frac_of_households_in_support_of_policy(transition_length)
                
            end if
                        
            if ((tax_system_type==0) .OR. (tax_system_type==2)) then
            
                distance_govt_budget=&
                    adjust_taxes*(1-check_tax_incidence)*(1-partial_equilibrium)*&
                    abs(net_govt_revenues(transition_length)-net_govt_revenue_constraint)
                    
            else
                
                distance_govt_budget=0
            
            end if
            
            if ((tax_system_type==2) .OR. (tax_system_type==1) .OR. (tax_system_type==0)) then
            
                if ((partial_equilibrium==0) .AND. (check_tax_incidence==0)) then
                
                    if ((skip_vf_and_dist==0) .AND. (adjust_taxes==1)) then
                    
                        property_tax(transition_length)=property_tax(transition_length)+0.0001*&
                            (net_govt_revenue_constraint-net_govt_revenues(transition_length))
            
!                        call update_tax_system(transition_length,tax_system_type)
                        
                    end if
                    
                end if
                                
            end if

            if ((share_of_free_market_rent==0.01) .AND. (distance_rental_market(transition_length)<0.04))  then

                distance_rental_market(transition_length)=0
                distance_housing_market=0
                distance_govt_budget=0
                
            end if
            
            if (check_tax_incidence==1) then
            
                distance_govt_budget=1
                
            end if
                               
            write(*,*) "distance_govt_budget=", distance_govt_budget   
            write(*,*) "net_govt_revenue_constraint=", net_govt_revenue_constraint
            write(*,*) "net_govt_revenues=", net_govt_revenues(transition_length)
            write(*,*) "sum(distribution_stationary(transition_length,:,:,:,:,:))=", sum_of_distribution
                        
            call cpu_time(simulation_stop_time)
            
            write(*,*) "running for", (simulation_stop_time-simulation_start_time)/3600
            write(*,*) "model_homeownership_rate(transition_length)=", model_homeownership_rate(transition_length)
            write(*,*) "do_internal_computations=", do_internal_computations
            write(*,*) "discount_factor=", discount_factor
            write(*,*) "relative_share_of_housing=", relative_share_of_housing
            write(*,*) "advantage_to_owning=", advantage_to_owning
            write(*,*) "discount_factor_on_child=", discount_factor_on_child
            write(*,*) "deterioration_controlled_units=", deterioration_controlled_units
            write(*,*) "rental_management_costs_calibrated=", rental_management_costs_calibrated
            write(*,*) "disutility_of_landlords=", disutility_of_landlords
            write(*,*) "labor_income_tax_rate=", labor_income_tax_rate
            write(*,*) "housing_status_grid_expander=", housing_status_grid_expander
            write(*,*) "index_of_smallest_house_hhld_can_buy=", index_of_smallest_house_hhld_can_buy 
            write(*,*) "proportion_inheritance=", proportion_inheritance
            write(*,*) "size_homeless=", size_homeless
            write(*,*) "size_move_back=", size_move_back
            write(*,*) "distance_proportion_inheritance=", distance_proportion_inheritance
            write(*,*) "model_bequests=", model_bequests(transition_length)
            write(*,*) "is_counterfactual=", is_counterfactual
            write(*,*) "tax_system_type=", tax_system_type
            write(*,*) "file_number=", file_number
            write(*,*) "benchmark_simulation=", benchmark_simulation
            write(*,*) "property_tax=", property_tax(transition_length)
            
            if (add_net_worth_bequest==2) then
            
                write(*,*) "stopping_rule_value_function*(1+abs(largest_value_function_value))=", &
                    stopping_rule_value_function*(1+abs(largest_value_function_value))
                write(*,*) "stopping_rule_value_function=", stopping_rule_value_function
                    
            end if
                                    
            call function_of_utility_advantage_from_homeowning(transition_length)
                        
            if (govt_rent_subsidies(transition_length)>0) then
                write(*,*) "govt_rent_subsidies=", govt_rent_subsidies(transition_length)
            end if
            write(*,*) "rent_decrease_per_year=", rent_decrease_per_year
            write(*,*) "share_of_free_market_rent=", share_of_free_market_rent
            write(*,*) "model_years_in_same_rental=", model_years_in_same_rental(transition_length,:)
                                         
            if (check_tax_incidence==1) then
                     
                if (infinite_elasticity_of_rental_supply==1) then
                
                    model_fraction_of_property_tax_paid_by_renters(transition_length)=&
                        (housing_price(transition_length)*property_tax_on_rental_housing)/&
                        (1+risk_free_rate(transition_length))
                        
                    model_fraction_of_property_tax_paid_by_renters(transition_length)=&
                        100*((model_average_rent(transition_length,2)-average_rent_calibrated(2))/&
                        model_fraction_of_property_tax_paid_by_renters(transition_length))
                        
                elseif (other_rental_supply_elasticity==1) then
                
                    model_fraction_of_property_tax_paid_by_renters(transition_length)=&
                        (disutility_of_landlords*rental_management_costs_calibrated*&
                        sum(total_rental_supply(transition_length,:))**&
                        (disutility_of_landlords-1)+housing_price(transition_length)-&
                        housing_price(transition_length)*&
                        (1-housing_depreciation_rate-property_tax(transition_length)-&
                        property_tax_on_rental_housing)/&
                        (1+risk_free_rate(transition_length)))-&
                        (disutility_of_landlords*rental_management_costs_calibrated*&
                        sum(total_rental_supply(transition_length,:))**&
                        (disutility_of_landlords-1)+housing_price(transition_length)-&
                        housing_price(transition_length)*&
                        (1-housing_depreciation_rate-property_tax(transition_length))/&
                        (1+risk_free_rate(transition_length)))
                                    
                    model_fraction_of_property_tax_paid_by_renters(transition_length)=&
                        100*((model_average_rent(transition_length,2)-average_rent_calibrated(2))/&
                        model_fraction_of_property_tax_paid_by_renters(transition_length))
                             
                end if
                
                model_fraction_of_property_tax_paid_by_renters(transition_length)=&
                    100*(model_average_rent(transition_length,2)-&
                    average_rent_calibrated(2))/(housing_price(transition_length)*&
                    property_tax_on_rental_housing)/(1+risk_free_rate(transition_length))
                                                   
                write(*,*) "model_fraction_of_property_tax_paid_by_renters=", &
                    model_fraction_of_property_tax_paid_by_renters(transition_length)
                    
            end if
                        
            if (property_tax_on_rental_housing>0) then
            
                write(*,*) "property_tax_on_rental_housing=", property_tax_on_rental_housing
            
            end if
              
            if (lump_sum_transfer(1)>0) then
       
                write(*,*) "lump_sum_transfer=", lump_sum_transfer(1)
            
            end if
            
            write(*,*) "two_rental_markets=", two_rental_markets
            write(*,*) "model_fraction_living_with_parents=", model_fraction_living_with_parents(transition_length)
            write(*,*) "model_fraction_homeless (%)=", 100*(model_fraction_homeless(transition_length))
            write(*,*) "govt_spending_homelessness=", govt_spending_homelessness(transition_length)
            write(*,*) "random_assignment=", random_assignment
            write(*,*) "rent_decrease_per_year=", rent_decrease_per_year
            write(*,*) "set_rental_opportunity_probability=", set_rental_opportunity_probability
            write(*,*) "probability_rental_opportunity=", probability_rental_opportunity
            write(*,*) "random_assignment=", random_assignment
            write(*,*) "share_of_free_market_rent=", share_of_free_market_rent
            
            open(unit=4,file="frac_in_support_by_age"//trim(file_number)//".txt",&
                action="write",status="replace")
                                    
            do age_index=1,life_span
                                    
                write(4,*) frac_in_support_by_age(transition_length,age_index)
                                           
            end do
                                
            close(4)
                                                           
        end do
        
        simulation_ended=1
                
        call calculate_homeownership_by_characteristics(1)
        call stationary_equilibrium_wealth_distribution(1)
        call calculate_gini_coefficient_wealth(1)
        
        call save_vars_and_functions(1)
        
        write(*,*) "simulation_ended"
                    
    end subroutine
    
    subroutine upload_functions()
    
        use Global_Vars
        
        implicit none
        
        character*1 :: string_used
        
        do i=1-benchmark_has_two_rental_markets,2
            
            write(string_used,'(i1)') i
                                
            open(unit=12,file="distribution_stationary"//trim(string_used)//trim(benchmark_simulation)//".dat",action='read')
                read(12,'(F30.20)') distribution_to_fetch(i)%vector_transition_real(1,:,:,:,:,:,:)
            close(12)
                           
            open(unit=12,file="policy_function_space_lived_in_size"&
                //trim(string_used)//trim(benchmark_simulation)//".dat",action='read')
                read(12,'(F25.20)') policy_function_space_lived_in_size_to_fetch(i)%vector_transition_real(1,:,:,:,:,:,:)
            close(12)
                    
        end do
        
        do i=1-two_rental_markets,2
                        
            write(string_used,'(i1)') i
                           
            open(unit=1,file="value_function"//trim(string_used)//trim(long_run_simulation)//".dat",action='read')
                read(1,'(F40.18)') value_function(i)%vector_transition_real(transition_length,:,:,:,:,:,:)
            close(1)
                            
            value_function_guess(i)%vector_youngest_real(:,:,:,:,:)=&
                value_function(i)%vector_transition_real(transition_length,1,:,:,:,:,:)

        end do
    
    end subroutine    
    
    subroutine transition_simulation()
    
        use Global_Vars
        
        implicit none
        
        integer :: start_updating_at_loop_number=0,use_price_distance=0
        real(8) :: largest_distance_govt_budget
        real(8) :: house_price_upon_implementation=0.99,slope_of_log_house_price=0.5
        real(8) :: rent_upon_implementation=0.95,slope_of_log_rent=-0.1
        real(8) :: slope_of_lump_sum_transfer=0.2,alpha_1         
                
        loop_count=0
        
        lump_sum_transfer=0
        
        call initialize_variables_and_grids()
                                
        if (use_old_results==1) then
        
            call upload_vars(1)
            call function_of_utility_advantage_from_homeowning(1)     
            
            call allocate_matrix() 
            call upload_functions()
            call initialize_feasible_distribution(transition_length)
                    
        else
        
            model_average_labour_income(:)=average_labor_income_calibrated
            
            net_govt_revenues(1)=net_govt_revenue_constraint
                
            housing_stock(1-solve_transition_dynamics)=calibrated_housing_stock
            housing_stock(transition_length)=long_run_housing_stock
                                                
            housing_price(transition_length)=long_run_stationary_housing_price
                
        end if
        
        housing_price(1-solve_transition_dynamics)=calibrated_stationary_housing_price
                
        if (use_old_results==1) then
                            
            housing_price(1)=calibrated_stationary_housing_price*house_price_upon_implementation
            
            slope_of_log_house_price=(housing_price(transition_length)-housing_price(1))/(real(transition_length)/real(2))
                                                  
            do time_index=min(2,transition_length),max(transition_length-1,1)
            
                housing_price(time_index)=min(housing_price(1)+&
                    slope_of_log_house_price*(real(time_index)-real(1)),housing_price(transition_length))
                  
            end do
            
        end if
        
        if (use_old_results==0) then
                            
            risk_free_rate(:)=calibrated_risk_free_rate
            mortgage_interest_rate(:)=risk_free_rate+mortgage_interest_gap
            
        end if
                                                            
        call initialize_variables_and_grids()
        
        if (use_old_results==0) then
                        
            call set_SS_benefits(1)
                         
        end if
                
        if (use_old_results==1) then
         
            total_housing_demand(1-solve_transition_dynamics)=calibrated_housing_stock
            total_housing_demand(transition_length)=long_run_housing_stock
            total_rental_supply(:,1-two_rental_markets)=total_rental_supply_calibrated(1-two_rental_markets)
            
            if (two_rental_markets==0) then
            
                total_rental_supply(:,1)=sum(total_rental_supply_calibrated(1-two_rental_markets:1))
                
            end if
            
            total_rental_demand(:,1-two_rental_markets)=total_rental_supply_calibrated(1-two_rental_markets)
        
            if (two_rental_markets==0) then
        
                total_rental_demand(:,1)=sum(total_rental_supply_upload(1-benchmark_has_two_rental_markets:1))
                
            end if
            
            rent(transition_length,:)=long_run_rent(:)
            
        end if
        
        open(unit=39,file="total_rental_supply"//trim(long_run_simulation)//".dat",action='read')
            read(39,'(F15.8)') total_rental_supply(transition_length,:)
        close(39)
        
        open(unit=40,file="total_profits"//trim(long_run_simulation)//".dat",action='read')
            read(40,'(F15.8)') total_profits(transition_length)
        close(40)
        
        total_rental_demand(transition_length,:)=total_rental_supply(transition_length,:)
        
        rent(1-solve_transition_dynamics,:)=rent_calibrated(1)
           
        if (use_old_results==1) then
       
            rent(1,:)=rent_calibrated(1)*rent_upon_implementation
            
            slope_of_log_rent=(rent(transition_length,1)-rent(1,1))/(real(transition_length)/real(2))
                                    
            do time_index=min(2,transition_length),max(transition_length-1,1)
                                                
                rent(time_index,1)=max(rent(1,1)+&
                    slope_of_log_rent*(real(time_index)-real(1)),rent(transition_length,1))
                                                                                        
                call function_of_utility_advantage_from_homeowning(time_index)
                                    
            end do 
            
        end if
        
        property_tax(1:transition_length-1)=property_tax_benchmark
        
        lump_sum_transfer(:)=0
        
!        if (use_old_results==1) then
!    
!            lump_sum_transfer(1)=0.0002
!            
!            slope_of_lump_sum_transfer=(lump_sum_transfer(transition_length)-lump_sum_transfer(1))/(transition_length)
!                        
!            do time_index=min(2,transition_length),max(transition_length-1,1)
!                                                
!                lump_sum_transfer(time_index)=min(lump_sum_transfer(1)+&
!                    slope_of_lump_sum_transfer*(real(time_index)-real(1)),lump_sum_transfer(transition_length))
!                    
!            end do
!            
!        end if
        
        call initialize_financial_grid(1)

        if (use_old_transition_results==1) then
                                
            open(unit=1,file="housing_price_in_transition"//trim(old_transition_resuls)//".txt",action='read')
                read(1,'(F15.8)') housing_price(0:transition_length)
            close(1)
            
            open(unit=1,file="housing_price"//trim(long_run_simulation)//".dat",action='read')
                read(1,'(F15.8)') housing_price(transition_length)
            close(1)
                        
            open(unit=1,file="rent_in_transition"//trim(old_transition_resuls)//".txt",action='read')
                read(1,'(F15.8)') rent(0:transition_length,1)
            close(1)
                        
            open(unit=1,file="rent"//trim(long_run_simulation)//".dat",action='read')
                read(1,'(F15.8)') rent(transition_length,1)
            close(1)
            
            open(unit=1,file="property_tax_in_transition"//trim(old_transition_resuls)//".txt",action='read')
                read(1,'(F15.8)') property_tax(0:transition_length)
            close(1)
            
            open(unit=1,file="property_tax"//trim(long_run_simulation)//".dat",action='read')
                read(1,'(F15.8)') property_tax(transition_length)
            close(1)
                                                                
            do time_index=1,transition_length
        
                call function_of_utility_advantage_from_homeowning(time_index)
                    
            end do
                                                                    
        end if
               
        write(*,*) "tax_system_type=", tax_system_type
        write(*,*) "computing tranition"
        
        write(*,*) "total_rental_demand"
        
        do time_index=1-solve_transition_dynamics,transition_length
        
!            write(*,*) sum(total_rental_demand(time_index,:))
            
            total_rental_supply(time_index,:)=sum(total_rental_demand(time_index,:))
        
        end do
                
        write(*,*) "total_rental_supply"
        
        do time_index=1-solve_transition_dynamics,transition_length
        
!            write(*,*) sum(total_rental_supply(time_index,:))
        
        end do
        
        write(*,*) "total_housing_demand"
        
        do time_index=1-solve_transition_dynamics,transition_length
        
!            write(*,*) total_housing_demand(time_index)
        
        end do
        
        write(*,*) "housing_stock"
        
        do time_index=1-solve_transition_dynamics,transition_length
        
!            write(*,*) housing_stock(time_index)
        
        end do
        
        write(*,*) "rent"
        
        do time_index=1-solve_transition_dynamics,transition_length
        
            write(*,*) rent(time_index,:)
        
        end do
        
        write(*,*) "house price"
        
        do time_index=1-solve_transition_dynamics,transition_length
        
            write(*,*) housing_price(time_index)
        
        end do
        
        write(*,*) "property_tax"
        
        do time_index=1-solve_transition_dynamics,transition_length
        
            write(*,*) property_tax(time_index)
        
        end do        
        
        largest_housing_market_distance=1
        largest_rental_market_distance=1
        largest_distance_govt_budget=1
        
        simulation_ended=0
        
        write(*,*) "average_owner_occupied_house_size_calibrated=", average_owner_occupied_house_size_calibrated
        write(*,*) "calibrated_stationary_housing_price=", calibrated_stationary_housing_price
        write(*,*) "long_run_stationary_housing_price=", long_run_stationary_housing_price
        write(*,*) "rent_calibrated=", rent_calibrated
        write(*,*) "long_run_rent=", long_run_rent
        write(*,*) "model_ss_benefits=", model_ss_benefits
        write(*,*) "intergenerational_employment_process(1,:)=", intergenerational_employment_process(1,:)
        write(*,*) "net_govt_revenue_constraint=", net_govt_revenue_constraint
        
        write(*,*) "housing_price(1-solve_transition_dynamics)=", housing_price(1-solve_transition_dynamics)
        write(*,*) "housing_price(transition_length)=", housing_price(transition_length)
        write(*,*) "rent(1-solve_transition_dynamics)=", rent(1-solve_transition_dynamics,1-two_rental_markets)
        write(*,*) "rent(transition_length)=", rent(transition_length,1-two_rental_markets)
        write(*,*) "total_rental_demand(1-solve_transition_dynamics)=", &
            total_rental_demand(1-solve_transition_dynamics,1-two_rental_markets) 
        write(*,*) "total_rental_demand(transition_length)=", total_rental_demand(transition_length,1-two_rental_markets) 
        write(*,*) "total_housing_demand(1-solve_transition_dynamics)=", total_housing_demand(1-solve_transition_dynamics) 
        write(*,*) "total_housing_demand(transition_length)=", total_housing_demand(transition_length) 
            
        stopping_rule_housing_market=0.05
        stopping_rule_rental_market=0.1
        stopping_rule_govt_budget=0.03
        
                                                             
        do while ((largest_housing_market_distance>stopping_rule_housing_market) .OR. &
            (largest_rental_market_distance>stopping_rule_rental_market) .OR. &
            (largest_distance_govt_budget>stopping_rule_govt_budget) .OR. (loop_count<1))
            
            write(*,*) "solving value functions"
                        
            do time_index=max(transition_length-1,1),1,-1
            
                call solve_value_function_and_policy_functions(time_index)
                
                do i=1-two_rental_markets,2
                
                    value_function_guess(i)%vector_youngest_real(:,:,:,:,:)=&
                        value_function(i)%vector_transition_real(time_index+1,1,:,:,:,:,:)
                        
                end do
                            
            end do
            
            write(*,*) "computing distributions"
            
            do time_index=1,max(transition_length-1,1)
            
                call converge_to_stationary_equilibrium(time_index)
                                                            
            end do
            
            write(*,*) "computing statistics, government revenues, and solving for the property tax"
            
            net_govt_revenues(1)=net_govt_revenue_constraint
            net_govt_revenues(transition_length)=net_govt_revenue_constraint
            
            largest_distance_govt_budget=0
            
            do time_index=1,max(transition_length-1,1)
                                        
                call stationary_equilibrium_statistics(time_index)
                call stationary_equilibirum_homeownership_rate(time_index)
                call calculate_homeownership_by_characteristics(time_index)
                call govt_revenues_calc(time_index)
                            
!                write(*,*) "time_index=", time_index
!                write(*,*) "homeownership_rate=", model_homeownership_rate(time_index)
!                write(*,*) "net_govt_revenues=", net_govt_revenues(time_index)
!                write(*,*) "govt_revenues_from_consumption=", govt_revenues_from_consumption(time_index)
!                write(*,*) "govt_revenues_from_property_tax=", govt_revenues_from_property_tax(time_index)
!                write(*,*) "govt_revenues_from_labour_income=", govt_revenues_from_labour_income(time_index)
!                write(*,*) "govt_spending_on_ss=", govt_spending_on_ss(time_index)
                
                if (loop_count>start_updating_at_loop_number) then
                                
                    call update_tax_system(time_index,tax_system_type)
                                
                    distance_govt_budget=adjust_taxes*abs(net_govt_revenues(time_index)-net_govt_revenue_constraint)
                        
                    if (distance_govt_budget>largest_distance_govt_budget) then
                    
                        largest_distance_govt_budget=distance_govt_budget
                        
                    end if
                    
                end if
                                
            end do
            
            largest_housing_market_distance=0
                        
            do time_index=1,max(transition_length-1,1)
                        
                housing_stock(time_index)=(1-housing_depreciation_rate)*housing_stock(time_index-solve_transition_dynamics)+&
                    (housing_price(time_index)/c_1_calibrated)**(empirical_housing_supplpy_elasticity)
                                            
                distance_housing_market(time_index)=abs(housing_stock(time_index)-total_housing_demand(time_index))
                                        
                if (distance_housing_market(time_index)>largest_housing_market_distance) then
                
                    largest_housing_market_distance=distance_housing_market(time_index)
                
                end if
                                                                    
            end do
            
!            largest_housing_market_distance=0
            distance_housing_market=0
            
            do time_index=1,max(transition_length-1,1)
                                                                                        
                distance_housing_market(time_index)=use_price_distance*abs(housing_price(time_index)-&
                    (c_1_calibrated)*(max(total_housing_demand(time_index)-&
                    (1-housing_depreciation_rate)*housing_stock(time_index-solve_transition_dynamics),0.d0))**&
                    (1/empirical_housing_supplpy_elasticity))
                                                                                            
                weight_on_housing_price=0.0001
                                         
                if (distance_housing_market(time_index)>largest_housing_market_distance) then
                
!                    largest_housing_market_distance=distance_housing_market(time_index)
                
                end if
                
                if (loop_count>start_updating_at_loop_number) then
                                    
                    housing_price(time_index)=(1-weight_on_housing_price)*&
                        housing_price(time_index)+weight_on_housing_price*&
                        (c_1_calibrated)*(max(total_housing_demand(time_index)-&
                        (1-housing_depreciation_rate)*housing_stock(time_index-solve_transition_dynamics),0.d0))**&
                        (1/empirical_housing_supplpy_elasticity)
                                                    
                end if
                                
            end do
            
            largest_rental_market_distance=0
            distance_rental_market=0
            
            do time_index=1,max(transition_length-1,1)
            
                rental_market_adjuster=0.005
                                            
                distance_rental_market(time_index)=maxval(abs(total_rental_demand(time_index,:)-&
                    total_rental_supply(time_index,:)))
                    
                if (distance_rental_market(time_index)>largest_rental_market_distance) then
            
                    largest_rental_market_distance=distance_rental_market(time_index)
            
                end if
                                
                if (loop_count>start_updating_at_loop_number) then
                                                  
                    rent(time_index,:)=rent(time_index,:)+&
                        rental_market_adjuster*(total_rental_demand(time_index,:)-total_rental_supply(time_index,:))
                                                                         
                end if                                                         
                                                     
                call function_of_utility_advantage_from_homeowning(time_index)
                                                                                                            
            end do
            
            alpha_1=1/(1+risk_free_rate(1))
            
            total_profits(1:transition_length-1)=0
                        
            if (loop_count>0) then
            
                do time_index=transition_length-1,1,-1
                                    
                    total_profits(time_index)=total_profits(time_index+1)*alpha_1+&
                        total_constrcution_profits(time_index)+total_rental_company_profits(time_index)
                                                
!                    write(*,*) "total_profits(time_index)=", total_profits(time_index)
                                                        
                end do
                
            end if
                                                
            write(*,*) "largest_housing_market_distance=", largest_housing_market_distance
            write(*,*) "largest_rental_market_distance=", largest_rental_market_distance
            write(*,*) "largest_distance_govt_budget=", largest_distance_govt_budget
                                            
            open(unit=1,file="housing_price_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                do i=0,transition_length
                    write(1,'(F15.8)') housing_price(i)
                end do
            close(1)
                            
            open(unit=1,file="rent_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                do i=0,transition_length
                    write(1,'(F15.8)') rent(i,1)
                end do
            close(1)
                
            open(unit=1,file="housing_supply_minus_housing_demand_in_transition"&
                //trim(file_number)//".txt",action="write",status="replace")
                do i=0,transition_length
                    write(1,'(F15.8)') housing_stock(i)-total_housing_demand(i)
                end do
            close(1)
            
            open(unit=1,file="housing_supply_transition"//trim(file_number)//".txt",action="write",status="replace")
                write(1,'(F15.8)') housing_stock(1-solve_transition_dynamics:transition_length)
            close(1)
            
            open(unit=1,file="rental_supply_minus_rental_demand_in_transition"&
                //trim(file_number)//".txt",action="write",status="replace")
                do i=0,transition_length
                    if (i==0) then
                        write(1,'(F15.8)') 0.d0
                    elseif ((i>0) .AND. (i<transition_length)) then
                        write(1,'(F15.8)') total_rental_supply(i,:)-total_rental_demand(i,:)
                    else
                        write(1,'(F15.8)') 0.d0
                    end if
                end do
            close(1)
            
            open(unit=3,file="net_govt_rev_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                write(3,'(F15.8)') net_govt_revenues(1:transition_length-1)
            close(3)
                                            
            open(unit=2,file="lump_sum_transfer_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                do i=0,transition_length 
                    write(2,'(F15.8)') lump_sum_transfer(i)
                end do                    
            close(2)
            
            open(unit=2,file="property_tax_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                do i=0,transition_length 
                    write(2,'(F15.8)') property_tax(i)
                end do                    
            close(2)
            
            open(unit=2,file="homeownership_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                write(2,'(F10.8)') model_homeownership_rate(1:transition_length-1)
            close(2)
            
            open(unit=2,file="homeownership_1st_period_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                write(2,'(F10.8)') homeownership_by_age(1,:)
            close(2)
            
            open(unit=2,file="homeownership_2nd_period_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                write(2,'(F10.8)') homeownership_by_age(2,:)
            close(2)            
            
            open(unit=2,file="model_average_rental_unit_size_in_transition"&
                //trim(file_number)//".txt",action="write",status="replace")
                write(2,'(F10.8)') model_average_rental_unit_size(1:transition_length-1)
            close(2)
            
            open(unit=2,file="model_average_owner_occupied_unit_size_in_transition"&
                //trim(file_number)//".txt",action="write",status="replace")
                write(2,'(F10.8)') model_average_owner_occupied_unit_size(1:transition_length-1)
            close(2)
                        
            open(unit=2,file="total_constrcution_profits_in_transition"//trim(file_number)//&
                ".text",action="write",status="replace")
                write(2,'(F20.15)') total_constrcution_profits
            close(2)
            
            open(unit=2,file="total_rental_company_profits_in_transition"//trim(file_number)//&
                ".text",action="write",status="replace")
                write(2,'(F20.15)') total_rental_company_profits
            close(2)
            
            open(unit=2,file="total_rental_supply_in_transition"//trim(file_number)//".text",action="write",status="replace")
                write(2,'(F20.15)') total_rental_supply
            close(2)
            
            open(unit=2,file="total_rental_demand_in_transition"//trim(file_number)//".text",action="write",status="replace")
                write(2,'(F20.15)') total_rental_demand
            close(2)
                        
            open(unit=2,file="total_housing_demand_in_transition"//trim(file_number)//".text",action="write",status="replace")
                write(2,'(F20.15)') total_housing_demand
            close(2)
            
            open(unit=2,file="total_profits_in_transition"//trim(file_number)//".text",action="write",status="replace")
                write(2,'(F20.15)') total_profits
            close(2)
                                                 
            open(unit=3,file="distance"//trim(file_number)//".txt",action="write",status="replace")
            
                write(3,*) "loop_count=", loop_count
                write(3,*) "largest_housing_market_distance=",largest_housing_market_distance
                write(3,*) "largest_rental_market_distance=", largest_rental_market_distance
                write(3,*) "largest_distance_govt_budget=", largest_distance_govt_budget
                write(3,*) "frac_in_support_incumbent_renters=", frac_in_support_incumbent_renters(1)
                write(3,*) "tax_system_type=", tax_system_type
                                
            close(3)
                            
            loop_count=loop_count+1
            
            write(*,*) "loop_count=", loop_count
            
            do time_index=1,max(transition_length-1,1)
        
                call calculate_welfare(time_index)
                
                if (compute_support==1) then
                
                    call frac_of_households_in_support_of_policy(time_index)
                    
                end if
            
            end do
            
            open(unit=1,file="welfare_in_transition_youngest"//trim(file_number)//".txt",action="write",status="replace")
                write(1,'(F15.8)') average_welfare_of_youngest_cohort(1:transition_length-1)
            close(1)
            
            open(unit=1,file="welfare_in_transition_everyone"//trim(file_number)//".txt",action="write",status="replace")
                write(1,'(F15.8)') average_welfare_of_all_cohorts(1:transition_length-1)
            close(1)
            
            open(unit=1,file="welfare_in_transition_retired"//trim(file_number)//".txt",action="write",status="replace")
                write(1,'(F15.8)') average_welfare_of_retired(1:transition_length-1)
            close(1)
            
            open(unit=2,file="support_in_transition"//trim(file_number)//".txt",action="write",status="replace")
                write(2,'(F10.8)') frac_in_support(1:transition_length-1)
            close(2)
                                    
        end do
        
        simulation_ended=2
        
    end subroutine