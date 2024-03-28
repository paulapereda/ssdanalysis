reg_coef_plots <- function(data, sample, module, path = here::here()) {

  set.seed(61625)

  # Defining modules and their respective variables inside the function
  modules <- list(

    food_security      = c("food_cons_score_w_z",       "fcs_profile1_z",            "fcs_profile2_z",             "fcs_profile3_z",
                           "fies_score_z",              "fies_profile1_z",           "fies_profile2_z",            "fies_profile3_z",
                           "min_diet_index_z",          "diet_score_z"),

    agriculture        = c("hasplots_z",                "farm_assets_tot_z",         "plot_n_z",                   "farm_size_z",
                           "ag_hh_mem_work_count_z",    "num_days_ag_avg_z",         "revenue_all_z"),

    livestock          = c("own_livestock_z",           "liv_hh_mem_work_z",         "num_days_lstock_avg_z",      "tot_lstock_count_z",
                           "tot_lstock_count_tlu_z",    "hh_chicken_all_z",          "hh_goat_all_z",              "livestock_sold_all_z",
                           "lstock_cons_all_z",         "lstock_turnover_all_z"),

    business           = c("hh_own_bus_z",              "member_bus_total_z",        "bus_labor_all_z",            "business_count_z",
                           "ent_profit_w_all_z"),

    wage_labor         = c("wage_job_any_z",            "adult_empl_z",              "avg_emp_days_z",             "tot_inc_w_all_z"),

    psychosocial       = c("ps_depression_sqr_z",       "ps_depression_cesd_z"),

    consumption        = c("tot_food_exp_month_wins_z", "food_exp_pc_w",             "tot_nfood_exp_month_wins_z",
                           "nfood_exp_pc_w_z",          "tot_exp_month_wins_z",      "tot_exp_month_pc_wins_z")
  )

  # Defining a named list for indicator mappings

  indicator_mappings <- list(

    food_security = c("Food Consumption Score",
                      "% of FCS Poor (FCS 0-28)",
                      "% of FCS Borderline (FCS 28.5-42)",
                      "% of FCS Acceptable (FCS 42+)",
                      "Food Insecurity Experience Scale",
                      "% of HHs with severe food insecurity (FIES 0-1)",
                      "% of HHs with moderate food insecurity (FIES 2-4)",
                      "% of food secure HHs (FIES 5-8)",
                      "Minimum Dietary Diversity for Women",
                      "Women's Dietary Diversity Score"),

    agriculture   = c("HH cultivated plots",
                     "# of farm assets (hoe, spade, axe), all HHs",
                     "Number of plots",
                     "Farm size",
                     "# of HH members that cultivated plots, all HHs",
                     "Avg # of days HH members spent cultivating plots, all HHs",
                     "Annual agricultural revenue"),

    livestock    = c("HH owned livestock",
                     "# of HH members that worked on livestock, all HHs",
                     "Avg # of days HH members spent working on livestock, all HHs",
                     "Total livestock count, all HHs",
                     "TLU, all households",
                     "Number of chickens, all HHs",
                     "Number of goats, all HHs",
                     "HH sold any livestock, all HHs",
                     "HH consumed any livestock, all HHs",
                     "Revenue from sales of livestock, all HHs"),

    business      = c("HH operated a business",
                      "# of HH members that worked on businesses, all HHs",
                      "Avg # of days HH members worked on businesses, all HHs",
                      "# of businesses, all HHs",
                      "Business profit, all HHs"),

    wage_labor    = c("HH was engaged in wage labor",
                      "# of HH members engaged in wage labor, all HHs",
                      "Avg # of days HH members engaged in wage labor, all HHs",
                      "Monthly salary from wage labor, all HHs"),

    psychosocial  = c("Mental health index: Depression (0-70)",
                      "Mental health index: Disability (0-28)"),

    consumption   = c("Monthly food expenditure",
                      "Monthly food expenditure per capita",
                      "Monthly non-food expenditure",
                      "Monthly non-food expenditure per capita",
                      "Total monthly expenditure",
                      "Total monthly expenditure per capita")
  )

  # Check if the specified module exists
  if (!module %in% names(modules)) {
    stop("Specified module not found.")
  }

  # Retrieve the variables for the specified module
  variables <- modules[[module]]

  # Initialize a list to store results for each variable
  results_list <- list()

  # Specify LASSO variables

  lasso_vars <- c(  "sex_hhh_imp",                   "age_hhh_imp", # HH CHARACTERISTICS
                    "adult_total",

                    "adult_empl_imp",                "wage_job_any_imp", # WAGE LABOUR/EMPLOYMENT

                    "hh_own_bus_imp",                "business_count_imp",#BUSINESS
                    "ent_profit_w_all_imp",

                    "hasplots_imp",                  "wet_crop_imp", #AGRICULTURE
                    "dry_crop_imp",

                    "own_livestock",                 "hh_chicken_all_imp",       # LIVESTOCK
                    "hh_goat_all_imp",               "livestock_sold_all_imp",
                    "lstock_cons_all_imp",           "lstock_turnover_all_imp",

                    "food_cons_score_w_imp",         "fies_score_imp", # FOOD SECURITY

                    "hh_assets_own_imp",             "hh_assets_tot_imp",  # ASSETS
                    "farm_assets_tot_imp",           "farm_assets_own_imp",
                    "both_assets_own_imp",           "asset_group_imp",

                    "ps_depression_cesd_imp",        "ps_depression_sqr_imp", # PSYCHOSOCIAL

                    "shock_group_1_imp",             "shock_group_2_imp", # SHOCKS
                    "shock_group_3_imp",             "shock_group_4_imp",
                    "shock_group_5_imp",             "shock_group_6_imp",
                    "shock_group_7_imp",             "shock_group_8_imp",
                    "tot_shocks_imp",                "avg_shocks_imp",

                    "tot_food_exp_month_wins_imp",   "food_exp_pc_w_imp",  # CONSUMPTION EXPENDITURE
                    "tot_nfood_exp_month_wins_imp",  "nfood_exp_pc_w_imp",
                    "tot_exp_month_wins_imp",        "tot_exp_month_pc_wins_imp",
                    "fes_imp",                       "avg_bot_unit_price_imp"
  )

  if (sample == "FULL") {

    df            <- data %>%
      filter(round_pooled != 0)

  } else if (sample == "END") {

    df            <- data %>%
      filter(round_pooled == 11)

  }

  # Iterate through the variables of the specified module
  for (variable in variables) {

    # Placeholder for analysis of each variable.
    # This is where you'd perform the regression, tidy up the results, etc.

    # Data pre-processing specific to the variable
    lasso         <- df %>%
      mutate(across(all_of(lasso_vars), ~ ifelse(is.na(.), 0, .)))

    # First, we need to get the list of lasso controls to be used for the regression

    outcome = eval(variable)

    # define control variable (i.e. outcome at baseline), which has the suffix _imp
    control       <- paste0(outcome, "_imp")

    # remove rows with NAs in the outcome variable, otherwise we can"t run glmnet
    lasso         <- lasso[complete.cases(lasso[, c(outcome, control)]), ]

    # Define response variable and predictor matrix for LASSO

    # vars
    vars          <- lasso[complete.cases(lasso[, c(outcome, lasso_vars)]), ]

    # define response variable
    depvar        <- data.matrix(vars[, outcome])

    # define matrix of possible predictor variables
    indepvars     <- data.matrix(vars[, lasso_vars])

    # LASSO model fitting
    # run a lasso to get a value for lambda
    model         <- cv.glmnet(indepvars,
                               depvar,
                               alpha = 1,
                               type.measure = "mse")

    # get the coefficients with the best lambda
    c             <- coef(model, s = "lambda.1se", exact = TRUE)

    # get the list of selected indicators (where coefficient is not 0, i.e. not dropped through the lasso regression)
    inds          <- which(c != 0)

    # store the indicators in a vector
    variables     <- row.names(c)[inds]

    # remove unnecessary variables from the lasso vector, these are the fixed effects and intercept using the function we defined above
    variables     <- variables[!(variables %in% c("(Intercept)", variable))]

    # convert the character vector into one string with + in between for the regression below
    controls      <- paste0(variables, collapse = " + ")
    controls      <- paste0("+ ", controls)
    control       <- paste0("+ ", control)

    # for some outcomes, no lasso controls will be selected, for this reason we need to adjust the vector of controls so that the regression below still runs
    variables_all <- toString(variables)
    controls      <- ifelse(variables_all == "", "", controls)

    # Now, we can run the regression with the selected lasso controls

    if (sample == "FULL") {

    reg_ssd <- df %>%

      felm(as.formula(paste(outcome, "~ treatment", controls, control, " | level1 + round_cont | 0 | level4 + hhid")), data = .) %>% # felm uses Stata robust se correction

      tidy() %>%

      mutate(conf.low  = estimate - std.error*1.645,
             conf.high = estimate + std.error*1.645) %>%

      filter(term == "treatment") %>%

      mutate(term = as.character(outcome))

    } else if (sample == "END") {

      reg_ssd <- df %>%

        felm(as.formula(paste(outcome, "~ treatment", controls, control, " | level1 + round_cont | 0 | level4")), data = .) %>%

        tidy() %>%

        mutate(conf.low  = estimate - std.error*1.645,
               conf.high = estimate + std.error*1.645) %>%

        filter(term == "treatment") %>%

        mutate(term = as.character(outcome))
    }

    results_list[[variable]] <- reg_ssd

  }

    # Combine results and categorize variables from specific modules
    combined_results <- bind_rows(results_list, .id = "variable") %>%
      mutate(group = case_when(

        variable %in% c("hasplots_z", "own_livestock_z", "hh_own_bus_z",
                        "wage_job_any_z") ~ "Participation",

        variable %in% c("farm_assets_tot_z", "plot_n_z", "farm_size_z", "tot_lstock_count_z",
                        "tot_animal_tlu_z", "tot_lstock_count_tlu_z", "hh_chicken_all_z",
                        "hh_goat_all_z") ~ "Investments/Assets",

        variable %in% c("ag_hh_mem_work_count_z", "num_days_ag_avg_z", "liv_hh_mem_work_z",
                        "num_days_lstock_avg_z", "member_bus_total_z", "bus_labor_all_z",
                        "business_count_z", "adult_empl_z", "avg_emp_days_z") ~ "Time use",

        variable %in% c("revenue_all_z", "livestock_sold_all_z", "lstock_cons_all_z",
                        "lstock_turnover_all_z", "ent_profit_w_all_z", "tot_inc_w_all_z") ~ "Revenue",

        variable %in% c("tot_food_exp_month_wins_z", "food_exp_pc_w") ~ "Food expenditure",

        variable %in% c("tot_nfood_exp_month_wins_z", "nfood_exp_pc_w_z") ~ "Non-food expenditure",

        variable %in% c("tot_exp_month_wins_z", "tot_exp_month_pc_wins_z") ~ "Total expenditure",

        variable %in% c("food_cons_score_w_z", "fcs_profile1_z", "fcs_profile2_z", "fcs_profile3_z") ~ "FCS",

        variable %in% c("fies_score_z", "fies_profile1_z", "fies_profile2_z", "fies_profile3_z") ~ "FIES",

        variable %in% c("min_diet_index_z") ~ "MDD-W",

        variable %in% c("diet_score_z") ~ "WDDS",

        variable %in% c("ps_depression_sqr_z") ~ "SRQ-20",

        variable %in% c("ps_depression_cesd_z") ~ "CESD-R-10"

      )) %>%
      # Remove cases that did not match any condition
      filter(!is.na(group)) %>%
      # Dynamically assigning colors based on the number of unique groups
      mutate(group = factor(group, levels = c("Participation", "Investments/Assets", "Time use", "Revenue",
                                              "Food expenditure", "Non-food expenditure", "Total expenditure",
                                              "FCS", "FIES", "MDD-W", "WDDS",
                                              "SRQ-20", "CESD-R-10"),
                            labels = c("Participation", "Investments/Assets", "Time use", "Revenue",
                                       "Food expenditure", "Non-food expenditure", "Total expenditure",
                                       "FCS", "FIES", "MDD-W", "WDDS",
                                       "SRQ-20", "CESD-R-10"), ordered = TRUE),
             group = gsub(" ", "<br>", group),
             group = factor(group, levels = c("Participation", "Investments/Assets", "Time<br>use", "Revenue",
                                              "Food<br>expenditure", "Non-food<br>expenditure", "Total<br>expenditure",
                                              "FCS", "FIES", "MDD-W", "WDDS",
                                              "SRQ-20", "CESD-R-10"),
                            labels = c("Participation", "Investments/Assets", "Time<br>use", "Revenue",
                                       "Food<br>expenditure", "Non-food<br>expenditure", "Total<br>expenditure",
                                       "FCS", "FIES", "MDD-W", "WDDS",
                                       "SRQ-20", "CESD-R-10"), ordered = TRUE),
             color = brewer.pal(n = length(unique(group)), name = "Set2")[match(group, unique(group))],
             name = glue("<i style='color:{color}'>{group}</i>"),
             name = fct_reorder(name, as.numeric(factor(group))))

    # Parametrize the use of indicator list in processing

    current_module <- module

    # Check if the current module has an indicator mapping
    if(current_module %in% names(indicator_mappings)) {
      # Get the indicator list for the current module
      indicator_list <- indicator_mappings[[current_module]]

      # Use the indicator list to relevel and rename terms in combined_results
      combined_results <- combined_results %>%
        mutate(term = factor(indicator_list, levels = indicator_list))

    } else {
      warning("No indicator list defined for the module: ", current_module)
    }

    combined_results %>%

      ggplot(

        aes(x =  estimate,
            y = term,
            color = group

        )) +

      geom_pointrange( # Confidence interval

        aes(
          xmin = conf.low,
          xmax = conf.high
        ),

        show.legend = FALSE

      ) +

      geom_vline(xintercept = 0) +
      labs(x = "Impact measured in standard deviations of the outcome") +
      scale_y_discrete(NULL, limits = rev) +
      scale_color_manual(values = setNames(combined_results$color, combined_results$group)) +
      facet_grid(name ~ ., scales = "free_y", switch = "y", space = "free_y") +
      theme_minimal(base_size = 18) +
      theme(strip.placement     = "outside",
            strip.text.y.left   = element_markdown(size = 18,
                                                   angle = 0,
                                                   margin = margin(10, 10, 10, 10)),
            panel.spacing.y     = unit(0, "mm")
      ) -> plot

    plot_file_name <- paste0(path, "/", module, "_coef_plot.png")
    ggsave(plot_file_name, plot, width = 17, height = 9, units = "in", dpi = 300)

    return(list("Plot saved" = plot_file_name, "Data" = combined_results))

}

