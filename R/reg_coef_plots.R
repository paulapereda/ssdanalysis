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

    psychosocial       = c("ps_depression_cesd_z",      "ps_depression_sqr_z"),

    consumption        = c("tot_food_exp_month_wins_z", "food_exp_pc_w",             "tot_nfood_exp_month_wins_z",
                           "nfood_exp_pc_w_z",          "tot_exp_month_wins_z",      "tot_exp_month_pc_wins_z"),

    assets             = c("both_assets_own_z",         "farm_assets_tot_z",         "hh_assets_tot_z",            "tot_assets_z",
                           "farm_assets_own_z",         "hh_assets_own_z",           "asset_group_z")
  )

  # Check if the specified module exists
  if (!module %in% names(modules)) {
    stop("Specified module not found.")
  }

  # Retrieve the variables for the specified module
  variables <- modules[[module]]

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


}
