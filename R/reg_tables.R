reg_tables <- function(data, sample, module, path = here::here()) {

  set.seed(61625)

  # Defining modules and their respective variables inside the function
  modules <- list(

    food_security      = c("food_cons_score_w",       "fcs_profile1",           "fcs_profile2",
                           "fcs_profile3",            "food_cons_score_n_w",
                           "fies_score",              "fies_profile1",          "fies_profile2",
                           "fies_profile3",           "min_diet_index",         "diet_score"),

    agriculture        = c("hasplots",                "farm_assets_tot",        "plot_n",
                           "farm_size",               "ag_hh_mem_work_count",   "num_days_ag_avg"),

    livestock          = c("own_livestock",           "liv_hh_mem_work",        "num_days_lstock_avg",
                           "tot_animal",              "tot_lstock_count",       "tot_animal_tlu",
                           "tot_lstock_count_tlu",    "hh_chicken",             "hh_chicken_all",
                           "hh_goat",                 "hh_goat_all",            "livestock_sold",
                           "livestock_sold_all",      "lstock_cons",            "lstock_cons_all",
                           "lstock_turnover",         "lstock_turnover_all"),

    business           = c("hh_own_bus",              "member_bus_total",       "bus_labor_all",
                           "business_count",          "asset_n_7_all",          "ent_profit_w",
                           "ent_profit_w_all"),

    wage_labor         = c("wage_job_any",            "adult_empl",             "avg_emp_days",
                           "tot_inc_w",               "tot_inc_w_all"),

    coping_strategies  = c("num_strg_used_12",        "num_strg_used_30",       "max_coping_behavior",
                           "cp_profile1",             "cp_profile2",            "cp_profile3",
                           "rcsi_score"),

    revenue            = c("revenue_all",             "harvest_kg_all_crop",    "revenue_hh_all",
                           "harvest_kg_hh_crop",      "harvest_hh_crop_34",     "harvest_hh_crop_17",
                           "revenue_shared_all",      "harvest_kg_shared_crop", "harvest_shared_crop_34",
                           "harvest_shared_crop_17",  "fertilizer"),

    psychosocial       = c("ps_depression_cesd",      "ps_depression_sqr"),

    assets             = c("both_assets_own",         "farm_assets_tot",        "hh_assets_tot",
                           "tot_assets",              "farm_assets_own",        "hh_assets_own",
                           "asset_group"),

    consumption        = c("tot_food_exp_month_wins", "food_exp_pc_w",          "tot_nfood_exp_month_wins",
                           "nfood_exp_pc_w",          "tot_exp_month_wins",     "tot_exp_month_pc_wins",
                           "fes",                     "avg_bot_unit_price"),

    finance            = c("any_savings",             "rtrans_family",          "val_trans_tohh",
                           "hht_family",              "val_trans_fromhh",       "fam_loan_out",
                           "fam_loan_out_amount",     "fam_loan_in",            "fam_loan_in_amount")
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
    outcome_2 = as.name(variable)

    mean_outcome  <- data %>%
      filter(treatment_arm == 0 & round_pooled == 11) %>%
      summarise(mean = round(mean(!!outcome_2, na.rm = T), 2)) %>%
      pull(mean)

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

    if (sample == "FULL") {

      reg1 <- felm(as.formula(paste(outcome, "~ treatment", " | level1 + round_cont | 0 | level4")), data = df)
      reg2 <- felm(as.formula(paste(outcome, "~ treatment", control, " | level1 + round_cont | 0 | level4 + hhid")), data = df)
      reg3 <- felm(as.formula(paste(outcome, "~ treatment", control, controls, " | level1 + round_cont | 0 | level4 + hhid")), data = df)
      reg4 <- felm(as.formula(paste(outcome, "~ treat_uct + treat_ffa", " | level1 + round_cont | 0 | level4 + hhid")), data = df)
      reg5 <- felm(as.formula(paste(outcome, "~ treat_uct + treat_ffa", control, " | level1 + round_cont | 0 | level4 + hhid")), data = df)
      reg6 <- felm(as.formula(paste(outcome, "~ treat_uct + treat_ffa", control, controls, " | level1 + round_cont | 0 | level4 + hhid")), data = df)

    } else if (sample == "END") {

      # Now, we can run the regression with the selected lasso controls
      reg1 <- felm(as.formula(paste(outcome, "~ treatment", " | level1 + round_cont | 0 | level4")), data = df)
      reg2 <- felm(as.formula(paste(outcome, "~ treatment", control, " | level1 + round_cont | 0 | level4")), data = df)
      reg3 <- felm(as.formula(paste(outcome, "~ treatment", control, controls, " | level1 + round_cont | 0 | level4")), data = df)
      reg4 <- felm(as.formula(paste(outcome, "~ treat_uct + treat_ffa", " | level1 + round_cont | 0 | level4")), data = df)
      reg5 <- felm(as.formula(paste(outcome, "~ treat_uct + treat_ffa", control, " | level1 + round_cont | 0 | level4")), data = df)
      reg6 <- felm(as.formula(paste(outcome, "~ treat_uct + treat_ffa", control, controls, " | level1 + round_cont | 0 | level4")), data = df)

    }

    # Generate and save the output
    stargazer(reg1, reg2,
              reg3, reg4,
              reg5, reg6,
              align = TRUE,
              dep.var.labels = c("(1)", "(2)","(3)", "(4)", "(5)", "(6)"),
              omit.stat = c("f", "adj.rsq", "ser"),
              no.space = TRUE,
              digits = 3, # number of decimals
              add.lines = list(c("Mean outcome", rep(mean_outcome, 6)),
                               c("Lasso controls", "No", "No", "Yes", "No", "No", "Yes"),
                               c("County FE", rep("Yes", 6)),
                               c("Month FE",  rep("Yes", 6))),
              dep.var.caption = "", # remove dep var header
              out = paste0(path, "/", outcome, ".tex"))

  }
}
