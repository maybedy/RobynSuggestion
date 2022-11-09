########### !!! Important !!! ###################
#### This is manual for our suggestion ####
# 1) During Step 1-4, every section is similar to original version
# except loading hyperparameter.
# This is not a main part, but our additional suggestion
# based on our practical usage of Robyn.
# 2) From Step 5-10, you can check our suggestion
# Goal (or Main purpose) for all suggestion is ...
# each step is running our suggested function
# 3) From Step 5'-10', you can check non-zero version
# ....


# validation_test(InputCollect, OutputCollect, dt_simulated_weekly, post_period, select_model)

library(Robyn)
library(RobynSuggestion)

print("=====================================================")
print("[Progress] Step 5: Get Channel Boundary")
InputCollect$paid_media_spends
budget_low <- c(200000, 200000, 60000, 100000, 30000)
budget_high <- c(400000, 400000, 100000, 200000, 100000)
exp_spends <- 800000
budget_bd <- budget_boundary(InputCollect, budget_low, budget_high, exp_spends)

Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)
getwd()
robyn_object <- "../MyRobyn.RDS"



################################################################
#### Step 1: Load data

## Check simulated dataset or load your own dataset
data("dt_simulated_weekly")
head(dt_simulated_weekly)

## Check holidays from Prophet
# 59 countries included. If your country is not included, please manually add it.
# Tip: any events can be added into this table, school break, events etc.
data("dt_prophet_holidays")
head(dt_prophet_holidays)

# Directory where you want to export results to (will create new folders)
# robyn_object <- "~/Desktop"

### DEPRECATED: It must have extension .RDS. The object name can be different than Robyn:
# results = readRDS(robyn_object)

################################################################
#### Step 2a: For first time user: Model specification in 4 steps
InputCollect <- robyn_inputs(
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  date_var = "DATE", # date format must be "2020-01-01"
  dep_var = "revenue", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "holiday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "DE", # input one country. dt_prophet_holidays includes 59 countries by default
  context_vars = c("competitor_sales_B", "events"), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("tv_S", "ooh_S", "print_S", "facebook_S", "search_S"), # mandatory input
  paid_media_vars = c("tv_S", "ooh_S", "print_S", "facebook_I", "search_clicks_P"), # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
  organic_vars = "newsletter", # marketing activity without media spend
  # factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2016-11-21",
  window_end = "2018-08-20",
  adstock = "weibull_cdf" # geometric, weibull_cdf or weibull_pdf.
)

#### 2a-2: Second, define and add hyperparameters
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)

## 1. IMPORTANT: set plot = TRUE to see helper plots of hyperparameter's effect in transformation
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)

#########################################################
### [Additional Suggestion: Interface of experiments] ###
### Comments: Reason for listing hyperparameters
### actual usage pattern of multiple hyper-param sets of media for experiments

### Example hyperparameters ranges for Geometric adstock
hyperparameters <- list(
  facebook_S_alphas = c(0.5, 3),
  facebook_S_gammas = c(0.3, 1),
  facebook_S_thetas = c(0, 0.3),
  print_S_alphas = c(0.5, 3),
  print_S_gammas = c(0.3, 1),
  print_S_thetas = c(0.1, 0.4),
  tv_S_alphas = c(0.5, 3),
  tv_S_gammas = c(0.3, 1),
  tv_S_thetas = c(0.3, 0.8),
  search_S_alphas = c(0.5, 3),
  search_S_gammas = c(0.3, 1),
  search_S_thetas = c(0, 0.3),
  ooh_S_alphas = c(0.5, 3),
  ooh_S_gammas = c(0.3, 1),
  ooh_S_thetas = c(0.1, 0.4),
  newsletter_alphas = c(0.5, 3),
  newsletter_gammas = c(0.3, 1),
  newsletter_thetas = c(0.1, 0.4)
)
input_vars <- list()
input_vars$type1 <- c("tv_S", "ooh_S", "facebook_S")
input_vars$type2 <- c("print_S", "search_S")
input_vars$type3 <- c("newsletter")
input_hyper <- list()
input_hyper$type1 <- list(
  alphas = c(0.5, 3),
  gammas = c(0.3, 1),
  shapes = c(0.0001, 5),
  scales = c(0, 0.1)
)
input_hyper$type2 <- list(
  alphas = c(0.5, 1),
  gammas = c(0.3, 1),
  shapes = c(0.0001, 10),
  scales = c(0, 0.1)
)
input_hyper$type3 <- list(
  alphas = c(0.5, 1),
  gammas = c(0.3, 1),
  shapes = c(0.0001, 1),
  scales = c(0, 0.5)
)
hyperparameters <- put_hyppar(InputCollect, input_vars, input_hyper)

InputCollect <- robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)


################################################################
#### Step 2b: For known model specification, setup in one single step
#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  InputCollect$modNLS$plots$facebook_I
  InputCollect$modNLS$plots$search_clicks_P
}

##### Manually save and import InputCollect as JSON file
# robyn_write(InputCollect, dir = "~/Desktop")
# InputCollect <- robyn_inputs(
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   json_file = "~/Desktop/RobynModel-inputs.json")

################################################################
#### Step 3: Build initial model

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  # cores = NULL, # default to max available
  # add_penalty_factor = FALSE, # Untested feature. Use with caution.
  # 2000 recommended for the dummy dataset with no calibration
  iterations = 200, # 2000,
  # 5 recommended for the dummy dataset
  trials = 1,
  # outputs = FALSE disables direct model output - robyn_outputs()
  outputs = FALSE
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot

## Calculate Pareto optimality, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- robyn_outputs(
  InputCollect, OutputModels,
  # pareto_fronts = "auto",
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto", "all", or NULL (for none)
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_pareto = TRUE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = robyn_object, # path for plots export
  export = TRUE # this will create files locally
)
print(OutputCollect)

## 4 csv files are exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R
# pareto_hyperparameters.csv, hyperparameters per Pareto output model
# pareto_aggregated.csv, aggregated decomposition per independent variable of all Pareto output
# pareto_media_transform_matrix.csv, all media transformation vectors
# pareto_alldecomp_matrix.csv, all decomposition vectors of independent variables

################################################################
#### Step 4: Select and save the any model

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- OutputCollect$allSolutions[1] # Pick one of the models from OutputCollect to proceed

#### Since 3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model)
print(ExportedModel)


#################################
#### Steps for suggestion #######
#################################

############################################
#### Step 5: Get channel boundary
# Suggest 1 function:
# 1) budget_boundary



###########################################
#### Step 6: Get analytical result of allocator
# Suggest 1 function:
# 1) Allocator_results
print("=====================================================")
print("[Progress] Step 6: Get Analytical result of allocator")
AllocatorCollect_opt <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = unlist(budget_bd$cost$low, use.names = FALSE),
  channel_constr_up = unlist(budget_bd$cost$high, use.names = FALSE),
  expected_spend = exp_spends * 2, # Total spend to be simulated
  expected_spend_days = 14, # Duration of expected_spend in days
  export = TRUE
)

AllocatorCollect_hist <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = unlist(budget_bd$hist$low, use.names = FALSE),
  channel_constr_up = unlist(budget_bd$hist$high, use.names = FALSE),
  expected_spend = exp_spends * 2, # Total spend to be simulated
  expected_spend_days = 14, # Duration of expected_spend in days
  export = TRUE
)

AllocatorCollect_recent <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = unlist(budget_bd$recent$low, use.names = FALSE),
  channel_constr_up = unlist(budget_bd$recent$high, use.names = FALSE),
  expected_spend = exp_spends * 2, # Total spend to be simulated
  expected_spend_days = 14, # Duration of expected_spend in days
  export = TRUE
)
# (From validate.R)
Allocator_results(AllocatorCollect_opt, AllocatorCollect_hist, AllocatorCollect_recent)


#########################
#### Step 7: Decompose each KPI of media, non-media, and whole
# Suggest 1 function:
# 1) decomp_dependent

print("=====================================================")
print("[Progress] Step 7: Decompose each KPI of media, non-media, and whole")
decomp_dependent(InputCollect)

## If you want to get result from pre period,
pre_period <- c(as.Date("2018-07-20"), as.Date("2018-08-19"))
decomp_dependent(InputCollect, pre_period = pre_period)


#########################
#### Step 8: Get Performance of Each Media
# Suggest 3 functions:
# 1) result_media
# 2) saturation_hill_new
# 3) result_media_post

print("=====================================================")
print("[Progress] Step 8: Get Performance of each media")
post_period <- c(as.Date("2018-08-27"), as.Date("2019-08-19"))
result_media(InputCollect, OutputCollect,
  "print_S",
  select_model,
  type = "mean"
)

result_media(InputCollect,
  OutputCollect,
  "print_S",
  select_model,
  type = "mean"
)

result_media_post(InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  c(InputCollect$window_start, InputCollect$window_end),
  "print_S",
  select_model,
  type = "mean"
)

result_media_post(InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  post_period,
  "tv_S",
  select_model,
  type = "mean"
)

#########################
#### Step 9: Get Total Performance from whole media
# Suggest 2 function:
# 1) result_total
# 2) result_total_post

print("=====================================================")
print("[Progress] Step 9: Get Total Performance from whole media")
decomp_dependent(InputCollect)
InputCollect$paid_media_spends
result_total(InputCollect, OutputCollect, select_model)
result_total_post(InputCollect, OutputCollect, dt_simulated_weekly, post_period, select_model)


############################
#### Key Suggestion ########
#### Step 10: Validate the reuslts by history and prediction
# Suggest 1 function:
# 1) validation_test

print("=====================================================")
print("[Progress] Step 10: Validate the results by history and prediction")
validation_test(InputCollect, OutputCollect, dt_simulated_weekly, post_period, select_model)


######### Non-zero #########


#### Step 5': Non-zero...
print("=====================================================")
print("             Steps for non-zero")
print("=====================================================")
print("[Progress] Step 5': ")
compare_nonzero(
  InputCollect,
  AllocatorCollect_hist,
  c(InputCollect$window_start, InputCollect$window_end)
)

Allocator_results_new(AllocatorCollect_opt, AllocatorCollect_hist, AllocatorCollect_recent)

#### Step 6':


result_media_new(InputCollect, OutputCollect,
  "tv_S",
  select_model,
  type = "mean"
)

media_metric <- "tv_S"
result_media_new(InputCollect, OutputCollect,
  "print_S",
  select_model,
  type = "mean"
)
result_media_post_new(InputCollect, OutputCollect,
  dt_simulated_weekly,
  c(InputCollect$window_start, InputCollect$window_end),
  "print_S",
  select_model,
  type = "mean"
)
result_media_post(InputCollect, OutputCollect,
  dt_simulated_weekly,
  post_period,
  "tv_S",
  select_model,
  type = "mean"
)

result_total_new(InputCollect, OutputCollect, select_model)
result_total_post_new(InputCollect, OutputCollect, dt_simulated_weekly, post_period, select_model)
validation_test_new(InputCollect, OutputCollect, post_data, post_period, select_model)