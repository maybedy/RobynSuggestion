########### !!! Important !!! ################################
#### This is description of demo including our suggestion ####
# 1. Step 1-4: every section is similar to original version
# except loading hyperparameter.
# This is not a main part, but our additional suggestion
# based on our practical usage of Robyn.
# 2. Step 5-10: *** Key Suggestion #1, 2, 3***
# You can check our key suggestion
# 3. Step 5'-10': ***Another Suggestion***
# You can check revised version for considering zero spending weeks
# 0. You can check details of our suggestion in each definition of function

################################################################
#### Step 0: Preparation

# remotes::install_github("facebookexperimental/Robyn/R")
library(Robyn)

# install.packages("remotes")
# library(remotes)
# remotes::install_github("maybedy/RobynSuggestion")
library(RobynSuggestion)

Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)
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
#### (Add #1) Setting Hyperparameter Ranges By Channels
print("(Add #1) Setting Hyperparameter Ranges By Channels")
hyperparameters <- put_hyper_params(InputCollect, input_vars, input_hyper)

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
  iterations = 2000, # 2000,
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
#### (Add #2) Setting Budget Constraints By Spending in Dollars
#### Step 5: Generate budge boundaries of each media channel.
# Suggest 1 function:
# 1) generate_budget_boundaries
print("[Progress] Step 5: Get Budget Boundaries of each channel.")
InputCollect$paid_media_spends
budget_low <- c(200000, 200000, 60000, 100000, 30000)
budget_high <- c(400000, 400000, 100000, 200000, 100000)
exp_spends <- 800000
budget_bd <- generate_budget_boundaries(
  InputCollect,
  budget_low,
  budget_high,
  exp_spends
)


###########################################
#### (Issue #1) Budget Allocator Optimum Results with Benchmarks
#### Step 6: Get some critical benchmarks to evaluate the efficiency of the suggested optimal allocation.
# Suggest 1 function:
# 1) get_allocator_benchmarks
print("(Issue #1) Budget Allocator Optimum Results with Benchmarks")
print("[Progress] Step 6: Get benchmarks of allocation.")
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
# (From allocator.R)
get_allocator_benchmarks(
  AllocatorCollect_opt,
  AllocatorCollect_hist,
  AllocatorCollect_recent
)


#########################
#### (Key Suggestion #1) Response-Driven Approaches
#### Step 7: Decompose dependent variable of each media and rest.
# Suggest 1 function:
# 1) decompose_dependent_vars
print("(!!! Key Suggestion #1, 2) Response-, Dependent-Driven Approaches")
print("[Progress] Step 7: Decompose dependent variable of each media and rest.")
decompose_dependent_vars(InputCollect)

## If you want to get result from pre period,
pre_period <- c(as.Date("2018-07-20"), as.Date("2018-08-19"))
decompose_dependent_vars(InputCollect, pre_period = pre_period)


#########################
#### (Key Suggestion #1, 2) Response-, Dependent-Driven Approaches
#### Step 8: Get the response of each media
# Suggest 3 functions:
# 1) get_individual_result
# 2) saturation_hill_revised
# 3) predict_individual_result
print("[Progress] Step 8: Get the response of each media")
post_period <- c(as.Date("2018-08-27"), as.Date("2019-08-19"))
get_individual_result(
  InputCollect,
  OutputCollect,
  "print_S",
  select_model,
  type = "mean"
)
get_individual_result(
  InputCollect,
  OutputCollect,
  "print_S",
  select_model,
  type = "mean"
)

predict_individual_result(
  InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  c(InputCollect$window_start, InputCollect$window_end),
  "print_S",
  select_model,
  type = "mean"
)

predict_individual_result(
  InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  post_period,
  "tv_S",
  select_model,
  type = "mean"
)

#########################
#### (Key Suggestion #1, 2) Response-, Dependent-Driven Approaches
#### Step 9: Get sum of the response from media channels
# Suggest 2 function:
# 1) get_response_sum_on_trainining
# 2) predict_response_sum_on_test
print("[Progress] Step 9: Get sum of the response from media channels")
decompose_dependent_vars(InputCollect)
InputCollect$paid_media_spends
get_response_sum_on_trainining(InputCollect, OutputCollect, select_model)
predict_response_sum_on_test(
  InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  post_period,
  select_model
)


############################
#### (Key Suggestion #3) Validation: Response- vs. Dependent-Driven Approaches
#### Step 10: Validation
# Suggest 1 function:
# 1) validate_predicts
print("(!!! Key Suggestion #3) Validation: Response- vs. Dependent-Driven Approaches")
print("[Progress] Step 10: Validation")
validate_predicts(
  InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  post_period,
  select_model
)


############################
#### (Issue #2) Budget Allocator Optimum Results Considering Weeks with Zero Spending
#### Steps for
print("=====================================================")
print("(!!! Issue #2) Budget Allocator Optimum Results Considering Weeks with Zero Spending")
print("[Progress] Step 5': Considers zero spending weeks for measuring the spend increase and response increase.")
compare_zero_spend(
  InputCollect,
  AllocatorCollect_hist,
  c(InputCollect$window_start, InputCollect$window_end)
)

print("[Progress] Step 6': Get benchmarks of allocation.")
get_allocator_benchmarks_zero_spend(
  InputCollect,
  AllocatorCollect_opt,
  AllocatorCollect_hist,
  AllocatorCollect_recent
)

print("[Progress] Step 7': No needs. - Decompose dependent variable of each media and rest.")

#### Steps
print("[Progress] Step 8': Get the response of each media")
get_individual_result_zero_spend(
  InputCollect,
  OutputCollect,
  "tv_S",
  select_model,
  type = "mean"
)

media_metric <- "tv_S"
get_individual_result_zero_spend(
  InputCollect,
  OutputCollect,
  "print_S",
  select_model,
  type = "mean"
)
predict_individual_result_zero_spend(
  InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  c(InputCollect$window_start, InputCollect$window_end),
  "print_S",
  select_model,
  type = "mean"
)
predict_individual_result(
  InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  post_period,
  "tv_S",
  select_model,
  type = "mean"
)

print("[Progress] Step 9': Get sum of the response from media channels")
get_response_sum_on_train_zero_spend(
  InputCollect,
  OutputCollect,
  select_model
)
predict_response_sum_on_test_zero_spend(
  InputCollect,
  OutputCollect,
  dt_simulated_weekly,
  post_period,
  select_model
)

print("[Progress] Step 10': Validation")
post_data <- data(dt_simulated_weekly)
validate_predicts_zero_spend(
  InputCollect,
  OutputCollect,
  post_data,
  post_period,
  select_model
)
