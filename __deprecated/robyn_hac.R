# Copyright (c) Meta Platforms, Inc. and its affiliates.
library(Robyn)
library(dplyr)
library(rlang)
library(prophet)
library(tidyr)
library(glue)
library(stringr)
library(lares)
library(patchwork)

library("reticulate") # Load the library
packageVersion("Robyn")
## Force multicore when using RStudio
Sys.setenv(R_FUTURE_FORK_ENABLE = "true")
options(future.fork.enable = TRUE)
virtualenv_create("r-reticulate")
use_virtualenv("r-reticulate", required = TRUE)
#py_install("nevergrad", pip = TRUE)
#load("C:/Users/YEEUN/Desktop/robyn/RObyn/hackathon_sample/sample_dataset/dt_simulated_weekly.RData")
#load("C:/Users/YEEUN/Desktop/robyn/RObyn/hackathon_sample/sample_dataset/dt_prophet_holidays.RData")
rd=read.csv("test_data.csv")
rd_h=read.csv("test_holiday.csv")

# Directory where you want to export results to (will create new folders)
robyn_object <- "test.RDS"
head(rd,3)
rd_h = rd_h[1,]
################################################################
#### Step 2a: For first time user: Model specification in 4 steps
InputCollect <- robyn_inputs(
  dt_input = rd,
  dt_holidays = rd_h,
  date_var = "date", # date format must be "2020-01-01"
  dep_var = "revenue", # there should be only one dependent variable
  dep_var_type = "revenue", # "revenue" (ROI) or "conversion" (CPA)
  prophet_vars = c("trend", "season", "weekday", "holiday"), # "trend","season", "weekday" & "holiday"
  prophet_country = "KR", # input one country. dt_prophet_holidays includes 59 countries by default
#  context_vars = c("competitor_sales_B", "events"), # e.g. competitors, discount, unemployment etc
  paid_media_spends = c("media1", "media2", "media3", "media4"), # mandatory input
#  paid_media_vars = c("media1", "media2", "media3", "media4"), # mandatory.
  # paid_media_vars must have same order as paid_media_spends. Use media exposure metrics like
  # impressions, GRP etc. If not applicable, use spend instead.
#  organic_vars = c("newsletter"), # marketing activity without media spend
#  factor_vars = c("events"), # force variables in context_vars or organic_vars to be categorical
  window_start = "2022-02-01",
  window_end = "2022-05-31",
  adstock = "weibull_cdf" # geometric, weibull_cdf or weibull_pdf.
)

print(InputCollect)
InputCollect$paid_media_vars
#### 2a-2: Second, define and add hyperparameters
hyper_names(adstock = InputCollect$adstock, all_media = InputCollect$all_media)
## 1. IMPORTANT: set plot = TRUE to see helper plots of hyperparameter's effect in transformation
plot_adstock(plot = FALSE)
plot_saturation(plot = FALSE)
################################################################
################################################################
#input.R
#hyper_names....
#function for putting hyperparameter

################################################################
################################################################
hyperparameters <- list(
  media1_alphas=c(0.5, 3),
  media1_gammas=c(0.3, 1),
  media1_shapes=c(0.0001, 10),
  media1_scales=c(0, 0.1),
  media2_alphas=c(0.5, 3),
  media2_gammas=c(0.3, 1),
  media2_shapes=c(0.0001, 10),
  media2_scales=c(0, 0.1),
  media3_alphas=c(0.5, 3),
  media3_gammas=c(0.3, 1),
  media3_shapes=c(0.0001, 10),
  media3_scales=c(0, 0.1),
  media4_alphas=c(0.5, 3),
  media4_gammas=c(0.3, 1),
  media4_shapes=c(0.0001, 10),
  media4_scales=c(0, 0.1)
)
#### 2a-3: Third, add hyperparameters into robyn_inputs()
InputCollect <- Robyn::robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
print(InputCollect)
head(InputCollect$dt_mod,2)

#### Check spend exposure fit if available
if (length(InputCollect$exposure_vars) > 0) {
  InputCollect$modNLS$plots$facebook_I
  InputCollect$modNLS$plots$search_clicks_P
}

InputCollect$dt_mod
################################################################
################################################################
#keep prophet output for prediction

#initial function
dt_input <- InputCollect$dt_input
dt_transform <- dt_input
colnames(dt_transform)[colnames(dt_transform) == InputCollect$date_var] <- "ds"
colnames(dt_transform)[colnames(dt_transform) == InputCollect$dep_var] <- "dep_var"
dt_transform <- arrange(dt_transform, .data$ds)
# dt_transformRollWind
rollingWindowStartWhich <- InputCollect$rollingWindowStartWhich
rollingWindowEndWhich <- InputCollect$rollingWindowEndWhich
dt_transformRollWind <- dt_transform[rollingWindowStartWhich:rollingWindowEndWhich, ]
paid_media_spends = InputCollect$paid_media_spends
factor_vars = InputCollect$factor_vars
#checks.R
opts_pnd <- c("positive", "negative", "default")
check_prophet <- function(dt_holidays, prophet_country, prophet_vars, prophet_signs, dayInterval) {
  if (is.null(dt_holidays) || is.null(prophet_vars)) {
    return(invisible(NULL))
  } else {
    opts <- c("trend", "season", "weekday", "holiday")
    if (!all(prophet_vars %in% opts)) {
      stop("Allowed values for 'prophet_vars' are: ", paste(opts, collapse = ", "))
    }
    if ("weekday" %in% prophet_vars && dayInterval > 7) {
      warning("Ignoring prophet_vars = 'weekday' input given your data granularity")
    }
    if (is.null(prophet_country) || length(prophet_country) > 1 |
        !prophet_country %in% unique(dt_holidays$country)) {
      stop(paste(
        "You must provide 1 country code in 'prophet_country' input.",
        length(unique(dt_holidays$country)), "countries are included:",
        paste(unique(dt_holidays$country), collapse = ", "),
        "\nIf your country is not available, please manually add it to 'dt_holidays'"
      ))
    }
    if (is.null(prophet_signs)) {
      prophet_signs <- rep("default", length(prophet_vars))
    }
    if (!all(prophet_signs %in% opts_pnd)) {
      stop("Allowed values for 'prophet_signs' are: ", paste(opts_pnd, collapse = ", "))
    }
    if (length(prophet_signs) != length(prophet_vars)) {
      stop("'prophet_signs' must have same length as 'prophet_vars'")
    }
    return(invisible(prophet_signs))
  }
}

set_holidays <- function(dt_transform, dt_holidays, intervalType) {
  opts <- c("day", "week", "month")
  if (!intervalType %in% opts) {
    stop("Pass a valid 'intervalType'. Any of: ", paste(opts, collapse = ", "))
  }

  if (intervalType == "day") {
    holidays <- dt_holidays
  }

  if (intervalType == "week") {
    weekStartInput <- lubridate::wday(dt_transform$ds[1], week_start = 1)
    if (!weekStartInput %in% c(1, 7)) stop("Week start has to be Monday or Sunday")
    holidays <- dt_holidays %>%
      mutate(ds = floor_date(.data$ds, unit = "week", week_start = weekStartInput)) %>%
      select(.data$ds, .data$holiday, .data$country, .data$year) %>%
      group_by(.data$ds, .data$country, .data$year) %>%
      summarise(holiday = paste(.data$holiday, collapse = ", "), n = n())
  }

  if (intervalType == "month") {
    if (!all(day(dt_transform$ds) == 1)) {
      stop("Monthly data should have first day of month as datestampe, e.g.'2020-01-01'")
    }
    holidays <- dt_holidays %>%
      # mutate(ds = cut(.data$ds, intervalType)) %>%
      mutate(ds = cut(.data$ds, intervalType)) %>%
      select(.data$ds, .data$holiday, .data$country, .data$year) %>%
      group_by(.data$ds, .data$country, .data$year) %>%
      summarise(holiday = paste(.data$holiday, collapse = ", "), n = n())
  }

  return(holidays)
}
#input.R
prophet_decomp_fix <- function(dt_transform, dt_holidays,
                           prophet_country, prophet_vars, prophet_signs,
                           factor_vars, context_vars, paid_media_spends,
                           intervalType, dayInterval, custom_params) {
  check_prophet(dt_holidays, prophet_country, prophet_vars, prophet_signs, dayInterval)
  recurrence <- select(dt_transform, .data$ds, .data$dep_var) %>% rename("y" = "dep_var")
  holidays <- set_holidays(dt_transform, dt_holidays, intervalType)
  use_trend <- "trend" %in% prophet_vars
  use_holiday <- "holiday" %in% prophet_vars
  use_season <- "season" %in% prophet_vars | "yearly.seasonality" %in% prophet_vars
  use_weekday <- "weekday" %in% prophet_vars | "weekly.seasonality" %in% prophet_vars

  dt_regressors <- bind_cols(recurrence, select(
    dt_transform, all_of(c(context_vars, paid_media_spends))
  )) %>%
    mutate(ds = as.Date(.data$ds))

  prophet_params <- list(
    holidays = if (use_holiday) holidays[holidays$country == prophet_country, ] else NULL,
    yearly.seasonality = ifelse("yearly.seasonality" %in% names(custom_params),
                                custom_params[["yearly.seasonality"]],
                                use_season
    ),
    weekly.seasonality = ifelse("weekly.seasonality" %in% names(custom_params) & dayInterval <= 7,
                                custom_params[["weekly.seasonality"]],
                                use_weekday
    ),
    daily.seasonality = FALSE # No hourly models allowed
  )
  prophet_params <- append(prophet_params, custom_params)
  modelRecurrence <- do.call(prophet::prophet, as.list(prophet_params))

  # dt_regressors <<- dt_regressors
  # modelRecurrence <<- modelRecurrence

  if (!is.null(factor_vars) && length(factor_vars) > 0) {
    dt_ohe <- dt_regressors %>%
      select(all_of(factor_vars)) %>%
      ohse(drop = FALSE) %>%
      select(-any_of(factor_vars))
    ohe_names <- names(dt_ohe)
    for (addreg in ohe_names) modelRecurrence <- add_regressor(modelRecurrence, addreg)
    dt_ohe <- select(dt_regressors, -all_of(factor_vars)) %>% bind_cols(dt_ohe)
    mod_ohe <- prophet::fit.prophet(modelRecurrence, dt_ohe)
    dt_forecastRegressor <- predict(mod_ohe, dt_ohe)
    forecastRecurrence <- select(dt_forecastRegressor, -contains("_lower"), -contains("_upper"))
    for (aggreg in factor_vars) {
      oheRegNames <- grep(paste0("^", aggreg, ".*"), names(forecastRecurrence), value = TRUE)
      get_reg <- rowSums(select(forecastRecurrence, all_of(oheRegNames)))
      dt_transform[, aggreg] <- scale(get_reg, center = min(get_reg), scale = FALSE)
    }
  } else {
    if (dayInterval == 1) {
      warning(
        "Currently, there's a known issue with prophet that may crash this use case.",
        "\n Read more here: https://github.com/facebook/prophet/pull/2252"
      )
      # mod <<- mod
      # dt_regressors <<- dt_regressors
    }
    mod <- fit.prophet(modelRecurrence, dt_regressors)
    forecastRecurrence <- predict(mod, dt_regressors)
  }

  these <- seq_along(unlist(recurrence[,1]))
  if (use_trend) dt_transform$trend <- forecastRecurrence$trend[these]
  if (use_season) dt_transform$season <- forecastRecurrence$yearly[these]
  if (use_weekday) dt_transform$weekday <- forecastRecurrence$weekly[these]
  if (use_holiday) dt_transform$holiday <- forecastRecurrence$holidays[these]
  return(dt_transform)
}

test_transform <- prophet_decomp_fix(
  dt_transform,
  dt_holidays = InputCollect$dt_holidays,
  prophet_country = InputCollect$prophet_country,
  prophet_vars = InputCollect$prophet_vars,
  prophet_signs = InputCollect$prophet_signs,
  factor_vars = factor_vars,
  context_vars = InputCollect$context_vars,
  paid_media_spends = paid_media_spends,
  intervalType = InputCollect$intervalType,
  dayInterval = InputCollect$dayInterval,
  custom_params = NULL
)


#inputs.R

#inputs.R
robyn_engineering <- function(x, quiet = FALSE, ...) {
  if (!quiet) message(">> Running feature engineering...")
  InputCollect <- x
  check_InputCollect(InputCollect)
  dt_input <- InputCollect$dt_input
  dt_input <- select(dt_input, -all_of(InputCollect$unused_vars))
  paid_media_vars <- InputCollect$paid_media_vars
  paid_media_spends <- InputCollect$paid_media_spends
  factor_vars <- InputCollect$factor_vars
  rollingWindowStartWhich <- InputCollect$rollingWindowStartWhich
  rollingWindowEndWhich <- InputCollect$rollingWindowEndWhich

  # dt_inputRollWind
  dt_inputRollWind <- dt_input[rollingWindowStartWhich:rollingWindowEndWhich, ]

  # dt_transform
  dt_transform <- dt_input
  colnames(dt_transform)[colnames(dt_transform) == InputCollect$date_var] <- "ds"
  colnames(dt_transform)[colnames(dt_transform) == InputCollect$dep_var] <- "dep_var"
  dt_transform <- arrange(dt_transform, .data$ds)

  # dt_transformRollWind
  dt_transformRollWind <- dt_transform[rollingWindowStartWhich:rollingWindowEndWhich, ]

  ################################################################
  #### Model exposure metric from spend

  exposure_selector <- paid_media_spends != paid_media_vars
  names(exposure_selector) <- paid_media_vars

  if (any(exposure_selector)) {
    modNLSCollect <- list()
    yhatCollect <- list()
    plotNLSCollect <- list()
    mediaCostFactor <- colSums(subset(dt_inputRollWind, select = paid_media_spends), na.rm = TRUE) /
      colSums(subset(dt_inputRollWind, select = paid_media_vars), na.rm = TRUE)

    for (i in 1:InputCollect$mediaVarCount) {
      if (exposure_selector[i]) {
        # Run models (NLS and/or LM)
        dt_spendModInput <- subset(dt_inputRollWind, select = c(paid_media_spends[i], paid_media_vars[i]))
        results <- fit_spend_exposure(dt_spendModInput, mediaCostFactor[i], paid_media_vars[i])
        # Compare NLS & LM, takes LM if NLS fits worse
        mod <- results$res
        exposure_selector[i] <- if (is.null(mod$rsq_nls)) FALSE else mod$rsq_nls > mod$rsq_lm
        # Data to create plot
        dt_plotNLS <- data.frame(
          channel = paid_media_vars[i],
          yhatNLS = if (exposure_selector[i]) results$yhatNLS else results$yhatLM,
          yhatLM = results$yhatLM,
          y = results$data$exposure,
          x = results$data$spend
        )
        caption <- glued("
          nls: AIC = {aic_nls} | R2 = {r2_nls}
          lm: AIC = {aic_lm} | R2 = {r2_lm}",
                         aic_nls = signif(AIC(if (exposure_selector[i]) results$modNLS else results$modLM), 3),
                         r2_nls = signif(if (exposure_selector[i]) mod$rsq_nls else mod$rsq_lm, 3),
                         aic_lm = signif(AIC(results$modLM), 3),
                         r2_lm = signif(mod$rsq_lm, 3)
        )
        dt_plotNLS <- dt_plotNLS %>%
          pivot_longer(
            cols = c("yhatNLS", "yhatLM"),
            names_to = "models", values_to = "yhat"
          ) %>%
          mutate(models = str_remove(tolower(.data$models), "yhat"))
        models_plot <- ggplot(
          dt_plotNLS, aes(x = .data$x, y = .data$y, color = .data$models)
        ) +
          geom_point() +
          geom_line(aes(y = .data$yhat, x = .data$x, color = .data$models)) +
          labs(
            title = "Exposure-Spend Models Fit Comparison",
            x = sprintf("Spend [%s]", paid_media_spends[i]),
            y = sprintf("Exposure [%s]", paid_media_vars[i]),
            caption = caption,
            color = "Model"
          ) +
          theme_lares(legend = "top") +
          scale_x_abbr() +
          scale_y_abbr()

        # Save results into modNLSCollect. plotNLSCollect, yhatCollect
        modNLSCollect[[paid_media_vars[i]]] <- mod
        plotNLSCollect[[paid_media_vars[i]]] <- models_plot
        yhatCollect[[paid_media_vars[i]]] <- dt_plotNLS
      }
    }
    modNLSCollect <- bind_rows(modNLSCollect)
    yhatNLSCollect <- bind_rows(yhatCollect)
    yhatNLSCollect$ds <- rep(dt_transformRollWind$ds, nrow(yhatNLSCollect) / nrow(dt_transformRollWind))
  } else {
    modNLSCollect <- plotNLSCollect <- yhatNLSCollect <- NULL
  }

  # Give recommendations and show warnings
  if (!is.null(modNLSCollect) && !quiet) {
    threshold <- 0.80
    final_print <- these <- NULL # TRUE if we accumulate a common message
    metrics <- c("R2 (nls)", "R2 (lm)")
    names(metrics) <- c("rsq_nls", "rsq_lm")
    for (m in seq_along(metrics)) {
      temp <- which(modNLSCollect[[names(metrics)[m]]] < threshold)
      if (length(temp) > 0) {
        warning(sprintf(
          "%s: weak relationship for %s and %s spend",
          metrics[m],
          v2t(modNLSCollect$channel[temp], and = "and"),
          ifelse(length(temp) > 1, "their", "its")
        ))
        final_print <- TRUE
        these <- modNLSCollect$channel[temp]
      }
    }
    if (isTRUE(final_print)) {
      message(
        paste(
          "NOTE: potential improvement on splitting channels for better exposure fitting.",
          "Threshold (Minimum R2) =", threshold,
          "\n  Check: InputCollect$plotNLSCollect outputs"
        ),
        "\n  Check data on: ", v2t(these)
      )
    }
  }

  ################################################################
  #### Clean & aggregate data

  ## Transform all factor variables
  if (length(factor_vars) > 0) {
    dt_transform <- mutate_at(dt_transform, factor_vars, as.factor)
  }

  ################################################################
  #### Obtain prophet trend, seasonality and change-points

  if (!is.null(InputCollect$prophet_vars) && length(InputCollect$prophet_vars) > 0) {
    if (length(InputCollect[["custom_params"]]) > 0) {
      custom_params <- InputCollect[["custom_params"]]
    } else {
      custom_params <- list(...)
    } # custom_params <- list()
    robyn_args <- setdiff(
      unique(c(
        names(as.list(args(robyn_run))),
        names(as.list(args(robyn_outputs))),
        names(as.list(args(robyn_inputs))),
        names(as.list(args(robyn_refresh)))
      )),
      c("", "...")
    )
    prophet_custom_args <- setdiff(names(custom_params), robyn_args)
    if (length(prophet_custom_args) > 0) {
      message(paste("Using custom prophet parameters:", paste(prophet_custom_args, collapse = ", ")))
    }

    dt_transform <- prophet_decomp(
      dt_transform,
      dt_holidays = InputCollect$dt_holidays,
      prophet_country = InputCollect$prophet_country,
      prophet_vars = InputCollect$prophet_vars,
      prophet_signs = InputCollect$prophet_signs,
      factor_vars = factor_vars,
      context_vars = InputCollect$context_vars,
      paid_media_spends = paid_media_spends,
      intervalType = InputCollect$intervalType,
      dayInterval = InputCollect$dayInterval,
      custom_params = custom_params
    )
  }

  ################################################################
  #### Finalize enriched input

  dt_transform <- subset(dt_transform, select = c("ds", "dep_var", InputCollect$all_ind_vars))
  InputCollect[["dt_mod"]] <- dt_transform
  InputCollect[["dt_modRollWind"]] <- dt_transform[rollingWindowStartWhich:rollingWindowEndWhich, ]
  InputCollect[["dt_inputRollWind"]] <- dt_inputRollWind
  InputCollect[["modNLS"]] <- list(
    results = modNLSCollect,
    yhat = yhatNLSCollect,
    plots = plotNLSCollect
  )
  return(InputCollect)
}
#checks.R
check_InputCollect <- function(list) {
  names_list <- c(
    "dt_input", "paid_media_vars", "paid_media_spends", "context_vars",
    "organic_vars", "all_ind_vars", "date_var", "dep_var",
    "rollingWindowStartWhich", "rollingWindowEndWhich", "mediaVarCount",
    "factor_vars", "prophet_vars", "prophet_signs", "prophet_country",
    "intervalType", "dt_holidays"
  )
  if (!all(names_list %in% names(list))) {
    not_present <- names_list[!names_list %in% names(list)]
    stop(paste(
      "Some elements where not provided in your inputs list:",
      paste(not_present, collapse = ", ")
    ))
  }

  if (length(list$dt_input) <= 1) {
    stop("Check your 'dt_input' object")
  }
}
robyn_inputs <- function(dt_input = NULL,
                         dt_holidays = Robyn::dt_prophet_holidays,
                         date_var = "auto",
                         dep_var = NULL,
                         dep_var_type = NULL,
                         prophet_vars = NULL,
                         prophet_signs = NULL,
                         prophet_country = NULL,
                         context_vars = NULL,
                         context_signs = NULL,
                         paid_media_spends = NULL,
                         paid_media_vars = NULL,
                         paid_media_signs = NULL,
                         organic_vars = NULL,
                         organic_signs = NULL,
                         factor_vars = NULL,
                         adstock = NULL,
                         hyperparameters = NULL,
                         window_start = NULL,
                         window_end = NULL,
                         calibration_input = NULL,
                         json_file = NULL,
                         InputCollect = NULL,
                         ...) {

  ### Use case 3: running robyn_inputs() with json_file
  if (!is.null(json_file)) {
    json <- robyn_read(json_file, step = 1, ...)
    if (is.null(dt_input) || is.null(dt_holidays)) stop("Provide 'dt_input' and 'dt_holidays'")
    for (i in seq_along(json$InputCollect)) {
      assign(names(json$InputCollect)[i], json$InputCollect[[i]])
    }
  }

  ### Use case 1: running robyn_inputs() for the first time
  if (is.null(InputCollect)) {
    dt_input <- as_tibble(dt_input)
    # if (!is.null(dt_holidays)) dt_holidays <- as_tibble(dt_holidays) %>%
    # mutate(ds = as.Date(.data$ds, origin = "1970-01-01"))
    if (!is.null(dt_holidays)) dt_holidays <- as_tibble(dt_holidays)

    ## Check for NA values
    # check_nas(dt_input)
    # check_nas(dt_holidays)

    ## Check vars names (duplicates and valid)
    check_varnames(
      dt_input, dt_holidays,
      dep_var, date_var,
      context_vars, paid_media_spends,
      organic_vars
    )

    ## Check date input (and set dayInterval and intervalType)
    date_input <- check_datevar(dt_input, date_var)
    dt_input <- date_input$dt_input # sorted date by ascending
    date_var <- date_input$date_var # when date_var = "auto"
    dayInterval <- date_input$dayInterval
    intervalType <- date_input$intervalType

    ## Check dependent var
    check_depvar(dt_input, dep_var, dep_var_type)

    ## Check prophet
    if (is.null(dt_holidays) || is.null(prophet_vars)) {
      dt_holidays <- prophet_vars <- prophet_country <- prophet_signs <- NULL
    }
    prophet_signs <- check_prophet(dt_holidays, prophet_country, prophet_vars, prophet_signs, dayInterval)

    ## Check baseline variables (and maybe transform context_signs)
    context <- check_context(dt_input, context_vars, context_signs)
    context_signs <- context$context_signs

    ## Check paid media variables (set mediaVarCount and maybe transform paid_media_signs)
    if (is.null(paid_media_vars)) paid_media_vars <- paid_media_spends
    paidmedia <- check_paidmedia(dt_input, paid_media_vars, paid_media_signs, paid_media_spends)
    paid_media_signs <- paidmedia$paid_media_signs
    mediaVarCount <- paidmedia$mediaVarCount
    exposure_vars <- paid_media_vars[!(paid_media_vars == paid_media_spends)]

    ## Check organic media variables (and maybe transform organic_signs)
    organic <- check_organicvars(dt_input, organic_vars, organic_signs)
    organic_signs <- organic$organic_signs

    ## Check factor_vars
    factor_vars <- check_factorvars(dt_input, factor_vars, context_vars, organic_vars)

    ## Check all vars
    all_media <- c(paid_media_spends, organic_vars)
    all_ind_vars <- c(prophet_vars, context_vars, all_media)
    check_allvars(all_ind_vars)

    ## Check data dimension
    check_datadim(dt_input, all_ind_vars, rel = 10)

    ## Check window_start & window_end (and transform parameters/data)
    windows <- check_windows(dt_input, date_var, all_media, window_start, window_end)

    if (TRUE) {
      dt_input <- windows$dt_input
      window_start <- windows$window_start
      rollingWindowStartWhich <- windows$rollingWindowStartWhich
      refreshAddedStart <- windows$refreshAddedStart
      window_end <- windows$window_end
      rollingWindowEndWhich <- windows$rollingWindowEndWhich
      rollingWindowLength <- windows$rollingWindowLength
    }

    ## Check adstock
    adstock <- check_adstock(adstock)

    ## Check hyperparameters (if passed)
    hyperparameters <- check_hyperparameters(
      hyperparameters, adstock, paid_media_spends, organic_vars, exposure_vars
    )

    ## Check calibration and iters/trials
    calibration_input <- check_calibration(
      dt_input, date_var, calibration_input, dayInterval, dep_var,
      window_start, window_end, paid_media_spends, organic_vars
    )

    ## Not used variables
    unused_vars <- colnames(dt_input)[!colnames(dt_input) %in% c(
      dep_var, date_var, context_vars, paid_media_vars, paid_media_spends, organic_vars
    )]

    # Check for no-variance columns (after removing not-used)
    check_novar(select(dt_input, -all_of(unused_vars)))

    ## Collect input
    InputCollect <- list(
      dt_input = dt_input,
      dt_holidays = dt_holidays,
      dt_mod = NULL,
      dt_modRollWind = NULL,
      xDecompAggPrev = NULL,
      date_var = date_var,
      dayInterval = dayInterval,
      intervalType = intervalType,
      dep_var = dep_var,
      dep_var_type = dep_var_type,
      prophet_vars = prophet_vars,
      prophet_signs = prophet_signs,
      prophet_country = prophet_country,
      context_vars = context_vars,
      context_signs = context_signs,
      paid_media_vars = paid_media_vars,
      paid_media_signs = paid_media_signs,
      paid_media_spends = paid_media_spends,
      mediaVarCount = mediaVarCount,
      exposure_vars = exposure_vars,
      organic_vars = organic_vars,
      organic_signs = organic_signs,
      all_media = all_media,
      all_ind_vars = all_ind_vars,
      factor_vars = factor_vars,
      unused_vars = unused_vars,
      window_start = window_start,
      rollingWindowStartWhich = rollingWindowStartWhich,
      window_end = window_end,
      rollingWindowEndWhich = rollingWindowEndWhich,
      rollingWindowLength = rollingWindowLength,
      refreshAddedStart = refreshAddedStart,
      adstock = adstock,
      hyperparameters = hyperparameters,
      calibration_input = calibration_input,
      custom_params = list(...)
    )

    if (!is.null(hyperparameters)) {
      ### Conditional output 1.2
      ## Running robyn_inputs() for the 1st time & 'hyperparameters' provided --> run robyn_engineering()
      InputCollect <- robyn_engineering(InputCollect, ...)
    }
  } else {
    ### Use case 2: adding 'hyperparameters' and/or 'calibration_input' using robyn_inputs()
    # Check for legacy (deprecated) inputs
    check_legacy_input(InputCollect)

    ## Check calibration data
    calibration_input <- check_calibration(
      dt_input = InputCollect$dt_input,
      date_var = InputCollect$date_var,
      calibration_input = calibration_input,
      dayInterval = InputCollect$dayInterval,
      dep_var = InputCollect$dep_var,
      window_start = InputCollect$window_start,
      window_end = InputCollect$window_end,
      paid_media_spends = InputCollect$paid_media_spends,
      organic_vars = InputCollect$organic_vars
    )

    ## Update calibration_input
    if (!is.null(calibration_input)) InputCollect$calibration_input <- calibration_input
    if (!is.null(hyperparameters)) InputCollect$hyperparameters <- hyperparameters
    if (is.null(InputCollect$hyperparameters) && is.null(hyperparameters)) {
      stop("Must provide hyperparameters in robyn_inputs()")
    } else {
      ### Conditional output 2.1
      ## 'hyperparameters' provided --> run robyn_engineering()
      ## Update & check hyperparameters
      if (is.null(InputCollect$hyperparameters)) InputCollect$hyperparameters <- hyperparameters
      check_hyperparameters(InputCollect$hyperparameters, InputCollect$adstock, InputCollect$all_media)
      InputCollect <- robyn_engineering(InputCollect, ...)
    }
  }

  if (!is.null(json_file)) {
    pending <- which(!names(json$InputCollect) %in% names(InputCollect))
    InputCollect <- append(InputCollect, json$InputCollect[pending])
  }

  # Save R and Robyn's versions
  if (TRUE) {
    ver <- as.character(utils::packageVersion("Robyn"))
    rver <- utils::sessionInfo()$R.version
    origin <- ifelse(is.null(utils::packageDescription("Robyn")$Repository), "dev", "stable")
    InputCollect$version <- sprintf(
      "Robyn (%s) v%s [R-%s.%s]",
      origin, ver, rver$major, rver$minor
    )
  }

  class(InputCollect) <- c("robyn_inputs", class(InputCollect))
  return(InputCollect)
}
LEGACY_PARAMS <- c("cores", "iterations", "trials", "intercept_sign", "nevergrad_algo")
check_legacy_input <- function(InputCollect,
                               cores = NULL, iterations = NULL, trials = NULL,
                               intercept_sign = NULL, nevergrad_algo = NULL) {
  if (!any(LEGACY_PARAMS %in% names(InputCollect))) {
    return(invisible(InputCollect))
  } # Legacy check
  # Warn the user these InputCollect params will be (are) deprecated
  legacyValues <- InputCollect[LEGACY_PARAMS]
  legacyValues <- legacyValues[!unlist(lapply(legacyValues, is.null))]
  if (length(legacyValues) > 0) {
    warning(sprintf(
      "Using legacy InputCollect values. Please set %s within robyn_run() instead",
      v2t(names(legacyValues))
    ))
  }
  # Overwrite InputCollect with robyn_run() inputs
  if (!is.null(cores)) InputCollect$cores <- cores
  if (!is.null(iterations)) InputCollect$iterations <- iterations
  if (!is.null(trials)) InputCollect$trials <- trials
  if (!is.null(intercept_sign)) InputCollect$intercept_sign <- intercept_sign
  if (!is.null(nevergrad_algo)) InputCollect$nevergrad_algo <- nevergrad_algo
  attr(InputCollect, "deprecated_params") <- TRUE
  return(invisible(InputCollect))
}
check_calibration <- function(dt_input, date_var, calibration_input, dayInterval, dep_var,
                              window_start, window_end, paid_media_spends, organic_vars) {
  if (!is.null(calibration_input)) {
    calibration_input <- as_tibble(as.data.frame(calibration_input))
    these <- c("channel", "liftStartDate", "liftEndDate", "liftAbs")
    if (!all(these %in% names(calibration_input))) {
      stop("Input 'calibration_input' must contain columns: ", v2t(these))
    }
    if (!is.numeric(calibration_input$liftAbs) || any(is.na(calibration_input$liftAbs))) {
      stop("Check 'calibration_input$liftAbs': all lift values must be valid numerical numbers")
    }
    all_media <- c(paid_media_spends, organic_vars)
    cal_media <- unique(stringr::str_split(calibration_input$channel, "\\+|,|;|\\s"))
    if (!all(unlist(cal_media) %in% all_media)) {
      these <- unique(unlist(cal_media)[which(!unlist(cal_media) %in% all_media)])
      stop(sprintf(
        "All channels from 'calibration_input' must be any of: %s.\n  Check: %s",
        v2t(all_media), v2t(these)
      ))
    }
    for (i in seq_along(calibration_input$channel)) {
      temp <- calibration_input[i, ]
      if (temp$liftStartDate < (window_start) || temp$liftEndDate > (window_end)) {
        stop(sprintf(
          paste(
            "Your calibration's date range for %s between %s and %s is not within modeling window (%s to %s).",
            "Please, remove this experiment from 'calibration_input'."
          ),
          temp$channel, temp$liftStartDate, temp$liftEndDate, window_start, window_end
        ))
      }
      if (temp$liftStartDate > temp$liftEndDate) {
        stop(sprintf(
          paste(
            "Your calibration's date range for %s between %s and %s should respect liftStartDate <= liftEndDate.",
            "Please, correct this experiment from 'calibration_input'."
          ),
          temp$channel, temp$liftStartDate, temp$liftEndDate
        ))
      }
    }
    if ("spend" %in% colnames(calibration_input)) {
      for (i in seq_along(calibration_input$channel)) {
        temp <- calibration_input[i, ]
        temp2 <- cal_media[[i]]
        if (all(temp2 %in% organic_vars)) next
        dt_input_spend <- filter(
          dt_input, get(date_var) >= temp$liftStartDate,
          get(date_var) <= temp$liftEndDate
        ) %>%
          select(all_of(temp2)) %>%
          sum(.) %>%
          round(., 0)
        if (dt_input_spend > temp$spend * 1.1 || dt_input_spend < temp$spend * 0.9) {
          warning(sprintf(
            paste(
              "Your calibration's spend (%s) for %s between %s and %s does not match your dt_input spend (~%s).",
              "Please, check again your dates or split your media inputs into separate media channels."
            ),
            formatNum(temp$spend, 0), temp$channel, temp$liftStartDate, temp$liftEndDate,
            formatNum(dt_input_spend, 3, abbr = TRUE)
          ))
        }
      }
    }
    if ("confidence" %in% colnames(calibration_input)) {
      for (i in seq_along(calibration_input$channel)) {
        temp <- calibration_input[i, ]
        if (temp$confidence < 0.8) {
          warning(sprintf(
            paste(
              "Your calibration's confidence for %s between %s and %s is lower than 80%%, thus low-confidence.",
              "Consider getting rid of this experiment and running it again."
            ),
            temp$channel, temp$liftStartDate, temp$liftEndDate
          ))
        }
      }
    }
    if ("metric" %in% colnames(calibration_input)) {
      for (i in seq_along(calibration_input$channel)) {
        temp <- calibration_input[i, ]
        if (temp$metric != dep_var) {
          stop(sprintf(
            paste(
              "Your calibration's metric for %s between %s and %s is not '%s'.",
              "Please, remove this experiment from 'calibration_input'."
            ),
            temp$channel, temp$liftStartDate, temp$liftEndDate, dep_var
          ))
        }
      }
    }
    if ("scope" %in% colnames(calibration_input)) {
      these <- c("immediate", "total")
      if (!all(calibration_input$scope %in% these)) {
        stop("Inputs in 'calibration_input$scope' must be any of: ", v2t(these))
      }
    }
  }
  return(calibration_input)
}
check_hyperparameters <- function(hyperparameters = NULL, adstock = NULL,
                                  paid_media_spends = NULL, organic_vars = NULL,
                                  exposure_vars = NULL, quiet = FALSE) {
  if (is.null(hyperparameters) && !quiet) {
    message(paste(
      "Input 'hyperparameters' not provided yet. To include them, run",
      "robyn_inputs(InputCollect = InputCollect, hyperparameters = ...)"
    ))
  } else {
    hyperparameters <- hyperparameters[which(!names(hyperparameters) %in% "lambda")]
    hyperparameters_ordered <- hyperparameters[order(names(hyperparameters))]
    get_hyp_names <- names(hyperparameters_ordered)
    ref_hyp_name_spend <- hyper_names(adstock, all_media = paid_media_spends)
    ref_hyp_name_expo <- hyper_names(adstock, all_media = exposure_vars)
    ref_hyp_name_org <- hyper_names(adstock, all_media = organic_vars)
    ref_all_media <- sort(c(ref_hyp_name_spend, ref_hyp_name_org))
    all_ref_names <- c(ref_hyp_name_spend, ref_hyp_name_expo, ref_hyp_name_org)
    if (!all(get_hyp_names %in% all_ref_names)) {
      wrong_hyp_names <- get_hyp_names[which(!(get_hyp_names %in% all_ref_names))]
      stop(
        "Input 'hyperparameters' contains following wrong names: ",
        paste(wrong_hyp_names, collapse = ", ")
      )
    }
    total <- length(get_hyp_names)
    total_in <- length(c(ref_hyp_name_spend, ref_hyp_name_org))
    if (total != total_in) {
      stop(sprintf(
        paste(
          "%s hyperparameter values are required, and %s were provided.",
          "\n Use hyper_names() function to help you with the correct hyperparameters names."
        ),
        total_in, total
      ))
    }
    # Old workflow: replace exposure with spend hyperparameters
    if (any(get_hyp_names %in% ref_hyp_name_expo)) {
      get_expo_pos <- which(get_hyp_names %in% ref_hyp_name_expo)
      get_hyp_names[get_expo_pos] <- ref_all_media[get_expo_pos]
      names(hyperparameters_ordered) <- get_hyp_names
    }
    if (!identical(get_hyp_names, ref_all_media)) {
      stop("Input 'hyperparameters' must contain: ", paste(ref_all_media, collapse = ", "))
    }
    check_hyper_limits(hyperparameters_ordered, "thetas")
    check_hyper_limits(hyperparameters_ordered, "alphas")
    check_hyper_limits(hyperparameters_ordered, "gammas")
    check_hyper_limits(hyperparameters_ordered, "shapes")
    check_hyper_limits(hyperparameters_ordered, "scales")
    return(hyperparameters_ordered)
  }
}
check_hyper_limits <- function(hyperparameters, hyper) {
  hyper_which <- which(endsWith(names(hyperparameters), hyper))
  if (length(hyper_which) == 0) {
    return(invisible(NULL))
  }
  limits <- hyper_limits()[[hyper]]
  for (i in hyper_which) {
    values <- hyperparameters[[i]]
    # Lower limit
    ineq <- paste(values[1], limits[1], sep = "", collapse = "")
    lower_pass <- eval(parse(text = ineq))
    if (!lower_pass) {
      stop(sprintf("%s's hyperparameter must have lower bound %s", names(hyperparameters)[i], limits[1]))
    }
    # Upper limit
    ineq <- paste(values[2], limits[2], sep = "", collapse = "")
    upper_pass <- eval(parse(text = ineq)) | length(values) == 1
    if (!upper_pass) {
      stop(sprintf("%s's hyperparameter must have upper bound %s", names(hyperparameters)[i], limits[2]))
    }
    # Order of limits
    order_pass <- !isFALSE(values[1] <= values[2])
    if (!order_pass) {
      stop(sprintf("%s's hyperparameter must have lower bound first and upper bound second", names(hyperparameters)[i]))
    }
  }
}
tmp = robyn_inputs(InputCollect = InputCollect, hyperparameters = hyperparameters)
head(tmp$dt_mod,2)

tmp = robyn_engineering(InputCollect)
print (tmp)
tmp$dt_mod

#initial function
dt_input <- InputCollect$dt_input
dt_transform <- dt_input
colnames(dt_transform)[colnames(dt_transform) == InputCollect$date_var] <- "ds"
colnames(dt_transform)[colnames(dt_transform) == InputCollect$dep_var] <- "dep_var"
dt_transform <- arrange(dt_transform, .data$ds)
# dt_transformRollWind
dt_transformRollWind <- dt_transform[rollingWindowStartWhich:rollingWindowEndWhich, ]
paid_media_spends = InputCollect$paid_media_spends
factor_vars = InputCollect$factor_vars

prophet_decomp(
  dt_transform,
  dt_holidays = InputCollect$dt_holidays,
  prophet_country = InputCollect$prophet_country,
  prophet_vars = InputCollect$prophet_vars,
  prophet_signs = InputCollect$prophet_signs,
  factor_vars = factor_vars,
  context_vars = InputCollect$context_vars,
  paid_media_spends = paid_media_spends,
  intervalType = InputCollect$intervalType,
  dayInterval = InputCollect$dayInterval,
  custom_params = NULL
)


head(InputCollect$dt_mod,3)

################################################################
################################################################


################################################################
#### Step 3: Build initial model

## Run all trials and iterations. Use ?robyn_run to check parameter definition
OutputModels <- robyn_run(
  InputCollect = InputCollect, # feed in all model specification
  # cores = NULL, # default to max available
  # add_penalty_factor = FALSE, # Untested feature. Use with caution.
  iterations = 20, # recommended for the dummy dataset
  trials = 5, # recommended for the dummy dataset
  outputs = FALSE # outputs = FALSE disables direct model output - robyn_outputs()
)
print(OutputModels)

## Check MOO (multi-objective optimization) convergence plots
OutputModels$convergence$moo_distrb_plot
OutputModels$convergence$moo_cloud_plot
# check convergence rules ?robyn_converge


#####################################################################3
#####################################################################3
#output.R
robyn_outputs <- function(InputCollect, OutputModels,
                          pareto_fronts = "auto",
                          calibration_constraint = 0.1,
                          plot_folder = NULL,
                          plot_pareto = TRUE,
                          csv_out = "pareto",
                          clusters = TRUE,
                          select_model = "clusters",
                          ui = FALSE, export = TRUE,
                          quiet = FALSE, ...) {
  if (is.null(plot_folder)) plot_folder <- getwd()
  plot_folder <- check_dir(plot_folder)

  # Check calibration constrains
  calibrated <- !is.null(InputCollect$calibration_input)
  calibration_constraint <- check_calibconstr(
    calibration_constraint,
    OutputModels$iterations,
    OutputModels$trials,
    InputCollect$calibration_input
  )

  #####################################
  #### Run robyn_pareto on OutputModels

  totalModels <- OutputModels$iterations * OutputModels$trials
  if (!isTRUE(attr(OutputModels, "hyper_fixed"))) {
    message(sprintf(
      ">>> Running Pareto calculations for %s models on %s front%s...",
      totalModels, pareto_fronts, ifelse(pareto_fronts > 1, "s", "")
    ))
  }
  pareto_results <- robyn_pareto(
    InputCollect, OutputModels,
    pareto_fronts = "auto",
    calibration_constraint = calibration_constraint,
    quiet = quiet,
    calibrated = calibrated,
    ...
  )
  pareto_fronts <- pareto_results$pareto_fronts
  allSolutions <- pareto_results$pareto_solutions

  #####################################
  #### Gather the results into output object

  # Auxiliary list with all results (wasn't previously exported but needed for robyn_outputs())
  allPareto <- list(
    resultHypParam = pareto_results$resultHypParam,
    xDecompAgg = pareto_results$xDecompAgg,
    resultCalibration = pareto_results$resultCalibration,
    plotDataCollect = pareto_results$plotDataCollect
  )

  # Set folder to save outputs: legacy plot_folder_sub
  if (TRUE) {
    depth <- ifelse(
      "refreshDepth" %in% names(InputCollect),
      InputCollect$refreshDepth,
      ifelse("refreshCounter" %in% names(InputCollect),
             InputCollect$refreshCounter, 0
      )
    )
    folder_var <- ifelse(!as.integer(depth) > 0, "init", paste0("rf", depth))
    plot_folder_sub <- paste("Robyn", format(Sys.time(), "%Y%m%d%H%M"), folder_var, sep = "_")
  }

  # Final results object
  OutputCollect <- list(
    resultHypParam = filter(pareto_results$resultHypParam, .data$solID %in% allSolutions),
    xDecompAgg = filter(pareto_results$xDecompAgg, .data$solID %in% allSolutions),
    mediaVecCollect = pareto_results$mediaVecCollect,
    xDecompVecCollect = pareto_results$xDecompVecCollect,
    resultCalibration = if (calibrated) {
      filter(pareto_results$resultCalibration, .data$solID %in% allSolutions)
    } else {
      NULL
    },
    allSolutions = allSolutions,
    allPareto = allPareto,
    calibration_constraint = calibration_constraint,
    OutputModels = OutputModels,
    cores = OutputModels$cores,
    iterations = OutputModels$iterations,
    trials = OutputModels$trials,
    intercept_sign = OutputModels$intercept_sign,
    nevergrad_algo = OutputModels$nevergrad_algo,
    add_penalty_factor = OutputModels$add_penalty_factor,
    seed = OutputModels$seed,
    UI = NULL,
    pareto_fronts = pareto_fronts,
    hyper_fixed = attr(OutputModels, "hyper_fixed"),
    plot_folder = gsub("//", "", paste0(plot_folder, "/", plot_folder_sub, "/"))
  )
  class(OutputCollect) <- c("robyn_outputs", class(OutputCollect))

  plotPath <- paste0(plot_folder, "/", plot_folder_sub, "/")
  OutputCollect$plot_folder <- gsub("//", "/", plotPath)
  if (export && !dir.exists(OutputCollect$plot_folder)) dir.create(OutputCollect$plot_folder, recursive = TRUE)

  # Cluster results and amend cluster output
  if (clusters) {
    if (!quiet) message(">>> Calculating clusters for model selection using Pareto fronts...")
    try(clusterCollect <- robyn_clusters(OutputCollect,
                                         dep_var_type = InputCollect$dep_var_type,
                                         quiet = quiet, export = export, ...
    ))
    OutputCollect$resultHypParam <- left_join(
      OutputCollect$resultHypParam,
      select(clusterCollect$data, .data$solID, .data$cluster, .data$top_sol),
      by = "solID"
    )
    OutputCollect$xDecompAgg <- left_join(
      OutputCollect$xDecompAgg,
      select(clusterCollect$data, .data$solID, .data$cluster, .data$top_sol),
      by = "solID"
    ) %>% left_join(
      select(clusterCollect$df_cluster_ci, .data$rn, .data$cluster, .data$boot_mean, .data$boot_se, .data$ci_low, .data$ci_up, .data$rn),
      by = c("rn", "cluster")
    )
    OutputCollect$mediaVecCollect <- left_join(
      OutputCollect$mediaVecCollect,
      select(clusterCollect$data, .data$solID, .data$cluster, .data$top_sol),
      by = "solID"
    )
    OutputCollect$xDecompVecCollect <- left_join(
      OutputCollect$xDecompVecCollect,
      select(clusterCollect$data, .data$solID, .data$cluster, .data$top_sol),
      by = "solID"
    )
    if (calibrated) {
      OutputCollect$resultCalibration <- left_join(
        OutputCollect$resultCalibration,
        select(clusterCollect$data, .data$solID, .data$cluster, .data$top_sol),
        by = "solID"
      )
    }
    OutputCollect[["clusters"]] <- clusterCollect
  }

  if (export) {
    tryCatch(
      {
        if (!quiet) message(paste0(">>> Collecting ", length(allSolutions), " pareto-optimum results into: ", OutputCollect$plot_folder))

        if (!quiet) message(">> Exporting general plots into directory...")
        all_plots <- robyn_plots(InputCollect, OutputCollect, export = export)

        if (csv_out %in% c("all", "pareto")) {
          if (!quiet) message(paste(">> Exporting", csv_out, "results as CSVs into directory..."))
          robyn_csv(InputCollect, OutputCollect, csv_out, export = export, calibrated = calibrated)
        }

        if (plot_pareto) {
          if (!quiet) {
            message(sprintf(
              ">>> Exporting %sone-pagers into directory...", ifelse(!OutputCollect$hyper_fixed, "pareto ", "")
            ))
          }
          select_model <- if (!clusters || is.null(OutputCollect[["clusters"]])) NULL else select_model
          pareto_onepagers <- robyn_onepagers(
            InputCollect, OutputCollect,
            select_model = select_model,
            quiet = quiet,
            export = export
          )
        }

        robyn_write(InputCollect, dir = OutputCollect$plot_folder, quiet = quiet)

        # For internal use -> UI Code
        if (ui && plot_pareto) OutputCollect$UI$pareto_onepagers <- pareto_onepagers
        OutputCollect[["UI"]] <- if (ui) list(pParFront = all_plots[["pParFront"]]) else NULL
      },
      error = function(err) {
        message(paste("Failed exporting results, but returned model results anyways:\n", err))
      }
    )
  }

  if (!is.null(OutputModels$hyper_updated)) OutputCollect$hyper_updated <- OutputModels$hyper_updated
  class(OutputCollect) <- c("robyn_outputs", class(OutputCollect))
  return(invisible(OutputCollect))
}
check_dir <- function(plot_folder) {
  file_end <- substr(plot_folder, nchar(plot_folder) - 3, nchar(plot_folder))
  if (file_end == ".RDS") {
    plot_folder <- dirname(plot_folder)
    message("Using robyn object location: ", plot_folder)
  } else {
    plot_folder <- file.path(dirname(plot_folder), basename(plot_folder))
  }
  if (!dir.exists(plot_folder)) {
    plot_folder <- getwd()
    message("WARNING: Provided 'plot_folder' doesn't exist. Using current working directory: ", plot_folder)
  }
  return(plot_folder)
}
check_calibconstr <- function(calibration_constraint, iterations, trials, calibration_input) {
  if (!is.null(calibration_input)) {
    total_iters <- iterations * trials
    if (calibration_constraint < 0.01 || calibration_constraint > 0.1) {
      message("Input 'calibration_constraint' must be >= 0.01 and <= 0.1. Changed to default: 0.1")
      calibration_constraint <- 0.1
    }
    models_lower <- 500
    if (total_iters * calibration_constraint < models_lower) {
      warning(sprintf(
        paste(
          "Input 'calibration_constraint' set for top %s%% calibrated models.",
          "%s models left for pareto-optimal selection. Minimum suggested: %s"
        ),
        calibration_constraint * 100,
        round(total_iters * calibration_constraint, 0),
        models_lower
      ))
    }
  }
  return(calibration_constraint)
}
hyper_fixed <- attr(OutputModels, "hyper_fixed")
OutModels <- OutputModels[unlist(lapply(OutputModels, function(x) "resultCollect" %in% names(x)))]

resultHypParam <- bind_rows(lapply(OutModels, function(x) {
  mutate(x$resultCollect$resultHypParam, trial = x$trial)
}))

xDecompAgg <- bind_rows(lapply(OutModels, function(x) {
  mutate(x$resultCollect$xDecompAgg, trial = x$trial)
}))
xDecompVec <- OutputModels$vec_collect$xDecompVec
xDecompVecImmediate <- OutputModels$vec_collect$xDecompVecImmediate
xDecompVecCarryover <- OutputModels$vec_collect$xDecompVecCarryover
xDecompVecImmeCaov <- bind_rows(
  select(xDecompVec, c("ds", InputCollect$all_media, "solID")) %>%
    mutate(type = "total"),
  select(xDecompVecImmediate, c("ds", InputCollect$all_media, "solID")) %>%
    mutate(type = "Immediate"),
  select(xDecompVecCarryover, c("ds", InputCollect$all_media, "solID")) %>%
    mutate(type = "Carryover")
) %>% pivot_longer(cols = InputCollect$all_media, names_to = "channels")
if (length(unique(xDecompVecImmeCaov$solID)) == 1) {
  xDecompVecImmeCaov$solID <- OutModels$trial1$resultCollect$resultHypParam$solID
}
robyn_pareto <- function(InputCollect, OutputModels,
                         pareto_fronts = "auto",
                         min_candidates = 100,
                         calibration_constraint = 0.1,
                         quiet = FALSE,
                         calibrated = FALSE,
                         ...) {
  hyper_fixed <- attr(OutputModels, "hyper_fixed")
  OutModels <- OutputModels[unlist(lapply(OutputModels, function(x) "resultCollect" %in% names(x)))]

  resultHypParam <- bind_rows(lapply(OutModels, function(x) {
    mutate(x$resultCollect$resultHypParam, trial = x$trial)
  }))

  xDecompAgg <- bind_rows(lapply(OutModels, function(x) {
    mutate(x$resultCollect$xDecompAgg, trial = x$trial)
  }))

  # Build immediate vs carryover dataframe
  xDecompVec <- OutputModels$vec_collect$xDecompVec
  xDecompVecImmediate <- OutputModels$vec_collect$xDecompVecImmediate
  xDecompVecCarryover <- OutputModels$vec_collect$xDecompVecCarryover
  xDecompVecImmeCaov <- bind_rows(
    select(xDecompVec, c("ds", InputCollect$all_media, "solID")) %>%
      mutate(type = "total"),
    select(xDecompVecImmediate, c("ds", InputCollect$all_media, "solID")) %>%
      mutate(type = "Immediate"),
    select(xDecompVecCarryover, c("ds", InputCollect$all_media, "solID")) %>%
      mutate(type = "Carryover")
  ) %>% pivot_longer(cols = InputCollect$all_media, names_to = "channels")
  if (length(unique(xDecompVecImmeCaov$solID)) == 1) {
    xDecompVecImmeCaov$solID <- OutModels$trial1$resultCollect$resultHypParam$solID
  }

  if (calibrated) {
    resultCalibration <- bind_rows(lapply(OutModels, function(x) {
      x$resultCollect$liftCalibration %>%
        mutate(trial = x$trial) %>%
        rename(rn = .data$liftMedia)
    }))
  } else {
    resultCalibration <- NULL
  }

  if (!hyper_fixed) {
    df_names <- if (calibrated) {
      c("resultHypParam", "xDecompAgg", "resultCalibration")
    } else {
      c("resultHypParam", "xDecompAgg")
    }
    for (df in df_names) {
      assign(df, get(df) %>% mutate(
        iterations = (.data$iterNG - 1) * OutputModels$cores + .data$iterPar,
        solID = paste(.data$trial, .data$iterNG, .data$iterPar, sep = "_")
      ))
    }
  }

  # If recreated model, inherit bootstrap results
  if (length(unique(xDecompAgg$solID)) == 1 & !"boot_mean" %in% colnames(xDecompAgg)) {
    bootstrap <- attr(OutputModels, "bootstrap")
    if (!is.null(bootstrap)) {
      xDecompAgg <- left_join(xDecompAgg, bootstrap, by = c("rn" = "variable"))
    }
  }

  xDecompAggCoef0 <- xDecompAgg %>%
    filter(.data$rn %in% InputCollect$paid_media_spends) %>%
    group_by(.data$solID) %>%
    summarise(coef0 = min(.data$coef, na.rm = TRUE) == 0)

  if (!hyper_fixed) {
    mape_lift_quantile10 <- quantile(resultHypParam$mape, probs = calibration_constraint, na.rm = TRUE)
    nrmse_quantile90 <- quantile(resultHypParam$nrmse, probs = 0.90, na.rm = TRUE)
    decomprssd_quantile90 <- quantile(resultHypParam$decomp.rssd, probs = 0.90, na.rm = TRUE)
    resultHypParam <- left_join(resultHypParam, xDecompAggCoef0, by = "solID") %>%
      mutate(
        mape.qt10 =
          .data$mape <= mape_lift_quantile10 &
          .data$nrmse <= nrmse_quantile90 &
          .data$decomp.rssd <= decomprssd_quantile90
      )

    resultHypParamPareto <- filter(resultHypParam, .data$mape.qt10 == TRUE)
    px <- rPref::low(resultHypParamPareto$nrmse) * rPref::low(resultHypParamPareto$decomp.rssd)
    resultHypParamPareto <- rPref::psel(resultHypParamPareto, px, top = nrow(resultHypParamPareto)) %>%
      arrange(.data$iterNG, .data$iterPar, .data$nrmse) %>%
      rename("robynPareto" = ".level") %>%
      select(.data$solID, .data$robynPareto)
    resultHypParam <- left_join(resultHypParam, resultHypParamPareto, by = "solID")
  } else {
    resultHypParam <- mutate(resultHypParam, mape.qt10 = TRUE, robynPareto = 1, coef0 = NA)
  }

  # Calculate combined weighted error scores
  resultHypParam$error_score <- errors_scores(resultHypParam)

  # Bind robynPareto results
  xDecompAgg <- left_join(xDecompAgg, select(resultHypParam, .data$robynPareto, .data$solID), by = "solID")
  decompSpendDist <- bind_rows(lapply(OutModels, function(x) {
    mutate(x$resultCollect$decompSpendDist, trial = x$trial)
  })) %>%
    {
      if (!hyper_fixed) mutate(., solID = paste(.data$trial, .data$iterNG, .data$iterPar, sep = "_")) else .
    } %>%
    left_join(select(resultHypParam, .data$robynPareto, .data$solID), by = "solID")

  # Prepare parallel loop
  if (TRUE) {
    if (check_parallel()) registerDoParallel(OutputModels$cores) else registerDoSEQ()
    if (hyper_fixed) pareto_fronts <- 1
    # Get at least 100 candidates for better clustering
    if (nrow(resultHypParam) == 1) pareto_fronts <- 1
    if ("auto" %in% pareto_fronts) {
      n_pareto <- resultHypParam %>%
        filter(!is.na(.data$robynPareto)) %>%
        nrow()
      if (n_pareto <= min_candidates & nrow(resultHypParam) > 1) {
        stop(paste(
          "Less than", min_candidates, "candidates in pareto fronts.",
          "Increaseiterations to get more model candidates"
        ))
      }
      auto_pareto <- resultHypParam %>%
        filter(!is.na(.data$robynPareto)) %>%
        group_by(.data$robynPareto) %>%
        summarise(n = n_distinct(.data$solID)) %>%
        mutate(n_cum = cumsum(.data$n)) %>%
        filter(.data$n_cum >= min_candidates) %>%
        slice(1)
      message(sprintf(
        ">> Automatically selected %s Pareto-fronts to contain at least %s pareto-optimal models (%s)",
        auto_pareto$robynPareto, min_candidates, auto_pareto$n_cum
      ))
      pareto_fronts <- as.integer(auto_pareto$robynPareto)
    }
    pareto_fronts_vec <- 1:pareto_fronts

    decompSpendDistPar <- decompSpendDist[decompSpendDist$robynPareto %in% pareto_fronts_vec, ]
    resultHypParamPar <- resultHypParam[resultHypParam$robynPareto %in% pareto_fronts_vec, ]
    xDecompAggPar <- xDecompAgg[xDecompAgg$robynPareto %in% pareto_fronts_vec, ]
    respN <- NULL
  }

  resp_collect <- foreach(
    respN = seq_along(decompSpendDistPar$rn), .combine = rbind
  ) %dorng% {
    get_resp <- robyn_response(
      media_metric = decompSpendDistPar$rn[respN],
      select_model = decompSpendDistPar$solID[respN],
      metric_value = decompSpendDistPar$mean_spend[respN],
      dt_hyppar = resultHypParamPar,
      dt_coef = xDecompAggPar,
      InputCollect = InputCollect,
      OutputCollect = OutputModels,
      quiet = quiet
    )$response
    dt_resp <- data.frame(
      mean_response = get_resp,
      rn = decompSpendDistPar$rn[respN],
      solID = decompSpendDistPar$solID[respN]
    )
    return(dt_resp)
  }
  stopImplicitCluster()
  registerDoSEQ()
  getDoParWorkers()

  decompSpendDist <- left_join(
    decompSpendDist,
    resp_collect,
    by = c("solID", "rn")
  ) %>%
    mutate(
      roi_mean = .data$mean_response / .data$mean_spend,
      roi_total = .data$xDecompAgg / .data$total_spend,
      cpa_mean = .data$mean_spend / .data$mean_response,
      cpa_total = .data$total_spend / .data$xDecompAgg
    )

  xDecompAgg <- left_join(
    xDecompAgg,
    select(
      decompSpendDist, .data$rn, .data$solID, .data$total_spend, .data$mean_spend,
      .data$spend_share, .data$effect_share, .data$roi_mean, .data$roi_total, .data$cpa_total
    ),
    by = c("solID", "rn")
  )

  # Pareto loop (no plots)
  mediaVecCollect <- list()
  xDecompVecCollect <- list()
  meanResponseCollect <- list()
  plotDataCollect <- list()

  for (pf in pareto_fronts_vec) {
    plotMediaShare <- filter(
      xDecompAgg,
      .data$robynPareto == pf,
      .data$rn %in% InputCollect$paid_media_spends
    )
    uniqueSol <- unique(plotMediaShare$solID)
    plotWaterfall <- xDecompAgg[xDecompAgg$robynPareto == pf, ]
    dt_mod <- InputCollect$dt_mod
    dt_modRollWind <- InputCollect$dt_modRollWind

    for (sid in uniqueSol) {
      # parallelResult <- foreach(sid = uniqueSol) %dorng% {

      # Calculations for pareto AND pareto plots

      ## 1. Spend x effect share comparison
      temp <- plotMediaShare[plotMediaShare$solID == sid, ] %>%
        tidyr::gather(
          "variable", "value",
          c("spend_share", "effect_share", "roi_total", "cpa_total")
        ) %>%
        select(c("rn", "nrmse", "decomp.rssd", "rsq_train", "variable", "value")) %>%
        mutate(rn = factor(.data$rn, levels = sort(InputCollect$paid_media_spends)))
      plotMediaShareLoopBar <- filter(temp, .data$variable %in% c("spend_share", "effect_share"))
      plotMediaShareLoopLine <- filter(temp, .data$variable == ifelse(
        InputCollect$dep_var_type == "conversion", "cpa_total", "roi_total"
      ))
      line_rm_inf <- !is.infinite(plotMediaShareLoopLine$value)
      ySecScale <- max(plotMediaShareLoopLine$value[line_rm_inf]) /
        max(plotMediaShareLoopBar$value) * 1.1
      plot1data <- list(
        plotMediaShareLoopBar = plotMediaShareLoopBar,
        plotMediaShareLoopLine = plotMediaShareLoopLine,
        ySecScale = ySecScale
      )

      ## 2. Waterfall
      plotWaterfallLoop <- plotWaterfall %>%
        filter(.data$solID == sid) %>%
        arrange(.data$xDecompPerc) %>%
        mutate(
          end = 1 - cumsum(.data$xDecompPerc),
          start = lag(.data$end),
          start = ifelse(is.na(.data$start), 1, .data$start),
          id = row_number(),
          rn = as.factor(.data$rn),
          sign = as.factor(ifelse(.data$xDecompPerc >= 0, "Positive", "Negative"))
        ) %>%
        select(
          .data$id, .data$rn, .data$coef,
          .data$xDecompAgg, .data$xDecompPerc,
          .data$start, .data$end, .data$sign
        )
      plot2data <- list(plotWaterfallLoop = plotWaterfallLoop)

      ## 3. Adstock rate
      dt_geometric <- weibullCollect <- wb_type <- NULL
      resultHypParamLoop <- resultHypParam[resultHypParam$solID == sid, ]
      get_hp_names <- !startsWith(names(InputCollect$hyperparameters), "penalty_")
      get_hp_names <- names(InputCollect$hyperparameters)[get_hp_names]
      hypParam <- resultHypParamLoop[, get_hp_names]
      if (InputCollect$adstock == "geometric") {
        hypParam_thetas <- unlist(hypParam[paste0(InputCollect$all_media, "_thetas")])
        dt_geometric <- data.frame(channels = InputCollect$all_media, thetas = hypParam_thetas)
      }
      if (InputCollect$adstock %in% c("weibull_cdf", "weibull_pdf")) {
        shapeVec <- unlist(hypParam[paste0(InputCollect$all_media, "_shapes")])
        scaleVec <- unlist(hypParam[paste0(InputCollect$all_media, "_scales")])
        wb_type <- substr(InputCollect$adstock, 9, 11)
        weibullCollect <- list()
        n <- 1
        for (v1 in seq_along(InputCollect$all_media)) {
          dt_weibull <- data.frame(
            x = 1:InputCollect$rollingWindowLength,
            decay_accumulated = adstock_weibull(
              1:InputCollect$rollingWindowLength,
              shape = shapeVec[v1],
              scale = scaleVec[v1],
              type = wb_type
            )$thetaVecCum,
            type = wb_type,
            channel = InputCollect$all_media[v1]
          ) %>%
            mutate(halflife = which.min(abs(.data$decay_accumulated - 0.5)))
          max_non0 <- max(which(dt_weibull$decay_accumulated > 0.001))
          dt_weibull$cut_time <- floor(max_non0 + max_non0 / 3)
          weibullCollect[[n]] <- dt_weibull
          n <- n + 1
        }
        weibullCollect <- bind_rows(weibullCollect)
        weibullCollect <- filter(weibullCollect, .data$x <= max(weibullCollect$cut_time))
      }

      plot3data <- list(
        dt_geometric = dt_geometric,
        weibullCollect = weibullCollect,
        wb_type = toupper(wb_type)
      )

      ## 4. Spend response curve
      dt_transformPlot <- select(dt_mod, .data$ds, all_of(InputCollect$all_media)) # independent variables
      dt_transformSpend <- cbind(dt_transformPlot[, "ds"], InputCollect$dt_input[, c(InputCollect$paid_media_spends)]) # spends of indep vars
      dt_transformSpendMod <- dt_transformPlot[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich, ]
      # update non-spend variables
      # if (length(InputCollect$exposure_vars) > 0) {
      #   for (expo in InputCollect$exposure_vars) {
      #     sel_nls <- ifelse(InputCollect$modNLSCollect[channel == expo, rsq_nls > rsq_lm], "nls", "lm")
      #     dt_transformSpendMod[, (expo) := InputCollect$yhatNLSCollect[channel == expo & models == sel_nls, yhat]]
      #   }
      # }
      dt_transformAdstock <- dt_transformPlot
      dt_transformSaturation <- dt_transformPlot[
        InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich,
      ]

      m_decayRate <- list()
      for (med in seq_along(InputCollect$all_media)) {
        med_select <- InputCollect$all_media[med]
        m <- dt_transformPlot[, med_select][[1]]
        # Adstocking
        if (InputCollect$adstock == "geometric") {
          theta <- hypParam[paste0(InputCollect$all_media[med], "_thetas")][[1]]
          x_list <- adstock_geometric(x = m, theta = theta)
        } else if (InputCollect$adstock == "weibull_cdf") {
          shape <- hypParam[paste0(InputCollect$all_media[med], "_shapes")][[1]]
          scale <- hypParam[paste0(InputCollect$all_media[med], "_scales")][[1]]
          x_list <- adstock_weibull(x = m, shape = shape, scale = scale, type = "cdf")
        } else if (InputCollect$adstock == "weibull_pdf") {
          shape <- hypParam[paste0(InputCollect$all_media[med], "_shapes")][[1]]
          scale <- hypParam[paste0(InputCollect$all_media[med], "_scales")][[1]]
          x_list <- adstock_weibull(x = m, shape = shape, scale = scale, type = "pdf")
        }
        m_adstocked <- x_list$x_decayed
        dt_transformAdstock[med_select] <- m_adstocked
        m_adstockedRollWind <- m_adstocked[
          InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich
        ]
        ## Saturation
        alpha <- hypParam[paste0(InputCollect$all_media[med], "_alphas")][[1]]
        gamma <- hypParam[paste0(InputCollect$all_media[med], "_gammas")][[1]]
        dt_transformSaturation[med_select] <- saturation_hill(
          x = m_adstockedRollWind, alpha = alpha, gamma = gamma
        )
      }
      dt_transformSaturationDecomp <- dt_transformSaturation
      for (i in 1:InputCollect$mediaVarCount) {
        coef <- plotWaterfallLoop$coef[plotWaterfallLoop$rn == InputCollect$all_media[i]]
        dt_transformSaturationDecomp[InputCollect$all_media[i]] <- coef *
          dt_transformSaturationDecomp[InputCollect$all_media[i]]
      }
      dt_transformSaturationSpendReverse <- dt_transformAdstock[
        InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich,
      ]

      ## Reverse MM fitting
      # dt_transformSaturationSpendReverse <- copy(dt_transformAdstock[, c("ds", InputCollect$all_media), with = FALSE])
      # for (i in 1:InputCollect$mediaVarCount) {
      #   chn <- InputCollect$paid_media_vars[i]
      #   if (chn %in% InputCollect$paid_media_vars[InputCollect$exposure_selector]) {
      #     # Get Michaelis Menten nls fitting param
      #     get_chn <- dt_transformSaturationSpendReverse[, chn, with = FALSE]
      #     Vmax <- InputCollect$modNLSCollect[channel == chn, Vmax]
      #     Km <- InputCollect$modNLSCollect[channel == chn, Km]
      #     # Reverse exposure to spend
      #     dt_transformSaturationSpendReverse[, (chn) := mic_men(x = .SD, Vmax = Vmax, Km = Km, reverse = TRUE), .SDcols = chn] # .SD * Km / (Vmax - .SD) exposure to spend, reverse Michaelis Menthen: x = y*Km/(Vmax-y)
      #   } else if (chn %in% InputCollect$exposure_vars) {
      #     coef_lm <- InputCollect$modNLSCollect[channel == chn, coef_lm]
      #     dt_transformSaturationSpendReverse[, (chn) := .SD / coef_lm, .SDcols = chn]
      #   }
      # }
      # dt_transformSaturationSpendReverse <- dt_transformSaturationSpendReverse[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]

      dt_scurvePlot <- tidyr::gather(
        dt_transformSaturationDecomp, "channel", "response",
        2:ncol(dt_transformSaturationDecomp)
      ) %>%
        mutate(spend = tidyr::gather(
          dt_transformSaturationSpendReverse, "channel", "spend",
          2:ncol(dt_transformSaturationSpendReverse)
        )$spend)

      # Remove outlier introduced by MM nls fitting
      dt_scurvePlot <- dt_scurvePlot[dt_scurvePlot$spend >= 0, ]

      dt_scurvePlotMean <- dt_transformSpend %>%
        slice(InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich) %>%
        select(-.data$ds) %>%
        dplyr::summarise_all(function(x) ifelse(is.na(mean(x[x > 0])), 0, mean(x[x > 0]))) %>%
        tidyr::gather("channel", "mean_spend") %>%
        mutate(mean_spend_scaled = 0, mean_response = 0, next_unit_response = 0)

      for (med in 1:InputCollect$mediaVarCount) {
        get_med <- InputCollect$paid_media_spends[med]
        get_spend_mm <- get_spend <- dt_scurvePlotMean$mean_spend[dt_scurvePlotMean$channel == get_med]

        # if (get_med %in% InputCollect$paid_media_vars[InputCollect$exposure_selector]) {
        #   Vmax <- InputCollect$modNLSCollect[channel == get_med, Vmax]
        #   Km <- InputCollect$modNLSCollect[channel == get_med, Km]
        #   # Vmax * get_spend/(Km + get_spend)
        #   get_spend_mm <- mic_men(x = get_spend, Vmax = Vmax, Km = Km)
        # } else if (get_med %in% InputCollect$exposure_vars) {
        #   coef_lm <- InputCollect$modNLSCollect[channel == get_med, coef_lm]
        #   get_spend_mm <- get_spend * coef_lm
        # } else {
        #   get_spend_mm <- get_spend
        # }

        m <- dt_transformAdstock[[get_med]][
          InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich
        ]
        # m <- m[m > 0] # remove outlier introduced by MM nls fitting
        alpha <- hypParam[which(paste0(get_med, "_alphas") == names(hypParam))][[1]]
        gamma <- hypParam[which(paste0(get_med, "_gammas") == names(hypParam))][[1]]
        get_response <- saturation_hill(x = m, alpha = alpha, gamma = gamma, x_marginal = get_spend_mm)
        get_response_marginal <- saturation_hill(x = m, alpha = alpha, gamma = gamma, x_marginal = get_spend_mm + 1)

        coef <- plotWaterfallLoop$coef[plotWaterfallLoop$rn == get_med]
        dt_scurvePlotMean$mean_spend_scaled[
          dt_scurvePlotMean$channel == get_med
        ] <- get_spend_mm
        dt_scurvePlotMean$mean_response[
          dt_scurvePlotMean$channel == get_med
        ] <- get_response * coef
        dt_scurvePlotMean$next_unit_response[
          dt_scurvePlotMean$channel == get_med
        ] <- get_response_marginal * coef - (get_response * coef)
      }
      dt_scurvePlotMean$solID <- sid

      # Exposure response curve
      # if (!identical(InputCollect$paid_media_vars, InputCollect$exposure_vars)) {
      #   exposure_which <- which(InputCollect$paid_media_vars %in% InputCollect$exposure_vars)
      #   spends_to_fit <- InputCollect$paid_media_spends[exposure_which]
      #   nls_lm_selector <- InputCollect$exposure_selector[exposure_which]
      #   dt_expoCurvePlot <- dt_scurvePlot[channel %in% spends_to_fit]
      #   dt_expoCurvePlot[, exposure_pred := 0]
      #   for (s in seq_along(spends_to_fit)) {
      #     get_med <- InputCollect$exposure_vars[s]
      #     if (nls_lm_selector[s]) {
      #       Vmax <- InputCollect$modNLSCollect[channel == get_med, Vmax]
      #       Km <- InputCollect$modNLSCollect[channel == get_med, Km]
      #       # Vmax * get_spend/(Km + get_spend)
      #       dt_expoCurvePlot[channel == spends_to_fit[s]
      #                        , ':='(exposure_pred = mic_men(x = spend, Vmax = Vmax, Km = Km)
      #                               ,channel = get_med)]
      #     } else {
      #       coef_lm <- InputCollect$modNLSCollect[channel == get_med, coef_lm]
      #       dt_expoCurvePlot[channel == spends_to_fit[s]
      #                        , ':='(exposure_pred = spend * coef_lm
      #                               , channel = get_med)]
      #     }
      #   }
      # } else {
      #   dt_expoCurvePlot <- NULL
      # }

      plot4data <- list(
        dt_scurvePlot = dt_scurvePlot,
        dt_scurvePlotMean = dt_scurvePlotMean
      )

      ## 5. Fitted vs actual
      col_order <- c("ds", "dep_var", InputCollect$all_ind_vars)
      dt_transformDecomp <- select(
        dt_modRollWind, .data$ds, .data$dep_var,
        any_of(c(InputCollect$prophet_vars, InputCollect$context_vars))
      ) %>%
        bind_cols(select(dt_transformSaturation, all_of(InputCollect$all_media))) %>%
        select(all_of(col_order))
      xDecompVec <- xDecompAgg %>%
        filter(.data$solID == sid) %>%
        select(.data$solID, .data$rn, .data$coef) %>%
        tidyr::spread(.data$rn, .data$coef)
      if (!("(Intercept)" %in% names(xDecompVec))) xDecompVec[["(Intercept)"]] <- 0
      xDecompVec <- select(xDecompVec, c("solID", "(Intercept)", col_order[!(col_order %in% c("ds", "dep_var"))]))
      intercept <- xDecompVec$`(Intercept)`
      xDecompVec <- data.frame(mapply(
        function(scurved, coefs) scurved * coefs,
        scurved = select(dt_transformDecomp, -.data$ds, -.data$dep_var),
        coefs = select(xDecompVec, -.data$solID, -.data$`(Intercept)`)
      ))
      xDecompVec <- mutate(xDecompVec,
                           intercept = intercept,
                           depVarHat = rowSums(xDecompVec) + intercept, solID = sid
      )
      xDecompVec <- bind_cols(select(dt_transformDecomp, .data$ds, .data$dep_var), xDecompVec)
      xDecompVecPlot <- select(xDecompVec, .data$ds, .data$dep_var, .data$depVarHat) %>%
        rename("actual" = "dep_var", "predicted" = "depVarHat")
      xDecompVecPlotMelted <- tidyr::gather(
        xDecompVecPlot,
        key = "variable", value = "value", -.data$ds
      )
      rsq <- filter(xDecompAgg, .data$solID == sid) %>%
        pull(.data$rsq_train) %>%
        .[1]
      plot5data <- list(xDecompVecPlotMelted = xDecompVecPlotMelted, rsq = rsq)

      ## 6. Diagnostic: fitted vs residual
      plot6data <- list(xDecompVecPlot = xDecompVecPlot)

      ## 7. Immediate vs carryover response
      plot7data <- filter(xDecompVecImmeCaov, .data$solID == sid, .data$type != "total") %>%
        select(c("type", "channels", "value")) %>%
        group_by(.data$channels, .data$type) %>%
        summarise(response = sum(.data$value), .groups = "drop_last") %>%
        mutate(percentage = .data$response / sum(.data$response)) %>%
        replace(., is.na(.), 0)

      ## 8. Bootstrapped ROI/CPA with CIs
      # plot8data <- "Empty" # Filled when running robyn_onepagers() with clustering data

      # Gather all results
      mediaVecCollect <- bind_rows(mediaVecCollect, list(
        mutate(dt_transformPlot, type = "rawMedia", solID = sid),
        mutate(dt_transformSpend, type = "rawSpend", solID = sid),
        mutate(dt_transformSpendMod, type = "predictedExposure", solID = sid),
        mutate(dt_transformAdstock, type = "adstockedMedia", solID = sid),
        mutate(dt_transformSaturation, type = "saturatedMedia", solID = sid),
        mutate(dt_transformSaturationSpendReverse, type = "saturatedSpendReversed", solID = sid),
        mutate(dt_transformSaturationDecomp, type = "decompMedia", solID = sid)
      ))
      xDecompVecCollect <- bind_rows(xDecompVecCollect, xDecompVec)
      meanResponseCollect <- bind_rows(meanResponseCollect, dt_scurvePlotMean)
      plotDataCollect[[sid]] <- list(
        plot1data = plot1data,
        plot2data = plot2data,
        plot3data = plot3data,
        plot4data = plot4data,
        plot5data = plot5data,
        plot6data = plot6data,
        plot7data = plot7data
        # plot8data = plot8data
      )
    }
  } # end pareto front loop

  meanResponseCollect <- rename(meanResponseCollect, "rn" = "channel")
  xDecompAgg <- left_join(xDecompAgg, select(
    meanResponseCollect, .data$rn, .data$solID, .data$mean_response, .data$next_unit_response
  ),
  by = c("rn", "solID")
  )

  pareto_results <- list(
    pareto_solutions = unique(xDecompVecCollect$solID),
    pareto_fronts = pareto_fronts,
    resultHypParam = resultHypParam,
    xDecompAgg = xDecompAgg,
    resultCalibration = resultCalibration,
    mediaVecCollect = mediaVecCollect,
    xDecompVecCollect = xDecompVecCollect,
    plotDataCollect = plotDataCollect
  )

  # if (check_parallel()) stopImplicitCluster()
  # close(pbplot)

  return(pareto_results)
}

## Calculate Pareto optimality, cluster and export results and plots. See ?robyn_outputs
OutputCollect <- Robyn::robyn_outputs(
  InputCollect, OutputModels,
  # pareto_fronts = "auto",
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto" or "all"
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_pareto = TRUE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = robyn_object # path for plots export
)
robyn_outputs(
  InputCollect, OutputModels,
  # pareto_fronts = "auto",
  # calibration_constraint = 0.1, # range c(0.01, 0.1) & default at 0.1
  csv_out = "pareto", # "pareto" or "all"
  clusters = TRUE, # Set to TRUE to cluster similar models by ROAS. See ?robyn_clusters
  plot_pareto = TRUE, # Set to FALSE to deactivate plotting and saving model one-pagers
  plot_folder = robyn_object # path for plots export
)
print(OutputCollect)
##################################################
##################################################
#want to modify outputcollect.... but it is not working :(
##################################################
##################################################

################################################################
#### Step 4: Select and save the any model

## Compare all model one-pagers and select one that mostly reflects your business reality
print(OutputCollect)
select_model <- "1_1_5" # Pick one of the models from OutputCollect to proceed

#### Since 3.7.1: JSON export and import (faster and lighter than RDS files)
ExportedModel <- robyn_write(InputCollect, OutputCollect, select_model)
print(ExportedModel)

################################################################
#### Step 5: Get budget allocation based on the selected model above
# Check media summary for selected model
print(ExportedModel)
############################################################
############################################################
robyn_allocator <- function(robyn_object = NULL,
                            select_build = 0,
                            InputCollect = NULL,
                            OutputCollect = NULL,
                            select_model = NULL,
                            json_file = NULL,
                            optim_algo = "SLSQP_AUGLAG",
                            scenario = "max_historical_response",
                            expected_spend = NULL,
                            expected_spend_days = NULL,
                            channel_constr_low = 0.5,
                            channel_constr_up = 2,
                            maxeval = 100000,
                            constr_mode = "eq",
                            date_min = NULL,
                            date_max = NULL,
                            export = TRUE,
                            quiet = FALSE,
                            ui = FALSE,
                            ...) {

  #####################################
  #### Set local environment

  ### Use previously exported model using json_file
  if (!is.null(json_file)) {
    if (is.null(InputCollect)) InputCollect <- robyn_inputs(json_file = json_file, ...)
    if (is.null(OutputCollect)) {
      OutputCollect <- robyn_run(
        json_file = json_file, plot_folder = robyn_object, ...
      )
    }
    if (is.null(select_model)) select_model <- OutputCollect$selectID
  }

  ## Collect inputs
  if (!is.null(robyn_object) && (is.null(InputCollect) && is.null(OutputCollect))) {
    if ("robyn_exported" %in% class(robyn_object)) {
      imported <- robyn_object
      robyn_object <- imported$robyn_object
    } else {
      imported <- robyn_load(robyn_object, select_build, quiet)
    }
    InputCollect <- imported$InputCollect
    OutputCollect <- imported$OutputCollect
    select_model <- imported$select_model
  } else if (any(is.null(InputCollect), is.null(OutputCollect), is.null(select_model))) {
    stop("When 'robyn_object' is not provided, then InputCollect, OutputCollect, select_model must be provided")
  }

  message(paste(">>> Running budget allocator for model ID", select_model, "..."))

  ## Set local data & params values
  if (TRUE) {
    dt_mod <- InputCollect$dt_mod
    paid_media_vars <- InputCollect$paid_media_vars
    paid_media_spends <- InputCollect$paid_media_spends
    startRW <- InputCollect$rollingWindowStartWhich
    endRW <- InputCollect$rollingWindowEndWhich
    adstock <- InputCollect$adstock
    media_order <- order(paid_media_spends)
    mediaVarSorted <- paid_media_vars[media_order]
    mediaSpendSorted <- paid_media_spends[media_order]
  }

  ## Check inputs and parameters
  check_allocator(
    OutputCollect, select_model, paid_media_spends, scenario,
    channel_constr_low, channel_constr_up,
    expected_spend, expected_spend_days, constr_mode
  )

  # Channels contrains
  # channel_constr_low <- rep(0.8, length(paid_media_spends))
  # channel_constr_up <- rep(1.2, length(paid_media_spends))
  if (length(channel_constr_low) == 1) {
    channel_constr_low <- rep(channel_constr_low, length(paid_media_spends))
  }
  if (length(channel_constr_up) == 1) {
    channel_constr_up <- rep(channel_constr_up, length(paid_media_spends))
  }
  names(channel_constr_low) <- paid_media_spends
  names(channel_constr_up) <- paid_media_spends
  channel_constr_low <- channel_constr_low[media_order]
  channel_constr_up <- channel_constr_up[media_order]

  # Hyper-parameters and results
  dt_hyppar <- filter(OutputCollect$resultHypParam, .data$solID == select_model)
  dt_bestCoef <- filter(OutputCollect$xDecompAgg, .data$solID == select_model, .data$rn %in% paid_media_spends)

  ## Sort table and get filter for channels mmm coef reduced to 0
  dt_coef <- select(dt_bestCoef, .data$rn, .data$coef)
  get_rn_order <- order(dt_bestCoef$rn)
  dt_coefSorted <- dt_coef[get_rn_order, ]
  dt_bestCoef <- dt_bestCoef[get_rn_order, ]
  coefSelectorSorted <- dt_coefSorted$coef > 0
  names(coefSelectorSorted) <- dt_coefSorted$rn

  ## Filter and sort all variables by name that is essential for the apply function later
  if (!all(coefSelectorSorted)) {
    chn_coef0 <- setdiff(names(coefSelectorSorted), mediaSpendSorted[coefSelectorSorted])
    message("Excluded in optimiser because their coefficients are 0: ", paste(chn_coef0, collapse = ", "))
  } else {
    chn_coef0 <- "None"
  }
  mediaSpendSortedFiltered <- mediaSpendSorted[coefSelectorSorted]
  dt_hyppar <- select(dt_hyppar, hyper_names(adstock, mediaSpendSortedFiltered)) %>%
    select(sort(colnames(.)))
  dt_bestCoef <- dt_bestCoef[dt_bestCoef$rn %in% mediaSpendSortedFiltered, ]
  channelConstrLowSorted <- channel_constr_low[mediaSpendSortedFiltered]
  channelConstrUpSorted <- channel_constr_up[mediaSpendSortedFiltered]

  ## Get adstock parameters for each channel
  getAdstockHypPar <- get_adstock_params(InputCollect, dt_hyppar)

  ## Get hill parameters for each channel
  hills <- get_hill_params(
    InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model
  )
  alphas <- hills$alphas
  gammaTrans <- hills$gammaTrans
  coefsFiltered <- hills$coefsFiltered

  # Spend values based on date range set
  dt_optimCost <- slice(dt_mod, startRW:endRW)
  check_daterange(date_min, date_max, dt_optimCost$ds)
  if (is.null(date_min)) date_min <- min(dt_optimCost$ds)
  if (is.null(date_max)) date_max <- max(dt_optimCost$ds)
  if (date_min < min(dt_optimCost$ds)) date_min <- min(dt_optimCost$ds)
  if (date_max > max(dt_optimCost$ds)) date_max <- max(dt_optimCost$ds)
  histFiltered <- filter(dt_optimCost, .data$ds >= date_min & .data$ds <= date_max)
  nPeriod <- nrow(histFiltered)
  message(sprintf("Date Window: %s:%s (%s %ss)", date_min, date_max, nPeriod, InputCollect$intervalType))

  histSpendB <- select(histFiltered, any_of(mediaSpendSortedFiltered))
  histSpendTotal <- sum(histSpendB)
  histSpend <- unlist(summarise_all(select(histFiltered, any_of(mediaSpendSortedFiltered)), sum))
  histSpendUnit <- unlist(summarise_all(histSpendB, function(x) sum(x) / sum(x > 0)))
  histSpendUnit[is.nan(histSpendUnit)] <- 0
  histSpendUnitTotal <- sum(histSpendUnit, na.rm = TRUE)
  histSpendShare <- histSpendUnit / histSpendUnitTotal

  # Response values based on date range -> mean spend
  noSpendMedia <- histResponseUnitModel <- NULL
  for (i in seq_along(mediaSpendSortedFiltered)) {
    if (histSpendUnit[i] > 0) {
      val <- robyn_response(
        json_file = json_file,
        robyn_object = robyn_object,
        select_build = select_build,
        media_metric = mediaSpendSortedFiltered[i],
        select_model = select_model,
        metric_value = histSpendUnit[i],
        dt_hyppar = OutputCollect$resultHypParam,
        dt_coef = OutputCollect$xDecompAgg,
        InputCollect = InputCollect,
        OutputCollect = OutputCollect,
        quiet = quiet
      )$response
    } else {
      val <- 0
      noSpendMedia <- c(noSpendMedia, mediaSpendSortedFiltered[i])
    }
    histResponseUnitModel <- c(histResponseUnitModel, val)
  }
  names(histResponseUnitModel) <- mediaSpendSortedFiltered
  if (!is.null(noSpendMedia) && !quiet) {
    message("Media variables with 0 spending during this date window: ", v2t(noSpendMedia))
  }

  ## Build constraints function with scenarios
  if ("max_historical_response" %in% scenario) {
    expected_spend <- histSpendTotal
    expSpendUnitTotal <- histSpendUnitTotal
  } else {
    expSpendUnitTotal <- expected_spend / (expected_spend_days / InputCollect$dayInterval)
  }

  # Gather all values that will be used internally on optim (nloptr)
  eval_list <- list(
    coefsFiltered = coefsFiltered,
    alphas = alphas,
    gammaTrans = gammaTrans,
    mediaSpendSortedFiltered = mediaSpendSortedFiltered,
    expSpendUnitTotal = expSpendUnitTotal
  )
  # So we can implicitly use these values within eval_f()
  options("ROBYN_TEMP" = eval_list)

  # eval_f(c(1,1))
  # $objective
  # [1] -0.02318446
  # $gradient
  # [1] -1.923670e-06 -8.148831e-06 -3.163465e-02 -3.553371e-05
  # $objective.channel
  # [1] -6.590166e-07 -3.087475e-06 -2.316821e-02 -1.250144e-05

  ## Set initial values and bounds
  x0 <- lb <- histSpendUnit * channelConstrLowSorted
  ub <- histSpendUnit * channelConstrUpSorted

  ## Set optim options
  if (optim_algo == "MMA_AUGLAG") {
    local_opts <- list(
      "algorithm" = "NLOPT_LD_MMA",
      "xtol_rel" = 1.0e-10
    )
  } else if (optim_algo == "SLSQP_AUGLAG") {
    local_opts <- list(
      "algorithm" = "NLOPT_LD_SLSQP",
      "xtol_rel" = 1.0e-10
    )
  }

  ## Run optim
  nlsMod <- nloptr::nloptr(
    x0 = x0,
    eval_f = eval_f,
    eval_g_eq = if (constr_mode == "eq") eval_g_eq else NULL,
    eval_g_ineq = if (constr_mode == "ineq") eval_g_ineq else NULL,
    lb = lb, ub = ub,
    opts = list(
      "algorithm" = "NLOPT_LD_AUGLAG",
      "xtol_rel" = 1.0e-10,
      "maxeval" = maxeval,
      "local_opts" = local_opts
    )
  )

  ## Collect output
  dt_optimOut <- data.frame(
    solID = select_model,
    dep_var_type = InputCollect$dep_var_type,
    channels = mediaSpendSortedFiltered,
    date_min = date_min,
    date_max = date_max,
    periods = sprintf("%s %ss", nPeriod, InputCollect$intervalType),
    constr_low = channelConstrLowSorted,
    constr_up = channelConstrUpSorted,
    # Initial
    histSpend = histSpend,
    histSpendTotal = histSpendTotal,
    initSpendUnitTotal = histSpendUnitTotal,
    initSpendUnit = histSpendUnit,
    initSpendShare = histSpendShare,
    initResponseUnit = histResponseUnitModel,
    initResponseUnitTotal = sum(histResponseUnitModel),
    initRoiUnit = histResponseUnitModel / histSpendUnit,
    # Expected
    expSpendTotal = expected_spend,
    expSpendUnitTotal = expSpendUnitTotal,
    expSpendUnitDelta = expSpendUnitTotal / histSpendUnitTotal - 1,
    # Optimized
    optmSpendUnit = nlsMod$solution,
    optmSpendUnitDelta = (nlsMod$solution / histSpendUnit - 1),
    optmSpendUnitTotal = sum(nlsMod$solution),
    optmSpendUnitTotalDelta = sum(nlsMod$solution) / histSpendUnitTotal - 1,
    optmSpendShareUnit = nlsMod$solution / sum(nlsMod$solution),
    optmResponseUnit = -eval_f(nlsMod$solution)[["objective.channel"]],
    optmResponseUnitTotal = sum(-eval_f(nlsMod$solution)[["objective.channel"]]),
    optmRoiUnit = -eval_f(nlsMod$solution)[["objective.channel"]] / nlsMod$solution,
    optmResponseUnitLift = (-eval_f(nlsMod$solution)[["objective.channel"]] / histResponseUnitModel) - 1
  ) %>%
    mutate(optmResponseUnitTotalLift = (.data$optmResponseUnitTotal / .data$initResponseUnitTotal) - 1)
  .Options$ROBYN_TEMP <- NULL # Clean auxiliary method

  ## Plot allocator results
  plots <- allocation_plots(InputCollect, OutputCollect, dt_optimOut, select_model, scenario, export, quiet)

  ## Export results into CSV
  if (export) {
    export_dt_optimOut <- dt_optimOut
    if (InputCollect$dep_var_type == "conversion") {
      colnames(export_dt_optimOut) <- gsub("Roi", "CPA", colnames(export_dt_optimOut))
    }
    write.csv(export_dt_optimOut, paste0(OutputCollect$plot_folder, select_model, "_reallocated.csv"))
  }

  output <- list(
    dt_optimOut = dt_optimOut,
    nlsMod = nlsMod,
    plots = plots,
    scenario = scenario,
    expected_spend = expected_spend,
    expected_spend_days = expected_spend_days,
    skipped = chn_coef0,
    no_spend = noSpendMedia,
    ui = if (ui) plots else NULL
  )

  class(output) <- c("robyn_allocator", class(output))
  return(output)
}

#' @rdname robyn_allocator
#' @aliases robyn_allocator
#' @param x \code{robyn_allocator()} output.
#' @export
print.robyn_allocator <- function(x, ...) {
  temp <- x$dt_optimOut[!is.nan(x$dt_optimOut$optmRoiUnit), ]
  print(glued(
    "
Model ID: {x$dt_optimOut$solID[1]}
Scenario: {scenario}
Dep. Variable Type: {temp$dep_var_type[1]}
Media Skipped (coef = 0): {paste0(x$skipped, collapse = ',')} {no_spend}
Relative Spend Increase: {spend_increase_p}% ({spend_increase}{scenario_plus})
Total Response Increase (Optimized): {signif(100 * x$dt_optimOut$optmResponseUnitTotalLift[1], 3)}%
Window: {x$dt_optimOut$date_min[1]}:{x$dt_optimOut$date_max[1]} ({x$dt_optimOut$periods[1]})
Allocation Summary:
  {summary}
",
    scenario = ifelse(
      x$scenario == "max_historical_response",
      "Maximum Historical Response",
      "Maximum Response with Expected Spend"
    ),
    no_spend = ifelse(!is.null(x$no_spend), paste("| (spend = 0):", v2t(x$no_spend, quotes = FALSE)), ""),
    spend_increase_p = signif(100 * x$dt_optimOut$expSpendUnitDelta[1], 3),
    spend_increase = formatNum(
      sum(x$dt_optimOut$optmSpendUnitTotal) - sum(x$dt_optimOut$initSpendUnitTotal),
      abbr = TRUE, sign = TRUE
    ),
    scenario_plus = ifelse(
      x$scenario == "max_response_expected_spend",
      sprintf(" in %s days", x$expected_spend_days), ""
    ),
    summary = paste(sprintf(
      "
- %s:
  Optimizable Range (bounds): [%s%%, %s%%]
  Mean Spend Share (avg): %s%% -> Optimized = %s%%
  Mean Response: %s -> Optimized = %s
  Mean Spend (per time unit): %s -> Optimized = %s [Delta = %s%%]",
      temp$channels,
      100 * temp$constr_low - 100,
      100 * temp$constr_up - 100,
      signif(100 * temp$initSpendShare, 3),
      signif(100 * temp$optmSpendShareUnit, 3),
      formatNum(temp$initResponseUnit, 0),
      formatNum(temp$optmResponseUnit, 0),
      formatNum(temp$initSpendUnit, 3, abbr = TRUE),
      formatNum(temp$optmSpendUnit, 3, abbr = TRUE),
      formatNum(100 * temp$optmSpendUnitDelta, signif = 2)
    ), collapse = "\n  ")
  ))
}

#' @rdname robyn_allocator
#' @aliases robyn_allocator
#' @param x \code{robyn_allocator()} output.
#' @export
plot.robyn_allocator <- function(x, ...) plot(x$plots$plots, ...)
#############################################
#############################################
dt_hyppar <- filter(OutputCollect$resultHypParam, .data$solID == select_model)
paid_media_spends <- InputCollect$paid_media_spends
media_order <- order(paid_media_spends)
mediaSpendSorted <- paid_media_spends[media_order]
dt_bestCoef <- filter(OutputCollect$xDecompAgg, .data$solID == select_model, .data$rn %in% paid_media_spends)
dt_coef <- select(dt_bestCoef, .data$rn, .data$coef)
get_rn_order <- order(dt_bestCoef$rn)
dt_coefSorted <- dt_coef[get_rn_order, ]
coefSelectorSorted <- dt_coefSorted$coef > 0
mediaSpendSortedFiltered <- mediaSpendSorted[coefSelectorSorted]
adstock <- InputCollect$adstock
dt_hyppar <- select(dt_hyppar, hyper_names(adstock, mediaSpendSortedFiltered)) %>%
  select(sort(colnames(.)))

adstocks = get_adstock_params(InputCollect, dt_hyppar)
adstocks["_scales" %in% names(adstocks)]
"scales" %in% names(adstocks)
adstocks[grep("scales", names(adstocks))]
get_hill_params(InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model)

hills <- get_hill_params(
  InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model
)
alphas <- hills$alphas
gammaTrans <- hills$gammaTrans
coefsFiltered <- hills$coefsFiltered
alphas

transform_to_adstock <- function(x, InputCollect, dt_hyppar, adstock){
  adstocks = get_adstock_params(InputCollect, dt_hyppar)
  if (adstock == "geometric"){
    thetas = adstocks
  } else {
    if ("cdf" %in% tolower(adstock)) {
      adstocks = 0;
    }
    else if ("pdf" %in% tolower(adstock)) {
      thetaVecCum <- .normalize(dweibull(x_bin, shape = shape, scale = scaleTrans)) # plot(thetaVecCum)
    }
  }
}
#############################################
#############################################


eval_f <- function(X) {

  # eval_list <- get("eval_list", pos = as.environment(-1))
  eval_list <- getOption("ROBYN_TEMP")
  # mm_lm_coefs <- eval_list[["mm_lm_coefs"]]
  coefsFiltered <- eval_list[["coefsFiltered"]]
  alphas <- eval_list[["alphas"]]
  gammaTrans <- eval_list[["gammaTrans"]]
  mediaSpendSortedFiltered <- eval_list[["mediaSpendSortedFiltered"]]
  # exposure_selectorSortedFiltered <- eval_list[["exposure_selectorSortedFiltered"]]
  # vmaxVec <- eval_list[["vmaxVec"]]
  # kmVec <- eval_list[["kmVec"]]

  fx_objective <- function(x, coeff, alpha, gammaTran
                           # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }

    # Adstock scales
    xAdstocked <- x
    # Hill transformation
    xOut <- coeff * sum((1 + gammaTran**alpha / xAdstocked**alpha)**-1)
    xOut
    return(xOut)
  }

  objective <- -sum(mapply(
    fx_objective,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  ))

  # https://www.derivative-calculator.net/ on the objective function 1/(1+gamma^alpha / x^alpha)
  fx_gradient <- function(x, coeff, alpha, gammaTran
                          # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }

    # Adstock scales
    xAdstocked <- x
    xOut <- -coeff * sum((alpha * (gammaTran**alpha) * (xAdstocked**(alpha - 1))) / (xAdstocked**alpha + gammaTran**alpha)**2)
    return(xOut)
  }

  gradient <- c(mapply(
    fx_gradient,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  ))

  fx_objective.chanel <- function(x, coeff, alpha, gammaTran
                                  # , chnName, vmax, km, criteria
  ) {
    # Apply Michaelis Menten model to scale spend to exposure
    # if (criteria) {
    #   xScaled <- mic_men(x = x, Vmax = vmax, Km = km) # vmax * x / (km + x)
    # } else if (chnName %in% names(mm_lm_coefs)) {
    #   xScaled <- x * mm_lm_coefs[chnName]
    # } else {
    #   xScaled <- x
    # }

    # Adstock scales
    xAdstocked <- x
    xOut <- -coeff * sum((1 + gammaTran**alpha / xAdstocked**alpha)**-1)
    return(xOut)
  }

  objective.channel <- mapply(
    fx_objective.chanel,
    x = X,
    coeff = coefsFiltered,
    alpha = alphas,
    gammaTran = gammaTrans,
    # chnName = mediaSpendSortedFiltered,
    # vmax = vmaxVec,
    # km = kmVec,
    # criteria = exposure_selectorSortedFiltered,
    SIMPLIFY = TRUE
  )

  optm <- list(objective = objective, gradient = gradient, objective.channel = objective.channel)
  return(optm)
}

eval_g_eq <- function(X) {
  eval_list <- getOption("ROBYN_TEMP")
  constr <- sum(X) - eval_list$expSpendUnitTotal
  grad <- rep(1, length(X))
  return(list(
    "constraints" = constr,
    "jacobian" = grad
  ))
}

eval_g_ineq <- function(X) {
  eval_list <- getOption("ROBYN_TEMP")
  constr <- sum(X) - eval_list$expSpendUnitTotal
  grad <- rep(1, length(X))
  return(list(
    "constraints" = constr,
    "jacobian" = grad
  ))
}

get_adstock_params <- function(InputCollect, dt_hyppar) {
  if (InputCollect$adstock == "geometric") {
    getAdstockHypPar <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_thetas"))))
  } else if (InputCollect$adstock %in% c("weibull_cdf", "weibull_pdf")) {
    getAdstockHypPar <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_shapes|.*_scales"))))
  }
  return(getAdstockHypPar)
}

get_hill_params <- function(InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model) {
  hillHypParVec <- unlist(select(dt_hyppar, na.omit(str_extract(names(dt_hyppar), ".*_alphas|.*_gammas"))))
  alphas <- hillHypParVec[str_which(names(hillHypParVec), "_alphas")]
  gammas <- hillHypParVec[str_which(names(hillHypParVec), "_gammas")]
  startRW <- InputCollect$rollingWindowStartWhich
  endRW <- InputCollect$rollingWindowEndWhich
  chnAdstocked <- filter(
    OutputCollect$mediaVecCollect,
    .data$type == "adstockedMedia",
    .data$solID == select_model
  ) %>%
    select(all_of(mediaSpendSortedFiltered)) %>%
    slice(startRW:endRW)
  gammaTrans <- mapply(function(gamma, x) {
    round(quantile(seq(range(x)[1], range(x)[2], length.out = 100), gamma), 4)
  }, gamma = gammas, x = chnAdstocked)
  names(gammaTrans) <- names(gammas)
  coefs <- dt_coef$coef
  names(coefs) <- dt_coef$rn
  coefsFiltered <- coefs[mediaSpendSortedFiltered]
  return(list(
    alphas = alphas,
    gammaTrans = gammaTrans,
    coefsFiltered = coefsFiltered
  ))
}
############################################################
############################################################

AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5),
  expected_spend = 10000, # Total spend to be simulated
  expected_spend_days = 10, # Duration of expected_spend in days
  export = TRUE
)
AllocatorCollect2 <- Robyn::robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5),
  expected_spend = 10000, # Total spend to be simulated
  expected_spend_days = 10, # Duration of expected_spend in days
  export = TRUE
)
check_allocator <- function(OutputCollect, select_model, paid_media_spends, scenario,
                            channel_constr_low, channel_constr_up,
                            expected_spend, expected_spend_days, constr_mode) {
  dt_hyppar <- OutputCollect$resultHypParam[OutputCollect$resultHypParam$solID == select_model, ]
  if (!(select_model %in% OutputCollect$allSolutions)) {
    stop(
      "Provided 'select_model' is not within the best results. Try any of: ",
      paste(OutputCollect$allSolutions, collapse = ", ")
    )
  }
  if (any(channel_constr_low < 0)) {
    stop("Inputs 'channel_constr_low' must be >= 0")
  }
  if (any(channel_constr_up < channel_constr_low)) {
    stop("Inputs 'channel_constr_up' must be >= 'channel_constr_low'")
  }
  if (any(channel_constr_up > 5)) {
    warning("Inputs 'channel_constr_up' > 5 might cause unrealistic allocation")
  }
  opts <- c("max_historical_response", "max_response_expected_spend")
  if (!(scenario %in% opts)) {
    stop("Input 'scenario' must be one of: ", paste(opts, collapse = ", "))
  }

  if (length(channel_constr_low) != 1 && length(channel_constr_low) != length(paid_media_spends)) {
    stop(paste(
      "Input 'channel_constr_low' have to contain either only 1",
      "value or have same length as 'InputCollect$paid_media_spends':", length(paid_media_spends)
    ))
  }
  if (length(channel_constr_up) != 1 && length(channel_constr_up) != length(paid_media_spends)) {
    stop(paste(
      "Input 'channel_constr_up' have to contain either only 1",
      "value or have same length as 'InputCollect$paid_media_spends':", length(paid_media_spends)
    ))
  }

  if ("max_response_expected_spend" %in% scenario) {
    if (any(is.null(expected_spend), is.null(expected_spend_days))) {
      stop("When scenario = 'max_response_expected_spend', expected_spend and expected_spend_days must be provided")
    }
  }
  opts <- c("eq", "ineq")
  if (!(constr_mode %in% opts)) {
    stop("Input 'constr_mode' must be one of: ", paste(opts, collapse = ", "))
  }
}
check_daterange <- function(date_min, date_max, dates) {
  if (!is.null(date_min)) {
    if (length(date_min) > 1) stop("Set a single date for 'date_min' parameter")
    if (date_min < min(dates)) {
      warning(sprintf(
        "Parameter 'date_min' not in your data's date range. Changed to '%s'", min(dates)
      ))
    }
  }
  if (!is.null(date_max)) {
    if (length(date_max) > 1) stop("Set a single date for 'date_max' parameter")
    if (date_max > max(dates)) {
      warning(sprintf(
        "Parameter 'date_max' not in your data's date range. Changed to '%s'", max(dates)
      ))
    }
  }
}
allocation_plots <- function(InputCollect, OutputCollect, dt_optimOut, select_model,
                             scenario, export = TRUE, quiet = FALSE) {
  outputs <- list()

  subtitle <- sprintf(
    paste0(
      "Total spend increase: %s%%",
      "\nTotal response increase: %s%% with optimised spend allocation"
    ),
    round(mean(dt_optimOut$optmSpendUnitTotalDelta) * 100, 1),
    round(mean(dt_optimOut$optmResponseUnitTotalLift) * 100, 1)
  )

  # Calculate errors for subtitles
  plotDT_scurveMeanResponse <- filter(
    OutputCollect$xDecompAgg,
    .data$solID == select_model,
    .data$rn %in% InputCollect$paid_media_spends
  )

  rsq_train_plot <- round(plotDT_scurveMeanResponse$rsq_train[1], 4)
  nrmse_plot <- round(plotDT_scurveMeanResponse$nrmse[1], 4)
  decomp_rssd_plot <- round(plotDT_scurveMeanResponse$decomp.rssd[1], 4)
  mape_lift_plot <- ifelse(!is.null(InputCollect$calibration_input),
                           round(plotDT_scurveMeanResponse$mape[1], 4), NA
  )
  errors <- paste0(
    "R2 train: ", rsq_train_plot,
    ", NRMSE = ", nrmse_plot,
    ", DECOMP.RSSD = ", decomp_rssd_plot,
    ifelse(!is.na(mape_lift_plot), paste0(", MAPE = ", mape_lift_plot), "")
  )

  # 1. Response comparison plot
  plotDT_resp <- select(dt_optimOut, .data$channels, .data$initResponseUnit, .data$optmResponseUnit) %>%
    mutate(channels = as.factor(.data$channels))
  names(plotDT_resp) <- c("channel", "Initial Mean Response", "Optimised Mean Response")
  plotDT_resp <- tidyr::gather(plotDT_resp, "variable", "response", -.data$channel)
  outputs[["p12"]] <- p12 <- ggplot(plotDT_resp, aes(
    y = reorder(.data$channel, -as.integer(.data$channel)),
    x = .data$response,
    fill = reorder(.data$variable, as.numeric(as.factor(.data$variable)))
  )) +
    geom_bar(stat = "identity", width = 0.5, position = position_dodge2(reverse = TRUE, padding = 0)) +
    scale_fill_brewer(palette = 3) +
    geom_text(aes(x = 0, label = formatNum(.data$response, 0), hjust = -0.1),
              position = position_dodge2(width = 0.5, reverse = TRUE), fontface = "bold", show.legend = FALSE
    ) +
    theme_lares(legend = "top") +
    scale_x_abbr() +
    labs(
      title = "Initial vs. Optimised Mean Response",
      subtitle = subtitle,
      fill = NULL, x = "Mean Response [#]", y = NULL
    )

  # 2. Budget share comparison plot
  plotDT_share <- select(dt_optimOut, .data$channels, .data$initSpendShare, .data$optmSpendShareUnit) %>%
    mutate(channels = as.factor(.data$channels))
  names(plotDT_share) <- c("channel", "Initial Avg. Spend Share", "Optimised Avg. Spend Share")
  plotDT_share <- tidyr::gather(plotDT_share, "variable", "spend_share", -.data$channel)
  outputs[["p13"]] <- p13 <- ggplot(plotDT_share, aes(
    y = reorder(.data$channel, -as.integer(.data$channel)),
    x = .data$spend_share, fill = .data$variable
  )) +
    geom_bar(stat = "identity", width = 0.5, position = position_dodge2(reverse = TRUE, padding = 0)) +
    scale_fill_brewer(palette = 3) +
    geom_text(aes(x = 0, label = formatNum(.data$spend_share * 100, 1, pos = "%"), hjust = -0.1),
              position = position_dodge2(width = 0.5, reverse = TRUE), fontface = "bold", show.legend = FALSE
    ) +
    theme_lares(legend = "top") +
    scale_x_percent() +
    labs(
      title = "Initial vs. Optimised Budget Allocation",
      subtitle = subtitle,
      fill = NULL, x = "Budget Allocation [%]", y = NULL
    )

  ## 3. Response curves
  plotDT_saturation <- OutputCollect$mediaVecCollect %>%
    filter(.data$solID == select_model, .data$type == "saturatedSpendReversed") %>%
    select(.data$ds, all_of(InputCollect$paid_media_spends)) %>%
    tidyr::gather("channel", "spend", -.data$ds)

  plotDT_decomp <- OutputCollect$mediaVecCollect %>%
    filter(.data$solID == select_model, .data$type == "decompMedia") %>%
    select(.data$ds, all_of(InputCollect$paid_media_spends)) %>%
    tidyr::gather("channel", "response", -.data$ds)

  plotDT_scurve <- data.frame(plotDT_saturation, response = plotDT_decomp$response) %>%
    filter(.data$spend >= 0) %>%
    as_tibble()

  dt_optimOutScurve <- rbind(
    select(dt_optimOut, .data$channels, .data$initSpendUnit, .data$initResponseUnit) %>% mutate(x = "Initial") %>% as.matrix(),
    select(dt_optimOut, .data$channels, .data$optmSpendUnit, .data$optmResponseUnit) %>% mutate(x = "Optimised") %>% as.matrix()
  ) %>% as.data.frame()
  colnames(dt_optimOutScurve) <- c("channels", "spend", "response", "type")
  dt_optimOutScurve <- dt_optimOutScurve %>%
    mutate(spend = as.numeric(.data$spend), response = as.numeric(.data$response)) %>%
    group_by(.data$channels) %>%
    mutate(
      spend_dif = dplyr::last(.data$spend) - dplyr::first(.data$spend),
      response_dif = dplyr::last(.data$response) - dplyr::first(.data$response)
    )

  trim_rate <- 1.6 # maybe enable as a parameter
  if (trim_rate > 0) {
    plotDT_scurve <- plotDT_scurve %>%
      filter(
        .data$spend < max(dt_optimOutScurve$spend) * trim_rate,
        .data$response < max(dt_optimOutScurve$response) * trim_rate
      )
  }
  outputs[["p14"]] <- p14 <- ggplot(data = plotDT_scurve, aes(
    x = .data$spend, y = .data$response, color = .data$channel
  )) +
    geom_line() +
    geom_point(data = dt_optimOutScurve, aes(
      x = .data$spend, y = .data$response,
      color = .data$channels, shape = .data$type
    ), size = 2.5) +
    # geom_text(
    #   data = dt_optimOutScurve, aes(
    #     x = .data$spend, y = .data$response, color = .data$channels,
    #     hjust = .data$hjust,
    #     label = formatNum(.data$spend, 2, abbr = TRUE)
    #   ),
    #   show.legend = FALSE
    # ) +
    theme_lares(legend.position = c(0.9, 0), pal = 2) +
    theme(
      legend.position = c(0.87, 0.5),
      legend.background = element_rect(fill = alpha("grey98", 0.6), color = "grey90"),
      legend.spacing.y = unit(0.2, "cm")
    ) +
    labs(
      title = "Response Curve and Mean* Spend by Channel",
      x = "Spend", y = "Response", shape = NULL, color = NULL,
      caption = sprintf(
        "*Based on date range: %s to %s (%s)",
        dt_optimOut$date_min[1],
        dt_optimOut$date_max[1],
        dt_optimOut$periods[1]
      )
    ) +
    scale_x_abbr() +
    scale_y_abbr()

  # Gather all plots into a single one
  p13 <- p13 + labs(subtitle = NULL)
  p12 <- p12 + labs(subtitle = NULL)
  outputs[["plots"]] <- plots <- ((p13 + p12) / p14) + plot_annotation(
    title = paste0("Budget Allocator Optimum Result for Model ID ", select_model),
    subtitle = subtitle,
    theme = theme_lares(background = "white")
  )

  # Gather all plots
  if (export) {
    scenario <- ifelse(scenario == "max_historical_response", "hist", "respo")
    filename <- paste0(OutputCollect$plot_folder, select_model, "_reallocated_", scenario, ".png")
    if (!quiet) message("Exporting charts into file: ", filename)
    ggsave(
      filename = filename,
      plot = plots, limitsize = FALSE,
      dpi = 350, width = 15, height = 12
    )
  }

  return(invisible(outputs))
}

print (AllocatorCollect1)
print (AllocatorCollect2)

#####################################################################
#####################################################################
#find inverse value
adstock_weibull(x, shape, scale, "CDF")
adstock_weibull(x, shape, scale, "PDF")
adstock_weibull(x, shape, scale, "PDF")
adstock_geometric(x, theta)

#adstock_gemotric: x/1-theta
#adstock_geometric(rep(10, 100), 0.5)
#adstock_geometric(rep(10, 100), 0.2)
adstock_weibull(rep(10, 100), 0.4, 0.4, 100, "PDF")
1-exp(-(1/0.4)^0.4)
dweibull(1, 0.4, 0.04)

x_bin <- 1:windlen
scaleTrans <- round(quantile(1:windlen, scale), 0)
exp((1/0.4)^0.4)
adstock_weibull(rep(10, 100), 0.4, 0.4, 100, "CDF")
scaleTrans <- round(quantile(1:100, 0.4), 0)
scaleTrans
x_bin = 1:100
shape = 0.4
thetaVec <- c(1, 1 - pweibull(head(x_bin, -1), shape = shape, scale = scaleTrans)) # plot(thetaVec)
thetaVecCum <- cumprod(thetaVec)
thetaVecCum
head(c(1:5), -1)
saturation_hill <- function(x, alpha, gamma, x_marginal = NULL)

robyn_allocator <- function(robyn_object = NULL,
                            select_build = 0,
                            InputCollect = NULL,
                            OutputCollect = NULL,
                            select_model = NULL,
                            json_file = NULL,
                            optim_algo = "SLSQP_AUGLAG",
                            scenario = "max_historical_response",
                            expected_spend = NULL,
                            expected_spend_days = NULL,
                            channel_constr_low = 0.5,
                            channel_constr_up = 2,
                            maxeval = 100000,
                            constr_mode = "eq",
                            date_min = NULL,
                            date_max = NULL,
                            export = TRUE,
                            quiet = FALSE,
                            ui = FALSE,
                            ...) {
#######################################################
######################################################
  robyn_response <- function(InputCollect = NULL,
                             OutputCollect = NULL,
                             json_file = NULL,
                             robyn_object = NULL,
                             select_build = NULL,
                             media_metric = NULL,
                             select_model = NULL,
                             metric_value = NULL,
                             dt_hyppar = NULL,
                             dt_coef = NULL,
                             quiet = FALSE,
                             ...) {

    ## Get input

    ### Use previously exported model using json_file
    if (!is.null(json_file)) {
      if (is.null(InputCollect)) InputCollect <- robyn_inputs(json_file = json_file, ...)
      if (is.null(OutputCollect)) {
        OutputCollect <- robyn_run(
          InputCollect = InputCollect,
          json_file = json_file,
          export = FALSE,
          quiet = quiet,
          ...
        )
      }
      if (is.null(dt_hyppar)) dt_hyppar <- OutputCollect$resultHypParam
      if (is.null(dt_coef)) dt_coef <- OutputCollect$xDecompAgg
    } else {
      if (!is.null(robyn_object)) {
        if (!file.exists(robyn_object)) {
          stop("File does not exist or is somewhere else. Check: ", robyn_object)
        } else {
          Robyn <- readRDS(robyn_object)
          objectPath <- dirname(robyn_object)
          objectName <- sub("'\\..*$", "", basename(robyn_object))
        }
        select_build_all <- 0:(length(Robyn) - 1)
        if (is.null(select_build)) {
          select_build <- max(select_build_all)
          if (!quiet && length(select_build_all) > 1) {
            message(
              "Using latest model: ", ifelse(select_build == 0, "initial model", paste0("refresh model #", select_build)),
              " for the response function. Use parameter 'select_build' to specify which run to use"
            )
          }
        }
        if (!(select_build %in% select_build_all) || length(select_build) != 1) {
          stop("'select_build' must be one value of ", paste(select_build_all, collapse = ", "))
        }
        listName <- ifelse(select_build == 0, "listInit", paste0("listRefresh", select_build))
        InputCollect <- Robyn[[listName]][["InputCollect"]]
        OutputCollect <- Robyn[[listName]][["OutputCollect"]]
        dt_hyppar <- OutputCollect$resultHypParam
        dt_coef <- OutputCollect$xDecompAgg
      } else {
        # Try to get some pre-filled values
        if (is.null(dt_hyppar)) dt_hyppar <- OutputCollect$resultHypParam
        if (is.null(dt_coef)) dt_coef <- OutputCollect$xDecompAgg
        if (any(is.null(dt_hyppar), is.null(dt_coef), is.null(InputCollect), is.null(OutputCollect))) {
          stop("When 'robyn_object' is not provided, 'InputCollect' & 'OutputCollect' must be provided")
        }
      }
    }

    if ("selectID" %in% names(OutputCollect)) {
      select_model <- OutputCollect$selectID
    }

    ## Prep environment
    if (TRUE) {
      dt_input <- InputCollect$dt_input
      startRW <- InputCollect$rollingWindowStartWhich
      endRW <- InputCollect$rollingWindowEndWhich
      adstock <- InputCollect$adstock
      spendExpoMod <- InputCollect$modNLS$results
      paid_media_vars <- InputCollect$paid_media_vars
      paid_media_spends <- InputCollect$paid_media_spends
      exposure_vars <- InputCollect$exposure_vars
      organic_vars <- InputCollect$organic_vars
      allSolutions <- unique(dt_hyppar$solID)
    }

    if (!(select_model %in% allSolutions)) {
      stop(paste0(
        "Input 'select_model' must be one of these values: ",
        paste(allSolutions, collapse = ", ")
      ))
    }

    ## Get media values
    if (media_metric %in% paid_media_spends && length(media_metric) == 1) {
      metric_type <- "spend"
    } else if (media_metric %in% exposure_vars && length(media_metric) == 1) {
      metric_type <- "exposure"
    } else if (media_metric %in% organic_vars && length(media_metric) == 1) {
      metric_type <- "organic"
    } else {
      stop(paste(
        "Invalid 'media_metric' input. It must be any media variable from",
        "paid_media_spends (spend), paid_media_vars (exposure),",
        "or organic_vars (organic); NOT:", media_metric,
        paste("\n- paid_media_spends:", v2t(paid_media_spends, quotes = FALSE)),
        paste("\n- paid_media_vars:", v2t(paid_media_vars, quotes = FALSE)),
        paste("\n- organic_vars:", v2t(organic_vars, quotes = FALSE))
      ))
    }

    if (any(is.nan(metric_value))) metric_value <- NULL
    check_metric_value(metric_value, media_metric)

    ## Transform exposure to spend when necessary
    if (metric_type == "exposure") {
      get_spend_name <- paid_media_spends[which(paid_media_vars == media_metric)]
      expo_vec <- dt_input[, media_metric][[1]]
      # Use non-0 mean as marginal level if metric_value not provided
      if (is.null(metric_value)) {
        metric_value <- mean(expo_vec[startRW:endRW][expo_vec[startRW:endRW] > 0])
        if (!quiet) message("Input 'metric_value' not provided. Using mean of ", media_metric, " instead")
      }
      # Fit spend to exposure
      spend_vec <- dt_input[, get_spend_name][[1]]
      if (is.null(spendExpoMod)) {
        stop("Can't calculate exposure to spend response. Please, recreate your InputCollect object")
      }
      temp <- filter(spendExpoMod, .data$channel == media_metric)
      nls_select <- temp$rsq_nls > temp$rsq_lm
      if (nls_select) {
        Vmax <- spendExpoMod$Vmax[spendExpoMod$channel == media_metric]
        Km <- spendExpoMod$Km[spendExpoMod$channel == media_metric]
        media_vec <- mic_men(x = spend_vec, Vmax = Vmax, Km = Km, reverse = FALSE)
      } else {
        coef_lm <- spendExpoMod$coef_lm[spendExpoMod$channel == media_metric]
        media_vec <- spend_vec * coef_lm
      }
      hpm_name <- get_spend_name
    } else {
      media_vec <- dt_input[, media_metric][[1]]
      # use non-0 means marginal level if spend not provided
      if (is.null(metric_value)) {
        metric_value <- mean(media_vec[startRW:endRW][media_vec[startRW:endRW] > 0])
        if (!quiet) message("Input 'metric_value' not provided. Using mean of ", media_metric, " instead")
      }
      hpm_name <- media_metric
    }

    ## Adstocking
    if (adstock == "geometric") {
      theta <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_thetas")]]
      x_list <- adstock_geometric(x = media_vec, theta = theta)
    } else if (adstock == "weibull_cdf") {
      shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_shapes")]]
      scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_scales")]]
      x_list <- adstock_weibull(x = media_vec, shape = shape, scale = scale, type = "cdf")
    } else if (adstock == "weibull_pdf") {
      shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_shapes")]]
      scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_scales")]]
      x_list <- adstock_weibull(x = media_vec, shape = shape, scale = scale, type = "pdf")
    }
    m_adstocked <- x_list$x_decayed

    ## Saturation
    m_adstockedRW <- m_adstocked[startRW:endRW]
    alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_alphas")]]
    gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_gammas")]]
    Saturated <- saturation_hill(x = m_adstockedRW, alpha = alpha, gamma = gamma, x_marginal = metric_value)
    m_saturated <- saturation_hill(x = m_adstockedRW, alpha = alpha, gamma = gamma)

    ## Decomp
    coeff <- dt_coef[dt_coef$solID == select_model & dt_coef$rn == hpm_name, ][["coef"]]
    response_vec <- m_saturated * coeff
    Response <- as.numeric(Saturated * coeff)

    ## Plot optimal response
    media_type <- ifelse(metric_type == "organic", "organic", "paid")
    dt_line <- data.frame(metric = m_adstockedRW, response = response_vec, channel = media_metric)
    dt_point <- data.frame(input = metric_value, output = Response)
    p_res <- ggplot(dt_line, aes(x = .data$metric, y = .data$response)) +
      geom_line(color = "steelblue") +
      geom_point(data = dt_point, aes(x = .data$input, y = .data$output), size = 3) +
      labs(
        title = paste(
          "Saturation curve of", media_type, "media:", media_metric,
          ifelse(metric_type == "spend", "spend metric", "exposure metric")
        ),
        subtitle = sprintf(
          "Response of %s @ %s",
          formatNum(dt_point$output, signif = 4),
          formatNum(dt_point$input, signif = 4)
        ),
        x = "Metric", y = "Response"
      ) +
      theme_lares() +
      scale_x_abbr() +
      scale_y_abbr()

    class(Response) <- unique(c("robyn_response", class(Response)))
    return(list(
      response = Response,
      plot = p_res
    ))
  }

tmp = OutputCollect$mediaVecCollect %>% filter(solID == select_model)
tmp %>% filter(type == "adstockedMedia")
tmp %>% filter(type == "decompMedia")

tmp %>% distinct(type)
head(tmp,3)
  robyn_response_only(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "media4",
  metric_value = 750
)
InputCollect$dt_input

rd_n=read.csv("C:/Users/YEEUN/Desktop/robyn/RObyn/hackathon_sample/sample_dataset/test_data_new.csv")
head(rd_n,3)

media_info = list()
media_info["media1"] = c(10, 20, 30)
media_info["media2"] = 10
media_info["media3"] = 10
media_info["media4"] = 10
rd_n = rd_n[1:30,]
rd_n["media1"]
media_name = "media3"
mean(rd_n[[media_name]])
robyn_response_only(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = media_name,
  metric_value = 300
)

mean(rd_n[[media_name]])

total_response <- function(InputCollect, cost_post){
  all_media = InputCollect$all_media
  for (i in all_media){
    robyn_response_only(
      InputCollect = InputCollect,
      OutputCollect = OutputCollect,
      select_model = select_model,
      media_metric = i,
      metric_value = 750
    )
  }
}


  AllocatorCollect1
  check_metric_value <- function(metric_value, media_metric) {
    if (!is.null(metric_value)) {
      if (!is.numeric(metric_value)) {
        stop(sprintf(
          "Input 'metric_value' for %s (%s) must be a numerical value\n", media_metric, toString(metric_value)
        ))
      }
      if (sum(metric_value <= 0) > 0) {
        stop(sprintf(
          "Input 'metric_value' for %s (%s) must be a positive value\n", media_metric, metric_value[metric_value <= 0]
        ))
      }
    }
  }
  robyn_response_only <- function(InputCollect = NULL,
                             OutputCollect = NULL,
                             json_file = NULL,
                             robyn_object = NULL,
                             select_build = NULL,
                             media_metric = NULL,
                             select_model = NULL,
                             metric_value = NULL,
                             dt_hyppar = NULL,
                             dt_coef = NULL,
                             quiet = FALSE,
                             ...) {

    ## Get input

    ### Use previously exported model using json_file
    if (!is.null(json_file)) {
      if (is.null(InputCollect)) InputCollect <- robyn_inputs(json_file = json_file, ...)
      if (is.null(OutputCollect)) {
        OutputCollect <- robyn_run(
          InputCollect = InputCollect,
          json_file = json_file,
          export = FALSE,
          quiet = quiet,
          ...
        )
      }
      if (is.null(dt_hyppar)) dt_hyppar <- OutputCollect$resultHypParam
      if (is.null(dt_coef)) dt_coef <- OutputCollect$xDecompAgg
    } else {
      if (!is.null(robyn_object)) {
        if (!file.exists(robyn_object)) {
          stop("File does not exist or is somewhere else. Check: ", robyn_object)
        } else {
          Robyn <- readRDS(robyn_object)
          objectPath <- dirname(robyn_object)
          objectName <- sub("'\\..*$", "", basename(robyn_object))
        }
        select_build_all <- 0:(length(Robyn) - 1)
        if (is.null(select_build)) {
          select_build <- max(select_build_all)
          if (!quiet && length(select_build_all) > 1) {
            message(
              "Using latest model: ", ifelse(select_build == 0, "initial model", paste0("refresh model #", select_build)),
              " for the response function. Use parameter 'select_build' to specify which run to use"
            )
          }
        }
        if (!(select_build %in% select_build_all) || length(select_build) != 1) {
          stop("'select_build' must be one value of ", paste(select_build_all, collapse = ", "))
        }
        listName <- ifelse(select_build == 0, "listInit", paste0("listRefresh", select_build))
        InputCollect <- Robyn[[listName]][["InputCollect"]]
        OutputCollect <- Robyn[[listName]][["OutputCollect"]]
        dt_hyppar <- OutputCollect$resultHypParam
        dt_coef <- OutputCollect$xDecompAgg
      } else {
        # Try to get some pre-filled values
        if (is.null(dt_hyppar)) dt_hyppar <- OutputCollect$resultHypParam
        if (is.null(dt_coef)) dt_coef <- OutputCollect$xDecompAgg
        if (any(is.null(dt_hyppar), is.null(dt_coef), is.null(InputCollect), is.null(OutputCollect))) {
          stop("When 'robyn_object' is not provided, 'InputCollect' & 'OutputCollect' must be provided")
        }
      }
    }

    if ("selectID" %in% names(OutputCollect)) {
      select_model <- OutputCollect$selectID
    }

    ## Prep environment
    if (TRUE) {
      dt_input <- InputCollect$dt_input
      startRW <- InputCollect$rollingWindowStartWhich
      endRW <- InputCollect$rollingWindowEndWhich
      adstock <- InputCollect$adstock
      spendExpoMod <- InputCollect$modNLS$results
      paid_media_vars <- InputCollect$paid_media_vars
      paid_media_spends <- InputCollect$paid_media_spends
      exposure_vars <- InputCollect$exposure_vars
      organic_vars <- InputCollect$organic_vars
      allSolutions <- unique(dt_hyppar$solID)
    }

    if (!(select_model %in% allSolutions)) {
      stop(paste0(
        "Input 'select_model' must be one of these values: ",
        paste(allSolutions, collapse = ", ")
      ))
    }

    ## Get media values
    if (media_metric %in% paid_media_spends && length(media_metric) == 1) {
      metric_type <- "spend"
    } else if (media_metric %in% exposure_vars && length(media_metric) == 1) {
      metric_type <- "exposure"
    } else if (media_metric %in% organic_vars && length(media_metric) == 1) {
      metric_type <- "organic"
    } else {
      stop(paste(
        "Invalid 'media_metric' input. It must be any media variable from",
        "paid_media_spends (spend), paid_media_vars (exposure),",
        "or organic_vars (organic); NOT:", media_metric,
        paste("\n- paid_media_spends:", v2t(paid_media_spends, quotes = FALSE)),
        paste("\n- paid_media_vars:", v2t(paid_media_vars, quotes = FALSE)),
        paste("\n- organic_vars:", v2t(organic_vars, quotes = FALSE))
      ))
    }

    if (any(is.nan(metric_value))) metric_value <- NULL
    check_metric_value(metric_value, media_metric)

    ## Transform exposure to spend when necessary
    if (metric_type == "exposure") {
      get_spend_name <- paid_media_spends[which(paid_media_vars == media_metric)]
      expo_vec <- dt_input[, media_metric][[1]]
      # Use non-0 mean as marginal level if metric_value not provided
      if (is.null(metric_value)) {
        metric_value <- mean(expo_vec[startRW:endRW][expo_vec[startRW:endRW] > 0])
        if (!quiet) message("Input 'metric_value' not provided. Using mean of ", media_metric, " instead")
      }
      # Fit spend to exposure
      spend_vec <- dt_input[, get_spend_name][[1]]
      if (is.null(spendExpoMod)) {
        stop("Can't calculate exposure to spend response. Please, recreate your InputCollect object")
      }
      temp <- filter(spendExpoMod, .data$channel == media_metric)
      nls_select <- temp$rsq_nls > temp$rsq_lm
      if (nls_select) {
        Vmax <- spendExpoMod$Vmax[spendExpoMod$channel == media_metric]
        Km <- spendExpoMod$Km[spendExpoMod$channel == media_metric]
        media_vec <- mic_men(x = spend_vec, Vmax = Vmax, Km = Km, reverse = FALSE)
      } else {
        coef_lm <- spendExpoMod$coef_lm[spendExpoMod$channel == media_metric]
        media_vec <- spend_vec * coef_lm
      }
      hpm_name <- get_spend_name
    } else {
      media_vec <- dt_input[, media_metric][[1]]
      # use non-0 means marginal level if spend not provided
      if (is.null(metric_value)) {
        metric_value <- mean(media_vec[startRW:endRW][media_vec[startRW:endRW] > 0])
        if (!quiet) message("Input 'metric_value' not provided. Using mean of ", media_metric, " instead")
      }
      hpm_name <- media_metric
    }

    ## Adstocking
    if (adstock == "geometric") {
      theta <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_thetas")]]
      x_list <- adstock_geometric(x = media_vec, theta = theta)
    } else if (adstock == "weibull_cdf") {
      shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_shapes")]]
      scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_scales")]]
      x_list <- adstock_weibull(x = media_vec, shape = shape, scale = scale, type = "cdf")
    } else if (adstock == "weibull_pdf") {
      shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_shapes")]]
      scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_scales")]]
      x_list <- adstock_weibull(x = media_vec, shape = shape, scale = scale, type = "pdf")
    }
    print ("x_list")
    print (x_list)
    print (media_vec)
    m_adstocked <- x_list$x_decayed

    ## Saturation
    m_adstockedRW <- m_adstocked[startRW:endRW]
    alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_alphas")]]
    gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(hpm_name, "_gammas")]]
    Saturated <- saturation_hill(x = m_adstockedRW, alpha = alpha, gamma = gamma, x_marginal = metric_value)
    print (Saturated)
    m_saturated <- saturation_hill(x = m_adstockedRW, alpha = alpha, gamma = gamma)

    ## Decomp
    coeff <- dt_coef[dt_coef$solID == select_model & dt_coef$rn == hpm_name, ][["coef"]]
    response_vec <- m_saturated * coeff
    Response <- as.numeric(Saturated * coeff)
    print ("Response")
    print (Response)
    class(Response) <- unique(c("robyn_response", class(Response)))
    return(
      response = Response
    )
  }

  robyn_response_only(InputCollect, OutputCollect)
  #####################################
  #### Set local environment

  ### Use previously exported model using json_file
  if (!is.null(json_file)) {
    if (is.null(InputCollect)) InputCollect <- robyn_inputs(json_file = json_file, ...)
    if (is.null(OutputCollect)) {
      OutputCollect <- robyn_run(
        json_file = json_file, plot_folder = robyn_object, ...
      )
    }
    if (is.null(select_model)) select_model <- OutputCollect$selectID
  }

  ## Collect inputs
  if (!is.null(robyn_object) && (is.null(InputCollect) && is.null(OutputCollect))) {
    if ("robyn_exported" %in% class(robyn_object)) {
      imported <- robyn_object
      robyn_object <- imported$robyn_object
    } else {
      imported <- robyn_load(robyn_object, select_build, quiet)
    }
    InputCollect <- imported$InputCollect
    OutputCollect <- imported$OutputCollect
    select_model <- imported$select_model
  } else if (any(is.null(InputCollect), is.null(OutputCollect), is.null(select_model))) {
    stop("When 'robyn_object' is not provided, then InputCollect, OutputCollect, select_model must be provided")
  }

  message(paste(">>> Running budget allocator for model ID", select_model, "..."))

  ## Set local data & params values
  if (TRUE) {
    dt_mod <- InputCollect$dt_mod
    paid_media_vars <- InputCollect$paid_media_vars
    paid_media_spends <- InputCollect$paid_media_spends
    startRW <- InputCollect$rollingWindowStartWhich
    endRW <- InputCollect$rollingWindowEndWhich
    adstock <- InputCollect$adstock
    media_order <- order(paid_media_spends)
    mediaVarSorted <- paid_media_vars[media_order]
    mediaSpendSorted <- paid_media_spends[media_order]
  }

  ## Check inputs and parameters
  check_allocator(
    OutputCollect, select_model, paid_media_spends, scenario,
    channel_constr_low, channel_constr_up,
    expected_spend, expected_spend_days, constr_mode
  )

  # Channels contrains
  # channel_constr_low <- rep(0.8, length(paid_media_spends))
  # channel_constr_up <- rep(1.2, length(paid_media_spends))
  if (length(channel_constr_low) == 1) {
    channel_constr_low <- rep(channel_constr_low, length(paid_media_spends))
  }
  if (length(channel_constr_up) == 1) {
    channel_constr_up <- rep(channel_constr_up, length(paid_media_spends))
  }
  names(channel_constr_low) <- paid_media_spends
  names(channel_constr_up) <- paid_media_spends
  channel_constr_low <- channel_constr_low[media_order]
  channel_constr_up <- channel_constr_up[media_order]

  # Hyper-parameters and results
  dt_hyppar <- filter(OutputCollect$resultHypParam, .data$solID == select_model)
  dt_bestCoef <- filter(OutputCollect$xDecompAgg, .data$solID == select_model, .data$rn %in% paid_media_spends)

  ## Sort table and get filter for channels mmm coef reduced to 0
  dt_coef <- select(dt_bestCoef, .data$rn, .data$coef)
  get_rn_order <- order(dt_bestCoef$rn)
  dt_coefSorted <- dt_coef[get_rn_order, ]
  dt_bestCoef <- dt_bestCoef[get_rn_order, ]
  coefSelectorSorted <- dt_coefSorted$coef > 0
  names(coefSelectorSorted) <- dt_coefSorted$rn

  ## Filter and sort all variables by name that is essential for the apply function later
  if (!all(coefSelectorSorted)) {
    chn_coef0 <- setdiff(names(coefSelectorSorted), mediaSpendSorted[coefSelectorSorted])
    message("Excluded in optimiser because their coefficients are 0: ", paste(chn_coef0, collapse = ", "))
  } else {
    chn_coef0 <- "None"
  }
  mediaSpendSortedFiltered <- mediaSpendSorted[coefSelectorSorted]
  dt_hyppar <- select(dt_hyppar, hyper_names(adstock, mediaSpendSortedFiltered)) %>%
    select(sort(colnames(.)))
  dt_bestCoef <- dt_bestCoef[dt_bestCoef$rn %in% mediaSpendSortedFiltered, ]
  channelConstrLowSorted <- channel_constr_low[mediaSpendSortedFiltered]
  channelConstrUpSorted <- channel_constr_up[mediaSpendSortedFiltered]

  ## Get adstock parameters for each channel
  getAdstockHypPar <- get_adstock_params(InputCollect, dt_hyppar)

  ## Get hill parameters for each channel
  hills <- get_hill_params(
    InputCollect, OutputCollect, dt_hyppar, dt_coef, mediaSpendSortedFiltered, select_model
  )
  alphas <- hills$alphas
  gammaTrans <- hills$gammaTrans
  coefsFiltered <- hills$coefsFiltered

  # Spend values based on date range set
  dt_optimCost <- slice(dt_mod, startRW:endRW)
  check_daterange(date_min, date_max, dt_optimCost$ds)
  if (is.null(date_min)) date_min <- min(dt_optimCost$ds)
  if (is.null(date_max)) date_max <- max(dt_optimCost$ds)
  if (date_min < min(dt_optimCost$ds)) date_min <- min(dt_optimCost$ds)
  if (date_max > max(dt_optimCost$ds)) date_max <- max(dt_optimCost$ds)
  histFiltered <- filter(dt_optimCost, .data$ds >= date_min & .data$ds <= date_max)
  nPeriod <- nrow(histFiltered)
  message(sprintf("Date Window: %s:%s (%s %ss)", date_min, date_max, nPeriod, InputCollect$intervalType))

  histSpendB <- select(histFiltered, any_of(mediaSpendSortedFiltered))
  histSpendTotal <- sum(histSpendB)
  histSpend <- unlist(summarise_all(select(histFiltered, any_of(mediaSpendSortedFiltered)), sum))
  histSpendUnit <- unlist(summarise_all(histSpendB, function(x) sum(x) / sum(x > 0)))
  histSpendUnit[is.nan(histSpendUnit)] <- 0
  histSpendUnitTotal <- sum(histSpendUnit, na.rm = TRUE)
  histSpendShare <- histSpendUnit / histSpendUnitTotal

  # Response values based on date range -> mean spend
  noSpendMedia <- histResponseUnitModel <- NULL
  for (i in seq_along(mediaSpendSortedFiltered)) {
    if (histSpendUnit[i] > 0) {
      val <- robyn_response(
        json_file = json_file,
        robyn_object = robyn_object,
        select_build = select_build,
        media_metric = mediaSpendSortedFiltered[i],
        select_model = select_model,
        metric_value = histSpendUnit[i],
        dt_hyppar = OutputCollect$resultHypParam,
        dt_coef = OutputCollect$xDecompAgg,
        InputCollect = InputCollect,
        OutputCollect = OutputCollect,
        quiet = quiet
      )$response
    } else {
      val <- 0
      noSpendMedia <- c(noSpendMedia, mediaSpendSortedFiltered[i])
    }
    histResponseUnitModel <- c(histResponseUnitModel, val)
  }
  names(histResponseUnitModel) <- mediaSpendSortedFiltered
  if (!is.null(noSpendMedia) && !quiet) {
    message("Media variables with 0 spending during this date window: ", v2t(noSpendMedia))
  }

  ## Build constraints function with scenarios
  if ("max_historical_response" %in% scenario) {
    expected_spend <- histSpendTotal
    expSpendUnitTotal <- histSpendUnitTotal
  } else {
    expSpendUnitTotal <- expected_spend / (expected_spend_days / InputCollect$dayInterval)
  }

  # Gather all values that will be used internally on optim (nloptr)
  eval_list <- list(
    coefsFiltered = coefsFiltered,
    alphas = alphas,
    gammaTrans = gammaTrans,
    mediaSpendSortedFiltered = mediaSpendSortedFiltered,
    expSpendUnitTotal = expSpendUnitTotal
  )
  # So we can implicitly use these values within eval_f()
  options("ROBYN_TEMP" = eval_list)

  # eval_f(c(1,1))
  # $objective
  # [1] -0.02318446
  # $gradient
  # [1] -1.923670e-06 -8.148831e-06 -3.163465e-02 -3.553371e-05
  # $objective.channel
  # [1] -6.590166e-07 -3.087475e-06 -2.316821e-02 -1.250144e-05

  ## Set initial values and bounds
  x0 <- lb <- histSpendUnit * channelConstrLowSorted
  ub <- histSpendUnit * channelConstrUpSorted

  ## Set optim options
  if (optim_algo == "MMA_AUGLAG") {
    local_opts <- list(
      "algorithm" = "NLOPT_LD_MMA",
      "xtol_rel" = 1.0e-10
    )
  } else if (optim_algo == "SLSQP_AUGLAG") {
    local_opts <- list(
      "algorithm" = "NLOPT_LD_SLSQP",
      "xtol_rel" = 1.0e-10
    )
  }

  ## Run optim
  nlsMod <- nloptr::nloptr(
    x0 = x0,
    eval_f = eval_f,
    eval_g_eq = if (constr_mode == "eq") eval_g_eq else NULL,
    eval_g_ineq = if (constr_mode == "ineq") eval_g_ineq else NULL,
    lb = lb, ub = ub,
    opts = list(
      "algorithm" = "NLOPT_LD_AUGLAG",
      "xtol_rel" = 1.0e-10,
      "maxeval" = maxeval,
      "local_opts" = local_opts
    )
  )

  ## Collect output
  dt_optimOut <- data.frame(
    solID = select_model,
    dep_var_type = InputCollect$dep_var_type,
    channels = mediaSpendSortedFiltered,
    date_min = date_min,
    date_max = date_max,
    periods = sprintf("%s %ss", nPeriod, InputCollect$intervalType),
    constr_low = channelConstrLowSorted,
    constr_up = channelConstrUpSorted,
    # Initial
    histSpend = histSpend,
    histSpendTotal = histSpendTotal,
    initSpendUnitTotal = histSpendUnitTotal,
    initSpendUnit = histSpendUnit,
    initSpendShare = histSpendShare,
    initResponseUnit = histResponseUnitModel,
    initResponseUnitTotal = sum(histResponseUnitModel),
    initRoiUnit = histResponseUnitModel / histSpendUnit,
    # Expected
    expSpendTotal = expected_spend,
    expSpendUnitTotal = expSpendUnitTotal,
    expSpendUnitDelta = expSpendUnitTotal / histSpendUnitTotal - 1,
    # Optimized
    optmSpendUnit = nlsMod$solution,
    optmSpendUnitDelta = (nlsMod$solution / histSpendUnit - 1),
    optmSpendUnitTotal = sum(nlsMod$solution),
    optmSpendUnitTotalDelta = sum(nlsMod$solution) / histSpendUnitTotal - 1,
    optmSpendShareUnit = nlsMod$solution / sum(nlsMod$solution),
    optmResponseUnit = -eval_f(nlsMod$solution)[["objective.channel"]],
    optmResponseUnitTotal = sum(-eval_f(nlsMod$solution)[["objective.channel"]]),
    optmRoiUnit = -eval_f(nlsMod$solution)[["objective.channel"]] / nlsMod$solution,
    optmResponseUnitLift = (-eval_f(nlsMod$solution)[["objective.channel"]] / histResponseUnitModel) - 1
  ) %>%
    mutate(optmResponseUnitTotalLift = (.data$optmResponseUnitTotal / .data$initResponseUnitTotal) - 1)
  .Options$ROBYN_TEMP <- NULL # Clean auxiliary method

  ## Plot allocator results
  plots <- allocation_plots(InputCollect, OutputCollect, dt_optimOut, select_model, scenario, export, quiet)

  ## Export results into CSV
  if (export) {
    export_dt_optimOut <- dt_optimOut
    if (InputCollect$dep_var_type == "conversion") {
      colnames(export_dt_optimOut) <- gsub("Roi", "CPA", colnames(export_dt_optimOut))
    }
    write.csv(export_dt_optimOut, paste0(OutputCollect$plot_folder, select_model, "_reallocated.csv"))
  }

  output <- list(
    dt_optimOut = dt_optimOut,
    nlsMod = nlsMod,
    plots = plots,
    scenario = scenario,
    expected_spend = expected_spend,
    expected_spend_days = expected_spend_days,
    skipped = chn_coef0,
    no_spend = noSpendMedia,
    ui = if (ui) plots else NULL
  )

  class(output) <- c("robyn_allocator", class(output))
  return(output)
}



library(dplyr)
hyper_model = OutputCollect$resultHypParam %>% filter(solID == select_model)
tv_raw = InputCollect$dt_input$tv_S

tv_adstock_rd = OutputCollect$mediaVecCollect %>% filter(type == "adstockedMedia") %>%
  filter(solID == select_model) %>% select(tv_S)
tv_adstock_rd$tv_S

adstock_geometric(tv_raw, hyper_model$tv_S_thetas)$x_decayed[1:10]
tv_adstock$tv_S[1:10]
length(tv_raw)
length(tv_adstock)

AllocatorCollect1 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_historical_response",
  channel_constr_low = 0.7,
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  export = TRUE,
  date_min = "2016-11-21",
  date_max = "2018-08-20"
)
print(AllocatorCollect1)
# plot(AllocatorCollect1)

# Run the "max_response_expected_spend" scenario: "What's the maximum response for a given
# total spend based on historical saturation and what is the spend mix?" "optmSpendShareUnit"
# is the optimum spend share.
AllocatorCollect2 <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  expected_spend = 1000000, # Total spend to be simulated
  expected_spend_days = 7, # Duration of expected_spend in days
  export = TRUE
)
print(AllocatorCollect2)
AllocatorCollect2$dt_optimOut
# plot(AllocatorCollect2)

## A csv is exported into the folder for further usage. Check schema here:
## https://github.com/facebookexperimental/Robyn/blob/main/demo/schema.R

## QA optimal response
# Pick any media variable: InputCollect$all_media
select_media <- "search_S"
# For paid_media_spends set metric_value as your optimal spend
metric_value <- AllocatorCollect1$dt_optimOut$optmSpendUnit[
  AllocatorCollect1$dt_optimOut$channels == select_media
  ]; metric_value
# # For paid_media_vars and organic_vars, manually pick a value
# metric_value <- 10000

if (TRUE) {
  optimal_response_allocator <- AllocatorCollect1$dt_optimOut$optmResponseUnit[
    AllocatorCollect1$dt_optimOut$channels == select_media
    ]
  optimal_response <- robyn_response(
    InputCollect = InputCollect,
    OutputCollect = OutputCollect,
    select_model = select_model,
    select_build = 0,
    media_metric = select_media,
    metric_value = metric_value
  )
  plot(optimal_response$plot)
  if (length(optimal_response_allocator) > 0) {
    cat("QA if results from robyn_allocator and robyn_response agree: ")
    cat(round(optimal_response_allocator) == round(optimal_response$response), "( ")
    cat(optimal_response$response, "==", optimal_response_allocator, ")\n")
  }
}

################################################################
#### Step 6: Model refresh based on selected model and saved results "Alpha" [v3.7.1]

## Must run robyn_write() (manually or automatically) to export any model first, before refreshing.
## The robyn_refresh() function is suitable for updating within "reasonable periods".
## Two situations are considered better to rebuild model:
## 1. most data is new. If initial model has 100 weeks and 80 weeks new data is added in refresh,
## it might be better to rebuild the model. Rule of thumb: 50% of data or less can be new.
## 2. new variables are added.

# Provide JSON file with your InputCollect and ExportedModel specifications
# It can be any model, initial or a refresh model
json_file <- "~/Desktop/Robyn_202208231837_init/RobynModel-1_100_6.json"
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 13,
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 1
)

json_file_rf1 <- "~/Desktop/Robyn_202208231837_init/Robyn_202208231841_rf1/RobynModel-1_12_5.json"
RobynRefresh <- robyn_refresh(
  json_file = json_file_rf1,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  refresh_steps = 7,
  refresh_iters = 1000, # 1k is an estimation
  refresh_trials = 1
)

# InputCollect <- RobynRefresh$listRefresh1$InputCollect
# OutputCollect <- RobynRefresh$listRefresh1$OutputCollect
# select_model <- RobynRefresh$listRefresh1$OutputCollect$selectID

###### DEPRECATED (<3.7.1) (might work)
# # Run ?robyn_refresh to check parameter definition
# Robyn <- robyn_refresh(
#   robyn_object = robyn_object,
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   refresh_steps = 4,
#   refresh_mode = "manual",
#   refresh_iters = 1000, # 1k is estimation. Use refresh_mode = "manual" to try out.
#   refresh_trials = 1
# )

## Besides plots: there are 4 CSV outputs saved in the folder for further usage
# report_hyperparameters.csv, hyperparameters of all selected model for reporting
# report_aggregated.csv, aggregated decomposition per independent variable
# report_media_transform_matrix.csv, all media transformation vectors
# report_alldecomp_matrix.csv,all decomposition vectors of independent variables

################################################################
#### Step 7: Get budget allocation recommendation based on selected refresh runs

# Run ?robyn_allocator to check parameter definition
AllocatorCollect <- robyn_allocator(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  scenario = "max_response_expected_spend",
  channel_constr_low = c(0.7, 0.7, 0.7, 0.7, 0.7),
  channel_constr_up = c(1.2, 1.5, 1.5, 1.5, 1.5),
  expected_spend = 2000000, # Total spend to be simulated
  expected_spend_days = 14 # Duration of expected_spend in days
)
print(AllocatorCollect)
# plot(AllocatorCollect)

################################################################
#### Step 8: get marginal returns

## Example of how to get marginal ROI of next 1000$ from the 80k spend level for search channel

# Run ?robyn_response to check parameter definition

## -------------------------------- NOTE v3.6.0 CHANGE !!! ---------------------------------- ##
## The robyn_response() function can now output response for both spends and exposures (imps,
## GRP, newsletter sendings etc.) as well as plotting individual saturation curves. New
## argument names "media_metric" and "metric_value" instead of "paid_media_var" and "spend"
## are now used to accommodate this change. Also the returned output is a list now and
## contains also the plot.
## ------------------------------------------------------------------------------------------ ##




# Get response for 80k from result saved in robyn_object
Spend1 <- 60000
Response1 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "search_S",
  metric_value = Spend1
)
Response1$response / Spend1 # ROI for search 80k
Response1$plot

#### Or you can call a JSON file directly (a bit slower)
# Response1 <- robyn_response(
#   json_file = json_file,
#   dt_input = dt_simulated_weekly,
#   dt_holidays = dt_prophet_holidays,
#   media_metric = "search_S",
#   metric_value = Spend1
# )

# Get response for +10%
Spend2 <- Spend1 * 1.1
Response2 <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "search_S",
  metric_value = Spend2
)
Response2$response / Spend2 # ROI for search 81k
Response2$plot

# Marginal ROI of next 1000$ from 80k spend level for search
(Response2$response - Response1$response) / (Spend2 - Spend1)

## Example of getting paid media exposure response curves
imps <- 50000000
response_imps <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "facebook_I",
  metric_value = imps
)
response_imps$response / imps * 1000
response_imps$plot

## Example of getting organic media exposure response curves
sendings <- 30000
response_sending <- robyn_response(
  InputCollect = InputCollect,
  OutputCollect = OutputCollect,
  select_model = select_model,
  media_metric = "newsletter",
  metric_value = sendings
)
response_sending$response / sendings * 1000
response_sending$plot

################################################################
#### Optional: recreate old models and replicate results [v3.7.1]

# From an exported JSON file (which is created automatically when exporting a model)
# we can re-create a previously trained model and outputs. Note: we need to provide
# the main dataset and the holidays dataset, which are NOT stored in the JSON file.
# These JSON files will be automatically created in most cases.

############ WRITE ############
# Manually create JSON file with inputs data only
robyn_write(InputCollect, dir = "~/Desktop")

# Manually create JSON file with inputs and specific model results
robyn_write(InputCollect, OutputCollect, select_model)


############ READ ############
# Recreate `InputCollect` and `OutputCollect` objects
# Pick any exported model (initial or refreshed)
json_file <- "~/Desktop/Robyn_202208231837_init/RobynModel-1_100_6.json"

# Optional: Manually read and check data stored in file
json_data <- robyn_read(json_file)
print(json_data)

# Re-create InputCollect
InputCollectX <- robyn_inputs(
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  json_file = json_file)

# Re-create OutputCollect
OutputCollectX <- robyn_run(
  InputCollect = InputCollectX,
  json_file = json_file,
  export = FALSE)

# Or re-create both by simply using robyn_recreate()
RobynRecreated <- robyn_recreate(
  json_file = json_file,
  dt_input = dt_simulated_weekly,
  dt_holidays = dt_prophet_holidays,
  quiet = FALSE)
InputCollectX <- RobynRecreated$InputCollect
OutputCollectX <- RobynRecreated$OutputCollect

# Re-export model and check summary (will get exported in your current working directory)
myModel <- robyn_write(InputCollectX, OutputCollectX, dir = "~/Desktop")
print(myModel)

# Re-create one-pager
myModelPlot <- robyn_onepagers(InputCollectX, OutputCollectX, export = FALSE)
myModelPlot

# Refresh any imported model
RobynRefresh <- robyn_refresh(
  json_file = json_file,
  dt_input = InputCollectX$dt_input,
  dt_holidays = InputCollectX$dt_holidays,
  refresh_steps = 6,
  refresh_mode = "manual",
  refresh_iters = 1000,
  refresh_trials = 1
)

# Recreate response curves
robyn_response(
  InputCollect = InputCollectX,
  OutputCollect = OutputCollectX,
  media_metric = "newsletter",
  metric_value = 50000
)




nlsMod <- nloptr::nloptr(
  x0 = x0,
  eval_f = eval_f,
  eval_g_eq = if (constr_mode == "eq") eval_g_eq else NULL,
  eval_g_ineq = if (constr_mode == "ineq") eval_g_ineq else NULL,
  lb = lb, ub = ub,
  opts = list(
    "algorithm" = "NLOPT_LD_AUGLAG",
    "xtol_rel" = 1.0e-10,
    "maxeval" = maxeval,
    "local_opts" = local_opts
  )
)
