
####################################################################
#' ***Suggestion*** compare_zero_spend
#'
#' focus on all days (w/ non-zero spend days)
#' check accuracy with dependent
#' @param InputCollect
#' @param AllocatorCollect
#' @param pre_date
#' @export
compare_zero_spend <- function(InputCollect,
                               AllocatorCollect,
                               pre_date) {
  dt_optimOut <- AllocatorCollect$dt_optimOut
  rollingWindowLength <- InputCollect$rollingWindowLength
  hist_spend <- dt_optimOut$histSpendTotal[1] / rollingWindowLength
  exp_spend <- dt_optimOut$expSpendUnitTotal[1]
  hist_response <- sum(dt_optimOut$initResponseUnit * dt_optimOut$histSpend / dt_optimOut$initSpendUnit) / rollingWindowLength
  exp_response <- dt_optimOut$optmResponseUnitTotal[1]

  return(list(
    "init_spend" = hist_spend,
    "exp_spend" = exp_spend,
    "init_response" = hist_response,
    "exp_response" = exp_response
  ))
}

####################################################################
#' ***Suggestion*** get_allocator_benchmarks_zero_spend
#'
#' @param InputCollect
#' @param AllocatorCollect_opt
#' @param AllocatorCollect_hist
#' @param AllocatorCollect_recent
#' @export
get_allocator_benchmarks_zero_spend <- function(InputCollect,
                                                AllocatorCollect_opt,
                                                AllocatorCollect_hist,
                                                AllocatorCollect_recent) {
  temp <- data.frame(
    scenario = character(),
    budget_init = numeric(),
    budget_exp = numeric(),
    budget_increase = numeric(),
    response_init = numeric(),
    response_exp = numeric(),
    response_increase = numeric()
  )
  temp_list <- list(
    AllocatorCollect_opt,
    AllocatorCollect_hist,
    AllocatorCollect_recent
  )
  temp_name <- c("optimal", "history", "recent")
  for (i in 1:3) {
    temp_result <- compare_zero_spend(InputCollect, temp_list[[i]])
    budget_ini <- round(temp_result$init_spend, 0)
    budget_exp <- round(temp_result$exp_spend, 0)
    budget_inc <- round(100 * (budget_exp / budget_ini - 1), 2)
    resp_ini <- round(temp_result$init_response, 0)
    resp_exp <- round(temp_result$exp_response, 0)
    resp_inc <- round(100 * (resp_exp / resp_ini - 1), 2)
    temp[i, ] <- c(
      temp_name[i],
      budget_ini,
      budget_exp,
      budget_inc,
      resp_ini,
      resp_exp,
      resp_inc
    )
  }

  return(temp)
}

####################################################################
#' ***Suggestion*** get_individual_result_zero_spend
#'
#' TODO:: Write description
#' @param InputCollect
#' @param OutputCollect
#' @param media_metric
#' @param select_model
#' @param type = "mean"
#' @param pre_period = Null
#' @return
#' @export
get_individual_result_zero_spend <- function(InputCollect,
                                             OutputCollect,
                                             media_metric,
                                             select_model,
                                             type = "mean",
                                             pre_period = NULL) {
  dt_hyppar <- OutputCollect$resultHypParam
  dt_coef <- OutputCollect$xDecompAgg
  date_var <- InputCollect$date_var
  media_vec <- InputCollect$dt_input[[media_metric]]
  media_range <- InputCollect$dt_modRollWind[[media_metric]]
  adstock <- InputCollect$adstock
  if (adstock == "geometric") {
    theta <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_thetas")]]
    x_list <- adstock_geometric(x = media_vec, theta = theta, type = "CDF")
  } else {
    shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_scales")]]
    if (str_detect(tolower(adstock), "cdf")) {
      x_list <- adstock_weibull(x = media_vec, shape = shape, scale = scale, type = "CDF")
    } else if (str_detect(tolower(adstock), "pdf")) {
      x_list <- adstock_weibull(x = media_vec, shape = shape, scale = scale, type = "PDF")
    }
  }
  m_adstocked <- x_list$x_decayed
  # saturation
  m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:InputCollect$rollingWindowEndWhich]
  alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_alphas")]]
  gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_gammas")]]
  saturation <- saturation_hill_revised(m_adstockedRW,
    alpha = alpha,
    gamma = gamma
  )
  mean_cost <- mean(media_range[media_range > 0])
  saturation_response <- saturation_hill_revised(m_adstockedRW,
    alpha = alpha,
    gamma = gamma,
    index_end = NULL,
    x_marginal = mean_cost
  )
  saturation_dependent <- saturation_hill_revised(m_adstockedRW,
    alpha = alpha,
    gamma = gamma
  )
  # Decomp
  coeff <- dt_coef[dt_coef$solID == select_model &
    dt_coef$rn == media_metric, ][["coef"]]
  response_var <- as.numeric(saturation_response * coeff)
  response_var_modify <- response_var * length(media_range[media_range > 0]) / length(media_range)
  dependent_var <- as.numeric(saturation_dependent * coeff)

  return(list(
    "response" = response_var_modify,
    "dependent" = sum(dependent_var, na.rm = TRUE) / length(dependent_var)
  ))
}

media_metric <- "tv_S"

####################################################################
#' ***Suggestion*** predict_individual_result_zero_spend
#'
#' TODO:: Write description
#' form of the post_data is equal to the form of the InputCollect$dt_input
#' just it has different date range
#' @param InputCollect
#' @param OutputCollect
#' @param post_data
#' @param post_period
#' @param media_metric
#' @param select_model
#' @param type = "mean"
#' @return
#' @export
predict_individual_result_zero_spend <- function(InputCollect,
                                                 OutputCollect,
                                                 post_data,
                                                 post_period,
                                                 media_metric,
                                                 select_model,
                                                 type = "mean") {
  adstock <- InputCollect$adstock
  dt_hyppar <- OutputCollect$resultHypParam
  dt_coef <- OutputCollect$xDecompAgg
  date_var <- InputCollect$date_var
  media_vec <- post_data %>% filter(DATE %in% seq(min(InputCollect$dt_input$DATE), post_period[2], by = "day"))
  media_vec <- media_vec[[media_metric]]
  media_range <- post_data %>%
    filter(DATE %in% seq(post_period[1], post_period[2], by = "day")) %>%
    select(media_metric)
  index_end <- length(InputCollect$dt_input[[media_metric]])
  if (adstock == "geometric") {
    theta <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_thetas")]]
    x_list <- adstock_geometric(x = media_vec, theta = theta, type = "CDF")
  } else {
    shape <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_shapes")]]
    scale <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_scales")]]
    if (str_detect(tolower(adstock), "cdf")) {
      x_list <- adstock_weibull_revised(x = media_vec, shape = shape, scale = scale, type = "CDF", index_end = index_end)
    } else if (str_detect(tolower(adstock), "pdf")) {
      x_list <- adstock_weibull_revised(x = media_vec, shape = shape, scale = scale, type = "PDF", index_end = index_end)
    }
  }
  m_adstocked <- x_list$x_decayed
  # saturation
  m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:length(media_vec)]
  alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_alphas")]]
  gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_gammas")]]
  index_end <- InputCollect$rollingWindowEndWhich - InputCollect$rollingWindowStartWhich + 1
  saturation <- saturation_hill_revised(m_adstockedRW,
    alpha = alpha,
    gamma = gamma,
    index_end
  )
  mean_cost <- mean(media_range[media_range > 0])
  saturation_response <- saturation_hill_revised(m_adstockedRW,
    alpha = alpha,
    gamma = gamma, ,
    x_marginal = mean_cost
  )
  saturation_dependent <- saturation_hill_revised(m_adstockedRW,
    alpha = alpha,
    gamma = gamma
  )
  # Decomp
  coeff <- dt_coef[dt_coef$solID == select_model &
    dt_coef$rn == media_metric, ][["coef"]]
  response_var <- as.numeric(saturation_response * coeff)
  response_var_modify <- response_var * length(media_range[media_range > 0]) / length(media_range[[media_metric]])
  dependent_var <- as.numeric(saturation_dependent * coeff)

  return(list(
    "response" = response_var_modify,
    "dependent" = sum(dependent_var, na.rm = TRUE) / length(dependent_var)
  ))
}

####################################################################
#' ***Suggestion*** get_response_sum_on_train_zero_spend
#'
#' TODO:: Write description
#' @param InputCollect
#' @param OutputCollect
#' @param select_model
#' @return
#' @export
get_response_sum_on_train_zero_spend <- function(InputCollect,
                                                 OutputCollect,
                                                 select_model) {
  paid_media_spends <- InputCollect$paid_media_spends
  response <- 0
  dependent <- 0
  for (i in paid_media_spends) {
    temp <- get_individual_result_zero_spend(InputCollect, OutputCollect, i, select_model, type = "mean")
    response <- response + temp$response
    dependent <- dependent + temp$dependent
  }

  return(list("response" = response, "dependent" = dependent))
}

####################################################################
#' ***Suggestion*** predict_response_sum_on_test_zero_spend
#'
#' TODO:: Write description
#' @param InputCollect
#' @param OutputCollect
#' @param post_data
#' @param post_period
#' @param select_model
#' @return
#' @export
predict_response_sum_on_test_zero_spend <- function(InputCollect,
                                                    OutputCollect,
                                                    post_data,
                                                    post_period,
                                                    select_model) {
  paid_media_spends <- InputCollect$paid_media_spends
  response <- 0
  dependent <- 0
  for (i in paid_media_spends) {
    temp <- predict_individual_result_zero_spend(InputCollect, OutputCollect, post_data, post_period, i, select_model, type = "mean")
    response <- response + temp$response
    dependent <- dependent + temp$dependent
  }

  return(list("response" = response, "dependent" = dependent))
}

####################################################################
#' ***Suggestion*** validate_predicts_zero_spend
#'
#' TODO:: Write description
#' compare history and prediction
#' need to modify the output form
#' @param InputCollect
#' @param OutputCollect
#' @param post_data
#' @param post_period
#' @param select_model
#' @export
validate_predicts_zero_spend <- function(InputCollect,
                                         OutputCollect,
                                         post_data,
                                         post_period,
                                         select_model) {
  history <- get_response_sum_on_train_zero_spend(InputCollect, OutputCollect, select_model)
  predict <- predict_response_sum_on_test_zero_spend(InputCollect, OutputCollect, post_data, post_period, select_model)
  decompose <- decompose_dependent_vars(InputCollect)
  predict_media_response <- decompose$non_media + decompose$media * predict$response / history$response
  predict_media_dependent <- decompose$non_media + predict$dependent
  realized_value <- post_data %>%
    filter(DATE %in% seq(post_period[1], post_period[2], by = "day")) %>%
    select(revenue)
  total_dependent <- sum(realized_value) / length(realized_value$revenue)

  temp <- data.frame(
    scenario = character(),
    predicted_value = numeric(),
    realized_value = numeric(),
    error = numeric()
  )
  temp[1, ] <- c("response", round(predict_media_response, 0), round(total_dependent, 0), round(100 * (predict_media_response / total_dependent - 1), 2))
  temp[2, ] <- c("dependent", round(predict_media_dependent, 0), round(total_dependent, 0), round(100 * (predict_media_dependent / total_dependent - 1), 2))

  return(temp)
}
