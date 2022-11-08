# 1. put_hyppar: Put Hyperparam
#    -> demo.R
# 2. budget_boundary: Get Boundary
#    -> suggest/budget.R
# 3. Allocator_results: Get analytical result of allocator
#    -> suggest/allocator.r
# 4. decomp_dependent: Decompose KPI of media, non-media, etc
#    -> suggest/media_analyzer.r
# 5. saturation_hill_new, adstock_weibull_new:
#    -> suggest/transformation.r
# 6. result_media, result_media_post: get the results of each media
#    -> suggest/media_analyzer.r
# 7. result_total, result_total_post: get the result of whole media
#    -> suggest/media_analyzer.r
# 8. validation_test: validate. Main Feature
#    -> suggest/validate.r
##### For non-zero analytics
# 9. compare_nonzero
#    -> suggest/non_zero/comparison.r
# 10. ...

####################################################################
#' [Suggestion-1] Set hyperparameters from Input
#'
#' TODO:: Write description
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param vars_lst TODO::Type. TODO::Description
#' @param hyper_lst TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
put_hyppar <- function(InputCollect = NULL, vars_lst, hyper_lst) {
  vars_temp <- c()
  vars_all <- InputCollect$all_media
  for (i in 1:length(vars_lst)) {
    vars_temp <- append(vars_temp, vars_lst[[i]])
  }

  Check_Vars_Equality <- length(vars_temp) == length(vars_all) && setequal(vars_temp, vars_all)
  if (Check_Vars_Equality) {
    print("OK")
  } else {
    sprintf(
      "[Error] There are missing components.\nvars_temp:%s\nvars_all:%s",
      vars_temp,
      vars_all
    )
  }

  hyperparameters <- list()
  for (i in 1:length(vars_lst)) {
    for (j in vars_lst[[i]]) {
      tmp_hyper <- list()
      for (k in hyper_lst[[i]]) {
        tmp_hyper <- append(tmp_hyper, list(k))
      }
      hyper_name <- paste(j, "_", names(hyper_lst[[i]]), sep = "")
      names(tmp_hyper) <- hyper_name
      hyperparameters <- append(hyperparameters, tmp_hyper)
    }
  }
  return(hyperparameters)
}

####################################################################
#' [Suggestion-2] Boundary
#'
#' TODO:: Write description
#' (originally) robyn channel boundary
#' need to modify the function into the genral form of the Robyn
#' library
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param cost_input_low TODO::Type. TODO::Description
#' @param cost_input_high TODO::Type. TODO::Description
#' @param total_spends TODO::Type. TODO::Description
#' @return List(). Contains list of boundaries.
#' @export
budget_boundary <- function(InputCollect,
                            cost_input_low,
                            cost_input_high,
                            total_spends) {
  dt_mod <- InputCollect$dt_modRollWind
  window_end <- InputCollect$window_end
  paid_media_spends <- InputCollect$paid_media_spends
  intervalType <- InputCollect$intervalType

  boundary <- list()
  initial_nonzero_cost <- dt_mod %>%
    select(c("ds", paid_media_spends)) %>%
    summarise_all(~ mean(.[. != 0], na.rm = TRUE)) %>%
    select(paid_media_spends)
  recent_nonzero_cost <- dt_mod %>%
    select(c("ds", paid_media_spends)) %>%
    filter(ds %in% seq(window_end %m-% months(1), window_end, by = "days")) %>%
    summarise_all(~ mean(.[. != 0], na.rm = TRUE)) %>%
    select(InputCollect$paid_media_spends)

  boundary$cost$low <- cost_input_low / initial_nonzero_cost
  boundary$cost$high <- cost_input_high / initial_nonzero_cost
  boundary$hist$low <- rep(total_spends / sum(initial_nonzero_cost), length(initial_nonzero_cost))
  boundary$hist$high <- rep(total_spends / sum(initial_nonzero_cost), length(initial_nonzero_cost))
  boundary$recent$low <- total_spends * recent_nonzero_cost / sum(recent_nonzero_cost, na.rm = TRUE) / initial_nonzero_cost
  boundary$recent$high <- total_spends * recent_nonzero_cost / sum(recent_nonzero_cost, na.rm = TRUE) / initial_nonzero_cost

  boundary
}


####################################################################
#' [Suggestion-3] Allocator Results
#'
#' TODO:: Write description
#' Generate analytical result of allocato
#'
#' @param AllocatorCollect_opt TODO::Type. TODO::Description
#' @param AllocatorCollect_hist TODO::Type. TODO::Description
#' @param AllocatorCollect_recent TODO::Type. TODO::Description
#' @return List(). Contains list of hyperparameters.
#' @export
Allocator_results <- function(AllocatorCollect_opt,
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
    budget_ini <- round(temp_list[[i]]$dt_optimOut$initSpendUnitTotal[1], 0)
    budget_exp <- round(temp_list[[i]]$dt_optimOut$expSpendUnitTotal[1], 0)
    budget_inc <- round(100 * temp_list[[i]]$dt_optimOut$optmSpendUnitTotalDelta[1], 2)
    resp_ini <- round(temp_list[[i]]$dt_optimOut$initResponseUnitTotal[1], 0)
    resp_exp <- round(temp_list[[i]]$dt_optimOut$optmResponseUnitTotal[1], 0)
    resp_inc <- round(100 * temp_list[[i]]$dt_optimOut$optmResponseUnitTotalLift[1], 2)
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
  temp
}

####################################################################
#' [Suggestion-4] Decompose KPI of media, non-media, etc
#'
#' TODO:: Write description
#' check accuracy with dependent
#' @param InputCollect TODO::Type. TODO::Description
#' @param pre_period TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
decomp_dependent <- function(InputCollect,
                             pre_period = NULL) {
  paid_media_spends <- InputCollect$paid_media_spends
  if (length(pre_period) == 0) {
    data_cut <- InputCollect$dt_modRollWind
  } else {
    data_cut <- InputCollect$dt_modRollWind %>%
      filter(ds %in% seq(pre_period[1], pre_period[2], by = "day"))
  }
  total_media <- sum(data_cut %>%
    select(all_of(paid_media_spends))) / nrow(data_cut)
  total_whole <- sum(data_cut %>%
    select(dep_var)) / nrow(data_cut)
  total_non_media <- total_whole - total_media
  media_share <- total_media / total_whole
  KPI <- list(
    "media" = total_media,
    "non_media" = total_non_media,
    "whole" = total_whole,
    "media_share" = media_share
  )
  KPI
}


####################################################################
#' [Suggestion-5a] Saturation Hill New
#'
#' TODO:: Write description
#'
#' @param x TODO::Type. TODO::Description
#' @param alpha TODO::Type. TODO::Description
#' @param gamma TODO::Type. TODO::Description
#' @param index_end = Null TODO::Type. TODO::Description
#' @param x_marginal = Null TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
saturation_hill_new <- function(x,
                                alpha,
                                gamma,
                                index_end = NULL,
                                x_marginal = NULL) {
  if (length(index_end) == 0) {
    x_saturation <- x #
  } else {
    x_saturation <- x[1:index_end] #
  }

  inflextion <- c(range(x_saturation) %*% c(1 - gamma, gamma)) # Scaling parts
  if (is.null(x_marginal)) {
    x_scurve <- x**alpha / (x**alpha + inflextion**alpha) #
  } else {
    x_scurve <- x_marginal**alpha / (x_marginal**alpha + inflextion**alpha) #
  }

  x_scurve
}

####################################################################
#' [Suggestion-5b] Adstock Weibull New
#'
#' TODO:: Write description
#'
#' @param x TODO::Type. TODO::Description
#' @param shape TODO::Type. TODO::Description
#' @param scale TODO::Type. TODO::Description
#' @param windlen = length(x) TODO::Type. TODO::Description
#' @param type = "cdf" TODO::Type. TODO::Description
#' @param index_end = Null
#' @return TODO::Type. TODO::Description
#' @export
adstock_weibull_new <- function(x,
                                shape,
                                scale,
                                windlen = length(x),
                                type = "cdf",
                                index_end = NULL) {
  x_bin <- 1:windlen
  if (length(index_end) == 0) {
    scaleTrans <- round(quantile(1:windlen, scale), 0) #
  } else {
    scaleTrans <- round(quantile(1:index_end, scale), 0) #
  }
  if (shape == 0) {
    thetaVecCum <- thetaVec <- rep(0, windlen)
  } else {
    if ("cdf" %in% tolower(type)) {
      thetaVec <- c(1, 1 - pweibull(head(x_bin, -1), shape = shape, scale = scaleTrans)) # plot(thetaVec)
      thetaVecCum <- cumprod(thetaVec) # plot(thetaVecCum)
    } else if ("pdf" %in% tolower(type)) {
      thetaVecCum <- .normalize(dweibull(x_bin, shape = shape, scale = scaleTrans)) # plot(thetaVecCum)
    }
  }
  x_decayed <- mapply(function(x_val, x_pos) {
    x.vec <- c(rep(0, x_pos - 1), rep(x_val, windlen - x_pos + 1))
    thetaVecCumLag <- lag(thetaVecCum, x_pos - 1, default = 0)
    x.prod <- x.vec * thetaVecCumLag
    return(x.prod)
  }, x_val = x, x_pos = x_bin[1:length(x)])
  x_decayed <- rowSums(x_decayed)[1:length(x)]

  list(x = x, x_decayed = x_decayed, thetaVecCum = thetaVecCum)
}
# end

####################################################################
#' [Suggestion-6] Get Performance of Each Media
#'
#' TODO:: Write description
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param media_metric TODO::Type. TODO::Description
#' @param select_model TODO::Type. TODO::Description
#' @param type = "mean" TODO::Type. TODO::Description
#' @param pre_period = Null
#' @return TODO::Type. TODO::Description
#' @export
result_media <- function(InputCollect,
                         OutputCollect,
                         media_metric,
                         select_model,
                         type = "mean",
                         pre_period = NULL) { # pre_period:
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
  saturation <- saturation_hill_new(m_adstockedRW,
    alpha = alpha,
    gamma = gamma
  )
  mean_cost <- mean(media_range[media_range > 0])
  mean_cost
  saturation_response <- saturation_hill_new(m_adstockedRW,
    alpha = alpha,
    gamma = gamma,
    index_end = NULL,
    x_marginal = mean_cost
  )
  saturation_dependent <- saturation_hill_new(m_adstockedRW,
    alpha = alpha,
    gamma = gamma
  )
  # Decomp
  coeff <- dt_coef[dt_coef$solID == select_model &
    dt_coef$rn == media_metric, ][["coef"]]
  response_var <- as.numeric(saturation_response * coeff)
  dependent_var <- as.numeric(saturation_dependent * coeff)
  return(list(
    "response" = response_var,
    "dependent" = sum(dependent_var, na.rm = TRUE) / length(dependent_var)
  ))
}

####################################################################
#' [Suggestion-8]
#'
#' TODO:: Write description
#' [Sub function for upper suggestion: make result based on post period]
#' form of the post_data is equal to the form of the InputCollect$dt_input
#' ust it has different date range
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param post_data TODO::Type. TODO::Description
#' @param post_period TODO::Type. TODO::Description
#' @param media_metric
#' @param type = "mean" TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
result_media_post <- function(InputCollect,
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
      x_list <- adstock_weibull_new(x = media_vec, shape = shape, scale = scale, type = "CDF", index_end = index_end)
    } else if (str_detect(tolower(adstock), "pdf")) {
      x_list <- adstock_weibull_new(x = media_vec, shape = shape, scale = scale, type = "PDF", index_end = index_end)
    }
  }

  m_adstocked <- x_list$x_decayed
  # saturation
  m_adstockedRW <- m_adstocked[InputCollect$rollingWindowStartWhich:length(media_vec)]
  alpha <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_alphas")]]
  gamma <- dt_hyppar[dt_hyppar$solID == select_model, ][[paste0(media_metric, "_gammas")]]
  index_end <- InputCollect$rollingWindowEndWhich - InputCollect$rollingWindowStartWhich + 1
  saturation <- saturation_hill_new(m_adstockedRW,
    alpha = alpha,
    gamma = gamma,
    index_end
  )
  mean_cost <- mean(media_range[media_range > 0])
  saturation_response <- saturation_hill_new(m_adstockedRW,
    alpha = alpha,
    gamma = gamma, ,
    x_marginal = mean_cost
  )
  saturation_dependent <- saturation_hill_new(m_adstockedRW,
    alpha = alpha,
    gamma = gamma
  )
  # Decomp
  coeff <- dt_coef[dt_coef$solID == select_model &
    dt_coef$rn == media_metric, ][["coef"]]
  response_var <- as.numeric(saturation_response * coeff)
  dependent_var <- as.numeric(saturation_dependent * coeff)
  return (list(
    "response" = response_var,
    "dependent" = sum(dependent_var, na.rm = TRUE) / length(dependent_var)
  ))
}

####################################################################
#' [Suggestion-9a] Get Total Performance from whole media
#'
#' TODO:: Write description
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param select_model TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
result_total <- function(InputCollect,
                         OutputCollect,
                         select_model) {
  paid_media_spends <- InputCollect$paid_media_spends
  response <- 0
  dependent <- 0

  for (i in paid_media_spends) {
    temp <- result_media(InputCollect, OutputCollect, i, select_model, type = "mean")
    response <- response + temp$response
    dependent <- dependent + temp$dependent
  }

  list("response" = response, "dependent" = dependent)
}

####################################################################
#' [Suggestion-9b] Get Total Performance from whole media
#'
#' TODO:: Write description
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param post_data TODO::Type. TODO::Description
#' @param post_period TODO::Type. TODO::Description
#' @param select_model TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
result_total_post <- function(InputCollect,
                              OutputCollect,
                              post_data,
                              post_period,
                              select_model) {
  paid_media_spends <- InputCollect$paid_media_spends
  response <- 0
  dependent <- 0

  for (i in paid_media_spends) {
    temp <- result_media_post(InputCollect, OutputCollect, post_data, post_period, i, select_model, type = "mean")
    response <- response + temp$response
    dependent <- dependent + temp$dependent
  }

  list("response" = response, "dependent" = dependent)
}

####################################################################
#' [Suggestion-10] Validate the results by history and prediction
#'
#' TODO:: Write description
#' compare history and prediction
#' need to modify the output form
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param post_data TODO::Type. TODO::Description
#' @param post_period TODO::Type. TODO::Description
#' @param select_model TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
validation_test <- function(InputCollect,
                            OutputCollect,
                            post_data,
                            post_period,
                            select_model) {
  history <- result_total(InputCollect, OutputCollect, select_model)
  predict <- result_total_post(InputCollect, OutputCollect, post_data, post_period, select_model)
  decompose <- decomp_dependent(InputCollect)
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

  temp
}
