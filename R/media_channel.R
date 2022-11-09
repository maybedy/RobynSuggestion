####################################################################
#' ***Suggestion*** generate_generate_budget_boundaries
#'
#' To help marketers set their lower and upper budget
#' boundaries in a more intuitive way.
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param cost_input_low TODO::Type. TODO::Description
#' @param cost_input_high TODO::Type. TODO::Description
#' @param total_spends TODO::Type. TODO::Description
#' @return List(). Contains list of boundaries.
#' @export
generate_budget_boundaries <- function(InputCollect,
                                       cost_input_low,
                                       cost_input_high,
                                       total_spends) {
  dt_mod <- InputCollect$dt_modRollWind
  window_end <- InputCollect$window_end
  paid_media_spends <- InputCollect$paid_media_spends
  intervalType <- InputCollect$intervalType
  # ds <- dt_mod$ds


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

  return(boundary)
}

####################################################################
#' ***Suggestion*** decompose_dependent_vars
#'
#' Decomposing the dependent variable into paid media channels and the rest
#'
#' Returns the average weekly (or daily) total dependent variables
#' on paid media channels and the rest subtracted from
#' the average weekly value of the dependent variable.
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param pre_period TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
decompose_dependent_vars <- function(InputCollect,
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

  return(list(
    "media" = total_media,
    "non_media" = total_non_media,
    "whole" = total_whole,
    "media_share" = media_share
  ))
}

####################################################################
#' ***Suggestion*** get_response_sum_on_trainining
#'
#' Returns the sum of the response made by each paid media channel
#' during the training period.
#'
#' The ratio of the response from the predict_response_sum_on_test
#' function to that from the get_response_sum_on_trainining function
#' indicates the predicted response change from the training period
#' to the test period.
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param select_model TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
get_response_sum_on_trainining <- function(InputCollect,
                                           OutputCollect,
                                           select_model) {
  paid_media_spends <- InputCollect$paid_media_spends
  response <- 0
  dependent <- 0

  for (i in paid_media_spends) {
    temp <- get_individual_result(InputCollect, OutputCollect, i, select_model, type = "mean")
    response <- response + temp$response
    dependent <- dependent + temp$dependent
  }

  return(list("response" = response, "dependent" = dependent))
}

####################################################################
#' ***Suggestion*** predict_response_sum_on_test
#'
#' Predicts the sum of the response made by each media
#' during the test period
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param post_data TODO::Type. TODO::Description
#' @param post_period TODO::Type. TODO::Description
#' @param select_model TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
predict_response_sum_on_test <- function(InputCollect,
                                         OutputCollect,
                                         post_data,
                                         post_period,
                                         select_model) {
  paid_media_spends <- InputCollect$paid_media_spends
  response <- 0
  dependent <- 0

  for (i in paid_media_spends) {
    temp <- predict_individual_result(InputCollect, OutputCollect, post_data, post_period, i, select_model, type = "mean")
    response <- response + temp$response
    dependent <- dependent + temp$dependent
  }

  return(list("response" = response, "dependent" = dependent))
}


####################################################################
#' ***Suggestion*** get_individual_result
#'
#' Returns the response and dependent variables made by
#' each paid media channel during the training period.
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param media_metric TODO::Type. TODO::Description
#' @param select_model TODO::Type. TODO::Description
#' @param type = "mean" TODO::Type. TODO::Description
#' @param pre_period = Null
#' @return TODO::Type. TODO::Description
#' @export
get_individual_result <- function(InputCollect,
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
  saturation <- saturation_hill_revised(m_adstockedRW,
    alpha = alpha,
    gamma = gamma
  )
  mean_cost <- mean(media_range[media_range > 0])
  mean_cost
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
  dependent_var <- as.numeric(saturation_dependent * coeff)

  return(list(
    "response" = response_var,
    "dependent" = sum(dependent_var, na.rm = TRUE) / length(dependent_var)
  ))
}

####################################################################
#' ***Suggestion*** predict_individual_result
#'
#' Predicts the response and dependent variables made by each media
#' during the test period
#'
#' @param InputCollect TODO::Type. TODO::Description
#' @param OutputCollect TODO::Type. TODO::Description
#' @param post_data TODO::Type. TODO::Description
#' @param post_period TODO::Type. TODO::Description
#' @param media_metric
#' @param type = "mean" TODO::Type. TODO::Description
#' @return TODO::Type. TODO::Description
#' @export
predict_individual_result <- function(InputCollect,
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
  dependent_var <- as.numeric(saturation_dependent * coeff)

  return(list(
    "response" = response_var,
    "dependent" = sum(dependent_var, na.rm = TRUE) / length(dependent_var)
  ))
}
