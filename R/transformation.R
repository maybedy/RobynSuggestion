# Util
.normalize <- function(x) {
  if (diff(range(x)) == 0) {
    return(c(1, rep(0, length(x) - 1)))
  } else {
    return((x - min(x)) / (max(x) - min(x)))
  }
}


####################################################################
#' ***Suggestion*** saturation_hill_revised
#'
#' Newly defined the saturation_hill_revised functions,
#' which receive an additional argument index_end that
#' makes scales have a fixed value generated from
#' the training period.
#'
#' @family Transformations
#' @param x Numeric vector.
#' @param alpha Numeric. Alpha controls the shape of the saturation curve.
#' The larger the alpha, the more S-shape. The smaller, the more C-shape.
#' @param gamma Numeric. Gamma controls the inflexion point of the
#' saturation curve. The larger the gamma, the later the inflexion point occurs.
#' @param index_end Numeric. Makes scales have a fixed value generated from
#' the training period
#' @param x_marginal Numeric. When provided, the function returns the
#' Hill-transformed value of the x_marginal input.
#' @examples
#' saturation_hill(c(100, 150, 170, 190, 200), alpha = 3, gamma = 0.5)
#' @return Numeric values. Transformed values.
#' @export
saturation_hill_revised <- function(x,
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

  return(x_scurve)
}


####################################################################
#' ***Suggestion*** adstock_weibull_revised
#'
#' Newly defined the adstock_weibull_revised, which receive
#' an additional argument index_end that makes scales have
#' a fixed value generated from the training period.
#'
#' @param x Numeric.
#' @param shape Numeric.
#' @param scale Numeric. Check "Details" section for more details.
#' @param windlen Integer. Length of modelling window. By default, same length as \code{x}.
#' @param type Character. Accepts "CDF" or "PDF". CDF, or cumulative density
#' function of the Weibull function allows changing decay rate over time in both
#' C and S shape, while the peak value will always stay at the first period,
#' meaning no lagged effect. PDF, or the probability density function, enables
#' peak value occurring after the first period when shape >=1, allowing lagged
#' effect.
#' @param index_end Numeric. Makes scales have a fixed value generated from
#' the training period
#' @return
#' @export
adstock_weibull_revised <- function(x,
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

  return(list(x = x, x_decayed = x_decayed, thetaVecCum = thetaVecCum))
}