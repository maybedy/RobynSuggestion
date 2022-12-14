% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transformation.R
\name{saturation_hill_revised}
\alias{saturation_hill_revised}
\title{\emph{\strong{Suggestion}} saturation_hill_revised}
\usage{
saturation_hill_revised(x, alpha, gamma, index_end = NULL, x_marginal = NULL)
}
\arguments{
\item{x}{Numeric vector.}

\item{alpha}{Numeric. Alpha controls the shape of the saturation curve.
The larger the alpha, the more S-shape. The smaller, the more C-shape.}

\item{gamma}{Numeric. Gamma controls the inflexion point of the
saturation curve. The larger the gamma, the later the inflexion point occurs.}

\item{index_end}{Numeric. Makes scales have a fixed value generated from
the training period}

\item{x_marginal}{Numeric. When provided, the function returns the
Hill-transformed value of the x_marginal input.}
}
\value{
Numeric values. Transformed values.
}
\description{
Newly defined the saturation_hill_revised functions,
which receive an additional argument index_end that
makes scales have a fixed value generated from
the training period.
}
\examples{
saturation_hill(c(100, 150, 170, 190, 200), alpha = 3, gamma = 0.5)
}
\concept{Transformations}
