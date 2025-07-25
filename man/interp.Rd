% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_model.R
\name{interp}
\alias{interp}
\title{Model covariate declaration}
\usage{
interp(type = "lm")
}
\arguments{
\item{type}{If \code{type = "lm"} (the default) or \code{type = "linear"},
the covariate value will be
linearly interpolated between values when fitting the model to the data.
in a model list \code{cov} item. To fix covariate values to the value at the
last time point, set \code{type = "none"}.}
}
\value{
A value of 1 for "lm" and 0 for "none", which will be passed to Rust.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Declare whether covariates in the data are to have
interpolation between values or not.
}
\examples{
\dontrun{
cov = c(
  wt = interp() # same as interp("lm") or interp("linear")
  visit = interp("none")
)
}
}
