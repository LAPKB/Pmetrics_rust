% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_op.R
\name{summary.PM_op}
\alias{summary.PM_op}
\title{Summarize Observations and Predictions}
\usage{
\method{summary}{PM_op}(
  object,
  digits = max(3, getOption("digits") - 3),
  pred.type = "post",
  icen = "median",
  outeq = 1,
  ...
)
}
\arguments{
\item{object}{A \link{PM_op} object}

\item{digits}{Integer, used for number of digits to print.}

\item{pred.type}{Either 'post' for a posterior object or 'pop' for a population object.  Default is 'post'.}

\item{icen}{Can be either "median" for the predictions based on medians of \code{pred.type} parameter value
distributions, or "mean".  Default is "median".}

\item{outeq}{Output equation number.  Default is 1.}

\item{...}{Not used.}
}
\value{
A list with three elements of class \emph{summary.PM_op}.
\itemize{
\item sumstat A data frame with the minimum, first quartile, median, third quartile, maximum,
mean and standard deviation for times, observations and predictions in \code{x}.
\item pe A named vector with mean prediction error (mpe),
the mean weighted prediction error (mwpe), the mean squared prediction error (mspe), root mean sqaured error (rmse),
percent root mean squared error (percent_rmse), the mean weighted
squared prediction error (mwspe), the bias-adjusted mean squared prediction error (bamspe), and the bias-
adjusted mean weighted squared prediction error (bamwspe).  The mwpe is bias and the bamwspe is imprecision on
plots of PM_op objects.
\item wtd.t A list of 6 elements based on a t test that the weighted mean prediction bias is different than zero
}
\itemize{
\item estimate: the weighted mean of the prediction bias for each observation
\item se: the standard error of the estimate
\item conf.int: the 95\% confidence interval of the mean
\item statistic: the t statistic of the standardized difference between mean and zero
\item df: degrees of freedom equal to number of observations minus one
\item p.value: the probability that the weighted mean is different than zero
}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Summarize a Pmetrics Observed vs. Predicted object
}
\details{
This is a function usually called by the \verb{$summary()} method for \link{PM_op} objects
within a \link{PM_result} to summarize observations, predictions and errors. The function can
be called directly on a \link{PM_op} object. See examples.
}
\examples{
\dontrun{
NPex$op$summary() # preferred
summary(NPex$op) # alternative
}
}
\seealso{
\link{PM_op}
}
\author{
Michael Neely
}
