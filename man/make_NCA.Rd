% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_NCA.R
\name{make_NCA}
\alias{make_NCA}
\title{Non-compartmental analysis}
\usage{
make_NCA(
  x,
  postPred = F,
  include,
  exclude,
  input = 1,
  icen = "median",
  outeq = 1,
  block = 1,
  start = 0,
  end = Inf,
  first = NA,
  last = NA,
  terminal = 3
)
}
\arguments{
\item{x}{PM_data object to analyze.}

\item{postPred}{Boolean switch to use the posterior predictions rather than the observed.
concentrations.  Default is \code{FALSE}. Ignored if an IT2B run is
used to supply the raw data file.}

\item{include}{A vector of subject IDs to include in the NCA, e.g. \code{c(1:3,5,15)}}

\item{exclude}{A vector of subject IDs to exclude in the NCA, e.g. \code{c(4,6:14,16:20)}.
When \code{postPred} is \code{TRUE}, any subject(s) excluded from the IT2B/NPAG run will be excluded as well.}

\item{input}{The number of the input (e.g. drug) to analyze; default 1.}

\item{icen}{If \code{postPred} is \code{TRUE}, use predictions based on median or mean of each
subject's Bayesian posterior parameter distribution.  Default is "median", but could be "mean".}

\item{outeq}{The number of the output equation to analyze; default 1}

\item{block}{The number of the observation block within subjects,
with each block delimited by EVID=4 in the data file; default 1}

\item{start}{The beginning of the time interval to look for doses and observations,
e.g. 120.  It can be a vector to allow for individual start times per subject,
e.g. \code{c(120,120,144,168)}.  If the length of \code{start}
is less than the number of subjects, the last value will be recycled as needed.
If the \code{start} time is not 0 (default),
then it is assumed that steady state (multiple dose) conditions apply.}

\item{end}{Analogous to \code{start}, set this equal to the end of the dosing interval.
It too can be a vector, with the last value
recycled as necessary.  Default is \code{Inf}, i.e. all data used.}

\item{first}{Alternative way to specify time interval for NCA by choosing dose number,
e.g. 1 or 3.  May be a numeric vector, like \code{start} and \code{end},
e.g. \code{c(1,1,1,3,1,...)} to allow for individualization by subject.
The last value will be recycled to ensure length equal to the
number of subjects.  Default is \code{NA}, which means \code{start} will be used.}

\item{last}{The complement to \code{first}, specifying the last dose to end the time interval.
If \code{NA}, which is the default, then the maximum time per subject will be the
upper bound of the time interval.Like \code{first}, \code{last} can be a vector,
with the last value recycled as necessary.  Use \code{NA} in the vector
to signify maximum time for that subject.}

\item{terminal}{Number of observations to use for terminal curve fitting (i.e. to estimate \emph{k}).
Default is 3.}
}
\value{
A dataframe of class \emph{PMnca} with columns
\item{id }{Subject identification}
\item{auc }{Area under the time-observation curve, using the trapezoidal approximation, from time 0 until the second dose,
or if only one dose, until the last observation}
\item{aumc }{Area under the first moment curve}
\item{k }{Slope by least-squares linear regression of the final 3 log-transformed observations vs. time.
If the final 3 concentrations are not decreasing such that linear regression results in a positive slope,
this value and all others that depend on \emph{k} will be suppressed.}
\item{auclast }{Area under the curve from the time of the last observation to infinity, calculated as \verb{Final obs/k}.
This value will be suppressed if \code{start != 0}.}
\item{aumclast }{Area under the first moment curve from the time of the last observation to infinity.
This value will be suppressed if \code{start != 0}.}
\item{aucinf }{Area under the curve from time 0 to infinity, caluculated as auc + auclast}
\item{aumcinf }{Area under the first moment curve from time 0 to infinity}
\item{mrt }{Mean residence time, calculated as 1/k}
\item{cmax }{Maximum predicted concentration after the first dose}
\item{tmax }{Time to cmax}
\item{cl }{Clearance, calculated as dose/aucinf}
\item{vdss }{Volume of distribution at steady state, calculated as cl*mrt}
\item{thalf }{Half life of elimination, calculated as ln(2)/k}
\item{dose }{Dose for each subject}
}
\description{
Performs a non-compartmental analysis from a \link{PM_result} object using
observed concentrations in the raw data
file \code{PM_result$data$standard_data} or from an individual
Bayesian posterior predicted
time-observation profiles \code{PM_result$post$data} generated automatically after an NPAG run
and loaded with \link{PM_load} .
}
\details{
If concentrations from multiple dose intervals are included in the \code{start} to \code{end} time interval,
\link{make_NCA} will superpose the concentrations using the time after dose.  An error will be generated if
different doses are within this interval as superposition would no longer be valid.

A minimum of 5 concentrations must be available to perform NCA for any given subject.  Fewer than this will
suppress all results for that subject.
}
\author{
Michael Neely
}
