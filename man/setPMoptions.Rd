% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PMoptions.R
\name{setPMoptions}
\alias{setPMoptions}
\title{Set Pmetrics User Options}
\usage{
setPMoptions(sep, dec, rust_template, report_template, quiet = FALSE)
}
\arguments{
\item{sep}{The field separator character; "," by default, but could be ";" or another separator.}

\item{dec}{The decimal separator character; "." by default, but could be "," for example.}

\item{rust_template}{Only used if \code{backend} is set to "rust".}

\item{report_template}{Format of the plots included in the summary report presented at the end of a run.
Default is to use "plotly", but can be set to "ggplot".}

\item{quiet}{Suppress warning messages. Default \code{FALSE}.}
}
\value{
The user preferences file will be updated.  This will persist from session to session
and if stored in the external location, through Pmetrics versions.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Set user options for Pmetrics
}
\details{
When the Pmetrics package is first loaded with \code{library(Pmetrics)},
this function will be called. It will obtain
the user's locale from system information and set the appropriate
language. It will look for a \emph{PMoptions.json} file in a hidden folder outside
of the Pmetrics package. If that does not exist, it will offer to create this for you.
With this choice, your options will persist when Pmetrics is updated. If you choose
to store the options within the package architecture, your options will be erased
every time you update Pmetrics. After you make your choice, you will not be presented
with that choice again. However, if you wish to later move your options from the internal
location to the external one, use \link{movePMoptions}.

The function will obtain Pmetrics user options from the \emph{PMoptions.json}
file by calling \link{getPMoptions}, set them for the session and update any
missing options with default values.
}
\author{
Michael Neely
}
