% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PMutilities.R
\name{cli_ask}
\alias{cli_ask}
\title{Ask with warning}
\usage{
cli_ask(text, prompt = ">> ", ...)
}
\arguments{
\item{text}{The warning text.}

\item{prompt}{The prompt preceding user input. Default is ">>".}

\item{...}{Additional parameters which could be passed to \link[cli:cli_text]{cli::cli_text}.}
}
\value{
The value of the user response
}
\description{
Get user input in warning situation
}
\details{
Combines the \link[cli:cli_text]{cli::cli_text} function with \link{readline}.
}
