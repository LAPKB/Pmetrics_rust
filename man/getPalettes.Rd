% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_Utils.R
\name{getPalettes}
\alias{getPalettes}
\title{Get color palette}
\usage{
getPalettes()
}
\value{
A character vector of palette names.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Generate list of palettes for plots.
}
\details{
If RColorBrewer package is installed, will return the list of palette names from
RColorBrewer::brewer.pal.info. If not, will return the current list as of April 2024.
}
\examples{
\dontrun{
getPalettes()
}
}
\author{
Michael Neely
}
