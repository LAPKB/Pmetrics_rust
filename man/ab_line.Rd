% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotly_Utils.R
\name{ab_line}
\alias{ab_line}
\title{Add lines to plotly plot}
\usage{
ab_line(a = NULL, b = NULL, h = NULL, v = NULL, line = TRUE)
}
\arguments{
\item{a}{Intercept y value in relative coordinates, i.e. 0 (bottom) to 1 (top).
The x value is 0.}

\item{b}{Slope}

\item{h}{Y-intercept of horizontal line, in absolute coordinates}

\item{v}{X-intercept of vertical line, in absolute coordinates}

\item{line}{Controls characteristics of lines. This argument maps to plotly line attributes. \code{TRUE} will plot default lines. \code{FALSE} will suppress lines. If a list, can control many line characteristics, including overriding defaults. Use the plotly \code{plotly::schema()} command in the console and navigate to traces > scatter > attributes > line to see all the ways the line can be formatted. Most common will be:  \itemize{ \item{\code{color}} Line color. \item{\code{dash}} Plotting character. See \code{plotly::schema()}, traces > scatter >  attributes > line > dash > values. \item{\code{width}} Thickness in points. }

Example: \code{line = list(color = "red", dash = "longdash", width = 2)}
.
Default is \code{line = list(color = "black", width = 1, dash = "dash")}.}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Analogous to \link{abline}, draws horizontal, vertical or sloped reference lines.
}
\details{
This function creates a line shape that can be added a plotly plot.
See schema() > layout > layoutAttributes > shapes for details. Use only one
of the following:
\itemize{
\item a and b to specify a line with intercept and slope
\item h to specify a horizontal line with y-intercept at \code{h}
\item v to specify a vertical line with x-intercept at \code{v}
}

If using this function to add a line to an existing plot, it must be used
with \link{add_shapes}. If used for a new plot, it can be included as an element
in the layout list.
}
\seealso{
\link{add_shapes}
\dontrun{
# add to an existing plot
NPex$op$plot() %>%
  add_shapes(shapes = ab_line(v = 12))
# add to a new plot
plotly::plot_ly(x = 1:10, y = 1:10, type = "scatter", mode = "lines+markers") %>%
  plotly::layout(shapes = ab_line(h = 5, line = list(color = "red", dash = "solid")))
}
}
