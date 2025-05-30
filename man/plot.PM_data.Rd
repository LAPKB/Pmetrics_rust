% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_data.R
\name{plot.PM_data}
\alias{plot.PM_data}
\title{Plot PM_data Time-Output Data}
\usage{
\method{plot}{PM_data}(
  x,
  include = NULL,
  exclude = NULL,
  line = list(join = TRUE, pred = FALSE),
  marker = TRUE,
  color = NULL,
  colors = "Set1",
  names = NULL,
  mult = 1,
  outeq = 1,
  block = 1,
  tad = FALSE,
  overlay = TRUE,
  legend,
  log = FALSE,
  grid = FALSE,
  xlab = "Time",
  ylab = "Output",
  title = "",
  xlim,
  ylim,
  ...
)
}
\arguments{
\item{x}{The name of an \code{PM_data} data object or loaded as a field
in a \link{PM_result} object}

\item{include}{A vector of subject IDs to include in the plot, e.g. c(1:3,5,15)}

\item{exclude}{A vector of subject IDs to exclude in the plot, e.g. c(4,6:14,16:20)}

\item{line}{Controls characteristics of lines as for all plotly plots.
Here  \code{line} is a list of two elements:
\itemize{
\item \code{join}  Can either be a boolean or a list. If set to \code{TRUE} or
a list of plotly line attributes, it
will generate line segments joining observations. If set to
\code{FALSE}, no segments will be generated. The default
values for the elements of the \code{join} list, all of which can be
overriden are:
\itemize{
\item \code{color} Color of the segments. Default is "dodgerblue".
\item \code{width }Width of the segments, default 1.
\item \code{dash} See \code{plotly::schema()}, traces > scatter > attributes >
line > dash > values. Default is "solid".
Example: \code{line = list(join = list(color = "red", dash = "longdash", width = 2))}
}
\item \code{pred} Default is \code{FALSE}, which means that predictions will not be included
in the plot. To include predictions, supply one of the following:
\item If plotting data contained in a \link{PM_result}, use "pop" or "post" to include population or posterior predictions.
** Example 1: \verb{run1 <- PM_load(1); run1$data$plot(line = list(pred = "post"))}
\item If plotting data not contained in a \link{PM_result}, you may add the
name of a population \link{PM_pop} or posterior \link{PM_post} prediction object in a \link{PM_result} object. This might be useful if you want to see how the predictions from one population match the raw data from another.
** Example 2: \verb{dat <- PM_data$new("new.csv"); dat$plot(line = list(pred = run1$post))}.
}

To format the predictions, supply \code{pred} as
a list, with the prediction object first, followed by named options to control the
prediction plot:
\itemize{
\item icen Chooses the median or mean of each
subject's Bayesian posterior parameter distribution.  Default is "median",
but could be "mean".
\item Other parameters to pass to plotly to control line characteristics that join
the predictions, including \code{color}, \code{width}, and \code{dash}.
Continuing Example 1 above: \code{pred = list("post", icen = "mean", color = "red", width = 2)}.
Default formats are the same as for the \code{join} argument, since normally one would not plot
both lines joining observations and prediction lines, i.e., typical use would be
\code{line = list(join = F, pred = "post")}.
}}

\item{marker}{Formats the symbols plotting observations. Controls the plotting symbol for observations.  This argument maps to the plotly marker object. It can be boolean or a list. \code{TRUE} will plot the marker with default characteristics. \code{FALSE} will suppress marker plotting. If a list, can control many marker characteristics, including overriding defaults. Use the plotly \code{plotly::schema()} command in the console and navigate to traces > scatter > attributes > marker to see all the ways the marker can be formatted. Most common will be: \itemize{ \item{\code{color}} Marker color. \item{\code{symbol}} Plotting character. See \code{plotly::schema()}, traces > scatter > attributes > marker > symbol > values. \item{\code{size}} Character size in points. \item{\code{opacity}} Ranging between 0 (fully transparent) to 1 (fully opaque). \item{\code{line}} A list of  additional attributes governing the outline for filled shapes, most commonly color and width. }

Example: \code{marker = list(color = "red", symbol = "triangle", opacity = 0.8, line = list(color = "black", width = 2))}}

\item{color}{Character vector naming a column in \code{x} to \strong{group} by, e.g. "id" or
a covariate like "gender"}

\item{colors}{to use for \strong{groups}. This can be a palette or a vector of colors.
For accepted palette names see \code{RColorBrewer::brewer.pal.info}. Examples include
"BrBG", or "Set2". An example vector could be \code{c("red", "green", "blue")}. It is not
necessary to specify the same number of colors as groups within \code{color}, as colors
will be interpolated to generate the correct number. The default when \code{color}
is specified is the "Set1" palette.}

\item{names}{A character vector of names to label the \strong{groups} if \code{legend = T}.
This vector does need to be the same length as the number of groups within \code{color}.
Example: \code{c("Male", "Female")} if \code{color = "gender"} and "gender" is a covariate
in the data.}

\item{mult}{Multiplication factor for y axis, e.g. to convert mg/L to ng/mL}

\item{outeq}{Which output equation to plot. Default is 1.
Default is 1, but can be multiple if present in the data, e.g. \code{1:2} or \code{c(1, 3)}.}

\item{block}{Which block to plot, where blocks are defined by dose reset events (EVID = 4) in the data.
Default is 1, but can be multiple if present in the data, as for \code{outeq}.}

\item{tad}{Boolean operator to use time after dose rather than time after start.  Default is \code{FALSE}.}

\item{overlay}{Operator to overlay all time concentration profiles in a single plot.
The default is \code{TRUE}. If \code{FALSE}, will trellisplot subjects one at a time. Can also be
specified as a vector with number of rows and columns, e.g. \code{c(3, 2)} for 3 rows and
2 columns of subject splots to include in each trellis.}

\item{legend}{Controls display of legend. This argument maps to the plotly layout showlegend and legend arguments. It is either a boolean operator (most common) or a list of parameters to be supplied to plotly. See \code{plotly::schema()} > layout > layoutAttributes > legend and showlegend for more details on the available options for formatting.  If legend is supplied as a list,  the plotly layout > layoutAttributes > showlegend value will be set to \code{TRUE} automatically.

Examples: \itemize{ \item{\code{legend = T}} \item{\code{legend = list(orientation = "h", font = list(color = "blue"))}} }
Default is \code{FALSE} unless groups are specified with \code{color}above.}

\item{log}{Boolean operator to plot the y axis in log base 10. This argument maps to the   the yaxis type value in the layout object in plotly.  Use the plotly \code{plotly::schema()} command in the console and navigate to layout > layoutAttributes > yaxis > type.

Example: \code{log = T}}

\item{grid}{Controls grid display. This argument maps to the xaxis and yaxis layout objects in plotly.  Use the plotly \code{plotly::schema()} command in the console and navigate to layout > layoutAttributes > xaxis or yaxis > gridcolor or gridwidth. It is a Boolean operator. If \code{FALSE}, no grid is plotted.  If \code{TRUE}, the default color \emph{grey} and width 1 will be plotted at  major tick marks. If a list, color and width can be customized.

Examples: \itemize{ \item{\code{grid = F}} \item{\code{grid = list(gridcolor = "black", gridwidth = 2)}} }}

\item{xlab}{Value for x axis label. This argument maps to the the xaxis title element of the layout object in plotly.  It can simply be a character vector of length 1 that specifies the name of  the axis, or it can be a list for greater control. Use the plotly \code{plotly::schema()} command in the console and navigate to layout > layoutAttributes > xaxis > title to see the ways to customize this axis label. In addition to the plotly attributes, a custom Pmetrics attribute \code{bold}  may be included as a list element, either on its own or within the font list. The default for \code{bold} is \code{TRUE}.

Examples: \itemize{ \item{\code{xlab = "Time (h)"}} \item{\code{xlab = list(text = "Time", bold = F, font = list(color = "red", family = "Arial", size = 10))}} \item{\code{xlab = list(text = "Time", font = list(bold = T))}} }
Default is "Time".}

\item{ylab}{Value for y axis label. This argument maps to the the yaxis title element of the layout object in plotly.  See \code{xlab} for details. If \code{xlab} is specified as a list with formatting, and \code{ylab} is simply a character label, then the formatting for the  \code{xlab} will be applied to the \code{ylab}. To format \code{ylab} independently, specify a formatting list as for \code{xlab}.

Default is "Output".}

\item{title}{Plot title. This argument maps to the the title layout object in plotly.  It can simply be a character vector of length 1 that specifies the name of  the plot title, or it can be a list for greater control. Use the plotly \code{plotly::schema()} command in the console and navigate to layout > layoutAttributes > title to see other ways to customize the title using lists as additional arguments. In addition to the plotly attributes, a custom Pmetrics attribute \code{bold}  may be included as a list element. The default for \code{bold} is \code{TRUE}.

Examples: \itemize{ \item{\code{title = "Observed vs. Predicted"}} \item{\code{title = list(text = "Raw Data", font = list(color = "red", family = "Arial", size = 10, bold = T))}} }
Default is to have no title.}

\item{xlim}{Limits of the x axis as a vector.   This argument maps to the   the xaxis range in the layout object in plotly.  Use the plotly \code{plotly::schema()} command in the console and navigate to layout > layoutAttributes > xaxis > range.

Example: \code{xlim = c(0,1)}}

\item{ylim}{Limits of the y axis as a vector.   This argument maps to the   the yaxis range in the layout object in plotly.  Use the plotly \code{plotly::schema()} command in the console and navigate to layout > layoutAttributes > yaxis > range.

Example: \code{ylim = c(0,100)}}

\item{\dots}{Other attributes which can be passed to the layout > xaxis/yaxis  in a plotly plot to further control formatting.  Note that \code{log}, \code{xlab}, \code{ylab}, \code{xlim}, and \code{ylim} are  all controlled by the layout object, but are provided throughout Pmetrics plotly function arguments as shortcuts that map to layout elements.  Therefore, the dots argument should be used to specify other aspects of the  x axis, y axis, or both. See \code{plotly::schema()} layout > layoutAttributes  > xaxis/yaxis for options. To add to single axis, name it as a list.  If attributes are specified without an enclosing xaxis or yaxis list,  they will be applied to both axes.

Examples:  \itemize{ \item{\code{NPex$data$plot(xaxis = list(tickcolor = "black", tickfont = list(family = "Arial", size = 14, color = "black"))) #applies to x axis only}} \item{\code{NPex$data$plot(linecolor = "red", ticks = "inside") #applies to both axes}} }  .}
}
\value{
Plots the object.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Plots \emph{PM_data} objects
}
\details{
This function will plot raw and fitted time and concentration data with a variety of options.
By default markers are included and  have the following plotly properties:
\code{list(symbol = "circle", color = "red", size = 10, opacity = 0.5, line = list(color = "black", width = 1))}.
Markers can be joined by lines, default is \code{FALSE}. If chosen to be \code{TRUE},
the joining lines will have the following properties:
\verb{list(color = "dodgerblue", width = 1, dash = "solid"}.
The grid and legend are omitted by default.
}
\examples{
\dontrun{
# basic spaghetti plot
dataEx$plot()
# format line and marker
dataEx$plot(
  marker = list(color = "blue", symbol = "square", size = 12, opacity = 0.4),
  line = list(join = list(color = "orange"))
)
# include predictions with default format and suppress joining lines
dataEx$plot(
  line = list(join = FALSE, pred = NPex$post),
  xlim = c(119, 146)
)
# customize prediction lines
dataEx$plot(
  line = list(
    pred = list(NPex$post, color = "slategrey", dash = "dash"),
    join = FALSE
  )
)
}
}
\seealso{
\link{PM_data}, \link{PM_result}

Other PMplots: 
\code{\link{plot.PM_cov}()},
\code{\link{plot.PM_cycle}()},
\code{\link{plot.PM_final}()},
\code{\link{plot.PM_model}()},
\code{\link{plot.PM_op}()},
\code{\link{plot.PM_opt}()},
\code{\link{plot.PM_pop}()},
\code{\link{plot.PM_post}()},
\code{\link{plot.PM_pta}()},
\code{\link{plot.PM_sim}()},
\code{\link{plot.PM_valid}()}
}
\author{
Michael Neely
}
\concept{PMplots}
