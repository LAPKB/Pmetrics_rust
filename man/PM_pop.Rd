% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_pop.R
\name{PM_pop}
\alias{PM_pop}
\title{Population predictions at short intervals}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Contains the population predictions at short intervals
specified as an argument to the run method of \link{PM_fit}. Default is every 12 minutes.
}
\details{
#' The \link{PM_pop} object is both a data field within a \link{PM_result}, and itself an R6 object
comprising data fields and associated methods suitable for
analysis and plotting of population predictions generated during the run.

Because \link{PM_pop} objects are automatically added to the \link{PM_result} at the end of a
successful run, it is generally not necessary for users to generate \link{PM_pop} objects
themselves.

The main results are contained in the \verb{$data} field,
and it is this field which is passed to the \verb{$plot} and \verb{$summary} methods.
data frame with population predicted outputs for all subjects.

To provide a more traditional experience in R,
the data frame is separated by columns into fields, e.g. \code{id} or \code{time}. This
allows you to access them in an S3 way, e.g. \code{run1$pop$time} if \code{run1} is a
\code{PM_result} object.

However, if you wish to manipulate the entire data frame,
use the \code{data} field, e.g. \code{trough <- run1$pop$data \%>\% filter(time == 24)}. If
you are unfamiliar with the \verb{\%>\%} pipe function, please type \code{help("\%>\%", "magrittr")}
into the R console and look online for instructions/tutorials in tidyverse, a
powerful approach to data manipulation upon which Pmetrics is built.
}
\author{
Michael Neely, Julian Otalvaro
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{data}}{A data frame with the following columns:
\itemize{
\item \strong{id} Subject id
\item \strong{time} Time of predictions in decimal hours
\item \strong{icen} Prediction based on mean or median of Bayesian posterior parameter distribution
\item \strong{outeq} Output equation number
\item \strong{pred} Predicted output for each outeq
\item \strong{block} Observation blocks within subjects as defined by \emph{EVID=4} dosing events
}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PM_pop-new}{\code{PM_pop$new()}}
\item \href{#method-PM_pop-plot}{\code{PM_pop$plot()}}
\item \href{#method-PM_pop-summary}{\code{PM_pop$summary()}}
\item \href{#method-PM_pop-auc}{\code{PM_pop$auc()}}
\item \href{#method-PM_pop-clone}{\code{PM_pop$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_pop-new"></a>}}
\if{latex}{\out{\hypertarget{method-PM_pop-new}{}}}
\subsection{Method \code{new()}}{
Create new object populated with population predicted data at
regular, frequent intervals
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_pop$new(PMdata = NULL, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{PMdata}}{include Saved, parsed output of prior run, used when source files are not available.
.}

\item{\code{...}}{Not currently used.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
Creation of new \code{PM_pop} object is automatic and not generally necessary
for the user to do.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_pop-plot"></a>}}
\if{latex}{\out{\hypertarget{method-PM_pop-plot}{}}}
\subsection{Method \code{plot()}}{
Plot method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_pop$plot(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{plot.PM_pop}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{plot.PM_pop}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_pop-summary"></a>}}
\if{latex}{\out{\hypertarget{method-PM_pop-summary}{}}}
\subsection{Method \code{summary()}}{
Summary method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_pop$summary(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{summary.PM_pop}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{summary.PM_pop}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_pop-auc"></a>}}
\if{latex}{\out{\hypertarget{method-PM_pop-auc}{}}}
\subsection{Method \code{auc()}}{
Calculate AUC
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_pop$auc(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{makeAUC}}

\item{\code{data}}{The object to use for AUC calculation}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{makeAUC}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_pop-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PM_pop-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_pop$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
