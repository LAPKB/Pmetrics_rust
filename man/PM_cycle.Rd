% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_cycle.R
\name{PM_cycle}
\alias{PM_cycle}
\title{Pmetrics Run Cycle Information}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Contains the cycle information after a run.
}
\details{
The \link{PM_cycle} object is both a data field within a \link{PM_result}, and itself an R6 object
comprising data fields and associated methods suitable for analysis and plotting of
cycle information generated during the run.

Because \link{PM_cycle} objects are automatically added to the \link{PM_result} at the end of a
successful run, it is generally not necessary for users to generate \link{PM_cycle} objects
themselves.

The main results are contained in the \verb{$data} field,
and it is this field which is passed to the \verb{$plot} and \verb{$summary} methods.
You can use this \verb{$data} field for custom manipulations, e.g. \code{last <- run1$cycle$data$aic \%>\% tail(1)}.
This will report the last cycle aic.
If you are unfamiliar with the \verb{\%>\%} pipe function, please type \code{help("\%>\%", "magrittr")}
into the R console and look online for instructions/tutorials in tidyverse, a
powerful approach to data manipulation upon which Pmetrics is built.

To provide a more traditional experience in R,
the \verb{$data} field is also separated by list items into the other data fields within the R6 object,
e.g. \code{mean} or \code{gamlam}. This
allows you to access them in an S3 way, e.g. \code{run1$cycle$mean} if \code{run1} is a
\link{PM_result} object.
}
\author{
Michael Neely, Julian Otalvaro
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{data}}{A list with the following elements, which can also be extracted by name.
e.e. \code{run1$cycle$objective}, which is equivalent to \code{run1$cycle$data$objective}.
\strong{names} Vector of names of the random parameters
\strong{objective} A tibble of -2*Log-likelihood, AIC and BIC at each cycle
\strong{gamlam} A tibble of cycle number and gamma or lambda at each cycle for each output equation
\strong{mean} A tibble of cycle number and the mean of each random parameter at each cycle, normalized to initial mean
\strong{median} A tibble of cycle number and the median of each random parameter at each cycle, normalized to initial median
\strong{sd} A tibble of cycle number and the standard deviation of each random parameter at each cycle, normalized to initial standard deviation
\strong{status} Status of the last cycle: "Converged", "Maximum cycles reached", or "Posterior".}
}
\if{html}{\out{</div>}}
}
\section{Active bindings}{
\if{html}{\out{<div class="r6-active-bindings">}}
\describe{
\item{\code{names}}{Vector of names of the random parameters}

\item{\code{objective}}{A tibble of -2*Log-likelihood, AIC and BIC at each cycle}

\item{\code{gamlam}}{A tibble of cycle number and gamma or lambda at each cycle for each output equation}

\item{\code{mean}}{A tibble of cycle number and the mean of each random parameter
at each cycle, normalized to initial mean}

\item{\code{median}}{A tibble of cycle number and the median of each random
parameter at each cycle,  normalized to initial median}

\item{\code{sd}}{A tibble of cycle number and the standard deviation of each random parameter
at each cycle,  normalized to initial standard deviation}

\item{\code{status}}{Status of the last cycle: "Converged", "Maximum cycles reached", or "Posterior"}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PM_cycle-new}{\code{PM_cycle$new()}}
\item \href{#method-PM_cycle-plot}{\code{PM_cycle$plot()}}
\item \href{#method-PM_cycle-summary}{\code{PM_cycle$summary()}}
\item \href{#method-PM_cycle-clone}{\code{PM_cycle$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_cycle-new"></a>}}
\if{latex}{\out{\hypertarget{method-PM_cycle-new}{}}}
\subsection{Method \code{new()}}{
Create new object populated with  cycle information
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_cycle$new(PMdata = NULL, ...)}\if{html}{\out{</div>}}
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
Creation of new \code{PM_cycle} object is automatic and not generally necessary
for the user to do.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_cycle-plot"></a>}}
\if{latex}{\out{\hypertarget{method-PM_cycle-plot}{}}}
\subsection{Method \code{plot()}}{
Plot method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_cycle$plot(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{plot.PM_cycle}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{plot.PM_cycle}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_cycle-summary"></a>}}
\if{latex}{\out{\hypertarget{method-PM_cycle-summary}{}}}
\subsection{Method \code{summary()}}{
Summary method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_cycle$summary(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{summary.PM_cycle}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{summary.PM_cycle}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_cycle-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PM_cycle-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_cycle$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
