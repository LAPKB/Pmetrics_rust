% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_op.R
\name{PM_op}
\alias{PM_op}
\title{Observed vs. predicted data}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

Contains observed vs. predicted data after a run, typically a field in a \link{PM_result}
}
\details{
The \link{PM_op} object is both a data field within a \link{PM_result}, and itself an R6 object
comprising data fields and associated methods suitable for analysis and plotting of
observed vs. population or individual predicted outputs.

Because \link{PM_op} objects are automatically added to the \link{PM_result} at the end of a
successful run, it is generally not necessary for users to generate \link{PM_op} objects
themselves.

The main results are contained in the \verb{$data} field,
and it is this field which is passed to the \verb{$plot} and \verb{$summary} methods.
You can use this \verb{$data} field for custom manipulations, e.g. \code{trough <- run1$op$data \%>\% filter(time == 24)}.
If you are unfamiliar with the \verb{\%>\%} pipe function, please type \code{help("\%>\%", "magrittr")}
into the R console and look online for instructions/tutorials in tidyverse, a
powerful approach to data manipulation upon which Pmetrics is built.

To provide a more traditional experience in R,
the \verb{$data} field is also separated by columns into the other data fields within the R6 object,
e.g. \code{id} or \code{time}. This
allows you to access them in an S3 way, e.g. \code{run1$op$time} if \code{run1} is a
\link{PM_result} object.
}
\author{
Michael Neely, Julian Otalvaro
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{id}}{subject identification}

\item{\code{time}}{observation time in relative units, usually hours}

\item{\code{obs}}{observation}

\item{\code{pred}}{prediction}

\item{\code{pred.type}}{Population predictions based on Bayesian prior parameter value distribution,
or individual predictions based on Bayesian posterior parameter value distributions}

\item{\code{icen}}{Predictions based on mean or median of Bayesian \code{pred.type}parameter values}

\item{\code{outeq}}{output equation number}

\item{\code{block}}{dosing block number for each subject, as defined by dose resets (evid=4).}

\item{\code{obsSD}}{standard deviation of the observation based on the assay error polynomial}

\item{\code{d}}{prediction error, \code{pred} - \code{obs}}

\item{\code{ds}}{squared prediction error}

\item{\code{wd}}{weighted prediction error, which is the prediction error divided by the \code{obsSD}}

\item{\code{wds}}{weighted squared prediction error}

\item{\code{data}}{A data frame of class \strong{PM_op_data} combining all the above fields as its columns}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PM_op-new}{\code{PM_op$new()}}
\item \href{#method-PM_op-plot}{\code{PM_op$plot()}}
\item \href{#method-PM_op-summary}{\code{PM_op$summary()}}
\item \href{#method-PM_op-auc}{\code{PM_op$auc()}}
\item \href{#method-PM_op-clone}{\code{PM_op$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_op-new"></a>}}
\if{latex}{\out{\hypertarget{method-PM_op-new}{}}}
\subsection{Method \code{new()}}{
Create new object populated with observed vs. predicted data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_op$new(PMdata = NULL, ...)}\if{html}{\out{</div>}}
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
Creation of new \code{PM_op} object is automatic at the end of a run and not generally necessary
for the user to do.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_op-plot"></a>}}
\if{latex}{\out{\hypertarget{method-PM_op-plot}{}}}
\subsection{Method \code{plot()}}{
Plot method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_op$plot(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{plot.PM_op}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{plot.PM_op}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_op-summary"></a>}}
\if{latex}{\out{\hypertarget{method-PM_op-summary}{}}}
\subsection{Method \code{summary()}}{
Summary method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_op$summary(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{summary.PM_op}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{summary.PM_op}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_op-auc"></a>}}
\if{latex}{\out{\hypertarget{method-PM_op-auc}{}}}
\subsection{Method \code{auc()}}{
Calculate AUC
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_op$auc(...)}\if{html}{\out{</div>}}
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
\if{html}{\out{<a id="method-PM_op-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PM_op-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_op$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
