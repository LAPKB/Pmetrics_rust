% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_fit.R
\name{PM_fit}
\alias{PM_fit}
\title{Object to define and run a model and data in Pmetrics}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

\code{PM_fit} objects comprise a \code{PM_data} and \code{PM_model} object ready for analysis
}
\details{
Data and model objects can be previously created as \link{PM_data} or \link{PM_model} objects,
or created on the fly when making a new PM_fit object. PM_fit objects contain
methods to cross-check data and model objects for compatibility, as well as to
run the analysis.
}
\author{
Michael Neely
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{data}}{\link{PM_data} object}

\item{\code{model}}{\link{PM_model} object}

\item{\code{backend}}{Backend used for calculations; default is value in PMoptions.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PM_fit-new}{\code{PM_fit$new()}}
\item \href{#method-PM_fit-run}{\code{PM_fit$run()}}
\item \href{#method-PM_fit-save}{\code{PM_fit$save()}}
\item \href{#method-PM_fit-load}{\code{PM_fit$load()}}
\item \href{#method-PM_fit-check}{\code{PM_fit$check()}}
\item \href{#method-PM_fit-clone}{\code{PM_fit$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_fit-new"></a>}}
\if{latex}{\out{\hypertarget{method-PM_fit-new}{}}}
\subsection{Method \code{new()}}{
Create a new object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_fit$new(data = data, model = model, backend = getPMoptions()$backend, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{Either the name of a  \link{PM_data}
object in memory or the quoted name of a Pmetrics
data file in the current working directory, which will crate a \link{PM_data}
object on the fly. However, if created on the fly, this object
will not be available to other
methods or other instances of \link{PM_fit}.}

\item{\code{model}}{Similarly, this is either the name of a \link{PM_model}
object in memory or the quoted name of a Pmetrics text model file
in the current working directory. Again, if created on the fly,
the object will not be available to other
methods or other instances of \link{PM_fit}.}

\item{\code{backend}}{Backend used for calculations; default is value in PMoptions.}

\item{\code{...}}{Other parameters passed to \link{PM_data} or \link{PM_model} if created
from a filename}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_fit-run"></a>}}
\if{latex}{\out{\hypertarget{method-PM_fit-run}{}}}
\subsection{Method \code{run()}}{
Fit the model to the data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_fit$run(
  run = NULL,
  include = NULL,
  exclude = NULL,
  cycles = 100,
  indpts = NULL,
  prior = "uniform",
  intern = TRUE,
  overwrite = FALSE,
  engine = "NPAG",
  sampler = "sobol",
  report = getPMoptions("report_template"),
  artifacts = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{run}}{Specify the run number of the output folder.  Default if missing is the next available number.}

\item{\code{include}}{Vector of subject id values in the data file to include in the analysis.  The default (missing) is all.}

\item{\code{exclude}}{A vector of subject IDs to exclude in the analysis, e.g. c(4,6:14,16:20)}

\item{\code{cycles}}{Number of cycles to run. Default is 100.}

\item{\code{indpts}}{Index of starting grid point number.  Default is missing, which allows NPAG to choose depending on the number of random parameters:}

\item{\code{prior}}{Either "uniform" (default) or the name of a suitable \link{PM_result} object from a prior run loaded with \link{PM_load}.
A \code{prior} may be specified if the user wishes to
start from a non-uniform prior distribution for the NPAG run. This is useful for continuing a previous
run which did not converge, e.g., \code{fit1$run(prior = run1)}, assuming \code{run1} is a \link{PM_result} object
that was loaded with \link{PM_load}.}

\item{\code{intern}}{Run NPAG in the R console without a batch script.  Default is TRUE.}

\item{\code{overwrite}}{Boolean operator to overwrite existing run result folders.  Default is \code{FALSE}.}

\item{\code{engine}}{The engine to use for the run.  Default is "NPAG". Alternatives: "NPOD".}

\item{\code{sampler}}{The pseudo-random sampler to use for the initial distribution of grid points when
\code{prior = "uniform"}.}

\item{\code{report}}{If missing, the default Pmetrics report template as specified in \link{getPMoptions}
is used. Otherwise can be "plotly", "ggplot", or "none".}

\item{\code{artifacts}}{Default is \code{TRUE}.  Set to \code{FALSE} to suppress creating the \code{etc} folder. This folder
will contain all the compilation artifacts created during the compilation and run steps.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
This function will fit the model contained in the \code{model} field to the data
in the \code{data} field, using the engine specified by the \code{engine} argument and
modified by other arguments. If all function arguments are default,
the simplest execution of this method is
\verb{$run()}.
}

\subsection{Returns}{
A successful run will result in creation of a new folder in the working
directory with the results inside the folder.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_fit-save"></a>}}
\if{latex}{\out{\hypertarget{method-PM_fit-save}{}}}
\subsection{Method \code{save()}}{
Save the current PM_fit object to a .rds file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_fit$save(file_name = "PMfit.rds")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file_name}}{Name of the file to be created. The
default is "PMfit.rds".}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_fit-load"></a>}}
\if{latex}{\out{\hypertarget{method-PM_fit-load}{}}}
\subsection{Method \code{load()}}{
\code{PM_fit} objects contain a \code{save} method which invokes \link{saveRDS} to write
the object to the hard drive as an .rds file. This is the corresponding load
function.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_fit$load(file_name)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file_name}}{Name of the file to be read, the default is "PMfit.rds".}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{PM_fit} object.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_fit-check"></a>}}
\if{latex}{\out{\hypertarget{method-PM_fit-check}{}}}
\subsection{Method \code{check()}}{
Checks for errors in data and model objects and agreement between them.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_fit$check()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_fit-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PM_fit-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_fit$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
