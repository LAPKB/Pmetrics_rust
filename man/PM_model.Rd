% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PM_model.R
\name{PM_model}
\alias{PM_model}
\title{Defines the PM_model class}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

PM_model objects contain the variables, covariates, equations and error models
necessary to run a population analysis.
}
\details{
PM_model objects are passed to \code{\link{PM_fit}} objects to initiate a
population analysis. The object is created by defining a list of lists
directly in R, or by reading a model text file in the current working directory.
}
\examples{

## ------------------------------------------------
## Method `PM_model$update`
## ------------------------------------------------

mod2 <- PmetricsData::modEx$clone() #create an independent copy of modEx called mod2
mod2$update(list(
  pri = list(
   Ke = ab(0, 1), #change the range
   V = NULL, #this deletes the variable
   V0 = ab(10, 100) #add a new variable
  ),
  sec = "V = V0 * WT" #add a new secondary equation
))
#note that they are different now
mod2
PmetricsData::modEx

## ------------------------------------------------
## Method `PM_model$write`
## ------------------------------------------------

\dontrun{
PmetricsData::modEx$write("model.txt")
}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-PM_Vmodel-new}{\code{PM_model$new()}}
\item \href{#method-PM_Vmodel-print}{\code{PM_model$print()}}
\item \href{#method-PM_Vmodel-update}{\code{PM_model$update()}}
\item \href{#method-PM_Vmodel-write}{\code{PM_model$write()}}
\item \href{#method-PM_Vmodel-plot}{\code{PM_model$plot()}}
\item \href{#method-PM_Vmodel-clone}{\code{PM_model$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_Vmodel-new"></a>}}
\if{latex}{\out{\hypertarget{method-PM_Vmodel-new}{}}}
\subsection{Method \code{new()}}{
Build a new PM_model from a variety of inputs.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_model$new(model, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{model}}{This can be a quoted name of a model text file in the
working directory which will be read and passed to Fortran engines.
It can be a list of lists that defines the model directly in R. It
can also be a \link{PM_model} object, which will simply rebuild it. See the user
manual for more help on directly defining models in R.}

\item{\code{...}}{Not currently used.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_Vmodel-print"></a>}}
\if{latex}{\out{\hypertarget{method-PM_Vmodel-print}{}}}
\subsection{Method \code{print()}}{
Print a model object to the console in readable format
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_model$print(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Not used currently.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_Vmodel-update"></a>}}
\if{latex}{\out{\hypertarget{method-PM_Vmodel-update}{}}}
\subsection{Method \code{update()}}{
Update selected elements of a model object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_model$update(changes_list)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{changes_list}}{The named list containing elements and values to update.
Because R6 objects are mini-environments, using typical
R notation to copy an object like mod2 <- mod1 can lead to unexpected
results since this syntax simply creates a copied object in the same
environment. Therefore updating any one object (e.g., mod1 or mod2)
will update the other. To avoid this behavior, use the $clone() function
first if you want to create a copied, yet independent new model.}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{mod2 <- PmetricsData::modEx$clone() #create an independent copy of modEx called mod2
mod2$update(list(
  pri = list(
   Ke = ab(0, 1), #change the range
   V = NULL, #this deletes the variable
   V0 = ab(10, 100) #add a new variable
  ),
  sec = "V = V0 * WT" #add a new secondary equation
))
#note that they are different now
mod2
PmetricsData::modEx
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_Vmodel-write"></a>}}
\if{latex}{\out{\hypertarget{method-PM_Vmodel-write}{}}}
\subsection{Method \code{write()}}{
Write a \code{PM_model} object to a text file
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_model$write(model_path = "genmodel.txt", engine = "npag")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{model_path}}{Full name of the file to be created, including the path
relative to the current working directory}

\item{\code{engine}}{Currently only "npag".}
}
\if{html}{\out{</div>}}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{\dontrun{
PmetricsData::modEx$write("model.txt")
}
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_Vmodel-plot"></a>}}
\if{latex}{\out{\hypertarget{method-PM_Vmodel-plot}{}}}
\subsection{Method \code{plot()}}{
Plot method
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_model$plot(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Arguments passed to \link{plot.PM_model}}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
See \link{plot.PM_model}.
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-PM_Vmodel-clone"></a>}}
\if{latex}{\out{\hypertarget{method-PM_Vmodel-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{PM_model$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
