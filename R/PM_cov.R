# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ------------------------------------------------------------------

#' @title Contains covariate data
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Contains a data frame with subject-specific covariate data output.
#'
#' @details
#' The [PM_cov] object is both a data field within a [PM_result], and itself an R6 object
#' comprising data fields and associated methods suitable for analysis and
#' plotting relationships between covariates and posterior parameters,
#' covariates over time, or parameter values over time.
#'
#' Because [PM_cov] objects are automatically added to the [PM_result] at the end of a
#' successful run, it is generally not necessary for users to generate [PM_cov] objects
#' themselves.
#'
#' The  results are contained in the `$data` field,
#' and it is this field which is passed to the `$plot` and `$summary` methods.
#' You can use this `$data` field for custom manipulations, although usually this
#' is better done on the `summary` object, e.g.,
#' `run1$cov$summary() %>% select(africa, gender) %>% table()`.
#' If you are unfamiliar with the `%>%` pipe function, please type `help("%>%", "magrittr")`
#' into the R console and look online for instructions/tutorials in tidyverse, a
#' powerful approach to data manipulation upon which Pmetrics is built.
#'
#'
#' This output of this function is suitable for exploration of covariate-
#' parameter, covariate-time, or parameter-time relationships.
#' @seealso [plot.PM_cov], [PM_step]
#' @author Michael Neely, Julian Otalvaro
#' @export

PM_cov <- R6::R6Class(
  "PM_cov",
  public <- list(
    #' @field data A data frame with the following columns
    #' * id Subject identification
    #' * time Times of covariate observations
    #' * covnames... Columns with each covariate observations in the dataset for each subject and `time`
    #' * parnames... Columns with each parameter in the model and the `icen` summary
    #' for each subject, replicated as necessary for covariate observation times and duplicated for Bayesian
    #' parameter means and medians
    #' * icen The type of summarized Bayesian posterior individual parameter values: mean or median
    data = NULL,
    #' @description
    #' Create new object populated with covariate-parameter information
    #' @details
    #' Creation of new `PM_cov` object is automatic and not generally necessary
    #' for the user to do.
    #' @param PMdata include `r template("PMdata")`.
    #' @param ... Not currently used.
    initialize = function(PMdata, ...) {
      self$data <- private$make(PMdata)
    },
    #' @description
    #' Stepwise linear regression of covariates and Bayesian posterior
    #' parameter values
    #' @details
    #' See [PMstep].
    #' @param ... Arguments passed to [PMstep]
    step = function(...) {
      PM_step(self$data, ...)
    },
    #' @description
    #' Summary method
    #' @details
    #' See [summary.PM_cov].
    #' @param ... Arguments passed to [summary.PM_cov]
    summary = function(...) {
      summary.PM_cov(self, ...)
    },
    #' @description
    #' Plot method
    #' @details
    #' See [plot.PM_cov].
    #' @param ... Arguments passed to [plot.PM_cov]
    plot = function(...) {
      plot.PM_cov(self, ...)
    },
    #' @description
    #' Print method
    #' @details
    #' Print method for [PM_cov]
    #' @param ... Arguments passed to [print]
    print = function(...) {
      print(x = self$data, ...)
    }
  ), # end public
  private = list(
    make = function(data) {
      if (file.exists("posterior.csv")) {
        posts <- readr::read_csv(file = "posterior.csv", show_col_types = FALSE)
      } else if (inherits(data, "PM_cov")) { # file not there, and already PM_cov
        class(data$data) <- c("PM_cov_data", "data.frame")
        return(data$data)
      } else {
        cli::cli_warn(c(
          "!" = "Unable to generate covariate-posterior information.",
          "i" = "Result does not have valid {.code PM_cov} object, and {.file {getwd()}/posterior.csv} does not exist."
        ))
        return(NULL)
      }

      if (file.exists("covs.csv")) {
        covs <- readr::read_csv(file = "covs.csv", show_col_types = FALSE)
      } else {
        cli::cli_warn(c(
          "!" = "Unable to generate covariate-posterior information.",
          "i" = "Result does not have valid {.code PM_cov} object, and {.file {getwd()}/covs.csv} does not exist."
        ))
        return(NULL)
      }

      covs <- covs %>% mutate(block = block + 1)


      post_mean <- posts %>%
        group_by(id) %>%
        summarise(across(-c(point, prob), \(x) wtd.mean(x = x, weights = prob))) %>%
        mutate(icen = "mean")

      post_med <- posts %>%
        group_by(id) %>%
        suppressWarnings(reframe(across(-c(point, prob), \(x) wtd.quantile(x, prob, 0.5)))) %>%
        mutate(icen = "median")



      res <- bind_rows(
        dplyr::left_join(covs, post_mean, by = "id"),
        dplyr::left_join(covs, post_med, by = "id")
      )

      class(res) <- c("PM_cov_data", "data.frame")
      attr(res, "ncov") <- ncol(covs) - 3 # subtract id, time, block

      return(res)
    }
  ) # end private
)


# PLOT --------------------------------------------------------------------
#' @title Plot Pmetrics Covariate objects
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Plot PMcov objects
#' @details
#' This method will plot any two columns, specified using a formula, of a PMcov object, which contains covariate and Bayesian posterior parameter information
#' for each subject.  Specifiying any two variables that do not include time will result in a scatter plot with optional regression and reference lines.  If
#' time is included as the x variable, the y variable will be plotted vs. time, aggregated by subject.  This can be useful to see time varying parameters,
#' although a formula within formula approach may be required, e.g. `$plot(I(cl_0*wt**0.75) ~ time)` in order to see the change in cl over time according to
#' the change in wt over time, even though cl_0 is constant for a given subject.
#'
#' @method plot PM_cov
#' @param x The name of an [PM_cov] data object
#' and loaded with [PM_load] as a [PM_result], e.g. `PM_result$cov`.
#' @param formula This is a mandatory formula of the form \code{y ~ x}, where \code{y} and \code{x}
#' are the two \code{data} parameters to plot.
#' @param legend Not used for this function.
#' @param include `r template("include")`
#' @param exclude `r template("exclude")`
#' @param icen `r template("icen")`
#' @param line Controls characteristics of lines. Unlike
#' some other Pmetrics plots, but like [plot.PM_op], `line` is a list of
#' three elements:
#' * `lm`  If set to `TRUE` or a list of plotly line attributes,
#' will generate a linear regression of the form y ~ x
#' Line attributes will control the appearance of the regression
#' line and the confidence interval around the line. If set to
#' `FALSE`, no linear regression will be generated. The default
#' values for the elements of the `lm` list, all of which can be
#' overriden are:
#'     - `ci` Confidence interval around the regression, default 0.95.
#'     - `color` Color of the regression line and the confidence area around
#' the line, but at opacity = 0.2. Default is "dodgerblue".
#'     - `width `Width of the regression line, default 1.
#'     - `dash` See `plotly::schema()`, traces > scatter > attributes >
#' line > dash > values. Default is "solid".
#' Example: `line = list(lm = list(color = "red", dash = "longdash", width = 2))`
#' * `loess` If set to `TRUE` or a list of plotly line attributes,
#' will generate a loess regression of the form y ~ x
#' The list elements and default values in the `loess` list are the
#' same as for `lm` except the default style is "dash".
#' Example: `line = list(lm = FALSE, loess = TRUE)`
#' * `ref` If set to `TRUE` or a list of plotly line attributes,
#' will generate a reference line with slope = 1 and intercept = 0.
#' The default values for the elements of the `ref` list are:
#'     - `color` "grey".
#'     - `width` 1.
#'     - `dash` "dot".
#' Note that there is no *ci* argument for the *ref* list.
#' Example: `line = list(lm = FALSE, loess = TRUE, ref = list(color = "lightgrey"))`
#' If the `line` argument is missing, it will be set to
#' `line = list(lm = FALSE, loess = TRUE, ref = FALSE)`, i.e. there will be a linear
#' regression with reference line, but no loess regression.
#' If *time* is chosen as the x variable in the formula, linear, loess and reference
#' lines will be suppressed, although formatting specified in the loess line (except color,
#' see below) will be applied to the lines joining the subject values.
#' @param marker `r template("marker")` Default is
#' `marker = list(color = orange, shape = "circle", size = 10, opacity = 0.5, line = list(color = black, width = 1))`.
#' @param colors to use for subjects when *time* is set as the x parameter.
#' This can be a palette or a vector of colors.
#' For accepted palette names see `RColorBrewer::brewer.pal.info`. Examples include
#' "BrBG", or "Set2". An example vector could be `c("red", "green", "blue")`. It is not
#' necessary to specify the same number of colors as groups within `color`, as colors
#' will be interpolated to generate the correct number. The default
#' is the "Spectral" palette. This will override any color in the `marker` or `line`.
#' @param log `r template("log")`
#' @param grid `r template("grid")`
#' @param xlim `r template("xlim")`
#' @param ylim `r template("ylim")`
#' @param xlab `r template("xlab")`   If missing, will default to the name of the
#' x variable in the formula.
#' @param ylab `r template("ylab")`   If missing, will default to the name of the
#' y variable in the formula.
#' @param title `r template("title")` Default is to have no title.
#' @param stats Add the statistics from linear regression to the plot. If
#' `FALSE`, will be suppressed. Default is `TRUE` which results in default format of
#' `list(x= 0.8, y = 0.1, bold = F, font = list(color = "black", family = "Arial", size = 14))`.
#' The coordinates are relative to the plot with lower left = (0,0), upper right = (1,1). This
#' argument maps to `plotly::add_text()`.
#' @param \dots `r template("dotsPlotly")`
#' @return Plots the object.
#' @author Michael Neely
#' @seealso [PM_cov], [PM_result], [schema]
#' @export
#' @examples
#' #'
#' \dontrun{
#' NPex$cov$plot(V ~ wt)
#' NPex$cov$plot(Ke ~ wt, line = list(lm = TRUE, ref = FALSE, loess = FALSE))
#' NPex$cov$plot(Ke ~ wt, line = list(loess = list(ci = 0.9, color = "green")))
#' NPex$cov$plot(V ~ time, marker = list(color = "blue"))
#' NPex$cov$plot(V ~ wt,
#'   line = list(lm = TRUE, loess = FALSE),
#'   stats = list(x = 0.5, y = 0.2, font = list(size = 7, color = "blue"))
#' )
#' }

#' @family PMplots

plot.PM_cov <- function(x,
                        formula,
                        line = list(lm = NULL, loess = NULL, ref = NULL),
                        marker = TRUE,
                        colors,
                        icen = "median",
                        include = NULL, exclude = NULL,
                        legend,
                        log = FALSE,
                        grid = TRUE,
                        xlab, ylab,
                        title,
                        stats = TRUE,
                        xlim, ylim, ...) {
  if (inherits(x, "PM_cov")) {
    x <- x$data
  }

  # include/exclude
  # if (missing(include)) include <- unique(x$id)
  # if (missing(exclude)) exclude <- NA

  if (missing(formula)) {
    choices <- paste(x %>% select(!c(id, icen)) %>% names(), collapse = ", ")
    cli::cli_abort(c("x" = "Missing formula", "i" = "Use the form {.code y ~ x}.\fValues for {.var (x, y)} include: {.var {choices}}."))
  }


  vars <- names(get_all_vars(formula = formula, data = x))
  timearg <- "time" %in% vars

  if (!timearg) { # if time is not part of the formula, collapse to summary
    x <- summary.PM_cov(x, icen = icen) %>%
      includeExclude(include, exclude)
  } else { # time is part of formula, so select icen but don't summarize
    x <- x %>%
      filter(icen == !!icen) %>%
      includeExclude(include, exclude) %>%
      dplyr::arrange(id, time) %>%
      select(-icen)
  }

  # final data, calling columns x and y
  dat <- x %>% select(id, x = vars[2], y = vars[1])



  # process reference lines
  if (any(!names(line) %in% c("lm", "loess", "ref"))) {
    cli::cli_warn(c("!" = "{.code line} should be a list with at most three named elements: {.code lm}, {.code loess}, and/or {.code ref}.", "i" = "See {.fn Pmetrics::plot.PM_cov}."))
  }
  if (!is.list(line)) {
    cli::cli_warn(c("!" = "{.code line} should be a list.", "i" = "See {.fn Pmetrics::plot.PM_cov}."))
    line <- list()
  }

  # defaults
  if (is.null(line$lm)) {
    line$lm <- FALSE
  }
  if (is.null(line$loess)) {
    line$loess <- TRUE
  }
  if (is.null(line$ref)) {
    line$ref <- FALSE
  }

  marker <- amendMarker(marker, default = list(color = "orange"))
  lmLine <- amendLine(line$lm, default = list(color = "dodgerblue", dash = "solid"))
  loessLine <- amendLine(line$loess, default = list(color = "dodgerblue", dash = "dash"))
  refLine <- amendLine(line$ref, default = list(color = "grey", dash = "dot"))

  if (missing(colors)) {
    colors <- "Spectral"
  }

  if (is.logical(line$lm)) {
    lmLine$plot <- line$lm
  } else {
    lmLine$plot <- T
  }

  if (is.logical(line$loess)) {
    loessLine$plot <- line$loess
  } else {
    loessLine$plot <- T
  }

  if (is.logical(line$ref)) {
    refLine$plot <- line$ref
  } else {
    refLine$plot <- T
  }


  # process dots
  layout <- amendDots(list(...))

  # legend - not needed for this function
  layout <- modifyList(layout, list(showlegend = F))

  # grid
  layout$xaxis <- setGrid(layout$xaxis, grid)
  layout$yaxis <- setGrid(layout$yaxis, grid)

  # axis ranges
  if (!missing(xlim)) {
    layout$xaxis <- modifyList(layout$xaxis, list(range = xlim))
  }
  if (!missing(ylim)) {
    layout$yaxis <- modifyList(layout$yaxis, list(range = ylim))
  }

  # title
  if (missing(title)) {
    title <- ""
  }
  layout$title <- amendTitle(title, default = list(size = 20))

  # axis labels
  xlab <- if (missing(xlab)) {
    vars[2]
  } else {
    xlab
  }
  ylab <- if (missing(ylab)) {
    vars[1]
  } else {
    ylab
  }

  layout$xaxis$title <- amendTitle(xlab)
  if (is.character(ylab)) {
    layout$yaxis$title <- amendTitle(ylab, layout$xaxis$title$font)
  } else {
    layout$yaxis$title <- amendTitle(ylab)
  }

  # log axes
  if (log) {
    layout$xaxis <- modifyList(layout$xaxis, list(type = "log"))
    layout$yaxis <- modifyList(layout$yaxis, list(type = "log"))
  }

  # PLOTS -------------------------------------------------------------------
  if (!timearg) { # default plot

    p <- dat %>%
      plotly::plot_ly(x = ~x) %>%
      plotly::add_markers(
        y = ~y,
        marker = marker,
        text = ~id,
        hovertemplate = paste0(vars[2], ": %{x:.2f}<br>", vars[1], ": %{y:.2f}<br>ID: %{text}<extra></extra>")
      )

    if (lmLine$plot) { # linear regression
      lmLine$plot <- NULL # remove to allow only formatting arguments below
      if (is.null(purrr::pluck(lmLine$ci))) {
        ci <- 0.95
      } else {
        ci <- lmLine$ci
        lmLine$ci <- NULL # remove to allow only formatting arguments below
      }

      p <- p %>% add_smooth(x = ~x, y = ~y, ci = ci, line = lmLine, stats = stats)
    }

    if (loessLine$plot) { # loess regression
      loessLine$plot <- NULL # remove to allow only formatting arguments below
      if (is.null(purrr::pluck(loessLine$ci))) {
        ci <- 0.95
      } else {
        ci <- loessLine$ci
        loessLine$ci <- NULL # remove to allow only formatting arguments below
      }
      p <- p %>% add_smooth(x = ~x, y = ~y, ci = ci, line = loessLine, method = "loess")
    }

    if (refLine$plot) { # reference line
      refLine$plot <- NULL # remove to allow only formatting arguments below
      # get ref line
      layout$refLine <- list(
        type = "line",
        x0 = ~ max(min(x), min(y)),
        y0 = ~ max(min(x), min(y)),
        x1 = ~ min(max(x), max(y)),
        y1 = ~ min(max(x), max(y)),
        xref = "x",
        yref = "y",
        line = refLine
      )
    }

    # set layout
    p <- p %>%
      plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        showlegend = layout$showlegend,
        shapes = layout$refLine,
        title = layout$title
      )

    print(p)
    return(p)
  } else { # timearg plot
    marker$color <- NULL
    loessLine$color <- NULL

    n_colors <- length(unique(dat$id))
    if (checkRequiredPackages("RColorBrewer")) {
      palettes <- RColorBrewer::brewer.pal.info %>% mutate(name = rownames(.))
      max_colors <- palettes$maxcolors[match(colors, palettes$name)]
      # expand colors as needed
      if (all(colors %in% palettes$name)) {
        colors <- colorRampPalette(RColorBrewer::brewer.pal(max_colors, colors))(n_colors)
      } else {
        colors <- colorRampPalette(colors)(n_colors)
      }
    } else {
      cat(paste0(crayon::green("Note: "), "Colors are better with RColorBrewer package installed.\n"))
      colors <- getDefaultColors(n_colors) # in plotly_Utils
    }

    p <- dat %>%
      group_by(id) %>%
      plotly::plot_ly(
        x = ~x,
        color = ~ as.factor(id),
        colors = colors
      ) %>%
      plotly::add_markers(
        y = ~y,
        marker = marker,
        text = ~id,
        hovertemplate = paste0(vars[2], ": %{x:.2f}<br>", vars[1], ": %{y:.2f}<br>ID: %{text}<extra></extra>")
      ) %>%
      plotly::add_lines(
        y = ~y,
        line = loessLine
      )

    # set layout
    p <- p %>%
      plotly::layout(
        xaxis = layout$xaxis,
        yaxis = layout$yaxis,
        showlegend = layout$showlegend,
        shapes = layout$refLine,
        title = layout$title
      )

    print(p)
    return(p)
  }
}


# SUMMARY ------------------------------------------------------------------


#' @title Summarize Covariates and Bayesian Posterior Parameter Values
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Summarize a Pmetrics Covariate object
#'
#' @details This is a function usually called by the `$summary()` method for [PM_cov] objects
#' with a [PM_result] to summarize covariates and Bayesian posterior parameter
#' values for each subject. The function can
#' be called directly on a [PM_cov] object. See examples.Summarize .
#'
#' @method summary PM_cov
#' @param object A PM_cov object
#' @param icen Summary function for covariates with time dependent values and posterior parameters.
#' Default is "median", but can specify "mean".
#' @param ... Not used.
#' @return A data frame with the summary of the PM_cov object for each subject's covariates and
#' Bayesian posterior parameter values.
#' @author Michael Neely
#' @seealso [PM_cov]
#' @examples
#' \dontrun{
#' NPex$cov$summary() # preferred
#' summary(NPex$cov) # alternative
#' NPex$cov$summary(icen = "mean") # use mean as summary
#' }

#' @export

summary.PM_cov <- function(object, icen = "median", ...) {
  if (inherits(object, "PM_cov")) { # user called summary(PM_cov)
    object <- object$data
  }

  if ("icen" %in% names(object)) {
    data <- object %>%
      filter(icen == !!icen) %>%
      select(-icen)
  } else {
    data <- object
  }


  sumCov <- data %>%
    group_by(id) %>%
    summarise(across(c(-time, -block), ~ purrr::exec(icen, x = .x, na.rm = TRUE))) %>%
    mutate(icen = !!icen)

  return(sumCov)
}


# PM_step -----------------------------------------------------------------


#' @title Stepwise covariate-parameter regressions
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is largely superseded as it is accessed through the `$step` methods
#' for [PM_result] and [PM_cov] objects. There is rarely a need to call it directly
#' any longer.
#' @details It will perform stepwise linear regressions on a [PM_cov] object.
#' Every covariate in the model will be tested in a stepwise linear regression for their relationships
#' to each parameter in the model.  Bayesian posterior parameters and individual covariates are used.
#'
#' @param x A PMcov object which is the `$data` field of a [PM_cov] object
#' @param icen A character vector to summarize covariate values.  Default is "median", but can also be
#' "mean".
#' @param direction The direction for covariate elmination can be "backward", "forward", or "both" (default).
#' @return A matrix with covariates in the rows and parameters in the columns.  Values for the matrix are the multi-variate P-values.
#' A value of `NA` indicates that the variable was not retained in the final model.
#' @author Michael Neely
#' @seealso [stats::step()]
#' @export

PM_step <- function(x, icen = "median", direction = "both") {
  if (inherits(x, "PM_cov")) {
    x <- x$data
  }

  ncov <- attr(x, "ncov")
  if (is.null(ncov)) {
    ncov <- as.numeric(readline("Your covariate object is from a previous version of Pmetrics.  Enter the number of covariates: "))
  }
  if (ncov == 0) {
    cli::cli_abort("There are no covariates in the data.")
  }
  if (!"icen" %in% names(x)) {
    cli::cli_inform("Please update your PMcov object with {.fn PM_cov$new}.")
    x$icen <- icen
  }

  # summarize cov object by icen
  sumX <- summary.PM_cov(x, icen = icen)

  nvar <- ncol(sumX) - ncov - 2 # subtract id, icen
  # get start and end column numbers for covariates and par
  covStart <- 2
  covEnd <- 1 + ncov
  parStart <- covEnd + 1
  parEnd <- ncol(sumX) - 1 # leave out icen column


  cov.cross <- data.frame(matrix(NA, ncol = nvar, nrow = ncov, dimnames = list(cov = names(sumX)[covStart:covEnd], par = names(sumX)[parStart:parEnd])))

  for (i in 1:ncol(cov.cross)) {
    temp <- data.frame(cbind(sumX[, (parStart + i - 1)], sumX[, covStart:covEnd]))
    names(temp) <- c(names(sumX)[parStart + i - 1], names(sumX)[covStart:covEnd])
    fo <- as.formula(paste(names(temp)[1], " ~ ", paste(names(temp)[-1], collapse = " + "), sep = ""))
    lm.temp <- eval(substitute(lm(fo, temp)))
    step.temp <- step(lm.temp, direction = direction, trace = 0)
    p.val <- round(summary(step.temp)$coefficients[, 4], 3)
    cov.cross[, i] <- sapply(row.names(cov.cross), function(x) p.val[match(x, names(p.val))])
  }
  return(cov.cross)
}
