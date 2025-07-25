# Use menu item Code -> Jump To... for rapid navigation
# Keyboard Option+Command+O (Mac) or Alt+O (Windows) to fold all

# R6 ---------------------------------------------------------------


#' @title Results of a Pmetrics run
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' This object contains all of the results after a Pmetrics runs. It is created
#' by using the [PM_load] function.
#'
#' @details After a run completes, results are stored on your hard drive. They are loaded
#' back into R with [PM_load] to create the [PM_result] object, which contains both
#' the results and functions to analyze or plot the result.
#'
#' @author Michael Neely, Julian Otalvaro
#' @export
PM_result <- R6::R6Class(
  "PM_result",
  public <- list(
    #' @field pop A [PM_pop] object
    pop = NULL,
    #' @field post A [PM_post] object
    post = NULL,
    #' @field final A [PM_final] object
    final = NULL,
    #' @field cycle A [PM_cycle] object
    cycle = NULL,
    #' @field op A [PM_op] object
    op = NULL,
    #' @field cov A [PM_cov] object
    cov = NULL,
    #' @field data [PM_data] object representing the original .csv data file used in the run. The predictions contained in the `$data` fields from `$pop` and `$post` will be added to this [PM_data] object to permit easy addition of such predictions to raw data plots. See [plot.PM_data] for more details.
    data = NULL,
    #' @field model text string representing the original model file used in the run
    model = NULL,
    #' @field errfile Name of error file if it exists
    errfile = NULL,
    #' @field success Boolean if successful run
    success = NULL,
    #' @field valid If the `$validate` method has been executed after a run,
    #' this field will contain the information required to plot
    #' visual predictive checks and normalized prediction
    #' error discrepancies via the npde code developed by Comets et al. Use the
    #' `$save` method on the augmented `PM_result` object to save it with the
    #' new validation results.
    valid = NULL,
    #' @field opt_samp If the `$opt` method has been executed after a run, this
    #' field will contain a [PM_opt] object which has optimal sampling times
    #' and methods to plot them.
    #' Use the `$save` method on the augmented `PM_result` object to save it with the
    #' new optimal sampling results.
    opt_samp = NULL,

    #' @description
    #' Create new object populated with data from previous run
    #' @details
    #' Creation of new `PM_result` objects is via [PM_load].
    #' @param out The parsed output from [PM_load], which is
    #' automatically generated. This is not a user-modifiable.
    #' @param quiet Quietly validate. Default is `FALSE`.
    initialize = function(out, quiet = TRUE) {
      # the following were saved as R6 objects
      purrr::walk(
        c("pop", "post", "final", "cycle", "op", "cov", "data", "model", "valid"),
        \(x){
          self[[x]] <- NULL
          if (!is.null(out[[x]])) { # if the object is loaded...
            if (!inherits(out[[x]], "R6")) { # older save
              cli::cli_abort(c("x" = "The object was saved in an older format. Please re-run the analysis."))
            } else {
              if(x == "model"){
                args <- list(x  = out[[x]], compile = FALSE)
              } else { 
                args <- list(out[[x]], quiet = TRUE)
              }
              self[[x]] <- do.call(get(paste0("PM_", x))$new, args = args) # was saved in R6 format, but remake to update if needed
            }
          }
        }
      )

      # these are diagnostics, not R6
      self$errfile <- out$errfile
      self$success <- out$success

      # add the pop/post data to data
      if (is.null(self$data$pop) | is.null(self$data$post)) {
        self$data <- PM_data$new(self$data$data, quiet = TRUE)
        self$data$pop <- self$pop$data
        self$data$post <- self$post$data
      }

      return(self)
    },

    #' @description
    #' Plot generic function based on type
    #' @param type Type of plot based on class of object
    #' @param ... Plot-specific arguments
    plot = function(type, ...) {
      if (is.null(type)) {
        cli::cli_abort(c("x" = "Please provide the type of plot."))
      } else {
        self[[type]]$plot(...)
      }
    },

    #' @description
    #' Summary generic function based on type
    #' @param type Type of summary based on class of object
    #' @param ... Summary-specific arguments
    summary = function(type, ...) {
      if (is.null(type)) {
        cli::cli_abort(c("x" = "Please provide the type of summary you want to obtain"))
      } else {
        self[[type]]$summary(...)
      }
    },

    #' @description
    #' AUC generic function based on type
    #' @param type Type of AUC based on class of object
    #' @param ... Summary-specific arguments
    auc = function(type, ...) {
      if (!type %in% c("op", "pop", "post", "sim")) {
        cli::cli_abort(c("x" = "{.fn makeAUC} is defined only for {.cls PM_op}, {.cls PM_pop}, {.cls PM_post}, and {.cls PM_sim} objects."))
      }
      self[[type]]$auc(...)
    },

    #' @description
    #' Perform non-compartmental analysis
    #' @details
    #' See [makeNCA].
    #' @param ... Arguments passed to [makeNCA].
    nca = function(...) {
      make_NCA(self, ...)
    },
    #' @description Re-generate the report
    #' @param ... Parameters passed to [PM_report].
    report = function(...) {
      PM_report(...)
    },
    #' @description
    #' Calls [PM_sim]. Default is to use the `$final`, `$model`, and `$data` objects
    #' within the [PM_result]. It is common to supply a different `data` template.
    #' Occasionally it is necessary to use a different `model` with the `$final` field,
    #' or vice versa. If all three are different, use `PM_sim$new()` instead.
    #' @param ...  Parameters passed to [PM_sim]. If using the `$final`, `$model`, and
    #' `$data` fields, it is not necessary to specify these. Alternates for any of these
    #' should be specified. Other parameters for [PM_sim] should be passed as named
    #' arguments, e.g. `$sim(include = 1:2, predInt = 1, limits = NA)`.
    sim = function(...) {
      dots <- list(...)
      if (!"poppar" %in% names(dots)) {
        dots$poppar <- self$final
      }

      if (!"data" %in% names(dots)) {
        dots$data <- self$data$standard_data
      }

      if (!"model" %in% names(dots)) {
        dots$model <- self$model
      }

      # store copy of the final object
      bk_final <- self$final$clone()
      sim <- do.call(PM_sim$new, dots)
      self$final <- bk_final
      return(sim)
    },

    #' @description
    #' Save the current PM_result object to an .Rdata file.
    #' @details
    #' This is useful if you have updated the result in some way, for example you
    #' have run the `$make_valid` method on the `PM_result` object, which returns
    #' an internal simulation based validation as a new `valid` field. To save this
    #' validation, use this `$save` method. Note that unless a `file` name is provided,
    #' the changes will overwrite the
    #' previous run results, although unchanged items will be preserved. This is the
    #' usual workflow. However, a custom file name may be useful to share the run
    #' results with someone.
    #'
    #' The saved object is an .Rdata file. When loaded, it should be assigned to an R
    #' object, e.g. `run2 <- PM_result$new("filename")`. An equivalent statement would
    #' be `run2 <- PM_load(file = "filename")`.
    #' @param run The run output folder number to save the revised result. If missing,
    #' will save in the current working directory. For example, if folder "1" is in
    #' your current working directory, specify `run = 1` to save the result to the "outputs"
    #' subfolder of the "1" folder.
    #' @param file Custom file name. Default is "PMout.Rdata".
    save = function(run, file) {
      if (missing(run)) {
        outputfolder <- getwd()
      } else {
        if (is.na(suppressWarnings(as.numeric(run)))) {
          cli::cli_abort(c("x" = "The {.code run} argument is not numeric. Do you need to say {.code file = }? See help for {.fn PM_result}."))
        }
        outputfolder <- paste0(run, "/outputs")
        if (!file.exists(outputfolder)) {
          cli::cli_abort(c("x" = "{outputfolder} does not exist in the current working directory."))
        }
      }
      if (missing(file)) {
        file <- "PMout.Rdata"
      }
      PMout <- list(
        pop = self$pop$data, post = self$post$data,
        final = self$final$data, cycle = self$cycle$data,
        op = self$op$data, cov = self$cov$data, data = self$data$data,
        model = self$model, errfile = self$errfile,
        success = self$success,
        valid = self$valid
      )
      save(PMout, file = paste0(outputfolder, "/", file))
    },

    #' @description
    #' Validate the result by internal simulation methods.
    #' @param ... Arguments passed to [make_valid].
    validate = function(...) {
      self$valid <- PM_valid$new(self, ...)
      return(self)
    },

    #' @description
    #' Conduct stepwise linear regression of Bayesian posterior parameter values
    #' and covariates.
    #' @param ... Arguments passed to [PM_step].
    step = function(...) {
      PM_step(self$cov$data, ...)
    },

    #' @description
    #' Calculate optimal sampling times.
    #'
    #' Method to compute optimal sampling times.
    #' @details
    #' See [PM_opt] for details.
    #'
    #' @param ... Parameters to pass to [PM_opt].
    opt = function(...) {
      self$opt_samp <- tryCatch(PM_opt$new(self, ...), error = function(e) {
        cat(crayon::red("Error:"), e$message, "\n")
      })
      return(self)
    },

    #' @description
    #' `r lifecycle::badge("deprecated")`
    #'
    #' Deprecated method to load prior results saved with the `$save` method.
    #' Replaced by [PM_load].
    #' @param ... Not used.
    #' @keywords internal
    load = function(...) {
      lifecycle::deprecate_warn("2.1.0", "PM_result$load()", details = "PM_result$load() is deprecated. Please use PM_load() instead.")
    }
  ) # end public
) # end PM_result

#' @keywords internal
#' @name PM_result
#' @export
PM_result$load <- function(...) {
  lifecycle::deprecate_warn("2.1.0", "PM_result$load()", details = "Please use PM_load() instead. ?PM_load for details.")
}









# LOAD --------------------------------------------------------------------
#' @title Load Pmetrics NPAG or IT2B output
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Loads all the data from an \emph{NPAG} or \emph{IT2B} run.
#'
#'
#' @param run The numerical value of the folder number containing the run results.
#' Loading results of a prior standard run in folder "1" are as
#' simple as `run1 <- PM_load(1)`. There is no default value for this, and if
#' missing, Pmetrics will only search the current working directory for output files.
#' @param file Optional name of an .Rdata file created by running the
#' `$save` method for a [PM_result] object. For example,
#' `run2 <- PM_load(2, "other.Rdata")` will look in the run 2 folder outputs
#' for a file named "other.Rdata". `PM_load(file = "other.Rdata")` will look in the
#' current working directory, since `run` is missing. If `file` is missing,
#' Pmetrics will attempt to find a "PMout.Rdata" or the older "NPAGout.Rdata" or
#' "IT2Bout.Rdata" files in either the current working directory (if `run` is not
#' specified) or the `run/outputs` folder, if `run` is provided.
# #' @param remote Default is `FALSE`.  Set to `TRUE` if loading results of an NPAG run on remote server.
# #' See [NPrun]. Currently remote runs are not configured for IT2B or the Simulator.
# #' @param server_address If missing, will use the default server address returned by getPMoptions().
# #' Pmetrics will prompt the user to set this address the first time the `remote` argument is set to `TRUE`
#' in [NPrun].
#' @return An R6 [PM_result].
#' @author Michael Neely and Julian Otalvaro
#' @seealso [PM_final],
#' [PM_cycle], [PM_op], [PM_cov],
#' [PM_pop], [PM_post]
#' @export


PM_load <- function(run, file) {
  # internal function
  output2List <- function(Out) {
    result <- list()
    for (i in 1:length(Out)) {
      aux_list <- list(Out[[i]])
      names(aux_list) <- names(Out)[i]
      result <- append(result, aux_list)
    }

    return(result)
  }

  # if (remote) { # only look on server - this needs to be updated
  #   if (missing(server_address)) server_address <- getPMoptions("server_address")
  #   status <- .remoteLoad(thisrun, server_address)
  #   if (status == "finished") {
  #     result <- output2List(Out = NPAGout)
  #     return(PM_result$new(result, quiet = T)) # no errors
  #   } else {
  #     sprintf("Warning: Remote run #%d has not finished yet.\nCurrent status: \"%s\"\n", thisrun, status) %>%
  #       cat()
  #     return(invisible(NULL))
  #   }
  # } else

  if (!missing(file)) { # not remote, file supplied, so look for it
    # try from current wd
    if (file.exists(file)) {
      found <- file
    } else {
      # nope, try in an outputs folder
      if (!missing(run)) {
        file <- paste0(run, "/outputs/", file)
        if (file.exists(file)) {
          found <- file
        }
      }
    }
  } else { # didn't have file, so check for other outputs
    if (missing(run)) {
      wd <- "./" # we can only look in current directory
    } else {
      wd <- paste0(run, "/outputs/")
    }
    file_list <- c("PMout.Rdata", "NPAGout.Rdata", "IT2Bout.Rdata")

    for (i in file_list) {
      file <- paste0(wd, i)
      if (file.exists(file)) {
        found <- file
        break
      }
    }
  }

  if (found != "") {
    result <- output2List(Out = get(load(found)))
    # update
    result2 <- result # update(result, found)
    # In order to rebuild correctly the wd must be set to inside the outputs folder
    cwd <- getwd()
    setwd(dirname(found))
    rebuild <- PM_result$new(result2, quiet = TRUE)
    setwd(cwd)
    return(rebuild)
  } else {
    cli::cli_abort(c("x" = "No Pmetrics output file found in {getwd()}."))
  }
}


# internal update function
update <- function(res, found) {
  return(invisible(NULL)) # remove when cycle.csv updated
  msg <- NULL
  # CYCLE
  if (!is.null(res$cycle)) {
    dat <- res$cycle
    if (
      !tibble::is_tibble(dat$gamlam) # version prior to 2.2, add next update via or join
    ) {
      # start conversion
      n_cyc <- nrow(dat$mean)
      n_out <- max(res$op$outeq)
      dat$gamlam <- dat$gamlam %>% select(starts_with("add")|starts_with("prop"))
      if (ncol(gamlam) == 1 & n_out > 1) {
        gamlam <- cbind(gamlam, replicate((n_out - 1), gamlam[, 1]))
      }
      gamlam <- gamlam %>%
      pivot_longer(
        cols = everything(),
        values_to = "value", names_to = c("type", "outeq"), 
        names_sep = "\\."
      ) %>%
      mutate(cycle = rep(1:n_cyc, each = n_out)) %>%
      select(cycle, value, outeq, type) %>% arrange(cycle, outeq)
      if (is.matrix(dat$mean)) { # old fortran format, but not rust format
        dat$mean <- tibble::tibble(cycle = 1:n_cyc) %>%
          dplyr::bind_cols(tidyr::as_tibble(dat$mean))
        dat$median <- tibble::tibble(cycle = 1:n_cyc) %>%
          dplyr::bind_cols(tidyr::as_tibble(dat$median))
        dat$sd <- tibble::tibble(cycle = 1:n_cyc) %>%
          dplyr::bind_cols(tidyr::as_tibble(dat$sd))
      }
      msg <- c(msg, "cycle")
      res$cycle <- dat
    }
  }

  ####### DONE PROCESSING, INFORM #########
  if (!is.null(msg)) {
    cat(
      crayon::blue("NOTE: "),
      "The",
      crayon::green(dplyr::case_when(
        length(msg) == 1 ~ msg,
        length(msg) == 2 ~ paste(msg, collapse = " and "),
        length(msg) > 2 ~ paste(msg, collapse = ", ")
      )[1]),
      ifelse(length(msg) > 1, "fields", "field"),
      "in your PM_result object",
      ifelse(length(msg) > 1, "have", "has"),
      "been updated",
      "to the most current format.",
      "\n\n",
      crayon::blue("1"), "Save the updates\n",
      crayon::blue("2"), "Do not save updates\n "
    )
    flush.console()
    ans <- readline(" ")
    if (ans == 1) {
      temp <- PM_result$new(res)
      temp$save(file = found)
      cat("Results saved\n")
    }
  }

  return(res)
}
