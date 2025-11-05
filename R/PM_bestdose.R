#' @title
#' Object to contain BestDose optimization results
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' This object is created after a successful BestDose optimization run.
#' BestDose finds optimal dosing regimens to achieve target drug concentrations
#' or AUC values using Bayesian optimization.
#'
#' @export
PM_bestdose <- R6::R6Class(
    "PM_bestdose",
    public = list(
        #' @field result A list containing:
        #' * **doses** Vector of optimal dose amounts (mg)
        #' * **objf** Objective function value (lower is better)
        #' * **status** Optimization status ("converged" or "max_iterations")
        #' * **predictions** Data frame with concentration-time predictions
        #' * **auc_predictions** Data frame with AUC predictions (if applicable)
        #' * **method** Optimization method used ("posterior" or "uniform")
        result = NULL,

        #' @description
        #' Create a new PM_bestdose object by running BestDose optimization
        #'
        #' @param prior A PM_result or PM_final object, or path to theta.csv file
        #' @param model A PM_model object or path to compiled model
        #' @param past_data Optional: PM_data object or path to CSV with patient history
        #' @param target PM_data object or path to CSV with target doses/observations
        #' @param dose_range Named list with min and max allowable doses, e.g.,
        #'   list(min = 0, max = 1000). Default: list(min = 0, max = 1000)
        #' @param bias_weight Numeric [0,1]. 0 = fully personalized, 1 = population-based.
        #'   Default = 0.5 (balanced)
        #' @param target_type One of "concentration", "auc_from_zero", or "auc_from_last_dose".
        #'   Default = "concentration"
        #' @param time_offset Optional: time offset for past/future concatenation
        #' @param settings Optional: list of settings (algorithm, error models, etc.)
        #'   If NULL, will use defaults from prior object
        #'
        #' @details
        #'
        #' ## Target Data Format
        #'
        #' The target data should be a Pmetrics-formatted CSV with:
        #' - Dose events: Set dose amount to 0 for doses to be optimized,
        #'   or non-zero for fixed doses
        #' - Observation events: Set OUT values to the target concentrations or AUCs
        #'
        #' ## Target Types
        #'
        #' - **concentration**: Optimize to match target concentrations at observation times
        #' - **auc_from_zero**: Optimize to match cumulative AUC from time 0
        #' - **auc_from_last_dose**: Optimize to match interval AUC from last dose
        #'
        #' ## Bias Weight
        #'
        #' Controls the balance between patient-specific and population-based optimization:
        #' - 0.0: Fully personalized (minimize variance)
        #' - 0.5: Balanced approach (recommended default)
        #' - 1.0: Population-based (minimize bias from population)
        #'
        #' @examples
        #' \dontrun{
        #' # Load NPAG result
        #' result <- PM_load(1)
        #'
        #' # Create target data
        #' target <- PM_data$new("target.csv")
        #'
        #' # Run BestDose optimization
        #' bd <- PM_bestdose$new(
        #'   prior = result,
        #'   model = result$model,
        #'   target = target,
        #'   dose_range = list(min = 50, max = 500),
        #'   bias_weight = 0.5
        #' )
        #'
        #' # View results
        #' print(bd)
        #' bd$get_doses()
        #' bd$get_predictions()
        #' }
        initialize = function(prior,
                              model,
                              past_data = NULL,
                              target,
                              dose_range = list(min = 0, max = 1000),
                              bias_weight = 0.5,
                              target_type = "concentration",
                              time_offset = NULL,
                              settings = NULL) {
            # Validate inputs
            if (!target_type %in% c("concentration", "auc_from_zero", "auc_from_last_dose")) {
                cli::cli_abort("target_type must be one of: concentration, auc_from_zero, auc_from_last_dose")
            }

            if (bias_weight < 0 || bias_weight > 1) {
                cli::cli_abort("bias_weight must be between 0 and 1")
            }

            if (is.null(dose_range$min) || is.null(dose_range$max)) {
                cli::cli_abort("dose_range must have both 'min' and 'max' elements")
            }

            if (dose_range$min >= dose_range$max) {
                cli::cli_abort("dose_range$min must be less than dose_range$max")
            }

            # Parse prior
            prior_path <- private$.parse_prior(prior)

            # Parse model
            model_info <- private$.parse_model(model)

            # Parse data files
            past_data_path <- if (!is.null(past_data)) {
                private$.parse_data(past_data)
            } else {
                NULL
            }

            target_data_path <- private$.parse_data(target)

            # Parse settings
            if (is.null(settings)) {
                settings <- private$.default_bestdose_settings(prior, model)
            }

            # Call Rust function
            cli::cli_alert_info("Running BestDose optimization...")

            result <- tryCatch(
                {
                    bestdose(
                        model_path = model_info$path,
                        prior_path = prior_path,
                        past_data_path = past_data_path,
                        target_data_path = target_data_path,
                        time_offset = time_offset,
                        dose_min = dose_range$min,
                        dose_max = dose_range$max,
                        bias_weight = bias_weight,
                        target_type = target_type,
                        params = settings,
                        kind = model_info$kind
                    )
                },
                error = function(e) {
                    cli::cli_abort(c(
                        "x" = "BestDose optimization failed",
                        "i" = conditionMessage(e)
                    ))
                }
            )

            self$result <- result

            cli::cli_alert_success("Optimization complete!")
            invisible(self)
        },

        #' @description
        #' Print summary of BestDose results
        print = function() {
            cat("BestDose Optimization Results\n")
            cat("==============================\n\n")
            cat(sprintf("Optimal doses: [%.2f, %.2f] mg\n", self$get_doses()[1], self$get_doses()[2]))
            cat(sprintf("Objective function: %.10f\n", self$get_objf()))
            cat(sprintf("ln(Objective): %.4f\n", log(self$get_objf())))
            cat(sprintf("Method: %s\n", self$get_method()))
            cat(sprintf("Status: %s\n", self$get_status()))
            cat(sprintf("\nNumber of predictions: %d\n", nrow(self$result$predictions)))
            if (!is.null(self$result$auc_predictions)) {
                cat(sprintf("Number of AUC predictions: %d\n", nrow(self$result$auc_predictions)))
            }
            invisible(self)
        },

        #' @description
        #' Get optimal dose values
        #' @return Numeric vector of optimal doses
        get_doses = function() {
            self$result$doses
        },

        #' @description
        #' Get concentration-time predictions
        #' @return Data frame with predictions
        get_predictions = function() {
            self$result$predictions
        },

        #' @description
        #' Get AUC predictions (if available)
        #' @return Data frame with AUC predictions or NULL
        get_auc_predictions = function() {
            self$result$auc_predictions
        },

        #' @description
        #' Get objective function value
        #' @return Numeric objective function value
        get_objf = function() {
            self$result$objf
        },

        #' @description
        #' Get optimization status
        #' @return Character string with status
        get_status = function() {
            self$result$status
        },

        #' @description
        #' Get optimization method used
        #' @return Character string: "posterior" or "uniform"
        get_method = function() {
            self$result$method
        },

        #' @description
        #' Save results to RDS file
        #' @param filename Path to save file. Default: "bestdose_result.rds"
        save = function(filename = "bestdose_result.rds") {
            saveRDS(self, filename)
            cli::cli_alert_success("Results saved to {filename}")
            invisible(self)
        }
    ),
    private = list(
        # Helper function to parse prior
        .parse_prior = function(prior) {
            if (inherits(prior, "PM_result")) {
                # Extract theta.csv from PM_result
                theta_path <- file.path(prior$rundir, "outputs", "theta.csv")
                if (!file.exists(theta_path)) {
                    cli::cli_abort("theta.csv not found in PM_result outputs")
                }
                return(theta_path)
            } else if (inherits(prior, "PM_final")) {
                # Need to write out PM_final to CSV
                temp_path <- tempfile(fileext = ".csv")
                private$.write_prior_csv(prior, temp_path)
                return(temp_path)
            } else if (is.character(prior)) {
                if (!file.exists(prior)) {
                    cli::cli_abort("Prior file not found: {prior}")
                }
                return(prior)
            } else {
                cli::cli_abort("prior must be PM_result, PM_final, or path to theta.csv")
            }
        },

        # Helper function to parse model
        .parse_model = function(model) {
            if (inherits(model, "PM_model")) {
                # Check if model is compiled
                compiled_path <- model$binary_path
                if (is.null(compiled_path) || !file.exists(compiled_path)) {
                    cli::cli_abort("Model must be compiled first. Use model$compile()")
                }

                # Determine model type from model_list
                kind <- if (!is.null(model$model_list$analytical) && model$model_list$analytical) {
                    "analytical"
                } else {
                    "ode"
                }

                return(list(
                    path = compiled_path,
                    kind = kind
                ))
            } else if (is.character(model)) {
                # Assume it's a path to compiled model
                if (!file.exists(model)) {
                    cli::cli_abort("Model file not found: {model}")
                }
                # Try to infer type from extension or filename
                kind <- if (grepl("analytical", model, ignore.case = TRUE)) {
                    "analytical"
                } else {
                    "ode"
                }
                return(list(path = model, kind = kind))
            } else {
                cli::cli_abort("model must be PM_model or path to compiled model")
            }
        },

        # Helper function to parse data
        .parse_data = function(data) {
            if (inherits(data, "PM_data")) {
                # Write to temp CSV
                temp_path <- tempfile(fileext = ".csv")
                write.csv(data$standard_data, temp_path, row.names = FALSE, quote = FALSE)
                return(temp_path)
            } else if (is.character(data)) {
                if (!file.exists(data)) {
                    cli::cli_abort("Data file not found: {data}")
                }
                return(data)
            } else {
                cli::cli_abort("data must be PM_data or path to CSV file")
            }
        },

        # Helper to create default settings
        .default_bestdose_settings = function(prior, model) {
            # Extract settings from prior if it's a PM_result
            if (inherits(prior, "PM_result")) {
                # Use the settings from the result
                settings <- prior$settings
                return(settings)
            } else {
                # Get parameter ranges from model (same format as in fit() method)
                param_ranges <- lapply(model$model_list$pri, function(x) {
                    c(x$min, x$max)
                })
                names(param_ranges) <- tolower(names(param_ranges))

                # Use sensible defaults
                settings <- list(
                    algorithm = "NPAG",
                    ranges = param_ranges,
                    error_models = list(
                        list(
                            initial = 0.0,
                            type = "additive",
                            coeff = c(0.0, 0.2, 0.0, 0.0)
                        )
                    ),
                    max_cycles = 500,
                    points = 2028,
                    seed = 22,
                    prior = "prior.csv",
                    idelta = 0.25, # 15 minutes for AUC calculations
                    tad = 0.0
                )
            }
            return(settings)
        },

        # Helper to write PM_final to CSV
        .write_prior_csv = function(prior, path) {
            # Convert PM_final to CSV with prob column
            df <- as.data.frame(prior$popPoints)
            df$prob <- prior$popProb
            write.csv(df, path, row.names = FALSE, quote = FALSE)
        }
    )
)

#' @export
PM_bestdose$load <- function(filename = "bestdose_result.rds") {
    if (!file.exists(filename)) {
        cli::cli_abort("File not found: {filename}")
    }
    readRDS(filename)
}
