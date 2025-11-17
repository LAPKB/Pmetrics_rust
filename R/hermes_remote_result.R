pm_remote_prepare_result <- function(result) {
    if (is.null(result)) {
        cli::cli_abort(c("x" = "Hermes response did not include a fit result."))
    }

    theta <- result$theta
    if (is.null(theta) && !is.null(result$theta_json)) {
        theta <- jsonlite::fromJSON(result$theta_json)
    }
    theta <- pm_remote_as_matrix(theta, "theta")

    weights <- result$weights
    if (is.null(weights)) {
        cli::cli_abort(c("x" = "Hermes result is missing support point weights."))
    }

    parameter_names <- result$parameter_names
    if (is.null(parameter_names) && !is.null(colnames(theta))) {
        parameter_names <- colnames(theta)
    }
    if (is.null(parameter_names)) {
        parameter_names <- paste0("param_", seq_len(ncol(theta)))
    }
    parameter_names <- as.character(parameter_names)
    if (!is.null(parameter_names) && length(parameter_names) != ncol(theta)) {
        cli::cli_abort(c(
            "x" = sprintf(
                "Theta columns (%d) do not match parameter names (%d).",
                ncol(theta),
                length(parameter_names)
            )
        ))
    }

    posterior <- result$posterior
    if (is.null(posterior) && !is.null(result$posterior_json)) {
        posterior <- jsonlite::fromJSON(result$posterior_json)
    }
    posterior <- pm_remote_as_matrix(posterior, "posterior")

    posterior_subject_ids <- result$posterior_subject_ids
    if (!is.null(posterior_subject_ids)) {
        posterior_subject_ids <- as.character(posterior_subject_ids)
    }
    if (is.null(posterior_subject_ids) && !is.null(posterior)) {
        cli::cli_abort(c("x" = "Hermes result is missing posterior subject ids."))
    }
    if (!is.null(posterior) && length(posterior_subject_ids) != nrow(posterior)) {
        cli::cli_abort(c(
            "x" = sprintf(
                "Posterior rows (%d) do not match subject ids (%d).",
                nrow(posterior),
                length(posterior_subject_ids)
            )
        ))
    }

    predictions <- result$predictions
    if (is.null(predictions) && !is.null(result$predictions_json)) {
        predictions <- jsonlite::fromJSON(result$predictions_json, simplifyDataFrame = TRUE)
    }
    predictions_df <- pm_remote_predictions_df(predictions)

    covariates <- result$covariates
    covariates_df <- pm_remote_covariates_df(covariates)

    settings <- result$settings_canonical
    if (is.null(settings) && !is.null(result$settings_json)) {
        settings <- jsonlite::fromJSON(result$settings_json, simplifyVector = TRUE)
    }
    if (is.null(settings)) {
        settings <- list()
    }

    cycle_log <- result$cycle_log
    if (!is.null(cycle_log) && !is.list(cycle_log)) {
        cycle_log <- jsonlite::fromJSON(cycle_log, simplifyVector = FALSE)
    }

    n_outputs <- NULL
    if (!is.null(predictions_df) && nrow(predictions_df) > 0 && "outeq" %in% names(predictions_df)) {
        n_outputs <- suppressWarnings(max(predictions_df$outeq, na.rm = TRUE))
        if (is.finite(n_outputs)) {
            n_outputs <- as.integer(n_outputs) + 1L
        } else {
            n_outputs <- NULL
        }
    }
    if (is.null(n_outputs) && !is.null(cycle_log)) {
        entries <- pm_remote_cycle_entries(cycle_log)
        if (length(entries) > 0) {
            first_models <- entries[[1]]$error_models
            if (!is.null(first_models)) {
                models <- first_models$models %||% first_models
                n_outputs <- length(models)
            }
        }
    }

    list(
        success = isTRUE(result$success),
        cycles_completed = result$cycles_completed %||% NA_integer_,
        objective_value = result$objective_value %||% NA_real_,
        converged = isTRUE(result$converged),
        theta = theta,
        weights = as.numeric(weights),
        parameter_names = parameter_names,
        posterior = posterior,
        posterior_subject_ids = posterior_subject_ids,
        predictions = predictions_df,
        covariates = covariates_df,
        settings = settings,
        cycle_log = cycle_log,
        n_outputs = n_outputs
    )
}

pm_remote_write_outputs <- function(prepared, out_dir) {
    pm_remote_write_theta(prepared$theta, prepared$weights, prepared$parameter_names, out_dir)
    pm_remote_write_posterior(
        prepared$theta,
        prepared$posterior,
        prepared$posterior_subject_ids,
        prepared$parameter_names,
        out_dir
    )
    pm_remote_write_predictions(prepared$predictions, out_dir)
    pm_remote_write_covariates(prepared$covariates, out_dir)
    pm_remote_write_cycles(
        prepared$cycle_log,
        prepared$parameter_names,
        prepared$n_outputs,
        out_dir
    )
    pm_remote_write_settings(prepared$settings, out_dir)
}

pm_remote_write_theta <- function(theta, weights, parameter_names, out_dir) {
    if (is.null(theta) || nrow(theta) == 0) {
        cli::cli_abort(c("x" = "Hermes result did not include theta support points."))
    }
    df <- as.data.frame(theta, stringsAsFactors = FALSE)
    colnames(df) <- parameter_names
    df$prob <- as.numeric(weights)
    readr::write_csv(df, file.path(out_dir, "theta.csv"), na = "")
}

pm_remote_write_posterior <- function(theta, posterior, subject_ids, parameter_names, out_dir) {
    if (is.null(posterior) || length(subject_ids) == 0) {
        cli::cli_abort(c("x" = "Hermes result did not include posterior probabilities."))
    }
    theta_df <- as.data.frame(theta, stringsAsFactors = FALSE)
    colnames(theta_df) <- parameter_names
    point_index <- seq_len(nrow(theta_df)) - 1L

    rows <- lapply(seq_along(subject_ids), function(idx) {
        subject <- subject_ids[[idx]]
        probs <- posterior[idx, ]
        df <- theta_df
        df$id <- subject
        df$point <- point_index
        df$prob <- probs
        df[c("id", "point", parameter_names, "prob")]
    })

    posterior_df <- dplyr::bind_rows(rows)
    posterior_df <- posterior_df[c("id", "point", parameter_names, "prob")]
    readr::write_csv(posterior_df, file.path(out_dir, "posterior.csv"), na = "")
}

pm_remote_write_predictions <- function(predictions, out_dir) {
    if (is.null(predictions)) {
        cli::cli_abort(c("x" = "Hermes result did not include predictions."))
    }
    expected_cols <- c(
        "id",
        "time",
        "outeq",
        "block",
        "obs",
        "censoring",
        "pop_mean",
        "pop_median",
        "post_mean",
        "post_median"
    )
    missing <- setdiff(expected_cols, names(predictions))
    if ("censoring" %in% missing && "cens" %in% names(predictions)) {
        predictions$censoring <- predictions$cens
        missing <- setdiff(missing, "censoring")
    }
    if (length(missing) > 0) {
        cli::cli_abort(c(
            "x" = sprintf(
                "Hermes predictions are missing required columns: %s",
                paste(missing, collapse = ", ")
            )
        ))
    }

    predictions$outeq <- as.integer(predictions$outeq)
    predictions$block <- as.integer(predictions$block)
    predictions$time <- as.numeric(predictions$time)
    if ("obs" %in% names(predictions)) {
        predictions$obs <- suppressWarnings(as.numeric(predictions$obs))
    }
    order_idx <- order(predictions$id, predictions$time, predictions$outeq, predictions$block)
    predictions <- predictions[order_idx, , drop = FALSE]
    output_cols <- c(
        "id",
        "time",
        "outeq",
        "block",
        "obs",
        "censoring",
        "pop_mean",
        "pop_median",
        "post_mean",
        "post_median"
    )
    readr::write_csv(predictions[, output_cols, drop = FALSE], file.path(out_dir, "pred.csv"), na = "")
}

pm_remote_write_covariates <- function(covariates, out_dir) {
    cov_path <- file.path(out_dir, "covs.csv")
    if (is.null(covariates) || nrow(covariates) == 0) {
        readr::write_csv(
            tibble::tibble(id = character(), time = numeric(), block = integer()),
            cov_path,
            na = ""
        )
        return(invisible(NULL))
    }
    readr::write_csv(covariates, cov_path, na = "")
}

pm_remote_write_cycles <- function(cycle_log, parameter_names, n_outputs, out_dir) {
    cycle_path <- file.path(out_dir, "cycles.csv")
    entries <- pm_remote_cycle_entries(cycle_log)
    if (length(entries) == 0) {
        empty_df <- pm_remote_empty_cycle_df(parameter_names, n_outputs)
        readr::write_csv(empty_df, cycle_path, na = "")
        return(invisible(NULL))
    }

    cycle_df <- pm_remote_cycle_df(entries, parameter_names, n_outputs)
    readr::write_csv(cycle_df, cycle_path, na = "")
}

pm_remote_write_settings <- function(settings, out_dir) {
    settings_path <- file.path(out_dir, "settings.json")
    jsonlite::write_json(settings, settings_path, pretty = TRUE, auto_unbox = TRUE, na = "null")
}

pm_remote_cycle_entries <- function(cycle_log) {
    if (is.null(cycle_log)) {
        return(list())
    }
    entries <- cycle_log$cycles %||% cycle_log
    if (is.data.frame(entries)) {
        return(pm_remote_cycle_entries_from_df(entries))
    }
    entries
}

pm_remote_cycle_entries_from_df <- function(cycle_df) {
    if (nrow(cycle_df) == 0) {
        return(list())
    }
    lapply(seq_len(nrow(cycle_df)), function(idx) {
        pm_remote_cycle_row_as_list(cycle_df, idx)
    })
}

pm_remote_cycle_row_as_list <- function(df, idx) {
    row <- lapply(df, function(column) {
        pm_remote_cycle_cell_value(column, idx)
    })
    names(row) <- names(df)
    row
}

pm_remote_cycle_cell_value <- function(column, idx) {
    if (is.null(column)) {
        return(NULL)
    }
    if (is.data.frame(column)) {
        if (nrow(column) == 0) {
            return(list())
        }
        return(pm_remote_cycle_row_as_list(column, idx))
    }
    if (is.list(column)) {
        return(column[[idx]])
    }
    column[[idx]]
}

pm_remote_cycle_df <- function(entries, parameter_names, n_outputs_hint) {
    n_outputs <- n_outputs_hint %||% pm_remote_detect_outputs_from_cycle(entries)
    gamlam_names <- if (is.null(n_outputs) || n_outputs == 0) {
        character()
    } else {
        paste0("gamlam.", seq_len(n_outputs) - 1L)
    }

    mean_names <- paste0(parameter_names, ".mean")
    median_names <- paste0(parameter_names, ".median")
    sd_names <- paste0(parameter_names, ".sd")

    rows <- lapply(entries, function(entry) {
        status <- pm_remote_cycle_status_label(entry$status)
        converged <- pm_remote_is_converged_status(status)
        neg2ll <- entry$objf %||% NA_real_
        nspp <- entry$nspp %||% NA_real_

        gamlam_vals <- pm_remote_cycle_gamlam(entry$error_models, length(gamlam_names))
        param_stats <- pm_remote_cycle_parameter_stats(entry$theta, parameter_names)

        row <- c(
            list(
                cycle = as.integer(entry$cycle %||% NA_integer_),
                converged = converged,
                status = status,
                neg2ll = as.numeric(neg2ll),
                nspp = as.integer(nspp)
            ),
            as.list(gamlam_vals),
            as.list(param_stats$mean),
            as.list(param_stats$median),
            as.list(param_stats$sd)
        )
        row
    })

    cycle_df <- dplyr::bind_rows(rows)
    expected_cols <- c("cycle", "converged", "status", "neg2ll", "nspp", gamlam_names, mean_names, median_names, sd_names)
    missing_cols <- setdiff(expected_cols, names(cycle_df))
    if (length(missing_cols) > 0) {
        for (col in missing_cols) {
            cycle_df[[col]] <- switch(col,
                cycle = rep(NA_integer_, nrow(cycle_df)),
                nspp = rep(NA_integer_, nrow(cycle_df)),
                converged = rep(NA, nrow(cycle_df)),
                status = rep(NA_character_, nrow(cycle_df)),
                rep(NA_real_, nrow(cycle_df))
            )
        }
    }
    cycle_df <- cycle_df[expected_cols]
    cycle_df
}

pm_remote_cycle_gamlam <- function(error_models, n_outputs) {
    if (n_outputs == 0) {
        return(numeric(0))
    }
    values <- rep(NA_real_, n_outputs)
    names(values) <- paste0("gamlam.", seq_len(n_outputs) - 1L)
    if (is.null(error_models)) {
        return(values)
    }
    models <- error_models$models %||% error_models
    for (idx in seq_len(min(length(models), n_outputs))) {
        values[idx] <- pm_remote_error_model_factor(models[[idx]])
    }
    values
}

pm_remote_cycle_parameter_stats <- function(theta_entry, parameter_names) {
    theta_matrix <- NULL
    if (is.list(theta_entry) && !is.null(theta_entry$matrix)) {
        theta_matrix <- theta_entry$matrix
    } else {
        theta_matrix <- theta_entry
    }
    theta_matrix <- pm_remote_as_matrix(theta_matrix, "cycle theta")
    if (is.null(theta_matrix) || ncol(theta_matrix) == 0) {
        na_vec <- rep(NA_real_, length(parameter_names))
        names(na_vec) <- parameter_names
        return(list(
            mean = setNames(na_vec, paste0(parameter_names, ".mean")),
            median = setNames(na_vec, paste0(parameter_names, ".median")),
            sd = setNames(na_vec, paste0(parameter_names, ".sd"))
        ))
    }
    colnames(theta_matrix) <- parameter_names
    mean_vals <- colMeans(theta_matrix)
    median_vals <- apply(theta_matrix, 2, stats::median)
    sd_vals <- apply(theta_matrix, 2, pm_remote_sample_sd)
    list(
        mean = setNames(mean_vals, paste0(parameter_names, ".mean")),
        median = setNames(median_vals, paste0(parameter_names, ".median")),
        sd = setNames(sd_vals, paste0(parameter_names, ".sd"))
    )
}

pm_remote_sample_sd <- function(x) {
    x <- as.numeric(x)
    n <- length(x)
    if (n <= 1) {
        return(NA_real_)
    }
    stats::sd(x)
}

pm_remote_error_model_factor <- function(model_entry) {
    if (is.null(model_entry)) {
        return(NA_real_)
    }
    if (!is.null(model_entry$Additive)) {
        lambda <- model_entry$Additive$lambda
        return(pm_remote_extract_scalar(lambda))
    }
    if (!is.null(model_entry$Proportional)) {
        gamma <- model_entry$Proportional$gamma
        return(pm_remote_extract_scalar(gamma))
    }
    if (!is.null(model_entry$None)) {
        return(NA_real_)
    }
    pm_remote_extract_scalar(model_entry)
}

pm_remote_extract_scalar <- function(value) {
    if (is.null(value)) {
        return(NA_real_)
    }
    if (is.list(value)) {
        for (key in c("Variable", "Fixed", "value")) {
            if (!is.null(value[[key]])) {
                return(as.numeric(value[[key]]))
            }
        }
        flat <- unlist(value, recursive = TRUE, use.names = FALSE)
        if (length(flat) > 0) {
            return(as.numeric(flat[[1]]))
        }
        return(NA_real_)
    }
    as.numeric(value)
}

pm_remote_cycle_status_label <- function(status) {
    if (is.null(status)) {
        return("")
    }
    if (is.character(status) && length(status) == 1) {
        return(status)
    }
    if (is.list(status)) {
        if (!is.null(status$Stop)) {
            stop_reason <- status$Stop
            if (is.list(stop_reason) && length(stop_reason) == 1) {
                stop_reason <- stop_reason[[1]]
            }
            return(paste0("Stop: ", stop_reason))
        }
        if (!is.null(status$Continue)) {
            return("Continue")
        }
    }
    as.character(status)
}

pm_remote_is_converged_status <- function(status) {
    if (is.null(status) || !nzchar(status)) {
        return(FALSE)
    }
    grepl("converged", status, ignore.case = TRUE)
}

pm_remote_detect_outputs_from_cycle <- function(entries) {
    for (entry in entries) {
        models <- entry$error_models
        if (!is.null(models)) {
            models <- models$models %||% models
            return(length(models))
        }
    }
    NULL
}

pm_remote_empty_cycle_df <- function(parameter_names, n_outputs) {
    n_outputs <- n_outputs %||% 0L
    gamlam_names <- if (n_outputs > 0) paste0("gamlam.", seq_len(n_outputs) - 1L) else character()
    mean_names <- paste0(parameter_names, ".mean")
    median_names <- paste0(parameter_names, ".median")
    sd_names <- paste0(parameter_names, ".sd")
    cols <- c("cycle", "converged", "status", "neg2ll", "nspp", gamlam_names, mean_names, median_names, sd_names)
    empty_cols <- setNames(lapply(cols, function(col_name) {
        if (col_name %in% c("cycle", "nspp")) {
            integer(0)
        } else if (col_name == "converged") {
            logical(0)
        } else if (col_name == "status") {
            character(0)
        } else {
            numeric(0)
        }
    }), cols)
    tibble::as_tibble(empty_cols)
}

pm_remote_as_matrix <- function(value, label) {
    if (is.null(value)) {
        return(NULL)
    }
    if (is.matrix(value)) {
        return(value)
    }
    if (is.data.frame(value)) {
        return(as.matrix(value))
    }
    if (is.list(value)) {
        row_list <- lapply(value, function(row) {
            if (is.list(row)) {
                unlist(row, recursive = FALSE, use.names = FALSE)
            } else {
                row
            }
        })
        suppressWarnings({
            mat <- do.call(rbind, row_list)
        })
        if (is.null(mat)) {
            cli::cli_abort(c("x" = "Unable to coerce {label} to a numeric matrix."))
        }
        if (!is.matrix(mat)) {
            mat <- as.matrix(mat)
        }
        mode(mat) <- "numeric"
        return(mat)
    }
    cli::cli_abort(c("x" = "Unsupported {label} representation from Hermes."))
}

pm_remote_predictions_df <- function(predictions) {
    if (is.null(predictions)) {
        return(NULL)
    }
    if (inherits(predictions, "data.frame")) {
        return(predictions)
    }
    if (is.list(predictions)) {
        df <- dplyr::bind_rows(predictions)
        return(df)
    }
    cli::cli_abort(c("x" = "Unable to coerce predictions into a data frame."))
}

pm_remote_covariates_df <- function(covariates) {
    if (is.null(covariates)) {
        return(NULL)
    }
    if (inherits(covariates, "data.frame")) {
        if (nrow(covariates) == 0) {
            return(tibble::tibble())
        }
        order_idx <- order(covariates$id, covariates$time, covariates$block)
        return(covariates[order_idx, , drop = FALSE])
    }
    if (length(covariates) == 0) {
        return(tibble::tibble())
    }
    cov_names <- unique(unlist(lapply(covariates, function(row) names(row$covariates))))
    cov_names <- sort(cov_names)
    rows <- lapply(covariates, function(row) {
        cov_values <- vapply(cov_names, function(name) {
            value <- row$covariates[[name]]
            if (is.null(value) || (is.na(value) && length(value) == 1)) {
                NA_real_
            } else {
                as.numeric(value)
            }
        }, numeric(1))
        data.frame(
            id = row$id,
            time = as.numeric(row$time),
            block = as.integer(row$block),
            t(cov_values),
            check.names = FALSE,
            stringsAsFactors = FALSE
        )
    })
    df <- dplyr::bind_rows(rows)
    order_idx <- order(df$id, df$time, df$block)
    df[order_idx, , drop = FALSE]
}
