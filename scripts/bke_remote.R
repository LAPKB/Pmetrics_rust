#!/usr/bin/env Rscript
#
# Bimodal ke remote fit example using PM_model/PM_fit + Hermes
# Reimplements hermes/hermes/examples/bke.rs with the high-level Pmetrics interfaces.
#
# Prerequisites:
#   * Pmetrics installed with the remote helpers (pm_remote_*)
#   * Hermes API reachable (defaults to http://localhost:8080)
#   * Either set PM_HERMES_API_KEY in your environment or store an API key in the
#     system keyring under the alias defined below (default "hermes-bke").
#
# Usage:
#   Rscript bke_remote.R
#
# Environment overrides:
#   HERMES_URL       Base URL for Hermes (default http://localhost:8080)
#   HERMES_QUEUE     Queue name (default heavy-jobs)
#   HERMES_POLL      Poll interval seconds (default 2)
#   HERMES_TIMEOUT   Request timeout seconds (default 3600)
#   HERMES_INSECURE  Set to 1 to skip TLS verification (local dev only)
#   HERMES_API_ALIAS Keyring alias to read (default hermes-bke)
#
# Data source:
#   ../PMcore/examples/bimodal_ke/bimodal_ke.csv (same as bke.rs)

suppressPackageStartupMessages({
  library(cli)
  library(readr)
})
devtools::load_all("/Users/siel/code/LAPKB/Pmetrics_rust")

# Resolve Hermes connection details ----------------------------------------
hermes_url <- Sys.getenv("HERMES_URL", "http://localhost:8080")
hermes_queue <- Sys.getenv("HERMES_QUEUE", "heavy-jobs")
hermes_poll <- as.numeric(Sys.getenv("HERMES_POLL", "2"))
hermes_timeout <- as.numeric(Sys.getenv("HERMES_TIMEOUT", "3600"))
hermes_verify <- !identical(Sys.getenv("HERMES_INSECURE", "0"), "1")
hermes_alias <- Sys.getenv("HERMES_API_ALIAS", "hermes-bke")

profile_cfg <- pm_remote_profile_config(config = list(
  profile_name = "bke-example",
  base_url = hermes_url,
  queue = hermes_queue,
  poll_interval_sec = hermes_poll,
  timeout_sec = hermes_timeout,
  verify_tls = hermes_verify,
  api_key_alias = hermes_alias
))
pm_remote_validate_profile_config(profile_cfg)

cli_inform(sprintf("Using Hermes profile %s (%s)", profile_cfg$profile_name, profile_cfg$base_url))
cli_inform("Set PM_HERMES_API_KEY or store a keyring secret under the alias above before running.")

# Build PM_model + PM_data objects ----------------------------------------
data_path <- file.path("PMcore", "examples", "bimodal_ke", "bimodal_ke.csv")
if (!file.exists(data_path)) {
  stop(sprintf("Unable to locate data file at %s", normalizePath(data_path, mustWork = FALSE)))
}

mod_list <- list(
  pri = c(
    ke = ab(0.001, 3.0),
    v = ab(25.0, 250.0)
  ),
  eqn = function() {
    dx[1] <- -ke * x[1] + rateiv[1]
  },
  out = function() {
    y[1] <- x[1] / v
  },
  err = c(
    additive(0, c(0.0, 0.5, 0.0, 0.0))
  )
)

mod <- PM_model$new(mod_list)
data_obj <- PM_data$new(data_path, quiet = TRUE)

fit_result <- mod$fit(
  data = data_obj,
  algorithm = "NPAG",
  idelta = 0.1,
  tad = 0,
  cycles = 1000,
  prior = "sobol",
  points = 2028,
  seed = 22,
  remote = TRUE,
  remote_config = profile_cfg
) # This returns a PM_result object via Hermes
print(fit_result$runInfo)
# fit_result$op$plot()

# # Stage working directories similar to PM_model$fit -----------------------
# run_root <- file.path("examples", "bke", "pmfit_remote")
# inputs_dir <- file.path(run_root, "inputs")
# outputs_dir <- file.path(run_root, "outputs")
# dir.create(inputs_dir, recursive = TRUE, showWarnings = FALSE)
# dir.create(outputs_dir, recursive = TRUE, showWarnings = FALSE)

# gendata_path <- file.path(inputs_dir, "gendata.csv")
# data_obj$save(gendata_path, header = FALSE)
# data_csv <- read_file(gendata_path)

# model_rust <- fit_obj$model$.__enclos_env__$private$render_model_to_rust()
# writeLines(model_rust, file.path(inputs_dir, "model.rs"))
# saveRDS(list(data = data_obj, model = mod), file = file.path(inputs_dir, "fit.rds"))

# # Prepare Hermes payload from fit configuration --------------------------
# ranges <- lapply(mod$model_list$pri, function(x) c(x$min, x$max))
# names(ranges) <- tolower(names(ranges))

# fit_params <- list(
#   ranges = ranges,
#   algorithm = "NPAG",
#   error_models = lapply(mod$model_list$err, function(x) x$flatten()),
#   idelta = 0.1,
#   tad = 0,
#   max_cycles = 1000,
#   prior = "sobol",
#   points = 2028,
#   seed = 22
# )

# payload <- pm_remote_build_payload(
#   model_txt = model_rust,
#   data_csv = data_csv,
#   settings = fit_params
# )

# # Submit job ---------------------------------------------------------------
# job <- pm_remote_enqueue(payload, config = profile_cfg)
# cli_inform(sprintf("Hermes job %s enqueued on %s", job$job_id, job$queue))

# status <- pm_remote_wait_for_job(job$job_id, config = profile_cfg)
# job_status <- status$status$status
# if (is.null(job_status)) {
#   job_status <- "(unknown)"
# }
# cli_inform(sprintf("Job %s finished with status: %s", job$job_id, job_status))

# result <- pm_remote_fetch_result(job$job_id, config = profile_cfg)
# if (is.null(result$result) || !isTRUE(result$result$success)) {
#   stop("Hermes job did not return a successful FitResult.")
# }

# # Materialize outputs + parse via Pmetrics ---------------------------------
# prepared <- pm_remote_prepare_result(result$result)
# pm_remote_write_outputs(prepared, outputs_dir)
# jsonlite::write_json(result, file.path(outputs_dir, "hermes_result.json"), pretty = TRUE, auto_unbox = TRUE)

# job_meta <- list(
#   job = job,
#   profile = profile_cfg[c("profile_name", "base_url", "queue")],
#   status = status$status,
#   history = status$history
# )
# jsonlite::write_json(job_meta, file.path(inputs_dir, "hermes_job.json"), pretty = TRUE, auto_unbox = TRUE)

# cli_inform(sprintf("Artifacts written to %s", normalizePath(run_root)))

# PM_parse(path = outputs_dir)
# fit_result <- PM_load(path = outputs_dir, file = "PMout.Rdata")
# cli_inform("PM_result successfully reconstructed from Hermes payload.")

# print(fit_result$runInfo)
