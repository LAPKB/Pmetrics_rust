#!/usr/bin/env Rscript
#
# Bimodal ke remote fit example using Pmetrics + Hermes
# This mirrors hermes/hermes/examples/bke.rs but drives the job from R instead of Rust.
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
  library(readr)
  library(cli)
})
devtools::load_all("/Users/siel/code/LAPKB/Pmetrics_rust")

# Resolve Hermes connection details ----------------------------------------
hermes_url <- Sys.getenv("HERMES_URL", "http://localhost:8080")
hermes_queue <- Sys.getenv("HERMES_QUEUE", "heavy-jobs")
hermes_poll <- as.numeric(Sys.getenv("HERMES_POLL", "2"))
hermes_timeout <- as.numeric(Sys.getenv("HERMES_TIMEOUT", "3600"))
hermes_verify <- !identical(Sys.getenv("HERMES_INSECURE", "0"), "1")
hermes_alias <- Sys.getenv("HERMES_API_ALIAS", "hermes-bke")

profile <- pm_remote_profile_config(config = list(
  profile_name = "bke-example",
  base_url = hermes_url,
  queue = hermes_queue,
  poll_interval_sec = hermes_poll,
  timeout_sec = hermes_timeout,
  verify_tls = hermes_verify,
  api_key_alias = hermes_alias
))
pm_remote_validate_profile_config(profile)

cli_inform(sprintf("Using Hermes profile %s (%s)", profile$profile_name, profile$base_url))
cli_inform("Set PM_HERMES_API_KEY or store a keyring secret under the alias above before running.")

# Prepare payload ----------------------------------------------------------
model_txt <- "equation::ODE::new(
        |x, p, _t, dx, _b, rateiv, _cov| {
            fetch_params!(p, ke, _v);
            dx[0] = -ke * x[0] + rateiv[0];
        },
        |_p, _t, _cov| lag! {},
        |_p, _t, _cov| fa! {},
        |_p, _t, _cov, _x| {},
        |x, p, _t, _cov, y| {
            fetch_params!(p, _ke, v);
            y[0] = x[0] / v;
        },
        (1, 1),
    )"

data_path <- file.path("PMcore", "examples", "bimodal_ke", "bimodal_ke.csv")
if (!file.exists(data_path)) {
  stop(sprintf("Unable to locate data file at %s", normalizePath(data_path, mustWork = FALSE)))
}
data_csv <- read_file(data_path)

settings <- list(
  ranges = list(
    ke = c(0.001, 3.0),
    v = c(25.0, 250.0)
  ),
  algorithm = "npag",
  error_models = list(
    list(type = "additive", initial = 0.0, coeff = c(0.0, 0.5, 0.0, 0.0))
  ),
  idelta = 0.1,
  tad = 0,
  max_cycles = 1000,
  prior = "sobol",
  points = 2028,
  seed = 22
)

payload <- pm_remote_build_payload(
  model_txt = model_txt,
  data_csv = data_csv,
  settings = settings
)

# Submit job ---------------------------------------------------------------
job <- pm_remote_enqueue(payload, config = profile)
cli_inform(sprintf("Hermes job %s enqueued on %s", job$job_id, job$queue))

status <- pm_remote_wait_for_job(job$job_id, config = profile)
job_status <- status$status$status
if (is.null(job_status)) {
  job_status <- "(unknown)"
}
cli_inform(sprintf("Job %s finished with status: %s", job$job_id, job_status))

result <- pm_remote_fetch_result(job$job_id, config = profile)
if (is.null(result$result) || !isTRUE(result$result$success)) {
  stop("Hermes job did not return a successful FitResult.")
}

# Materialize outputs + parse via Pmetrics ---------------------------------
output_dir <- file.path("examples", "bke", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
prepared <- pm_remote_prepare_result(result$result)
pm_remote_write_outputs(prepared, output_dir)
jsonlite::write_json(result, file.path(output_dir, "hermes_result.json"), pretty = TRUE, auto_unbox = TRUE)

cli_inform(sprintf("Artifacts written to %s", normalizePath(output_dir)))

PM_parse(path = output_dir)
fit <- PM_load(path = output_dir, file = "PMout.Rdata")
cli_inform("PM_result successfully reconstructed from Hermes payload.")

print(fit$runInfo)
