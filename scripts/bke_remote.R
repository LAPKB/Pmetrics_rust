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
# Remote configuration:
#   Use `setPMoptions(launch.app = TRUE)` to point Pmetrics at your Hermes deployment.
#   The packaged defaults target http://localhost:8080, queue heavy-jobs, poll interval 2s,
#   timeout 3600s, TLS verification enabled, API alias hermes-bke, profile bke-example.
#
# Data source:
#   ../PMcore/examples/bimodal_ke/bimodal_ke.csv (same as bke.rs)

suppressPackageStartupMessages({
  library(cli)
  library(readr)
})
devtools::load_all("/Users/siel/code/LAPKB/Pmetrics_rust")

profile_cfg <- pm_remote_profile_config()
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
) # This returns a PM_result object via Hermes
