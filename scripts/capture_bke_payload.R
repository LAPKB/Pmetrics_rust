suppressPackageStartupMessages({
    library(readr)
    library(cli)
})
devtools::load_all("/Users/siel/code/LAPKB/Pmetrics_rust")

profile <- pm_remote_profile_config(config = list(
    profile_name = "bke-example",
    base_url = Sys.getenv("HERMES_URL", "http://localhost:8080"),
    queue = Sys.getenv("HERMES_QUEUE", "heavy-jobs"),
    poll_interval_sec = as.numeric(Sys.getenv("HERMES_POLL", "2")),
    timeout_sec = as.numeric(Sys.getenv("HERMES_TIMEOUT", "3600")),
    verify_tls = !identical(Sys.getenv("HERMES_INSECURE", "0"), "1"),
    api_key_alias = Sys.getenv("HERMES_API_ALIAS", "hermes-bke")
))

model_txt <- "equation::ODE::new(\n        |x, p, _t, dx, _b, rateiv, _cov| {\n            fetch_params!(p, ke, _v);\n            dx[0] = -ke * x[0] + rateiv[0];\n        },\n        |_p, _t, _cov| lag! {},\n        |_p, _t, _cov| fa! {},\n        |_p, _t, _cov, _x| {},\n        |x, p, _t, _cov, y| {\n            fetch_params!(p, _ke, v);\n            y[0] = x[0] / v;\n        },\n        (1, 1),\n    )"

data_csv <- read_file(file.path("PMcore", "examples", "bimodal_ke", "bimodal_ke.csv"))

settings <- list(
    ranges = list(
        ke = c(0.001, 3.0),
        v = c(25.0, 250.0)
    ),
    algorithm = "npag",
    error_models = list(
        list(type = "additive", initial = 0.0, coeff = c(0.0, 0.5, 0.0, 0.0)),
        list(type = "none", initial = 0.0)
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

job <- pm_remote_enqueue(payload, config = profile)
status <- pm_remote_wait_for_job(job$job_id, config = profile)
result <- pm_remote_fetch_result(job$job_id, config = profile)

saveRDS(
    list(job = job, status = status, result = result),
    file = "Pmetrics_rust/tmp_result.rds"
)

cli_inform(sprintf("Captured job %s outputs to Pmetrics_rust/tmp_result.rds", job$job_id))
