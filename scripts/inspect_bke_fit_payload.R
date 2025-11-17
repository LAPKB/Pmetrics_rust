suppressPackageStartupMessages({
    library(jsonlite)
    library(cli)
})

devtools::load_all("/Users/siel/code/LAPKB/Pmetrics_rust")

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
        additive(1, c(0.0, 0.5, 0.0, 0.0))
    )
)

mod <- PM_model$new(mod_list)

data_path <- file.path("..", "PMcore", "examples", "bimodal_ke", "bimodal_ke.csv")
if (!file.exists(data_path)) {
    stop(sprintf("Data not found at %s", normalizePath(data_path, mustWork = FALSE)))
}
data_obj <- PM_data$new(data_path, quiet = TRUE)

data_filtered <- data_obj$standard_data
inputs_dir <- tempfile(pattern = "pmfit_inputs_")
dir.create(inputs_dir, recursive = TRUE, showWarnings = FALSE)
gendata_path <- file.path(inputs_dir, "gendata.csv")
data_obj$save(gendata_path, header = FALSE)
model_rust <- mod$.__enclos_env__$private$render_model_to_rust()

ranges <- lapply(mod$model_list$pri, function(x) c(x$min, x$max))
names(ranges) <- tolower(names(ranges))
error_models <- lapply(mod$model_list$err, function(x) x$flatten())

settings <- list(
    ranges = ranges,
    algorithm = "NPAG",
    error_models = error_models,
    idelta = 0.1,
    tad = 0,
    max_cycles = 1000,
    prior = "sobol",
    points = 2028,
    seed = 22
)

cat(cli::col_green("Quick payload diff helper"), "\n", sep = "")
cat(cli::col_cyan("Rust model snippet:"), "\n", sep = "")
cat(substr(model_rust, 1, 200), "...\n\n", sep = "")

cat(cli::col_cyan("Settings JSON:"), "\n", sep = "")
cat(prettify(toJSON(settings, auto_unbox = TRUE, null = "null", digits = NA)))
cat("\n\n")

cat(cli::col_cyan("First five lines of gendata.csv:"), "\n", sep = "")
cat(paste(readLines(gendata_path, n = 5), collapse = "\n"))
cat("\n")
