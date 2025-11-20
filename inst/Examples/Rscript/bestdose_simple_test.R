library(Pmetrics)
devtools::load_all()

setwd("inst/Examples/Runs")

mod_onecomp <- PM_model$new(
  pri = list(
    ke = ab(0.001, 3.0),
    v = ab(25.0, 250.0)
  ),
  eqn = function() {
    dx[1] <- -ke * X[1] + B[1]
  },
  out = function() {
    Y[1] <- X[1] / v
  },
  err = list(
    additive(1, c(0, 0.20, 0, 0))
  )
)


past_file <- "inst/Examples/src/bestdose_past.csv"
target_file <- "inst/Examples/src/bestdose_target.csv"
prior_file <- "inst/Examples/src/bestdose_prior.csv"


# Prepare the problem once (posterior + handle to optimized model)
problem <- PM_bestdose_problem$new(
  prior = prior_file,
  model = mod_onecomp,
  past_data = past_file,
  target = target_file,
  dose_range = list(min = 0, max = 300),
  bias_weight = 0.0,
  target_type = "concentration" # "concentration", "auc_from_zero", "auc_from_last_dose"
)

cat("\nPosterior support points:\n")
print(head(problem$theta))

# Reuse the same problem for different bias weights
bias_weights <- seq(0, 1, by = 0.25)
results <- lapply(bias_weights, function(lambda) {
  problem$optimize(bias_weight = lambda)
})

for (i in seq_along(results)) {
  cat("\n=== Bias weight:", bias_weights[i], "===\n")
  results[[i]]$print()
}


# ==============================================================================
# 10. Custom VANCOMYCIN MODEL TEST
# ==============================================================================


# past data
past_file <- "inst/Examples/src/vanco_test.csv"
target_file <- "inst/Examples/src/vanco_target.csv"
prior_file <- "inst/Examples/src/vanco_prior.csv"


# model
vanco_2cp <- PM_model$new(
  pri = list(
    ke = ab(0.001, 3.0),
    v = ab(25.0, 250.0),
    kcp = ab(0.001, 3.0),
    kpc = ab(0.001, 3.0)
  ),
  eqn = function() {
    dx[1] <- rateiv[1] - ke * X[1] + kpc * X[2] - kcp * X[1]
    dx[2] <- -kpc * X[2] + kcp * X[1]
  },
  out = function() {
    Y[1] <- X[1] / v
  },
  err = list(
    additive(1, c(0, 0.20, 0, 0))
  )
)

# fit vanco

problem_vanco <- PM_bestdose_problem$new(
  prior = prior_file,
  model = vanco_2cp,
  past_data = past_file,
  target = target_file,
  dose_range = list(min = 0, max = 300),
  bias_weight = 0.0,
  target_type = "concentration" # "concentration", "auc_from_zero", "auc_from_last_dose"
)

problem_vanco$theta
problem_vanco$population_weights
problem_vanco$posterior_weights

posterio_df <- cbind(
  problem_vanco$theta,
  prior_weight = problem_vanco$population_weights,
  posterior_weight = problem_vanco$posterior_weights
)


vanco_optim <- problem_vanco$optimize(0)
