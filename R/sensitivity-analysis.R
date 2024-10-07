#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
print(args)

# Setup -------------------------------------------------------------------

ncores = as.integer(args[1])
# Number of MC replications in the sensitivity analysis
n_sim = 50
# Number of MC samples for computing the ICA and related measures.
n_prec = 5e3
# Number of bootstrap replications for computing uncertainty intervals.
B = 50

library(Surrogate)
library(dplyr)
library(tidyr)
library(copula)
library(cubature)

# We need the best fitted vine copula model.
best_fitted_model = readRDS("results/best-fitted-model.rds")

# We define all different scenarios for the set of sensitivity analysis in the
# scenarios_tbl.
sensitivity_ranges = tibble(
  ranges = list(list(
    lower = c(0.60, 0, 0, 0.15),
    upper = c(0.975, 0, 0, 0.8)
  ), list(
    lower = c(0.40, 0, 0, 0.05),
    upper = c(0.99, 0.20, 0.20, 0.90)
  )),
  range_class = c("Main Assumptions", "Relaxed Assumptions"),
  cond_ind = c(TRUE, FALSE)
)
# We consider all combinations of parameter ranges, unidentifiable copula
# families, and ICA (the default R_H or the SICC with the upper bound
# transformed to 1).
scenarios_tbl = expand_grid(
  sensitivity_ranges,
  copula_family = c("gaussian", "frank", "gumbel", "clayton"),
  ICA_type = c("R_H", "SICC-transformed")
)
# The SICC can be replaced with any measure by replacing the mutual information
# estimator with an estimator of -0.5 * log(1 - measure).
scenarios_tbl = scenarios_tbl %>%
  mutate(ICA_estimator = list(endpoint_types = constructor_ICA_estimator(
    c("ordinal", "continuous"),
    ICA_def = function(mutinfo, H_DeltaS, H_DeltaT) {
      # SICC divided by its upper bound in the ordinal-continuous setting.
      (1 - exp(-2 * mutinfo)) / (1 - exp(-2 * H_DeltaT))
    }
  )))
# ICA_estimator is set to NULL for R_H. The default ICA definition (i.e., the
# definition for R_H) will then be used.
scenarios_tbl$ICA_estimator[scenarios_tbl$ICA_type == "R_H"] = list(NULL)

# Sensitivity Analysis ----------------------------------------------------

# We use a wrapper function for the sensitivity analysis such that we set the
# same seed for each different version of the sensitivity analysis.
wrapper_sensitivity_analysis = function(copula_family, lower, upper, ICA_estimator, ncores) {
  set.seed(1)
  print("try sens analysis")
  sensitivity_analysis_copula(
    fitted_model = best_fitted_model,
    n_sim = n_sim,
    eq_cond_association = TRUE,
    lower = lower,
    upper = upper,
    degrees = 0,
    marg_association = TRUE,
    copula_family2 = copula_family,
    n_prec = n_prec,
    ncores = ncores,
    ICA_estimator = ICA_estimator
  )
}
# Similarly for the uncertainty intervals.
wrapper_uncertainty_intervals = function(sens_results, ICA_estimator, measure, ncores) {
  set.seed(1)
  # We save some computational time if we do not compute the ICA when we're only
  # looking at Spearman's rho in the full population.
  if (measure == "sp_rho") {
    ICA_estimator = function(x, y) 0
  }

  sensitivity_intervals_Dvine(
    fitted_model = best_fitted_model,
    sens_results = sens_results,
    measure = measure,
    ICA_estimator = ICA_estimator,
    n_prec = n_prec,
    B = B,
    ncores = ncores
  )
}

# The sensitivity analysis is implemented in this file, but the results of the
# sensitivity analysis are saved into an .RData file and processed elsewhere.
# This is done because the sensitivity analysis is computer intensive and not
# interactive; whereas processing the results is not computer intensive, but
# interactive.
a = Sys.time()
sens_results = purrr::pmap(
  .l = list(
    copula_family = scenarios_tbl$copula_family,
    lower = purrr::map(scenarios_tbl$ranges, "lower"),
    upper = purrr::map(scenarios_tbl$ranges, "upper"),
    ICA_estimator = scenarios_tbl$ICA_estimator
  ),
  .f = function(...) wrapper_sensitivity_analysis(...),
  ncores = ncores
)
sens_results_tbl = scenarios_tbl
sens_results_tbl$sens_results = sens_results
  

# The uncertainty intervals are computed as well.
sens_results_tbl$sens_interval_ICA = purrr::map2(
  .x = sens_results_tbl$sens_results,
  .y = sens_results_tbl$ICA_estimator,
  .f = wrapper_uncertainty_intervals, 
  measure = "ICA",
  ncores = ncores
)
sens_results_tbl$sens_interval_sp_rho = purrr::map2(
  .x = sens_results_tbl$sens_results,
  .y = sens_results_tbl$ICA_estimator,
  .f = wrapper_uncertainty_intervals, 
  measure = "sp_rho",
  ncores = ncores
)
print(Sys.time() - a)

# Saving Results ----------------------------------------------------------

# The results of the sensitivity analysis are saved to a file. These results are
# analyzed in a separate file.
saveRDS(
  object = sens_results_tbl,
  file = "results/sensitivity-analysis-results-relaxed.rds"
)