# Setup -------------------------------------------------------------------

# Load the required packages
library(Surrogate)
library(tidyverse)

save_to_main = "figures-tables/figures/"
save_to_appendix = "figures-tables/tables/"

# Load data sets. 
data("Schizo_BinCont")
data("ARMD")
data("Schizo")


# Data Exploration
Schizo_BinCont %>%
  ggplot()


# Model Fitting -----------------------------------------------------------

# After ensuring that the data are in the correct format, the survival-survival
# model is fitted. Models are fitted for different combinations of the number of
# internal knots and the parametric copula families.

# Four parametric copula families are considered.
possible_copulas = c("gaussian", "clayton", "frank", "gumbel")
# 2 through 5 internal knots are considered. The same number of internal knots
# are considered for each potential outcome, although this can be relaxed.
possible_nknots = 2:5
# Construct a tibble with all possible combinations of the number of internal
# knots and the parametric copula families.
model_combinations = expand_grid(copula = possible_copulas,
                                 nknots = possible_nknots)
# Models for all the above combinations are fitted. The fitted models are saved
# in a column of the tibble.
fitted_models = model_combinations %>%
  mutate(fitted_model = purrr::map2(
    .x = copula,
    .y = nknots,
    .f = function(.x, .y) {
      fit_model_SurvSurv(data, .x, .y)
    }
  ))

# For all fitted models, goodness-of-fit measures are computed and saved into
# the same tibble. The maximized loglikelihood of the entire identifiable model
# is the sum of the loglikelihoods of the two fitted submodels.
fitted_models = fitted_models %>%
  mutate(
    LogLik = purrr::map_dbl(
      .x = fitted_model,
      .f = function(.x) {
        logLik(.x$fit_0) +
          logLik(.x$fit_1)
      }
    ),
    df = 2 * (2 * (nknots + 2) + 1),
    AIC = -2 * LogLik + 2 * df
  ) %>%
  arrange(AIC)

# Print summary of all fitted models order from lowest to largest AIC. A lower
# AIC corresponds to a better fit.
sink(file = paste0(save_to_appendix, "fitted-models.txt")) # Open connection to .txt file to print output to
cat("Table of fitted models:\n\n")
print(fitted_models %>%
        mutate(
          LogLik = num(LogLik, digits = 2),
          AIC = num(AIC, digits = 2)
        ))
sink()


# The best fitting model, in terms of AIC, is the Gaussian copula model with 2
# internal knots. This model is extracted from the list of fitted models and
# named best_fitted_model. Since the fitted_models tibble is already sorted on
# AIC, the best model is the first one.
sink(file = "results/best-fitted-model-summary.txt")
best_fitted_model = fitted_models$fitted_model[[1]]
# Print summary of the selected model.
cat("\nBest fitted model:\n\n")
best_fitted_model
sink() # Close connection to .txt file.

# Saving Results ----------------------------------------------------------

saveRDS(fitted_models, file = "results/fitted-models.rds")
saveRDS(best_fitted_model, file = "results/best-fitted-model.rds")