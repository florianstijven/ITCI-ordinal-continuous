# Setup -------------------------------------------------------------------

# Load the required packages
library(Surrogate)
library(tidyverse)

save_to_main = "figures-tables/figures/"
save_to_appendix = "figures-tables/tables/"

# Load data sets. 
data("Schizo_BinCont")
# Do rowwise deletation of missing observations.
Schizo_BinCont = Schizo_BinCont %>%
  na.omit()

# Model Fitting -----------------------------------------------------------

# Four parametric copula families are considered.
possible_copulas = c("gaussian", "clayton", "frank", "gumbel")

# DIFFERENT MARGINAL DISTRIBUTIONS CAN BE SPECIFIED HERE.
normal_dist = list(
  function(x, para) {
    dnorm(x, mean = para[1], sd = para[2])
  },
  function(x, para) {
    pnorm(x, mean = para[1], sd = para[2])
  },
  function(p, para) {
    qnorm(p, mean = para[1], sd = para[2])
  },
  n_para = 2,
  starting_values = c(0, 1)
)
marginal_S0 = normal_dist
marginal_S0[[5]] = c(mean(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == -1]), sd(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == -1]))
marginal_S1 = normal_dist
marginal_S1[[5]] = c(mean(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == 1]), sd(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == 1]))

# Construct a tibble with all possible combinations of the number of internal
# knots and the parametric copula families.
model_combinations = expand_grid(copula = possible_copulas,
                                 marginal_S0 = list(marginal_S0),
                                 marginal_S1 = list(marginal_S1))
# Models for all the above combinations are fitted. The fitted models are saved
# in a column of the tibble.
data = data.frame(
  Schizo_BinCont$PANSS,
  Schizo_BinCont$CGI,
  ifelse(Schizo_BinCont$Treat == 1, 1, 0)
)
fitted_models = model_combinations[1, ] %>%
  mutate(fitted_model = purrr::pmap(
    .l = list(
      copula_family = copula,
      marginal_S0 = marginal_S0,
      marginal_S1 = marginal_S1
    ),
    .f = function(copula_family, marginal_S0, marginal_S1) {
      Surrogate:::fit_copula_OrdCont(data = data,
                                     copula_family = copula_family,
                                     marginal_S0 = marginal_S0,
                                     marginal_S1 = marginal_S1,
                                     K_T = 7,
                                     start_copula = 0.75)
    }
  ))

plot(fitted_models$fitted_model)
fitted = fitted_models$fitted_model[[1]]
Surrogate:::plot.vine_copula_fit(fitted)

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