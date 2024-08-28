# Setup -------------------------------------------------------------------

# Load the required packages
library(Surrogate)
library(tidyverse)

save_to_main = "figures-tables/figures/"
save_to_appendix = "figures-tables/tables/"

# Load data sets. 
data("Schizo_BinCont")
# Do rowwise deletion of missing observations.
Schizo_BinCont = Schizo_BinCont %>%
  na.omit()

# Model Fitting -----------------------------------------------------------

# Four parametric copula families are considered.
possible_copulas = c("gaussian", "clayton", "frank", "gumbel")

# We use normal distributions for the marginals of the surrogate (PANNS) because
# the data exploration did not reveal any significant deviations from normality.
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
  starting_values = c(-20, 27)
)
# We also use a skewed normal distribution to allow for additional flexibility.
# xi is the location parameter, omega the scale parameter, and alpha the slant
# parameter. When alpha = 0, the distribution is normal.
skewed_normal_dist = list(
  function(x, para) {
    sn::dsn(x, xi = para[1], omega = para[2], alpha = para[3])
  },
  function(x, para) {
    sn::psn(x, xi = para[1], omega = para[2], alpha = para[3])
  },
  function(p, para) {
    sn::qsn(p, xi = para[1], omega = para[2], alpha = para[3], tol = 1e-10)
  },
  n_para = 3,
  starting_values = c(15, 35, -1) 
)

# Put the marginal distribution functions into a list. We assume that the
# marginal distributions for S_0 and S_1 are of the same form. 
list_marginal_S0 = list(normal_dist, skewed_normal_dist)
list_marginal_S1 = list_marginal_S0
marginals = c("Normal", "Skewed Normal")
# # The samples means and standard deviations are used as starting values in each
# # treatment group.
# list_marginal_S0[[1]][[5]] = c(mean(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == -1]), sd(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == -1]))
# list_marginal_S1[[1]][[5]] = c(mean(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == 1]), sd(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == 1]))
# 
# list_marginal_S0[[2]][[5]] = c(mean(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == -1]), sd(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == -1]), -1)
# list_marginal_S1[[2]][[5]] = c(mean(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == 1]), sd(Schizo_BinCont$PANSS[Schizo_BinCont$Treat == 1]), -1)

# Construct a tibble with all possible combinations of model characteristics.
model_combinations = expand_grid(copula = possible_copulas,
                                 marginal_S0 = marginals,
                                 marginal_S1 = marginals)
# Models for all the above combinations are fitted. The fitted models are saved
# in a column of the tibble.
data = data.frame(
  Schizo_BinCont$PANSS,
  Schizo_BinCont$CGI,
  ifelse(Schizo_BinCont$Treat == 1, 1, 0)
)
fitted_models = model_combinations %>%
  mutate(fitted_model = purrr::pmap(
    .l = list(
      copula_family = copula,
      marginal_S0_name = marginal_S0,
      marginal_S1_name = marginal_S1
    ),
    .f = function(copula_family, marginal_S0_name, marginal_S1_name) {
      # The starting value for the copula depends on the parametric copula.
      start_copula = switch(
        copula_family,
        gaussian = 0.8,
        clayton = 3,
        frank = 10,
        gumbel = 2.7
      )
      # Select the marginal distirbution functions corresponding to marginal.
      marginal_S0 = list_marginal_S0[[which(marginal_S0_name == marginals)]]
      marginal_S1 = list_marginal_S1[[which(marginal_S1_name == marginals)]]
      # Fit the copula model.
      fit_copula_OrdCont(
        data = data,
        copula_family = copula_family,
        marginal_S0 = marginal_S0,
        marginal_S1 = marginal_S1,
        K_T = 7,
        start_copula = start_copula,
        method = "BFGS",
        finalHessian = "BHHH",
        control = list(reltol = 1e-9,
                       iterlim = 2e3)
      )
    }
  ))


# For all fitted models, goodness-of-fit measures are computed and saved into
# the same tibble. The maximized loglikelihood of the entire identifiable model
# is the sum of the loglikelihoods of the two fitted submodels.
fitted_models = fitted_models %>%
  mutate(
    loglik0 = purrr::map_dbl(
      .x = fitted_model,
      .f = function(.x) {
        logLik(.x$fit_0$ml_fit)
      }
    ),
    loglik1 = purrr::map_dbl(
      .x = fitted_model,
      .f = function(.x) {
        logLik(.x$fit_1$ml_fit)
      }
    ),
    loglik = loglik0 + loglik1,
    AIC = purrr::map2_dbl(
      .x = loglik,
      .y = fitted_model,
      .f = function(.x, .y) {
        2 * (length(coef(.y$fit_0$ml_fit)) + length(coef(.y$fit_1$ml_fit))) - 2 * .x
      }
    )
  ) %>%
  arrange(AIC)

# Print summary of all fitted models order from largest to lowest maximized
# loglikelihood. A larger loglikelihood corresponds to a better fit.
sink(file = paste0(save_to_appendix, "fitted-models.txt")) # Open connection to .txt file to print output to
cat("Table of fitted models:\n\n")
print(fitted_models %>%
        mutate(
          loglik = num(loglik, digits = 2, notation = "dec"),
          loglik0 = num(loglik0, digits = 2, notation = "dec"),
          loglik1 = num(loglik1, digits = 2, notation = "dec"),
          AIC = num(AIC, digits = 2, notation = "dec")
        ))
sink()


# The best fitting model, in terms of the maximized logelikelihood, is the
# Frank copula model with normal margins for the surrogate (PANNS). This model
# is extracted from the list of fitted models and named best_fitted_model. Since
# the fitted_models tibble is already sorted on maximized loglikelihood, the
# best model is the first one.
sink(file = paste0(save_to_appendix, "best-fitted-model-summary.txt"))
best_fitted_model = fitted_models$fitted_model[[1]]
# Print summary of the selected model.
cat("\nBest fitted model:\n\n")
best_fitted_model
sink() # Close connection to .txt file.

# Saving Results ----------------------------------------------------------

saveRDS(fitted_models, file = "results/fitted-models.rds")
saveRDS(best_fitted_model, file = "results/best-fitted-model.rds")