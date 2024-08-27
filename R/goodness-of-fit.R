# Setup -------------------------------------------------------------------

# Load the required packages
library(Surrogate)
library(tidyverse)

save_to = "figures-tables/figures/goodness-of-fit/"

# Load fitted models.
fitted_models = readRDS("results/fitted-models.rds")
best_fitted_model = readRDS("results/best-fitted-model.rds")


# Goodness-of-Fit Plots ---------------------------------------------------

## Default plots produced by the Surrogate functions ----------------------

# The Surrogate packages contains easy-to-use function to produce
# goodness-of-fit plots for the fitted copula models. The default option is to
# call plot() on the fitted-model object. This will produce all goodness-of-fit
# plots. However, to save them one by one, we need to call individual plotting
# functions which are currently not exported.

# Function to produce and save the default goodness-of-fit plots.
gof_plots_default = function(copula_family, fitted_model) {
  pdf(
    file = paste0(save_to, "default/", copula_family, "-", "marginal-gof-t0.pdf"),
    width = between_width,
    height = between_height
  )
  par(mar = c(3.5, 3.5, 1, 1), oma = c(0, 0, 0, 0), mgp = c(2, 1, 0))
  Surrogate:::marginal_gof_copula(
    marginal = fitted_model$fit_0$marginal_X,
    observed = fitted_model$fit_0$data$X,
    name = "True Endpoint",
    treat = 0,
    type = "ordinal"
  )
  dev.off()
  
  pdf(file = paste0(save_to, "default/", copula_family, "-", "marginal-gof-t1.pdf"), width = between_width, height = between_height)
  par(mar = c(3.5, 3.5, 1, 1), oma = c(0, 0, 0, 0), mgp = c(2, 1, 0))
  Surrogate:::marginal_gof_copula(
    marginal = fitted_model$fit_1$marginal_X,
    observed = fitted_model$fit_1$data$X,
    name = "True Endpoint",
    treat = 1,
    type = "ordinal"
  )
  dev.off()
  
  pdf(
    file = paste0(save_to, "default/", copula_family, "-", "marginal-gof-s0.pdf"),
    width = between_width,
    height = between_height
  )
  par(mar = c(3.5, 3.5, 1, 1), oma = c(0, 0, 0, 0), mgp = c(2, 1, 0))
  Surrogate:::marginal_gof_copula(
    marginal = fitted_model$fit_0$marginal_Y,
    observed = fitted_model$fit_0$data$Y,
    name = "Surrogate",
    treat = 0,
    type = "continuous"
  )
  dev.off()
  
  pdf(file = paste0(save_to, "default/", copula_family, "-", "marginal-gof-s1.pdf"), width = between_width, height = between_height)
  par(mar = c(3.5, 3.5, 1, 1), oma = c(0, 0, 0, 0), mgp = c(2, 1, 0))
  Surrogate:::marginal_gof_copula(
    marginal = fitted_model$fit_1$marginal_Y,
    observed = fitted_model$fit_1$data$Y,
    name = "Surrogate",
    treat = 1,
    type = "continuous"
  )
  dev.off()
  
  pdf(file = paste0(save_to, "default/", copula_family, "-", "association-gof0.pdf"), width = between_width, height = between_height)
  par(mar = c(3.5, 3.5, 1, 1), oma = c(0, 0, 0, 0), mgp = c(2, 1, 0))
  Surrogate:::association_gof_copula(
    fitted_submodel = fitted_model$fit_0,
    treat = 0,
    endpoint_types = c("ordinal", "continuous")
  )
  dev.off()
  
  pdf(file = paste0(save_to, "default/", copula_family, "-", "association-gof1.pdf"), width = between_width, height = between_height)
  par(mar = c(3.5, 3.5, 1, 1), oma = c(0, 0, 0, 0), mgp = c(2, 1, 0))
  Surrogate:::association_gof_copula(
    fitted_submodel = fitted_model$fit_1,
    treat = 1,
    endpoint_types = c("ordinal", "continuous")
  )
  dev.off()
}

# Produce all goodness-of-fit plots for the fitted models.
fitted_models %>%
  group_by(copula) %>%
  summarize(gof_plots_default(copula, fitted_model[[1]]))

## Customized plots using ggplot2 -----------------------------------------

# We call the individual plotting functions (which are currently not exported)
# with the return_data argument set to TRUE. These functions will then return
# the "goodness-of-fit data" which can be used with ggplot2 to produce
# customized plots.

# Function to produce and save the custom ggplot2 goodness-of-fit plots.
gof_plots_ggplot = function(copula_family, fitted_model) {
  Surrogate:::marginal_gof_copula(
    marginal = fitted_model$fit_0$marginal_X,
    observed = fitted_model$fit_0$data$X,
    name = "True Endpoint",
    treat = 0,
    type = "ordinal",
    return_data = TRUE 
  ) %>%
    ggplot(aes(x = value, y = observed)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
    geom_point(aes(y = model_based), color = "red", shape = 15) +
    scale_x_continuous(breaks = 1:7, name = "True Endpoint (CGI)") +
    scale_y_continuous(name = "Proportion", lim = c(0, 0.5))
  ggsave(filename = paste0(save_to, "ggplot2/", copula_family, "-", "marginal-gof-t0.pdf"), 
         device = "pdf", 
         width = single_width, 
         height = single_height,
         units = "mm")
  
  Surrogate:::marginal_gof_copula(
    marginal = fitted_model$fit_1$marginal_X,
    observed = fitted_model$fit_1$data$X,
    name = "True Endpoint",
    treat = 1,
    type = "ordinal",
    return_data = TRUE 
  ) %>%
    ggplot(aes(x = value, y = observed)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
    geom_point(aes(y = model_based), color = "red", shape = 15) +
    scale_x_continuous(breaks = 1:7, name = "True Endpoint (CGI)") +
    scale_y_continuous(name = "Proportion", lim = c(0, 0.5))
  ggsave(filename = paste0(save_to, "ggplot2/", copula_family, "-", "marginal-gof-t1.pdf"), 
         device = "pdf", 
         width = single_width, 
         height = single_height,
         units = "mm")
  
  # Make histogram with original data.
  gof_data_S0 = Surrogate:::marginal_gof_copula(
    marginal = fitted_model$fit_0$marginal_Y,
    observed = fitted_model$fit_0$data$Y,
    name = "Surrogate",
    treat = 0,
    type = "continuous",
    grid = seq(from = -120, to = 70, length.out = 5e2),
    return_data = TRUE
  )
  data.frame(x = fitted_model$fit_0$data$Y) %>%
    ggplot(aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), color = "black", fill = "gray", bins = 15) +
    geom_line(aes(x = grid, y = model_based), data = gof_data_S0, color = "red") +
    scale_y_continuous(name = "Density") +
    scale_x_continuous(name = "Surrogate Endpoints (PANSS)")
  ggsave(filename = paste0(save_to, "ggplot2/", copula_family, "-", "marginal-gof-s0.pdf"), 
         device = "pdf", 
         width = single_width, 
         height = single_height,
         units = "mm")
  
  gof_data_S1 = Surrogate:::marginal_gof_copula(
    marginal = fitted_model$fit_1$marginal_Y,
    observed = fitted_model$fit_1$data$Y,
    name = "Surrogate",
    treat = 1,
    type = "continuous",
    grid = seq(from = -120, to = 70, length.out = 5e2),
    return_data = TRUE
  )
  data.frame(x = fitted_model$fit_1$data$Y) %>%
    ggplot(aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), color = "black", fill = "gray", bins = 15) +
    geom_line(aes(x = grid, y = model_based), data = gof_data_S1, color = "red") +
    scale_y_continuous(name = "Density") +
    scale_x_continuous(name = "Surrogate Endpoints (PANSS)")
  ggsave(filename = paste0(save_to, "ggplot2/", copula_family, "-", "marginal-gof-s1.pdf"), 
         device = "pdf", 
         width = single_width, 
         height = single_height,
         units = "mm")
  

  Surrogate:::association_gof_copula(
    fitted_submodel = fitted_model$fit_0,
    treat = 0,
    endpoint_types = c("ordinal", "continuous"),
    grid = seq(from = -120, to = 70, length.out = 5e2),
    return_data = TRUE
  ) %>%
    ggplot(aes(x = grid, y = observed)) +
    geom_point(aes(x = Y, y = X),
               data = data.frame(fitted_model$fit_0$data),
               color = "gray") +
    geom_line() +
    geom_line(aes(y = lower_ci), linetype = "dashed") +
    geom_line(aes(y = upper_ci), linetype = "dashed") +
    geom_line(aes(y = model_based), color = "red") +
    scale_x_continuous(name = "Surrogate (PANSS)") +
    scale_y_continuous(name = "E(T | S)", breaks = 1:7) +
    coord_cartesian(ylim = c(1, 7))
  ggsave(filename = paste0(save_to, "ggplot2/", copula_family, "-", "association-s0.pdf"), 
         device = "pdf", 
         width = single_width, 
         height = single_height,
         units = "mm")
  
  Surrogate:::association_gof_copula(
    fitted_submodel = fitted_model$fit_1,
    treat = 1,
    endpoint_types = c("ordinal", "continuous"),
    grid = seq(from = -120, to = 70, length.out = 5e2),
    return_data = TRUE
  ) %>%
    ggplot(aes(x = grid, y = observed)) +
    geom_point(aes(x = Y, y = X),
               data = data.frame(fitted_model$fit_0$data),
               color = "gray") +
    geom_line() +
    geom_line(aes(y = lower_ci), linetype = "dashed") +
    geom_line(aes(y = upper_ci), linetype = "dashed") +
    geom_line(aes(y = model_based), color = "red") +
    scale_x_continuous(name = "Surrogate (PANSS)") +
    scale_y_continuous(name = "E(T | S)", breaks = 1:7) +
    coord_cartesian(ylim = c(1, 7))
  ggsave(filename = paste0(save_to, "ggplot2/", copula_family, "-", "association-s1.pdf"), 
         device = "pdf", 
         width = single_width, 
         height = single_height,
         units = "mm")
}

# Produce all goodness-of-fit plots for the fitted models.
fitted_models %>%
  group_by(copula) %>%
  summarize(gof_plots_ggplot(copula, fitted_model[[1]]))
