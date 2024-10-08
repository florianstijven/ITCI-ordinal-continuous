# Setup -------------------------------------------------------------------
set.seed(1)
# Load the required packages
library(tidyverse)
library(latex2exp)
library(GGally)
library(Surrogate)

# Path to save figures and tables.
figure_save_to = "figures-tables/figures/sensitivity-analysis/"
table_save_to = "figures-tables/tables/sensitivity-analysis/"


# Loading Results ---------------------------------------------------------

# The results of the main analysis are contained in a single file which contains
# a tibble with the "raw" results (i.e., the results of each replication of the
# sensitivity analysis) and "processed" results (i.e., the ignorance and
# uncertainty intervals). The corresponding tibble contains two rows, one for
# the R_H as ICA and on for the standardized SICC as ICA.
sens_results_main = readRDS("results/sensitivity-analysis-results-main.rds")

# The results of the analyses under relaxed assumptions are contained in a
# single file, again containing a tibble with the same structure as
# sens_results_main.
sens_results_relaxed = readRDS("results/sensitivity-analysis-results-relaxed.rds")

# For plotting, the original tibble with one sensitivity analysis per row should
# be converted to a tibble with one MC replication per row.
sens_results_main_tbl = sens_results_main %>%
  rowwise(range_class, copula_family, cond_ind, ICA_type) %>%
  reframe(sens_results) 

sens_results_relaxed_tbl = sens_results_relaxed %>%
  rowwise(range_class, copula_family, cond_ind, ICA_type) %>%
  reframe(sens_results) 

# Read in the selected model.
best_fitted_model = read_rds("results/best-fitted-model.rds")

# Processing Results ------------------------------------------------------

## Histograms -------------------------------------------------------------

# Histogram for the R_H or the standardized SICC as ICA for the main sensitivity
# analysis.
sens_results_main_tbl %>%
  ggplot(aes(x = ICA)) +
  coord_cartesian(xlim = c(0, 1)) +
  # scale_x_continuous(name = expression({R[h]^2}(hat(bold(beta))[N], bold(nu^{(l)})))) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    fill = "gray",
    color = "black",
    binwidth = 0.05,
    boundary = 1,
  ) +
  facet_grid(.~ICA_type) +
  scale_x_continuous(name = "ICA") +
  scale_y_continuous(name = "Density")
ggsave(filename = paste0(figure_save_to, "main-results-ICA-histogram.pdf"),
       device = "pdf",
       width = single_width,
       height = single_height,
       units = "mm",
       dpi = res)

# Histogram for the R_H or the standardized SICC as ICA for the relaxed
# sensitivity analysis.
sens_results_relaxed_tbl %>%
  ggplot(aes(x = ICA, fill = range_class)) +
  coord_cartesian(xlim = c(0, 1)) +
  # scale_x_continuous(name = expression({R[h]^2}(hat(bold(beta))[N], bold(nu^{(l)})))) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    color = "black",
    position = "identity",
    binwidth = 0.05,
    boundary = 1,
    alpha = 0.5
  ) +
  facet_grid(ICA_type ~ copula_family, scales = "free_y") +
  scale_x_continuous(name = "ICA") +
  scale_y_continuous(name = "Density") +
  scale_fill_viridis_d(name = "Setting") +
  theme(legend.position = "bottom")
ggsave(filename = paste0(figure_save_to, "relaxed-results-ICA-histogram.pdf"),
       device = "pdf",
       width = double_width,
       height = double_height,
       units = "mm",
       dpi = res)


# Histogram for ICA = Spearman's rho. Main results.
sens_results_main_tbl %>%
  ggplot(aes(x = sp_rho)) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(name = TeX("$\\rho_{sp}$")) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    fill = "gray",
    color = "black",
    binwidth = 0.05,
    boundary = 1,
  ) +
  scale_y_continuous(name = "Density") 
ggsave(filename = paste0(figure_save_to, "main-results-sprho-histogram.pdf"),
       device = "pdf",
       width = single_width,
       height = single_height,
       units = "mm",
       dpi = res)

# Histogram for ICA = Spearman's rho. Relaxed results.
sens_results_relaxed_tbl %>%
  ggplot(aes(x = sp_rho)) +
  coord_cartesian(xlim = c(0, 1)) +
  scale_x_continuous(name = TeX("$\\rho_{sp}$")) +
  geom_histogram(
    mapping = aes(y = after_stat(density)),
    fill = "gray",
    color = "black",
    binwidth = 0.05,
    boundary = 1,
  ) +
  facet_grid(range_class~copula_family) +
  scale_y_continuous(name = "Density") 
ggsave(filename = paste0(figure_save_to, "relaxed-results-sprho-histogram.pdf"),
       device = "pdf",
       width = double_width,
       height = double_height,
       units = "mm",
       dpi = res)


## Uncertainty Intervals --------------------------------------------------

# Increase text width.
withr::local_options(.new = list(width = 100))

# To prevent having to run this file multiple times, the sensitivity intervals
# are printed to a .txt file.
sink(file = paste0(table_save_to, "sensitivity-intervals-main.txt"))
cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
cat("ICA = R_H \n\n")
print(sens_results_main[sens_results_main$ICA_type == "R_H", ]$sens_interval_ICA[[1]])
cat("\n"); cat(paste(rep("=", 80), collapse = '')); cat("\n\n")

cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
cat("ICA = Standardized SICC \n\n")
print(sens_results_main[sens_results_main$ICA_type == "SICC-transformed", ]$sens_interval_ICA[[1]])
cat("\n"); cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
sink()

# For the sensitive intervals under relaxed assumptions, we add the sensitivity
# intervals directly to the original tibble.
interval_paste = function(interval, digits = 3) {
  paste0("(", round(interval[1], digits), ", ", round(interval[2], digits), ")")
}

sink(file = paste0(table_save_to, "sensitivity-intervals-relaxed.txt"))
cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
cat("Intervals for sensitivity analyses under relaxed assumptions for ICA = R_H or standardized SICC \n\n")
sens_results_relaxed %>%
  ungroup() %>%
  rowwise(c("range_class", "copula_family", "ICA_type")) %>%
  summarise(
    "Est. II" = interval_paste(sens_interval_ICA$est_interval_of_ignorance, 2),
    "UI (pointwise)" = interval_paste(
      sens_interval_ICA$interval_of_uncertainty_pointwise_coverage,
      2
    )
  ) %>%
  ungroup()
cat("\n"); cat(paste(rep("=", 80), collapse = '')); cat("\n\n")

cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
cat("Intervals for sensitivity analyses under relaxed assumptions for sp_rho \n\n")
sens_results_relaxed %>%
  ungroup() %>%
  rowwise(c("range_class", "copula_family", "ICA_type")) %>%
  summarise(
    "Est. II, sp_rho" = interval_paste(sens_interval_sp_rho$est_interval_of_ignorance, 2),
    "UI (pointwise), sp_rho" = interval_paste(
      sens_interval_sp_rho$interval_of_uncertainty_pointwise_coverage,
      2
    )
  ) %>%
  ungroup()
cat("\n"); cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
sink()

sink(file = paste0(table_save_to, "sensitivity-intervals-relaxed-latex.txt"))
cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
cat("Intervals for sensitivity analyses under relaxed assumptions for ICA = R_H or standardized SICC \n\n")
sens_results_relaxed %>%
  ungroup() %>%
  rowwise(c("range_class", "copula_family", "ICA_type")) %>%
  summarise(
    "Est. II" = interval_paste(sens_interval_ICA$est_interval_of_ignorance, 2),
    "UI (pointwise)" = interval_paste(
      sens_interval_ICA$interval_of_uncertainty_pointwise_coverage,
      2
    )
  ) %>%
  ungroup() %>%
  knitr::kable(format = "latex") %>%
  print()
cat("\n"); cat(paste(rep("=", 80), collapse = '')); cat("\n\n")

cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
cat("Intervals for sensitivity analyses under relaxed assumptions for sp_rho \n\n")
sens_results_relaxed %>%
  ungroup() %>%
  rowwise(c("range_class", "copula_family", "ICA_type")) %>%
  summarise(
    "Est. II, sp_rho" = interval_paste(sens_interval_sp_rho$est_interval_of_ignorance, 2),
    "UI (pointwise), sp_rho" = interval_paste(
      sens_interval_sp_rho$interval_of_uncertainty_pointwise_coverage,
      2
    )
  ) %>%
  ungroup() %>%
  knitr::kable(format = "latex") %>%
  print()
cat("\n"); cat(paste(rep("=", 80), collapse = '')); cat("\n\n")
sink()