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


# Data Exploration --------------------------------------------------------

# Convert Schizo_BinCont to long format. This makes plotting easier.
Schizo_BinCont_long = Schizo_BinCont %>%
  mutate(Treatment = factor(
    Treat,
    levels = c(-1, 1),
    labels = c("Control", "Experimental")
  )) %>%
  pivot_longer(
    cols = c("PANSS", "BPRS", "CGI"),
    names_to = "Endpoint",
    values_to = "value"
  ) 

# Function to determine separate bin widths for the various endpoints.
binwidth_f = function(x) {
  if (length(unique(x)) < 10) {
    return(1)
  }
  else {
    return((max(x) - min(x)) / 15)
  }
}

# Plots for the marginal distributions. We also add the estimated normal
# densities to highlight the lack of normality. 
Schizo_BinCont_long %>%
  ggplot(aes(x = value)) +
  geom_histogram(color = "black",
                 fill = "gray",
                 binwidth = binwidth_f) +
  facet_grid(
    cols = vars(Endpoint),
    rows = vars(Treatment),
    scales = "free"
  ) +
  # Add estimated normal densities.
  geom_line(
    data = Schizo_BinCont_long %>%
      group_by(Endpoint, Treatment) %>%
      summarize(
        mean = mean(value),
        sd = sd(value),
        binwidth = binwidth_f(value),
        n = n()
      ) %>%
      ungroup() %>%
      left_join(
        Schizo_BinCont_long %>%
          group_by(Endpoint) %>%
          summarize(min = min(value), max = max(value))
      ) %>%
      rowwise(everything()) %>%
      reframe(
        x = seq(
          from = min,
          to = max,
          length.out = 5e2
        ),
        dens = dnorm(
          x = seq(
            from = min,
            to = max,
            length.out = 5e2
          ),
          mean = mean,
          sd = sd
        )
      ),
    aes(x = x, y = dens * n * binwidth)
  )

# QQ-plots for the z-scores.
Schizo_BinCont_long %>%
  left_join(Schizo_BinCont_long %>%
              group_by(Endpoint, Treatment) %>%
              summarize(
                mean = mean(value),
                sd = sd(value)
              )) %>%
  mutate(z_score = (value - mean) / sd) %>%
  ggplot(aes(sample = z_score)) +
  geom_qq() +
  geom_qq_line() +
  facet_grid(
    cols = vars(Endpoint),
    rows = vars(Treatment),
    scales = "free"
  )


# Normality tests for each setting.
