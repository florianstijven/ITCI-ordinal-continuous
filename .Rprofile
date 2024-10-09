# By default renv will try to install binary packages, instead of installing
# them from source. This can lead to very vague errors like libRlapack.so:
# cannot open shared object file: No such file or directory. Setting
# renv.config.ppm.enabled to FALSE should solve these issues.
options(renv.config.ppm.enabled = FALSE)

source("renv/activate.R")

# Size parameter for saving plots to disk.
single_width = 90
double_width = 140
single_height = 82
double_height = 128
res = 600

# Check whether ggplot2 is installed. If yes, system.file() returns the
# location; if no, system.file() returns an empty string.
if (system.file(package = "ggplot2") != ""){
  ggplot2::theme_set(ggplot2::theme_bw())
}

# WORKAROUND: https://github.com/rstudio/rstudio/issues/6692
# Revert to 'sequential' setup of PSOCK cluster in R 4.0.0
if (getRversion() >= "4.0.0") {
  parallel:::setDefaultClusterOptions(setup_strategy = "sequential")
}

