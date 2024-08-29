# By default renv will try to install binary packages, instead of installing
# them from source. This can lead to very vague errors like libRlapack.so:
# cannot open shared object file: No such file or directory. Setting
# renv.config.ppm.enabled to FALSE should solve these issues.
options(renv.config.ppm.enabled = FALSE)

source("renv/activate.R")

# Size parameter for saving plots to disk.
single_width = 61
double_width = 140
between_width = single_width * 1.5
single_height = 55
double_height = 128
between_height = single_height * 1.5 
res = 600

if (require(ggplot2)){
  ggplot2::theme_set(ggplot2::theme_bw())
}

