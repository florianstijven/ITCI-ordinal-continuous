# This makefile contains all Rscripts except R/sensitivity-analysis-main.R 
# because this Rscript is run on a supercomputer. It would take about 8 hours to 
# run on a 12 core personal computer. The data are part of the Surrogate package 
# and are, therefore, not included in the makefile. It is highly unlikely that 
# the data will change across version of this package. 

.PHONY: all
all: R/data-exploration.Rout R/goodness-of-fit.Rout R/sensitivity-analysis-processing.Rout

R/data-exploration.Rout: R/data-exploration.R
	Rscript --verbose R/data-exploration.R > R/data-exploration.Rout
R/goodness-of-fit.Rout: R/goodness-of-fit.R R/model-fitting.Rout
	Rscript --verbose R/goodness-of-fit.R > R/goodness-of-fit.Rout
R/model-fitting.Rout: R/model-fitting.R
	Rscript --verbose R/model-fitting.R > R/model-fitting.Rout
# It takes about 30 minutes to run the Rscript below on 12 cores. 
R/sensitivity-analysis-main.Rout: R/sensitivity-analysis-main.R R/model-fitting.Rout
	Rscript --verbose R/sensitivity-analysis-main.R 10 > R/sensitivity-analysis-main.Rout
R/sensitivity-analysis-processing.Rout: R/sensitivity-analysis-processing.R R/model-fitting.Rout results/sensitivity-analysis-results-relaxed.rds results/sensitivity-analysis-results-main.rds
	Rscript --verbose R/sensitivity-analysis-processing.R > R/sensitivity-analysis-processing.Rout