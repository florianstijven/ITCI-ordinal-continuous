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
	Rscript --verbose R/sensitivity-analysis-main.R 12 > R/sensitivity-analysis-main.Rout
R/sensitivity-analysis-processing.Rout: R/sensitivity-analysis-processing.R results/sensitivity-analysis-results-relaxed.rds results/sensitivity-analysis-results-main.rds
	Rscript --verbose R/sensitivity-analysis-processing.R > R/sensitivity-analysis-processing.Rout