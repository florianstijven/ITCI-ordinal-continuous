.PHONY: all
all: R/data-exploration.Rout R/goodness-of-fit.Rout R/sensitivity-analysis-processing.Rout

R/data-exploration.Rout: R/data-exploration.R

R/goodness-of-fit.Rout: R/goodness-of-fit.R R/model-fitting.Rout

R/model-fitting.Rout: R/model-fitting.R

R/sensitivity-analysis-processing.Rout: R/sensitivity-analysis-processing.R results/sensitivity-analysis-results-relaxed.rds

include r-rules.mk