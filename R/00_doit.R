# Load libraries ----------------------------------------------------------
library(rmarkdown)

# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/04_analysis_morphometric.R")
source(file = "R/05_analysis_pca_kmeans.R")
source(file = "R/06_analysis_physiological.R")
render(input = "doc/report.Rmd")