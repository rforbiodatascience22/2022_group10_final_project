# Load libraries ----------------------------------------------------------
library(rmarkdown)

# Run all scripts ---------------------------------------------------------
source(file = "R/01_load.R")
source(file = "R/02_clean.R")
source(file = "R/04_analysis_recreate_morphometric_figures.R")
source(file = "R/06_analysis_ii_Recreate_Fig_8.R")
source(file = "R/05_analysis_PCA_kmeans.R")
render(input = "doc/report.Rmd")