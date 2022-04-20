# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
phys_data <- read_tsv(file = "data/01_physiological_data.tsv")

# Wrangle data ------------------------------------------------------------
my_data_clean_aug
View(morphometric_data_clean)
# Model data --------------------------------------------------------------


# Visualise data ----------------------------------------------------------


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)