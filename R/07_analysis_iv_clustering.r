# Load libraries ----------------------------------------------------------
library("tidyverse")
library("patchwork")
library("broom")
library("vroom")
library("purrr")
library("factoextra")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
morph_data  <- read_tsv(file = "data/02_morphometric_data_clean.tsv")

View(morph_data)

# Wrangle data ------------------------------------------------------------

#Chose numeric variables

morph_data <- select(morph_data, 
                     sensillae_count,
                     pronotum_length,
                     hind_femur_length,
                     spiracle_length,
                     tympana_anterior_proximo_distal_length,
                     tympana_anterior_dorso_ventral_length,
                     tympana_posterior_proximo_distal_length,
                     tympana_posterior_dorso_ventral_length,
) %>%
  #Remove NA variables so they don't impact the clustering
  na.omit()



# Model data --------------------------------------------------------------
KMM <- kmeans(morph_data, iter.max = 100, centers = 5, nstart = 5)



# Visualise data ----------------------------------------------------------
fviz_cluster(KMM, data = morph_data)

# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)