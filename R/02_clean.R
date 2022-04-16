# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
morphometric_data <- read_tsv(file = "data/01_morphometric_data.tsv")

physiological_data <- read_tsv(file = "data/01_physiological_data.tsv")

meta_data <- read_tsv(file = "data/01_meta_data.tsv")

# Wrangle data ------------------------------------------------------------
## Morphometric data ------------------------------------------------------
# Rename columns
morphometric_data_clean <- morphometric_data %>%
  rename_with(.fn = ~ .x %>%
                str_to_lower() %>%
                str_replace_all(pattern = "\\s",
                            replacement = "_") %>%
                str_replace(pattern = fixed("[mm]"),
                            replacement = "length")) %>%
  rename(sensillae_count = sensillae)
  
morphometric_data_clean <- morphometric_data_clean %>%
  mutate(sex = case_when(sex == "m" ~ "male",
                         sex == "f" ~ "female"))

# Write data --------------------------------------------------------------
write_tsv(x = morphometric_data_clean,
          file = "data/02_morphometric_data_clean.tsv")