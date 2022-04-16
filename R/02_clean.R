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
morphometric_data <- morphometric_data %>%
  rename_with(.fn = ~ .x %>%
                str_to_lower() %>%
                str_replace_all(pattern = "\\s",
                            replacement = "_") %>%
                str_replace(pattern = "\\[mm\\]$",
                            replacement = "length")) %>%
  rename(sensillae_count = sensillae)

# Clean column values
morphometric_data_clean <- morphometric_data %>%
  mutate(sex = case_when(sex == "m" ~ "male",
                         sex == "f" ~ "female"))

## Physiological data -----------------------------------------------------
# Clean column values
physiological_data_clean <- physiological_data %>%
  mutate(frequency = str_replace(string = frequency,
                                 pattern = "^(\\d+)(\\s*kHz)?$",
                                 replacement = "\\1"),
         cricket_id = str_replace(string = cricket_id,
                                  pattern = "^Nr.\\s*(\\d+(\\s*(re|li))?)$",
                                  replacement = "\\1"),
         auditory_threshold = str_replace(string = auditory_threshold,
                                          pattern = "^(\\d+)(\\s*dB)?$",
                                          replacement = "\\1"),
         sex = str_replace(string = sex,
                           pattern = "^(male|female)\\s+individuals$",
                           replacement = "\\1"))

# Write data --------------------------------------------------------------
write_tsv(x = morphometric_data_clean,
          file = "data/02_morphometric_data_clean.tsv")

write_tsv(x = physiological_data_clean,
          file = "data/02_physiological_data_clean.tsv")