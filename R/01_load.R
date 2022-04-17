# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
morphometric_data_raw <- read_xls(path = "data/_raw/JEvolBiol_Sensory evolution Poecilimon_Database.xls",
                                  sheet = "Database")

physiological_data_raw <- read_xls(path = "data/_raw/JEvolBiol_Sensory evolution Poecilimon_Database.xls",
                                   sheet = "Database 2",
                                   skip = 1,
                                   col_names = FALSE)

meta_data_raw <- read_xls(path = "data/_raw/jeb12294-sup-0001-tables1.xls",
                          skip = 3,
                          col_names = FALSE)

# Wrangle data ------------------------------------------------------------
## Physiological data -----------------------------------------------------
# Create table of coordinates of subtables that should be extracted
physiological_coordinates <- tibble(body_top = c(3, 
                                                 3,
                                                 21,
                                                 21,
                                                 39,
                                                 39,
                                                 57),
                                    body_bottom = c(17, 
                                                    17,
                                                    35,
                                                    35,
                                                    53,
                                                    53,
                                                    71), 
                                    body_left = c(1,
                                                  11,
                                                  1,
                                                  11,
                                                  1,
                                                  11,
                                                  1), 
                                    body_right = c(9, 
                                                   18,
                                                   7,
                                                   19,
                                                   9,
                                                   19,
                                                   9))

# Extract and combine subtables
physiological_data <- physiological_coordinates %>%
  pmap_dfr(.f = ~ tidy_extract_physiological(data = physiological_data_raw,
                                             body_top = ..1,
                                             body_bottom = ..2,
                                             body_left = ..3,
                                             body_right = ..4))

## Meta data --------------------------------------------------------------
# Create table of coordinates of subtables that should be extracted
meta_coordinates <- tibble(body_top = c(3, 
                                        6,
                                        11,
                                        15,
                                        18,
                                        26),
                           body_bottom = c(3,
                                           9,
                                           13,
                                           16,  
                                           24,
                                           28))

# Extract and combine subtables
meta_data <- meta_coordinates %>%
  pmap_dfr(.f = ~ tidy_extract_meta(data = meta_data_raw,
                                    body_top = ..1,
                                    body_bottom = ..2))

# Write data --------------------------------------------------------------
write_tsv(x = morphometric_data_raw,
          file = "data/01_morphometric_data.tsv",
          na = "")

write_tsv(x = physiological_data,
          file = "data/01_physiological_data.tsv",
          na = "")

write_tsv(x = meta_data,
          file = "data/01_meta_data.tsv",
          na = "")