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
physiological_subtable_coordinates <- tibble(first_row = c(1,
                                                           1,
                                                           19,
                                                           19,
                                                           37,
                                                           37,
                                                           55),
                                             first_column = c(1,
                                                              11,
                                                              1,
                                                              11,
                                                              1,
                                                              11,
                                                              1),
                                             last_row = c(17,
                                                          17,
                                                          35,
                                                          35,
                                                          53,
                                                          53,
                                                          71),
                                             last_column = c(9,
                                                             18,
                                                             7,
                                                             19,
                                                             9,
                                                             19,
                                                             9))

physiological_data <- physiological_subtable_coordinates %>%
  pmap_dfr(.f = ~ tidy_extract_physiological(data = physiological_data_raw,
                                              first_row = ..1,
                                              first_column = ..2,
                                              last_row = ..3,
                                              last_column = ..4))

meta_subtable_coordinates <- tibble(first_row = c(2,
                                                  5,
                                                  10,
                                                  14,
                                                  17,
                                                  25),
                                    last_row = c(3,
                                                 9,
                                                 13,
                                                 16,
                                                 24,
                                                 28))

meta_data <- meta_subtable_coordinates %>%
  pmap_dfr(.f = ~ tidy_extract_meta(data = meta_data_raw,
                                             first_row = ..1,
                                             first_column = 1,
                                             last_row = ..2,
                                             last_column = 6))

# Write data --------------------------------------------------------------
write_tsv(x = morphometric_data_raw,
          file = "data/01_morphometric_data")
write_tsv(x = physiological_data,
          file = "data/01_physiological_data")
write_tsv(x = meta_data,
          file = "data/01_meta_data")