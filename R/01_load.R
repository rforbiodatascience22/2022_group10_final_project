# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")
library("bench")
library("profvis")

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
  pmap_dfr(.f = ~ tidy_subtable_physiological(data = physiological_data_raw,
                                              first_row = ..1,
                                              first_column = ..2,
                                              last_row = ..3,
                                              last_column = ..4))

# Write data --------------------------------------------------------------
#write_tsv(x = my_data,
#          file = "data/01_my_data.tsv")