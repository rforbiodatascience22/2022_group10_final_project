# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
#gzfile(description = "data/_raw/JEvolBiol_Sensory evolution Poecilimon_Database.xls.gz") %>%
#  convert(out_file = "data.xlsx")

#gzfile(description = "data/_raw/JEvolBiol_Sensory evolution Poecilimon_Database.xls.gz") %>%
data1_raw <- read_xls(path = "data/_raw/JEvolBiol_Sensory evolution Poecilimon_Database.xls",
                      sheet = "Database")
data2_raw <- read_xls(path = "data/_raw/JEvolBiol_Sensory evolution Poecilimon_Database.xls",
                      sheet = "Database 2",
                      skip = 1,
                      col_names = FALSE)

data2_slice <- function(data, first_row, first_col, last_row, last_col){
  headers <- data %>%
    select(first_col: (first_col + 1) ) %>%
    slice(first_row)
  
  name <- headers %>%
    pull(1)
  
  sex <- headers %>%
    pull(2)
  
  cricket_indicies <- data %>%
    select((first_col + 1) :last_col) %>%
    slice(first_row + 1) %>%
    as.character()
  
  data_slice <- data %>%
    select(first_col:last_col) %>%
    slice( (first_row + 2) :last_row)
  
  colnames(data_slice) <- c("hertz",
                            cricket_indicies)
  
  data_slice <- data_slice %>%
    pivot_longer(cols = !hertz,
                 names_to = "cricket_id",
                 values_to = "decibel")
  
  data_slice %>%
    mutate(name = name,
           sex = sex)
}

data2_1 <- data2_slice(data = data2_raw,
                       first_row = 1,
                       first_col = 1,
                       last_row = 17,
                       last_col = 9)

#data_raw <- read_tsv(file = "data/_raw/my_raw_data.tsv")

# Wrangle data ------------------------------------------------------------
#my_data <- my_data_raw # %>% ...

# Write data --------------------------------------------------------------
#write_tsv(x = my_data,
#          file = "data/01_my_data.tsv")
