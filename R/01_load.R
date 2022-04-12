# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
extractor <- function(path = "data/_raw/JEvolBiol_Sensory evolution Poecilimon_Database.xls", 
                      coords){
  
  df_test <- read_xls(path = path, range = coords, sheet = "Database 2")
  
  names <- df_test %>% 
    names()
  specie <- names[[1]]
  
  sex <- names[[2]]
  
  indices <- df_test %>% 
    select(!1) %>% 
    slice(1) %>% 
    as.character()
  
  data <- df_test %>% 
    slice(2:n())
  
  colnames(data) <- c("hertz", 
                      indices)
  
  data <- data %>% 
    pivot_longer(cols = !hertz,
                 names_to = "id",
                 values_to = "decibel")
  
  data %>% 
    mutate(specie = specie, 
           sex = sex, 
           hertz = as.character(hertz))
  
}

places <- c("A2:I18","K2:R18","A20:G36", 
            "K20:S36", "A38:I54", "K38:S54", "A56:I72")

combiner <- function(coords){
  map_dfr(.x = coords, .f = ~ extractor(coords = .x))
  
}

combiner(coords = places)