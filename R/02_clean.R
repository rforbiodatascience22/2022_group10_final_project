# Load libraries ----------------------------------------------------------
library("tidyverse")
library("dplyr")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data1 <- read_tsv(file = "data/01_morphometric_data")
my_data2 <- read_tsv(file = "data/01_physiological_data")
my_data3 <- read_tsv(file = "data/01_meta_data")

# Wrangle data ------------------------------------------------------------
names(my_data1) <- c("Genus","Species","Sex","Pronotum","Hind femur","Spiracle","Tympana anterior proximo-distal","Tympana anterior dorso-ventral","Tympana posterior proximo-distal","Tympana posteriord orso-ventral","Sensillae")
my_data_clean1 <- my_data1   
my_data_clean2 <- my_data2 %>% mutate(frequency = str_extract(frequency,
                                                                        pattern = "\\d+")) %>%
                                      mutate(cricket_id = str_extract(cricket_id,pattern="\\d+")) %>%
                                      mutate(auditory_threshold = str_extract(auditory_threshold,pattern="\\d+")) %>%
                                      mutate(sex = str_remove_all(sex,pattern="individuals"))
names(my_data3) <- c("No.","Species","Communication_system","Location","Year","Anatomy_Neuroanatomy_Physiology","group")
my_data_clean3 <- my_data3 %>%#mutate(Communication_system=str_replace(Communication_system,"(Uni) Obligat parthenogenetic","Uni-directional"),
                      mutate(Year = str_extract(Year,pattern="\\d+"))%>%mutate(Species=str_extract(Species,pattern="P.\\s\\w*\\s"))

# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean1,
          file = "data/02_morphometric_data_cleaned.tsv")
write_tsv(x = my_data_clean2,
          file = "data/02_physiological_data_cleaned.tsv")
write_tsv(x = my_data_clean3,
          file = "data/02_metadata_cleaned.tsv")

