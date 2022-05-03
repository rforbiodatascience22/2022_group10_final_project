# Load libraries ----------------------------------------------------------
library("tidyverse")
library("parzer")
library("lubridate")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
morphometric_data <- read_tsv(file = "data/01_morphometric_data.tsv",
                              na = "",
                              col_types = cols(.default = "c"))

physiological_data <- read_tsv(file = "data/01_physiological_data.tsv",
                               na = "",
                               col_types = cols(.default = "c"))

meta_data <- read_tsv(file = "data/01_meta_data.tsv",
                      na = "",
                      col_types = cols(.default = "c"))

# Wrangle data ------------------------------------------------------------
## Morphometric data ------------------------------------------------------
# Rename columns
morphometric_data_clean <- morphometric_data %>%
  rename_with(.fn = ~ .x %>%
                str_to_lower() %>%
                str_replace_all(pattern = "[\\s-]",
                                replacement = "_") %>%
                str_replace(pattern = fixed("[mm]"),
                            replacement = "length")) %>%
  rename(sensillae_count = sensillae)

# Clean column values
morphometric_data_clean <- morphometric_data_clean %>%
  mutate(across(.cols = everything(),
                .fns = str_squish),
         sex = case_when(sex == "m" ~ "male",
                         sex == "f" ~ "female"))

## Physiological data -----------------------------------------------------
# Clean column values by removing units and other junk
physiological_data_clean <- physiological_data %>%
  mutate(across(.cols = everything(),
                .fns = str_squish),
         across(.cols = c(frequency,
                          auditory_threshold,
                          sex),
                .fns = ~ word(string = .x,
                            start = 1,
                            end = 1)),
         cricket_id = str_remove(string = cricket_id,
                                 pattern = fixed("Nr.")) %>% 
           str_trim(side = "left"))

## Meta data --------------------------------------------------------------
# Rename columns
meta_data <- meta_data %>%
  rename(species_id = No.,
         communication_system = `Communication system`)

# Remove junk rows and since values are only recorded in the data set when they
# change replace NA values with values in entry above
meta_data_clean <- meta_data %>%
  filter(if_any(.cols = !c(Location,
                           species_group),
                .fns = ~ !is.na(.))) %>%
  fill(everything()) %>%
  mutate(across(.cols = everything(),
                .fns = str_squish))

# Separate Species column into binomial name, individual authors and article
# year
meta_data_clean <- meta_data_clean %>%
  mutate(genus_species = word(string = Species,
                              start = 1,
                              end = 2),
         genus_species = str_replace(string = genus_species,
                                     pattern = "^(\\w)\\w+",
                                     replacement = "\\1\\."),
         article = word(string = Species,
                        start = 3, 
                        end = -1),
         article = str_remove_all(string = article,
                                  pattern = "[\\(\\)]")) %>%
  separate(col = article,
           into = c("authors",
                    "article_year"),
           sep = ",") %>%
  separate(col = authors,
           into = c("author1",
                    "author2"),
           sep = " and ",
           fill = "right") %>%
  mutate(article_year = str_trim(string = article_year,
                                 side = "left"))

# Create genus and species columns from genus_species column
meta_data_clean <- meta_data_clean %>%
  separate(col = genus_species,
           into = c("genus",
                    "species"),
           sep = " ",
           remove = FALSE) %>%
  mutate(genus = case_when(genus == "P." ~ "Poecilimon",
                           genus == "I." ~ "Isophya"))

# Add missing species ID for I. modestior
meta_data_clean <- meta_data_clean %>%
  mutate(species_id = case_when(genus_species == "I. modestior" ~ "0",
                                TRUE ~ species_id))

# Make communication_system column values consistent and move info about obligat
# parthenogenesis into its own column
meta_data_clean <- meta_data_clean %>%
  mutate(communication_system = str_to_lower(string = communication_system),
         obligat_parthenogenetic = str_ends(string = communication_system,
                                              pattern = fixed("obligat parthenogenetic")),
         communication_system = str_replace(string = communication_system,
                                            pattern = "^\\((uni|bi)\\).*$",
                                            replacement = "\\1-directional"))

# Separate Location column into the names of the location, latitude and
# longitude
meta_data_clean <- meta_data_clean %>%
  separate(col = Location,
           into = c("location",
                    "gps_coordinates"),
           sep = "(?=\\()") %>%
  mutate(location = str_trim(string = location),
         gps_coordinates = str_extract(string = gps_coordinates,
                                       pattern = "(?<=\\().+(?=\\))")) %>%
  separate(col = gps_coordinates,
           into = c("latitude",
                    "longitude"),
           sep = ",") %>%
  mutate(latitude = parse_lat(lat = latitude),
         longitude = parse_lon(lon = longitude))

# Convert Date column into  ISO 8601 format (YYYY-MM-DD) and divide year
# intervals into separate columns
meta_data_clean <- meta_data_clean %>%
  separate(col = Date,
           into = c("collection_date_start",
                    "collection_date_end"),
           sep = "-",
           fill = "right") %>%
  mutate(across(.cols = c(collection_date_start,
                          collection_date_end),
                .fns = ~ parse_date_time(x = ., 
                                         orders = c("bY",
                                                    "Y")) %>%
                  as_date()))

# Separate info about for which species that there was done anatomy,
# neuroanatomy and physiology experiments into each own column
meta_data_clean <- meta_data_clean %>%
  bind_cols(map(.x = c(anatomy = "A", 
                       neuroanatomy = "NA",
                       physiology = "P"),
                .f = ~ map2_lgl(.x = .x,
                                .y = meta_data_clean %>%
                                  pull(`Anatomy, Neuroanatomy, Physiology`) %>%
                                  str_split(pattern = fixed(" ")),
                                .f = ~ .x %in% .y)))

# Change name of outgoup species group to costata and make new column with info
# about which species is an outgroup
meta_data_clean <- meta_data_clean %>%
  mutate(species_group = {case_when(species_group == "outgroup" ~ "costata",
                                    TRUE ~ species_group) %>%
      str_remove(pattern = fixed("-group"))},
      outgroup = species_group == "costata")

# Remove redundant columns
meta_data_clean <- meta_data_clean %>%
  select(!c(Species,
            `Anatomy, Neuroanatomy, Physiology`))

## Join data --------------------------------------------------------------
# Join morphometric data with meta data and remove NA columns
morphometric_data_clean <- meta_data_clean %>%
  filter(anatomy == TRUE) %>%
  left_join(x = morphometric_data_clean,
            y = .,
            by = c("genus",
                   "species")) %>%
  select(where(~sum(!is.na(.x)) > 0))

# Join physiological data with meta data and remove NA columns
physiological_data_clean <- meta_data_clean %>%
  filter(physiology == TRUE) %>%
  left_join(x = physiological_data_clean,
            y = .,
            by = "genus_species") %>%
  select(where(~sum(!is.na(.x)) > 0))

# Write data --------------------------------------------------------------
write_tsv(x = morphometric_data_clean,
          file = "data/02_morphometric_data_clean.tsv")

write_tsv(x = physiological_data_clean,
          file = "data/02_physiological_data_clean.tsv")