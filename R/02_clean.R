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
  rename(sensillae_count = sensillae) %>%
  
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

# Clean column values
meta_data_clean <- meta_data %>%
  filter(if_any(.cols = !c(Location,
                           species_group),
                .fns = ~ !is.na(.))) %>%
  fill(everything()) %>%
  mutate(across(.cols = everything(),
                .fns = str_trim)) %>%
  extract(col = Species,
          into = c("genus_species",
                   "article_authors",
                   "article_year"),
          regex = "^(\\w(?:\\.|\\w+)\\s+\\w+)\\s+\\(?(.+),\\s+(\\d{4})\\)?$") %>%
  extract(col = Location,
          into = c("location", 
                   "latitude",
                   "longitude"),
          regex = "^(.+?)\\s*\\((\\d+°\\d+'(?:\\d+'')?\\s*N),\\s*(\\d+°\\d+'(?:\\d+'')?\\s*E)\\)") %>%
  separate(col = Date,
           into = c("collection_date_start",
                    "collection_date_end"),
           sep = "-",
           fill = "right") %>%
  mutate(genus_species = str_replace(string = genus_species,
                                     pattern = "^(\\w)(?:\\.|\\w+)\\s+(\\w+)$",
                                     replacement = "\\1\\. \\2"),
         species_id = case_when(genus_species == "I. modestior" ~ "0",
                                TRUE ~ species_id),
         obligat_parthenogenetic = str_detect(string = communication_system,
                                              pattern = "\\s+Obligat\\s+parthenogenetic$"),
         communication_system = {communication_system %>%
             str_to_lower() %>%
             str_replace(pattern = "^\\((uni|bi)\\).*$",
                         replacement = "\\1-directional")},
         species_group = {case_when(species_group == "outgroup" ~ "costata",
                                    TRUE ~ species_group) %>%
             str_replace(pattern = "^(\\w+)-group$",
                         replacement = "\\1")},
         outgroup = species_group == "costata",
         anatomy = str_detect(string = `Anatomy, Neuroanatomy, Physiology`,
                              pattern = "^A(\\s+|$)"),
         neuroanatomy = str_detect(string = `Anatomy, Neuroanatomy, Physiology`,
                                   pattern = "(^|\\s+)NA(\\s+|$)"),
         physiology = str_detect(string = `Anatomy, Neuroanatomy, Physiology`,
                                 pattern = "(^|\\s+)P$"),
         article_authors_count = {article_authors %>% 
             str_count(pattern = "(\\s+and\\s+|,\\s+)") %>% 
             + 1},
         latitude = parse_lat(lat = latitude),
         longitude = parse_lon(lon = longitude),
         across(.cols = c(collection_date_start,
                          collection_date_end),
                .fns = ~ parse_date_time(x = ., 
                                         orders = c("bY",
                                                    "Y")) %>%
                  as_date())) %>%
  separate(col = genus_species,
           into = c("genus",
                    "species"),
           sep = "\\s+",
           remove = FALSE) %>%
  mutate(genus = case_when(genus == "P." ~ "Poecilimon",
                           genus == "I." ~ "Isophya"))

article_authors_max_count <- meta_data_clean %>%
  pull(var = article_authors_count) %>%
  max()

meta_data_clean <- meta_data_clean %>%
  separate(col = article_authors,
           into = str_c("author",
                        1:article_authors_max_count),
           sep = "(\\s+and\\s+|,\\s+)",
           fill = "right") %>%
  select(!c(`Anatomy, Neuroanatomy, Physiology`,
            article_authors_count))






















# Remove junk rows and since values are only recorded in the data set when they
# change replace NA values with values in entry above
meta_data_clean_2 <- meta_data %>%
  filter(if_any(.cols = !c(Location,
                           species_group),
                .fns = ~ !is.na(.))) %>%
  fill(everything()) %>%
  mutate(across(.cols = everything(),
                .fns = str_squish))

# Separate Species column into binomial name, individual authors and article
# year
meta_data_clean_2 <- meta_data_clean_2 %>%
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
meta_data_clean_2 <- meta_data_clean_2 %>%
  separate(col = genus_species,
           into = c("genus",
                    "species"),
           sep = " ",
           remove = FALSE) %>%
  mutate(genus = case_when(genus == "P." ~ "Poecilimon",
                           genus == "I." ~ "Isophya"))

# Add missing species ID for I. modestior
meta_data_clean_2 <- meta_data_clean_2 %>%
  mutate(species_id = case_when(genus_species == "I. modestior" ~ "0",
                                TRUE ~ species_id))

# Make communication_system column values consistent and move info about obligat
# parthenogenesis into its own column
meta_data_clean_2 <- meta_data_clean_2 %>%
  mutate(communication_system = str_to_lower(string = communication_system),
         obligat_parthenogenetic = str_ends(string = communication_system,
                                              pattern = fixed("obligat parthenogenetic")),
         communication_system = str_replace(string = communication_system,
                                            pattern = "^\\((uni|bi)\\).*$",
                                            replacement = "\\1-directional"))









# meta_data_clean <- meta_data %>%
#   filter(if_any(.cols = !c(Location,
#                            species_group),
#                 .fns = ~ !is.na(.))) %>%
#   fill(everything()) %>%
#   mutate(across(.cols = everything(),
#                 .fns = str_trim)) %>%
#   extract(col = Species,
#           into = c("genus_species",
#                    "article_authors",
#                    "article_year"),
#           regex = "^(\\w(?:\\.|\\w+)\\s+\\w+)\\s+\\(?(.+),\\s+(\\d{4})\\)?$") %>%
  extract(col = Location,
          into = c("location", 
                   "latitude",
                   "longitude"),
          regex = "^(.+?)\\s*\\((\\d+°\\d+'(?:\\d+'')?\\s*N),\\s*(\\d+°\\d+'(?:\\d+'')?\\s*E)\\)") %>%
  separate(col = Date,
           into = c("collection_date_start",
                    "collection_date_end"),
           sep = "-",
           fill = "right") %>%
  # mutate(genus_species = str_replace(string = genus_species,
  #                                    pattern = "^(\\w)(?:\\.|\\w+)\\s+(\\w+)$",
  #                                    replacement = "\\1\\. \\2"),
  #        species_id = case_when(genus_species == "I. modestior" ~ "0",
  #                               TRUE ~ species_id),
         # obligat_parthenogenetic = str_detect(string = communication_system,
         #                                      pattern = "\\s+Obligat\\s+parthenogenetic$"),
         # communication_system = {communication_system %>%
         #     str_to_lower() %>%
         #     str_replace(pattern = "^\\((uni|bi)\\).*$",
         #                 replacement = "\\1-directional")},
         species_group = {case_when(species_group == "outgroup" ~ "costata",
                                    TRUE ~ species_group) %>%
             str_replace(pattern = "^(\\w+)-group$",
                         replacement = "\\1")},
         outgroup = species_group == "costata",
         anatomy = str_detect(string = `Anatomy, Neuroanatomy, Physiology`,
                              pattern = "^A(\\s+|$)"),
         neuroanatomy = str_detect(string = `Anatomy, Neuroanatomy, Physiology`,
                                   pattern = "(^|\\s+)NA(\\s+|$)"),
         physiology = str_detect(string = `Anatomy, Neuroanatomy, Physiology`,
                                 pattern = "(^|\\s+)P$"),
         # article_authors_count = {article_authors %>% 
         #     str_count(pattern = "(\\s+and\\s+|,\\s+)") %>% 
         #     + 1},
         latitude = parse_lat(lat = latitude),
         longitude = parse_lon(lon = longitude),
         across(.cols = c(collection_date_start,
                          collection_date_end),
                .fns = ~ parse_date_time(x = ., 
                                         orders = c("bY",
                                                    "Y")) %>%
                  as_date())) %>%
#   separate(col = genus_species,
#            into = c("genus",
#                     "species"),
#            sep = "\\s+",
#            remove = FALSE) %>%
#   mutate(genus = case_when(genus == "P." ~ "Poecilimon",
#                            genus == "I." ~ "Isophya"))
# 
# article_authors_max_count <- meta_data_clean %>%
#   pull(var = article_authors_count) %>%
#   max()
# 
# meta_data_clean <- meta_data_clean %>%
#   separate(col = article_authors,
#            into = str_c("author",
#                         1:article_authors_max_count),
#            sep = "(\\s+and\\s+|,\\s+)",
#            fill = "right") %>%
  select(!c(`Anatomy, Neuroanatomy, Physiology`,
            article_authors_count))









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