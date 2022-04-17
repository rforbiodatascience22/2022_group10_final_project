# Load libraries ----------------------------------------------------------
library("tidyverse")

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
  mutate_all(.funs = str_trim) %>%
  mutate(sex = case_when(sex == "m" ~ "male",
                         sex == "f" ~ "female"))

## Physiological data -----------------------------------------------------
# Clean column values
physiological_data_clean <- physiological_data %>%
  mutate_all(.funs = str_trim) %>%
  mutate(frequency = str_replace(string = frequency,
                                 pattern = "^(\\d+)\\s*kHz$",
                                 replacement = "\\1"),
         cricket_id = str_replace(string = cricket_id,
                                  pattern = "^Nr.\\s*(\\d+)(?:\\s*(re|li))?$",
                                  replacement = "\\1 \\2"),
         auditory_threshold = str_replace(string = auditory_threshold,
                                          pattern = "^(\\d+)\\s*dB$",
                                          replacement = "\\1"),
         sex = str_replace(string = sex,
                           pattern = "^(male|female)\\s+individuals$",
                           replacement = "\\1"))


## Meta data --------------------------------------------------------------
# Rename columns
meta_data <- meta_data %>%
  rename(species_id = No.,
         genus_species_article = Species,
         communication_system_reproduction_method = `Communication system`,
         location = Location,
         collection_date_interval = Date,
         experiments = `Anatomy, Neuroanatomy, Physiology`)

# Clean column values
meta_data_clean <- meta_data %>%
  filter(if_any(.cols = !c(location,
                           group),
                .fns = ~ !is.na(.))) %>%
  fill(everything()) %>%
  mutate_all(.funs = str_trim) %>%
  extract(col = genus_species_article,
          into = c("genus_species",
                   "article_authors",
                   "article_year"),
          regex = "^(\\w(?:\\.|\\w+)\\s+\\w+)\\s+\\(?(.+),\\s+(\\d{4})\\)?$") %>%
  extract(col = communication_system_reproduction_method,
          into = c("communication_system",
                   "obligat_parthenogenetic"),
          regex = "^(\\(?(?:Uni|Bi)(?:\\)|-directional))(?:\\s+(Obligat\\s+parthenogenetic))?") %>%
  mutate(genus_species = str_replace(string = genus_species,
                                     pattern = "^(\\w)(?:\\.|\\w+)\\s+(\\w+)$",
                                     replacement = "\\1\\. \\2"),
         species_id = case_when(genus_species == "I. modestior" ~ "0",
                                TRUE ~ species_id),
         communication_system = {communication_system %>%
             str_to_lower() %>%
             str_replace(pattern = "^\\((uni|bi)\\)$",
                         replacement = "\\1-directional")},
         obligat_parthenogenetic = obligat_parthenogenetic == "Obligat parthenogenetic",
         group = str_replace(string = group,
                             pattern = "^(\\w+)-group$",
                             replacement = "\\1"),
         anatomy = str_detect(string = experiments,
                              pattern = "^A(\\s+|$)"),
         neuroanatomy = str_detect(string = experiments,
                                   pattern = "(^|\\s+)NA(\\s+|$)"),
         physiology = str_detect(string = experiments,
                                 pattern = "(^|\\s+)P$")) %>%
  select(!experiments)

# Write data --------------------------------------------------------------
write_tsv(x = morphometric_data_clean,
          file = "data/02_morphometric_data_clean.tsv")

write_tsv(x = physiological_data_clean,
          file = "data/02_physiological_data_clean.tsv")