# Clean column values
physiological_data_clean <- physiological_data %>%
  mutate(across(.cols = everything(),
                .fns = str_trim)) %>%
  separate(col = genus_species,
           into = c("genus",
                    "species"),
           sep = "\\s+",
           remove = FALSE) %>%
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
                           replacement = "\\1"),
         genus = case_when(genus == "P." ~ "Poecilimon",
                           genus == "I." ~ "Isophya"))


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
         across(.cols = c(latitude,
                          longitude),
                .fns = ~ char2dms(from = .,
                                  chd = "°",
                                  chm = "'",
                                  chs = "''") %>%
                  as.numeric()),
         across(.cols = c(collection_date_start,
                          collection_date_end),
                .fns = ~ parse_date_time(x = ., 
                                         orders = c("bY",
                                                    "Y"))
                %>% as_date())) %>%
  separate(col = genus_species,
           into = c("genus",
                    "species"),
           sep = "\\s+",
           remove = FALSE) %>%
  mutate(genus = case_when(genus == "P." ~ "Poecilimon",
                           genus == "I." ~ "Isophya"))

article_authors_max_count <- meta_data_clean %>%
  summarise(article_authors_max_count = max(article_authors_count)) %>%
  pull()

meta_data_clean <- meta_data_clean %>%
  separate(col = article_authors,
           into = str_c("author",
                        1:article_authors_max_count),
           sep = "(\\s+and\\s+|,\\s+)",
           fill = "right") %>%
  select(!c(`Anatomy, Neuroanatomy, Physiology`,
            article_authors_count))

# Write data --------------------------------------------------------------
write_tsv(x = morphometric_data_clean,
          file = "data/02_morphometric_data_clean.tsv")

write_tsv(x = physiological_data_clean,
          file = "data/02_physiological_data_clean.tsv")

write_tsv(x = meta_data_clean,
          file = "data/02_meta_data_clean.tsv")