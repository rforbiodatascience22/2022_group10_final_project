# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
morphometric_data <- read_tsv(file = "data/02_morphometric_data_clean.tsv",
                              col_select = c(genus,
                                             genus_species,
                                             species_group,
                                             sex,
                                             communication_system,
                                             tympana_anterior_proximo_distal_length,
                                             hind_femur_length,
                                             spiracle_length,
                                             sensillae_count),
                              col_types = cols(genus = "f",
                                               genus_species = "f",
                                               species_group = "f",
                                               sex = "f",
                                               communication_system = "f"))

# Wrangle data ------------------------------------------------------------
# Create new grouping "communication group" based on communication system,
# genus and species group
morphometric_data <- morphometric_data %>%
  mutate(communication_group = {case_when(genus == "Poecilimon" & communication_system == "bi-directional" ~ "Poecilimon bi-directional",
                                          TRUE ~ as.character(species_group)) %>%
      as_factor()})

# Visualise data ----------------------------------------------------------
morphometric_data %>%
  ggplot(mapping = aes(x = hind_femur_length,
                       y = tympana_anterior_proximo_distal_length,
                       color = genus_species)) +
  geom_point(data = morphometric_data %>% select(!communication_group),
             color = "grey") +
  geom_point(mapping = aes(shape = sex),
             fill = "white") +
  facet_wrap(facets = vars(communication_group)) +
  scale_shape_manual(values = c(21,
                                19))


morphometric_data_dark_grey <- morphometric_data %>%
  group_by(communication_group) %>%
  expand(nesting(genus_species)) %>%
  left_join(y = morphometric_data %>%
              select(!genus_species),
            by = "communication_group")

tympana_vs_femur_alternative_plot <- morphometric_data %>%
  ggplot(mapping = aes(x = hind_femur_length,
                       y = tympana_anterior_proximo_distal_length,
                       color = communication_group)) +
  geom_point(data = morphometric_data %>%
               select(!c(communication_group,
                         genus_species)),
             color = "grey") +
  geom_point(data = morphometric_data_dark_grey,
             color = "grey50") +
  geom_point(mapping = aes(shape = sex),
             fill = "white") +
  facet_wrap(facets = vars(communication_group, 
                           genus_species)) +
  scale_shape_manual(values = c(21,
                                19)) +
  theme(legend.position = "none")

# Write data --------------------------------------------------------------
ggsave(filename = str_c("04_",
                        ..9,
                        "_plot.svg"),
       plot = ..10,
       path = "results",
       width = 30,
       height = 20,
       units = "cm")