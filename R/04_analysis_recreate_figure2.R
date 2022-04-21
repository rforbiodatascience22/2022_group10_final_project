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
                                             hind_femur_length),
                              col_types = cols(genus = "f",
                                               genus_species = "f",
                                               species_group = "f",
                                               sex = "f",
                                               communication_system = "f"))

# Wrangle data ------------------------------------------------------------
morphometric_data_summary <- morphometric_data %>%
  mutate(plot_grouping = {case_when(genus == "Poecilimon" & communication_system == "bi-directional" ~ "Poecilimon bi-directional",
                                   TRUE ~ as.character(species_group)) %>%
      as.factor()}) %>%
  group_by(genus_species,
           sex) %>%
  summarise(tympana_anterior_proximo_distal_length_mean = mean(tympana_anterior_proximo_distal_length,
                                                               na.rm = TRUE),
            tympana_anterior_proximo_distal_length_sd = sd(tympana_anterior_proximo_distal_length,
                                                           na.rm = TRUE),
            hind_femur_length_mean = mean(hind_femur_length,
                                          na.rm = TRUE),
            hind_femur_length_sd = sd(hind_femur_length,
                                      na.rm = TRUE),
            plot_grouping) %>%
  mutate(tympana_anterior_proximo_distal_length_low = tympana_anterior_proximo_distal_length_mean - tympana_anterior_proximo_distal_length_sd,
         tympana_anterior_proximo_distal_length_high = tympana_anterior_proximo_distal_length_mean + tympana_anterior_proximo_distal_length_sd,
         hind_femur_length_low = hind_femur_length_mean - hind_femur_length_sd,
         hind_femur_length_high = hind_femur_length_mean + hind_femur_length_sd)

# Model data --------------------------------------------------------------
#my_data_clean_aug %>% ...

# Visualise data ----------------------------------------------------------
morphometric_data_summary %>%
ggplot(mapping = aes(x = hind_femur_length_mean,
                     y = tympana_anterior_proximo_distal_length_mean,
                     color = plot_grouping,
                     shape = sex)) +
geom_pointrange(mapping = aes(ymin = tympana_anterior_proximo_distal_length_low,
                              ymax = tympana_anterior_proximo_distal_length_high),
                fill = "white") +
geom_pointrange(mapping = aes(xmin = hind_femur_length_low,
                              xmax = hind_femur_length_high),
                fill = "white") +
scale_shape_manual(values = c(21,
                              19))

# Write data --------------------------------------------------------------
#write_tsv(...)
#ggsave(...)