# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggtext")
library("ggrepel")
library("broom")
library("glue")

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

# Calculate species means +-sd for both sexes for selected morphometric
# variables
morphometric_summary <- morphometric_data %>%
  group_by(genus_species,
           sex,
           communication_group) %>%
  summarise(across(.cols = c(tympana_anterior_proximo_distal_length,
                             hind_femur_length,
                             spiracle_length,
                             sensillae_count),
                   .fns = list(mean = ~ mean(x = .x,
                                             na.rm = TRUE),
                               low = ~ mean(x = .x,
                                            na.rm = TRUE) - sd(x = .x,
                                                               na.rm = TRUE),
                               high = ~ mean(x = .x,
                                             na.rm = TRUE) + sd(x = .x,
                                                                na.rm = TRUE))))

# Model data --------------------------------------------------------------
# Create linear models for bi-directional Poecilimon species grouped by sex and
# for selected variables
lm_models <- morphometric_summary %>%
  filter(communication_group == "Poecilimon bi-directional") %>%
  group_by(sex,
           communication_group) %>%
  nest() %>%
  mutate(formula = "spiracle_length_mean ~ hind_femur_length_mean") %>%
  bind_rows(morphometric_summary %>%
              filter(sex == "female",
                     communication_group == "Poecilimon bi-directional") %>%
              group_by(sex,
                       communication_group) %>%
              nest() %>% 
              mutate(formula = "sensillae_count_mean ~ hind_femur_length_mean")) %>%
  ungroup() %>%
  mutate(model = map(.x = data,
                     .f = ~ lm(formula = formula,
                               data = .x)),
         model_tidy = map(.x = model,
                             .f = tidy),
         model_glance = map(.x = model,
                               .f = glance))

# Extract table from fitted linear models with info about the statistical
# findings (coefficients etc.)
lm_models_tidy <- lm_models %>%
  ungroup() %>%
  select(sex,
         communication_group,
         formula,
         model_tidy) %>%
  unnest(model_tidy) %>%
  pivot_wider(id_cols = c(sex,
                          communication_group,
                          formula),
              names_from = term,
              values_from = estimate)

# Extract table from fitted linear models with summaries of the models
# (R^2 etc.)
lm_models_glance <- lm_models %>%
  select(sex,
         communication_group,
         formula,
         model_glance) %>%
  unnest(model_glance)

# Visualise data ----------------------------------------------------------
color_groups_count <- morphometric_summary %>%
  pull(communication_group) %>%
  nlevels()

color_groups_names <- morphometric_summary %>%
  pull(communication_group) %>%
  levels()

color_palette <- scales::hue_pal()(color_groups_count) %>%
  set_names(color_groups_names)


colored_name_modestior <- color_palette %>%
  glue_data("<span style='color:{costata};'>I. modestior</span>")

colored_name_poecilimon <- color_palette %>%
  glue_data("<span style='color:{`Poecilimon bi-directional`};'>",
            "Poecilimon</span>")

colored_name_propinquus <- color_palette %>%
  glue_data("<span style='color:{propinquus};'>P. propinquus</span>")

colored_name_ampliatus <- color_palette %>%
  glue_data("<span style='color:{ampliatus};'>P. ampliatus</span>")


subtitle_main <- " for the bidirectional outgroup species
  ${colored_name_modestior}, the bidirectional ${colored_name_poecilimon}
  species, \n\n the unidirectional species of the ${colored_name_propinquus}
  group and ${colored_name_ampliatus} group. Lengths [mm] are presented as
  species means ±SD" %>%
  str_interp()

r_squared_sensillae_femur <- lm_models_glance %>%
  filter(formula == "sensillae_count_mean ~ hind_femur_length_mean") %>%
  pluck("r.squared") %>%
  round(3)

r_squared_spiracle_femur_male <- lm_models_glance %>%
  filter(formula == "spiracle_length_mean ~ hind_femur_length_mean",
         sex == "male") %>%
  pluck("r.squared") %>%
  round(3)

r_squared_spiracle_femur_female <- lm_models_glance %>%
  filter(formula == "spiracle_length_mean ~ hind_femur_length_mean",
         sex == "female") %>%
  pluck("r.squared") %>%
  round(3)

plotting_table <- tibble(data = list(morphometric_summary,
                                     morphometric_summary,
                                     morphometric_summary,
                                     morphometric_summary %>%
                                       filter(sex == "female")),
                         x = c("hind_femur_length",
                               "hind_femur_length",
                               "tympana_anterior_proximo_distal_length",
                               "hind_femur_length"),
                         y = c("tympana_anterior_proximo_distal_length",
                               "spiracle_length",
                               "spiracle_length",
                               "sensillae_count"),
                         shape = list("sex",
                                      "sex",
                                      "sex",
                                      NULL),
                         title = c("Tympana length scales with femur length for
                                   tettigoniids",
                                   
                                   "Spiracles in bidirectional species of
                                   Tettigoniidae are consistently larger than in
                                   the unidirectional species",
                                   
                                   "There is no clear relationship between
                                   spiracle and tympana size in Tettigoniidae",
                                   
                                   "Sensilla number is highest in bidirectional
                                   species of Tettigoniidae"),
                         y_text = c("Proximodistal length of the anterior
                                    tympana",
                                    "Spiracle length",
                                    "Spiracle length",
                                    "Number of auditory sensilla"),
                         x_text = c("hind fermur length",
                                    "hind fermur length",
                                    "proximodistal length of the anterior
                                    tympana",
                                    "hind fermur length"),
                         subtitle_end = list("\n\n for females 🌑 and  males 🌕",
                                             
                                             "\n\n for females 🌑 and  males 🌕.
                                             Regression lines are calculated for
                                             bidirectional
                                             ${colored_name_poecilimon} species
                                             for males ╏ (R<sup>2</sup> =
                                             ${r_squared_spiracle_femur_male})
                                             and females ┃ (R<sup>2</sup> =
                                             ${r_squared_spiracle_femur_female})" %>%
                                               str_interp(),
                                             
                                             "\n\n for females 🌑 and  males 🌕.
                                             The line represents equal lengths
                                             for both hearing structures",
                                             
                                             " for females. \n\n A regression
                                             line is calculated for bidirectional
                                             ${colored_name_poecilimon} species
                                             (R<sup>2</sup> =
                                             ${r_squared_sensillae_femur})" %>% 
                                               str_interp()))

plotting_table <- plotting_table %>%
  mutate(filename = str_c(y,
                          x,
                          sep = "_vs_"),
         plot = pmap(.l = plotting_table,
                     .f = ~ plot_mean_scatter(data = ..1,
                                         x = ..2,
                                         y = ..3,
                                         color = "communication_group",
                                         shape = ..4,
                                         title = ..5,
                                         y_text = ..6,
                                         x_text = ..7,
                                         subtitle_main = subtitle_main,
                                         subtitle_end = ..8,
                                         color_palette = color_palette)),
         plot = map_if(.x = plot,
                       .p = filename == "spiracle_length_vs_hind_femur_length",
                       .f = ~ .x +
                         geom_abline(mapping = aes(slope = hind_femur_length_mean, 
                                                   intercept = `(Intercept)`,
                                                   linetype = sex,
                                                   color = communication_group),
                                     data = lm_models_tidy %>%
                                       filter(formula == "spiracle_length_mean ~ hind_femur_length_mean")) +
                         scale_linetype_manual(values = c("dashed",
                                                          "solid"))),
         plot = map_if(.x = plot,
                       .p = filename == "spiracle_length_vs_tympana_anterior_proximo_distal_length",
                       .f = ~ .x +
                         geom_abline(slope = 1,
                                     intercept = 0) +
                         scale_y_continuous(breaks = scales::breaks_width(width = 0.2),
                                            minor_breaks = scales::breaks_width(width = 0.2,
                                                                                offset = 0.1))),
         plot = map_if(.x = plot,
                       .p = filename == "sensillae_count_vs_hind_femur_length",
                       .f = ~ .x +
                         geom_abline(mapping = aes(slope = hind_femur_length_mean,
                                                   intercept = `(Intercept)`,
                                                   color = communication_group),
                                     data = lm_models_tidy %>%
                                       filter(formula == "sensillae_count_mean ~ hind_femur_length_mean")) +
                         geom_text_repel(mapping = aes(label = genus_species),
                                         color = "black",
                                         seed = 69420,
                                         box.padding = 1)))

# Write data --------------------------------------------------------------
plotting_table %>%
  pwalk(.f = ~ ggsave(filename = str_c("04_",
                                       ..9,
                                       "_plot.pdf"),
                      plot = ..10,
                      device = cairo_pdf,
                      path = "data/images/",
                      width = 27,
                      height = 20,
                      units = "cm"))