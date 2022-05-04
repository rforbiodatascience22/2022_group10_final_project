# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggrepel")
library("broom")
library(ggrepel)
library(tidytext)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------

morphometric_data <- read_tsv(file = "data/02_morphometric_data_clean.tsv",
                              col_types = cols(genus = "f",
                                               genus_species = "f",
                                               species_group = "f",
                                               sex = "f",
                                               communication_system = "f")) %>% 
  select(!c(species_id,
            article_year,
            longitude, 
            latitude) &
           !starts_with(c("author","collection")) &
           !where(is.logical))

# Wrangle data ------------------------------------------------------------

morphometric_data <- morphometric_data %>%
  mutate(communication_group = {case_when(genus == "Poecilimon" & 
                                            communication_system == "bi-directional" ~ "Poecilimon bi-directional",
                                          TRUE ~ as.character(species_group)) %>%
      as_factor()}) %>% 
  select(!c(sensillae_count))

morphometric_data_numeric <- 
  morphometric_data  %>% 
  select(where(is.numeric)) %>% 
  drop_na() 
# Model data --------------------------------------------------------------

insect_pca <- 
  morphometric_data_test %>% 
  prcomp(scale = TRUE, 
         center = TRUE)

pca_values <- insect_pca %>% 
  tidy(matrix = "loadings") %>% 
  mutate(PC = as_factor(PC)) %>% 
  mutate(PC = fct_recode(.f = PC,
    "PC1" = "1",
    "PC2" = "2",
    "PC3" = "3",
    "PC4" = "4",
    "PC5" = "5",
    "PC6" = "6",
    "PC7" = "7"
  ))
              
augmented_morpho <- insect_pca %>% 
  augment(morphometric_data %>%
            drop_na() %>% 
            select(genus_species,
                   sex,
                   communication_system,
                   communication_group)) 

### Contribution to first 2 PCs --------------------------------------------
colors = scales::hue_pal()(2) %>% 
  set_names(c("negative","positive"))
colors <- colors %>% 
  map2_chr(.x = colors,
          .y = names(colors),
          .f = ~glue("<span style='color:{.x};'>{.y}</span>"))


contribution_plot <- 
  pca_values %>% 
    filter(PC %in% c("PC1", "PC2")) %>% 
    group_by(PC) %>% 
    ungroup() %>% 
    mutate(column = reorder_within(column, abs(value), PC)) %>% 
    ggplot(mapping = aes(x = abs(value),
                         y = column,
                         fill = value < 0)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(vars(PC),scales = "free") +
    scale_y_discrete(labels = function(x) str_remove(string = x,
                                                     pattern = "__.+")) +
    labs(title = "Contribution of insect data features to first two principal components.",
           subtitle = glue("Absolute contribution of the {pluck(colors,'positive')} and
                           {pluck(colors,'negative')} features.")) +
    theme(plot.title = element_markdown(face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_markdown(),
          axis.title = element_blank())
  
  
 ## PCA Plot -----------------------------------------------------------------


biplot <-
augmented_morpho %>% 
  ggplot(mapping = aes(x = .fittedPC1,
                       y =.fittedPC2)) +
  geom_point(aes(color = communication_group,
                 shape = sex),
             size = 2) + 
  geom_text(aes(label = genus_species),
            size = 1) +
  geom_segment(data = 
                 pca_values %>% 
                 pivot_wider(names_from = PC,
                             values_from = value) %>% 
                 select(column, PC1, PC2) ,
               aes(x = PC1,
               y = PC2,
               xend = 0,
               yend = 0)) + 
  geom_text_repel(data = 
                    pca_values %>% 
                    pivot_wider(names_from = PC,
                                values_from = value) %>% 
                    select(column, PC1, PC2),
                  aes(x = PC1,
                  y = PC2,
                  label = column),
                  size = 1)

### Scree Plot

insect_pca %>% 
  tidy(matrix = "eigenvalues") %>%
  ggplot(mapping = aes(x = PC,
                       y = percent,
                       label = round(cumulative, 2))) +
  geom_col() +
  geom_text(vjust = -.5) 


### K-cluster

kmeans_all <- tibble(
  k = 1:6 ) %>% 
  mutate(
  classifier = map(.x = k, 
                   .f = ~kmeans(x = augmented_morpho %>% 
                                  select(where(is.numeric) & c(.fittedPC1,.fittedPC2)),
                                centers = .x)),
  clusters = map(.x = classifier,
                  .f = tidy),
  sum_of_squares = map(.x = classifier,
                        .f = glance),
  data = map(.x = classifier,
              .f = ~ augment(.x,
                             augmented_morpho))
  )

### marking geoms seem to go crazy with facet_wrap
kmeans_all %>% 
  unnest(cols = c(data)) %>% 
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2)) +
  geom_point() + 
  ggforce::geom_mark_hull(aes(fill = .cluster,
                              group = .cluster)) +
  facet_wrap(vars(k))
  

r_squared_sensillae_femur = "bam bam"
colored_name_poecilimon = "sup"

glue("for females. \n\n A regression
line is calculated for bidirectional
{colored_name_poecilimon} species
(R<sup>2</sup> = {r_squared_sensillae_femur})" )




