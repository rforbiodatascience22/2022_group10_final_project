# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggrepel")
library("broom")
library(ggrepel)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------

morphometric_data <- read_tsv(file = "data/02_morphometric_data_clean.tsv",
                              col_types = cols(genus = "f",
                                               genus_species = "f",
                                               species_group = "f",
                                               sex = "f",
                                               communication_system = "f")) %>% 
  select(!species_id &
           !starts_with(c("author","collection")) &
           !article_year &
           !where(is.logical
         ))

# Wrangle data ------------------------------------------------------------

morphometric_data <- morphometric_data %>%
  mutate(communication_group = {case_when(genus == "Poecilimon" & 
                                            communication_system == "bi-directional" ~ "Poecilimon bi-directional",
                                          TRUE ~ as.character(species_group)) %>%
      as_factor()})

morphometric_data_test <- 
  morphometric_data  %>% 
  select(where(is.numeric)) %>% 
  select(!c(sensillae_count)) %>% 
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
              

### Contribution to first 3 PCs --------------------------------------------

augmented_morpho <- insect_pca %>% 
  augment(morphometric_data %>% 
            select(!c(sensillae_count)) %>% 
            drop_na() %>% 
            select(genus_species,
                   sex,
                   communication_system,
                   communication_group)) 
pca_values %>% 
  filter(PC %in% c("PC1", "PC2")) %>% 
  ggplot(mapping = aes(x = value,
                       y = column,
                       fill = value > 0)) +
  geom_col() +
  facet_wrap(vars(PC), nrow = 1)



## PCA Plot -----------------------------------------------------------------

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
  ggforce::geom_mark_ellipse(aes(fill = .cluster,
                              group = .cluster)) +
  facet_wrap(vars(k))
  
