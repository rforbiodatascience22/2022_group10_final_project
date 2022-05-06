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
  select(!c(sensillae_count,tympana_anterior_dorso_ventral_length, tympana_posterior_dorso_ventral_length))

morphometric_data_numeric <- 
  morphometric_data  %>% 
  select(where(is.numeric)) %>% 
  drop_na() 
# Model data --------------------------------------------------------------

insect_pca <- 
  morphometric_data_numeric %>% 
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
    "PC5" = "5"#,
    #"PC6" = "6",
    #"PC7" = "7",
    #"PC8" = "8"
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

contribution_data <-
    pca_values %>% 
    filter(PC %in% c("PC1", "PC2")) %>% 
    group_by(PC) %>% 
    ungroup() %>% 
    mutate(column = reorder_within(column, abs(value), PC)) 

label_data <- pull(contribution_data, column)

condition = case_when((str_length(label_data) >= 30 & 
                         str_detect(label_data,"PC1")) ~ 1.1,
                      str_length(label_data) <= 23 & 
                        str_detect(label_data,"PC1") ~ 1.1,
                      str_length(label_data) >= 30 & 
                        str_detect(label_data,"PC2") ~ -0.1,
                      #str_length(label_data) <= 23 & 
                        str_detect(label_data,"spiracle_length___PC2") ~ 1.1,
                      str_detect(label_data, "pronotum_length___PC2") ~ -0.2,
                      str_detect(label_data, "hind_femur_length___PC2") ~ -0.1)#1.1)

contribution_plot <- 
  contribution_data %>% 
    ggplot(mapping = aes(x = abs(value),
                         y = column,
                         label = str_remove(column, "__.+"),
                         fill = value < 0)) +
    geom_col(show.legend = FALSE) +
    geom_text(hjust = condition, 
              size = 3,
              fontface = "bold") +
    facet_wrap(vars(PC),
               scales = "free") +
    scale_y_discrete(labels = function(x) str_remove(string = x,
                                                     pattern = "__.+")) +
    scale_x_continuous(expand = expansion(mult = c(0,0.1))) +
    labs(title = "Contribution of insect data features to first two principal components.",
           subtitle = glue("Absolute contribution of the {pluck(colors,'positive')} and
                           {pluck(colors,'negative')} features.")) +
    theme(plot.title = element_markdown(face = "bold"),
          plot.title.position = "plot",
          plot.subtitle = element_markdown(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  
 ## PCA Plot -----------------------------------------------------------------

PCA_colors <- 
  scales::hue_pal()(morphometric_data %>%
                      pull(communication_group) %>%
                      nlevels()) %>% 
  set_names(morphometric_data %>% 
              pull(communication_group) %>% 
              levels())
PCA_colors <- 
  map2_chr(.x = PCA_colors,
           .y = names(PCA_colors),
           .f = ~glue("<span style='color:{.x};'>{.y}</span>"))


loading_arrow <- arrow(angle = 30,
                       length = unit(0.1, "cm"),
                       ends = "first",
                       type = "closed") 
  
biplot <-
augmented_morpho %>% 
  ggplot(mapping = aes(x = .fittedPC1,
                       y =.fittedPC2)) +
  geom_hline(yintercept = 0, 
             linetype = "dashed",
             alpha = 0.5) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             alpha = 0.5) +
  geom_point(aes(color = communication_group,
                 shape = sex),
             size = 2,
             show.legend = FALSE,
             fill = "white") + 
  geom_text_repel(aes(label = genus_species,
                      color = communication_group),
            size = 2,
            show.legend = FALSE,
            max.overlaps = 5) +
  geom_segment(data = 
                 pca_values %>% 
                 pivot_wider(names_from = PC,
                             values_from = value) %>% 
                 select(column, PC1, PC2) ,
               aes(x = PC1,
               y = PC2,
               xend = 0,
               yend = 0),
               arrow = loading_arrow) +
  geom_text_repel(data = 
                    pca_values %>% 
                    pivot_wider(names_from = PC,
                                values_from = value) %>% 
                    select(column, PC1, PC2),
                  aes(x = PC1,
                  y = PC2,
                  label = column),
                  size = 3) + 
  scale_shape_manual(values = c(21,
                                19)
                     ) +
  labs(title = "A biplot of numeric insect data showing clear seperation between communication groups",
       subtitle = glue("A biplot with scores (points) and  loadings (arrows), with most variation in 
       pronutum length and spiracle length. \n
       Scores are divided into {str_c(PCA_colors, collapse = ', ')}, as well as females 🌑 and males 🌕."),
       x = "PC1", 
       y = "PC2") +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_markdown(),
        panel.grid = element_blank())



### Scree Plot

scree_plot <- 
  insect_pca %>% 
    tidy(matrix = "eigenvalues") %>%
    ggplot(mapping = aes(x = as_factor(PC),
                         y = percent,
                         label = str_c(round(cumulative, 
                                             2)*100,
                                       "%"))) +
    geom_col(fill = "#00abaf",
             alpha = 0.9) +
    geom_text(vjust = -.5,
              fontface = "bold",
              ) +
    scale_y_continuous(labels = scales::label_percent(),
                       breaks = seq(0,1,
                                    by = 0.1),
                       expand = expansion(mult = c(0,0.1))) +
    scale_x_discrete(labels = str_c("PC",1:7)) +
    labs(title = "Variance explained by principal components.",
         subtitle = "Percentage and **cumulative** percent
         of variability explained by each principal component") +
    theme(axis.title = element_blank(),
          plot.title = element_markdown(face = "bold"),
          plot.subtitle = element_markdown()) 
  



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

### marking geoms(hull) seem to go crazy with facet_wrap

label_args <- list(
  xend = c(-4.2, 1.5, 1, -0.8, 1.2),
  yend = c(0.3, 0.5, 0.25, -0.25, -1),
  x = c(-3, 1, -1, -3, 1.5),
  y = c(0.6, 1, 1, -1.5, -1.75),
  label = c("Poecilimon",
            "Propinquus",
            "Ampliatus", 
            "I.Modestior",
            "Poecilimon"),
  vjust = c("bottom", 
            "bottom", 
            "bottom",
            "top",
            "top"),
  hjust = rep("none", 5)
)

k <- kmeans_all %>% 
  unnest(cols = c(sum_of_squares)) %>% 
  pull(k)

ss <- kmeans_all %>% 
  unnest(cols = c(sum_of_squares)) %>% 
  pull(tot.withinss)

labels <-  map2_chr(.x = k,
                    .y = round(ss,0),
                    .f = ~glue("{.x} clusters. \n 
                   {.y} total within sum of squares.")) %>% 
  set_names(1:6)


multiple_kluster <- 
 kmeans_all  %>%
   unnest(cols = c(data))  %>% 
   ggplot(aes(x = .fittedPC1,
              y = .fittedPC2)) +
   geom_point(aes(shape = sex),
              fill = "white",
              show.legend = FALSE) +
   ggforce::geom_mark_ellipse(aes(fill = .cluster,
                               group = .cluster),
                              alpha = 0.3,
                              size = 0.1,
                              show.legend = FALSE)  +
   scale_shape_manual(values = c(21,19)) +
   facet_wrap(~k, labeller = as_labeller(labels)) +
   theme(plot.title = element_markdown(face = "bold"),
         plot.subtitle = element_markdown(),
         axis.title = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         panel.grid = element_blank()) +
   pmap(.l = label_args,
         .f = annotate_with_arrow) + 
   labs(title = "K-Means clustering on insect data*",
        subtitle = "Clustering run with varying number of clusters, females 🌑, males 🌕.")


### total within sum of squares plot 

bold_point <- c(x = 4,
                y = kmeans_all %>% 
                  unnest(cols = c(sum_of_squares)) %>% 
                  filter(k == 4) %>% 
                  pluck("tot.withinss")
                )

twss <- kmeans_all %>% 
  unnest(cols = c(sum_of_squares)) %>% 
  ggplot(mapping = aes(x = k ,
                       y = tot.withinss)) + 
  geom_line(color = "#00abaf") +
  annotate(geom = "point", 
           x = bold_point %>% pluck("x"),
           y = bold_point %>% pluck("y"), 
           color = "#00abaf",
           size = 4, 
  ) +
  geom_point() +
  scale_x_continuous(breaks = 1:6) +
  annotate(geom = "curve",
           x = bold_point %>% pluck("x") + 0.1 * bold_point %>% pluck("x"), 
           y = bold_point %>% pluck("y") + 1 * bold_point %>% pluck("y"),
           xend = bold_point %>% pluck("x"),
           yend = bold_point %>% pluck("y"),
           curvature = 0.4,
           arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text",
           x = bold_point %>% pluck("x") + 0.1 * bold_point %>% pluck("x"),
           y = bold_point %>% pluck("y") + 1 * bold_point %>% pluck("y"),
           label = "4 clusters",
           hjust = "left") +
  labs(title = "Total within sum of squares vs number of clusters.") +
  theme(plot.title = element_markdown(face  = "bold"),
        plot.title.position = "plot",
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())


kmeans_all %>% 
  unnest(cols = c(sum_of_squares)) %>% 
  mutate(k = as_factor(k)) 

### export plots

plots <- list("contribution_plot" = contribution_plot,
              "scree_plot" = scree_plot,
              "biplot" = biplot,
              "multiple_kluster" = multiple_kluster,
              "twss" = twss)

walk2(.x = plots,
     .y = names(plots),
      .f = ~ ggsave(filename = str_c("05_",
                                    .y,
                                    "_plot.pdf"),
                   plot = .x,
                   device = cairo_pdf,
                   path = "results/",
                   width = 27,
                   height = 20,
                   units = "cm"))



