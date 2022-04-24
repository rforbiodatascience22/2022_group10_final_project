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

### Summary data function

mean_sd <- function(df, mean_of) {
  
  mean_name <- map_chr(.x = mean_of, ~str_c(.x, "_mean"))
  sd_name <- map_chr(.x = mean_of, ~str_c(.x, "_sd"))
  low <- map_chr(.x = mean_of, ~str_c(.x, "_low"))
  high <- map_chr(.x = mean_of, ~str_c(.x, "_high"))
  
  tib <- tibble("{mean_name[1]}" := mean(df[[mean_of[1]]], na.rm = TRUE)) %>% 
    mutate("{sd_name[1]}" := sd(df[[mean_of[1]]], na.rm = TRUE),
           "{low[1]}" := .data[[mean_name[1]]] - .data[[sd_name[1]]],
           "{high[1]}" := .data[[mean_name[1]]] + .data[[sd_name[1]]])
  
  
  for (i in 2:length(mean_of)) {
    tib <- tib %>% 
      mutate("{mean_name[i]}" := mean(df[[mean_of[i]]], 
                                      na.rm = TRUE),
             "{sd_name[i]}" := sd(df[[mean_of[i]]],
                                  na.rm = TRUE),
             "{low[i]}" := .data[[mean_name[i]]] - .data[[sd_name[i]]],
             "{high[i]}" := .data[[mean_name[i]]] + .data[[sd_name[i]]]
      )
  }
  tib
  
}

### Plots the Hollow figures

hollow_fig <- function(data, x_var, y_var, color, shape) {
  x_var_char <- as.character(substitute(x_var))
  y_var_char <- as.character(substitute(y_var))
  suffix <- c("_mean","_low","_high")
  x_var <- map_chr(.x = suffix, .f = ~str_c(x_var_char, .x))
  y_var <- map_chr(.x = suffix, .f = ~ str_c(y_var_char,.x))
  x_var
  y_var
  
  
  
  ggplot(data, mapping = aes_(x = as.name(x_var[1]),
                              y = as.name(y_var[1]),
                              color = substitute(color),
                              shape = substitute(shape))) +
    list(
      geom_pointrange(aes_(ymin = as.name(y_var[2]),
                           ymax = as.name(y_var[3])),
                      fill = "white"),
      geom_pointrange(aes_(xmin = as.name(x_var[2]),
                           xmax = as.name(x_var[3])),
                      fill = "white"),
      scale_shape_manual(values = c(21,
                                    19))
    )
  
}


### Fig 2 data
morphometric_data_summary_2 <- morphometric_data %>%
  mutate(plot_grouping = {case_when(genus == "Poecilimon" & communication_system == "bi-directional" ~ "Poecilimon bi-directional",
                                    TRUE ~ as.character(species_group)) %>%
      as.factor()}) %>%
  group_by(genus_species,
           sex) %>% 
  summarise(mean_sd(df = .data, mean_of = c("tympana_anterior_proximo_distal_length",
                                            "hind_femur_length")),
            plot_grouping)

### Fig 4 data
morphometric_data_summary_4 <- morphometric_data %>%
  mutate(plot_grouping = {case_when(genus == "Poecilimon" & communication_system == "bi-directional" ~ "Poecilimon bi-directional",
                                    TRUE ~ as.character(species_group)) %>%
      as.factor()}) %>%
  group_by(genus_species,
           sex) %>% 
  summarise(mean_sd(df = .data, mean_of = c("spiracle_length",
                                            "hind_femur_length")),
            plot_grouping) 


### Fig 5 data
morphometric_data_summary_5 <- morphometric_data %>%
  mutate(plot_grouping = {case_when(genus == "Poecilimon" & communication_system == "bi-directional" ~ "Poecilimon bi-directional",
                                    TRUE ~ as.character(species_group)) %>%
      as.factor()}) %>%
  group_by(genus_species,
           sex) %>% 
  summarise(mean_sd(df = .data, mean_of = c("spiracle_length",
                                            "tympana_anterior_proximo_distal_length")),
            plot_grouping)


### Fig 7 data
morphometric_data_summary_7 <- morphometric_data %>%
  mutate(plot_grouping = {case_when(genus == "Poecilimon" & communication_system == "bi-directional" ~ "Poecilimon bi-directional",
                                    TRUE ~ as.character(species_group)) %>%
      as.factor()}) %>%
  group_by(genus_species,
           sex) %>% 
  summarise(mean_sd(df = .data, mean_of = c("sensillae_count",
                                            "hind_femur_length")),
            plot_grouping) 



### Fig 2

morphometric_data_summary_2 %>% 
  hollow_fig(x_var = hind_femur_length,
             y_var = tympana_anterior_proximo_distal_length,
             color = plot_grouping,
             shape = sex)
### Fig 4

data_2 <- morphometric_data_summary_4 %>% 
  filter(plot_grouping == "Poecilimon bi-directional") 
morphometric_data_summary_4 %>% 
  hollow_fig(x_var = hind_femur_length,
             y_var = spiracle_length,
             color = plot_grouping,
             shape = sex) + 
  geom_smooth(data = data_2, 
              aes(x = hind_femur_length_mean,
                  y = spiracle_length_mean,
                  linetype = sex),
              method = "lm", 
  ) 
### Fig 5

morphometric_data_summary_5 %>% 
  hollow_fig(x_var = tympana_anterior_proximo_distal_length,
             y_var = spiracle_length,
             color = plot_grouping,
             shape = sex) +
  geom_abline(slope = 1)
?geom_smooth()

### Fig 7

morphometric_data_summary_7 %>% 
  filter(sex == "female") %>% 
  hollow_fig(x_var = hind_femur_length,
             y_var = sensillae_count,
             color = plot_grouping,
             shape = sex)


