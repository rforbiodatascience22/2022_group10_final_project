data <- list(morphometric_data_clean, 
             physiological_data_clean,
             meta_data_clean)
morphomet <- read_tsv("data/02_morphometric_data_clean.tsv"
                      )
physio <- read_tsv("data/02_physiological_data_clean.tsv")
meta_d <- read_tsv("data/02_meta_data_clean.tsv"
                   )

view(morphomet)
view(physio)
view(meta_d)


morpho <- morphomet %>% 
  select(species, 
         hind_femur_length,
         `tympana_anterior_proximo-distal_length`,
         sex)

meta <- meta_d  %>% 
  select(species,
         species_group,
         communication_system)


morpho_meta <- left_join(morpho, meta)
view(morpho_meta)


morpho_meta %>%
  filter(species %in% c("ornatus", "gracilis", "zimmeri", "intermedius")) %>% 
  drop_na() %>% 
  ggplot(
    mapping = aes(x = hind_femur_length,
                       y = `tympana_anterior_proximo-distal_length`,
                       color = species,
                       fill = factor(if_else(sex == "male", 
                                      species, sex)))) + 
  geom_point(shape = 21) + 
  scale_color_manual(
    values = c("gracilis" = "red", 
               "intermedius" = "green", 
               "ornatus" = "cyan",
               "zimmeri" = "purple")
  ) +
  scale_fill_manual(
    name = "sex",
    values = c("female" = "white", "gracilis" = "red", 
               "intermedius" = "green", 
               "ornatus" = "cyan",
               "zimmeri" = "purple"),
    guide = "none"
  )
  
morpho_meta %>% 
  drop_na() %>% 
  ggplot(aes(x = hind_femur_length,
             y = `tympana_anterior_proximo-distal_length`,
             color = sex)) + 
  geom_smooth(method = "lm")


morpho_meta %>% 
  drop_na() %>% 
  ggplot(aes(y = hind_femur_length,
             x  = sex)) + 
  geom_boxplot()

morpho_meta %>% 
  drop_na() %>% 
  ggplot(aes(y = `tympana_anterior_proximo-distal_length`,
             x  = sex)) + 
  geom_boxplot()

