library(dplyr)

morphometric_data <- read_tsv(file = "data/02_morphometric_data_clean.tsv")
morphometric_data %>% group_by(species,sex) %>%
  summarize(sd_hind_femur=sd(`hind_femur_length`, na.rm = TRUE), sd_proximo=sd(`tympana_posterior_proximo-distal_length`,na.rm=TRUE))

ggplot(data = morphometric_data ,
       mapping = aes(x = g2E09)) +
  geom_boxplot()


  