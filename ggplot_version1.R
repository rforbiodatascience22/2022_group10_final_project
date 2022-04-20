library(dplyr)
morphometric_data <- read_tsv(file = "data/02_morphometric_data_clean.tsv")
physiological_data <- read_tsv(file = "data/02_physiological_data_clean.tsv")
metadata_data <- read_tsv(file = "data/02_meta_data_clean.tsv")

data1 <- morphometric_data %>%select(1,2,4,6,12)
data2 <- metadata_data %>%select(2,8)
data3 <- physiological_data %>%select(4,7)


data4 <- merge(morphometric_data,physiological_data, by = c("genus_species"))
merged_data <- merge(data4,metadata_data, by = c("genus_species"))

mean_values_plot<-morphometric_data %>% group_by(species,sex) %>%filter(species=="modestior","poecilimon","propinquus","ampliatus")
summarize(sd_hind_femur=sd(`hind_femur_length`, na.rm = TRUE), sd_proximo=sd(`tympana_posterior_proximo-distal_length`,na.rm=TRUE))


ggplot(data = mean_values_plot ,
       mapping = aes(x = sd_hind_femur,y=sd_proximo)) +
  geom_point(aes(shape = species  ))




  