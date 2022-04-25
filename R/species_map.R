### load the data for map
library(sf)
library(spData)
library(ggrepel)

map_data <- read_tsv(file = "data/02_morphometric_data_clean.tsv",
                     col_select = c(
                       genus_species,
                       location,
                       latitude,
                       longitude))

map_data_specie_count <- map_data %>% 
  group_by(genus_species) %>% 
  tally()

map_data <- map_data %>% 
  left_join(map_data_specie_count) %>% 
  distinct()



theworld <- spData::world %>% filter(region_un == "Europe")
view(theworld)
ggplot(theworld) +
  geom_sf(color = "gray",
          fill = "lightgreen",
          alpha = 0.5) + 
  coord_sf(xlim = c(10,30),
           ylim = c(35,50), 
           expand = FALSE) +
  geom_point(data = map_data,
             mapping = aes(x = longitude,
                           y = latitude,
                           size = n),
             color = "darkred",
             alpha = 0.5) +
  geom_text_repel(data = map_data,
             mapping = aes(x = longitude,
                           y = latitude,
                           label = genus_species),
             max.overlaps = Inf,
             min.segment.length = 0,
             force_pull = 0.2) + 
  theme_void() + 
  theme(panel.background = element_rect(fill = "aliceblue"))


