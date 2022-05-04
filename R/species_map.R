### load the data for map
library(sf)
library(spData)
library(ggrepel)

map_data <- read_tsv(file = "data/02_morphometric_data_clean.tsv",
                     col_select = c(
                       genus_species,
                       location,
                       latitude,
                       longitude
                     )) %>% 
  distinct()

map_data_specie_count <- map_data %>% 
  group_by(genus_species) %>% 
  tally

world_map <- map_data("world")

eu <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
        "Czech Rep.","Denmark","Estonia","Finland","France",
        "Germany","Greece","Hungary","Ireland","Italy","Latvia",
        "Lithuania","Luxembourg","Malta","Netherlands","Poland",
        "Portugal","Romania","Slovakia","Slovenia","Spain",
        "Sweden","United Kingdom")

eu_map <- world_map %>% 
  as_tibble() %>%  
  filter(region %in% eu)

world_map %>% 
  ggplot(mapping = aes(
    x = long,
    y = lat
  )) + 
  geom_map(map = eu_map,
           mapping = aes(
    map_id = region),
    color = "black",
    fill = "lightgreen",
    size = 0.1,
    alpha = 0.5
  ) +
  geom_point(data = map_data,
             mapping = aes(x = longitude,
                           y = latitude)) +
  xlim(c(-10,40)) +
  ylim(c(30,70))

theworld <- spData::world %>% filter(region_un == "Europe")

p <- ggplot(theworld) +
  geom_sf(color = "gray",
          fill = "lightgreen",
          alpha = 0.5) + 
  coord_sf(xlim = c(10,30),
           ylim = c(35,50), 
           expand = FALSE) +
  geom_point(data = map_data,
             mapping = aes(x = longitude,
                           y = latitude),
             color = "darkred") +
  geom_text_repel(data = map_data,
             mapping = aes(x = longitude,
                           y = latitude,
                           label = genus_species),
             max.overlaps = Inf) + 
  theme(panel.background = element_rect(fill = "aliceblue"))

dir.create(path = "results")

ggsave(filename = "06_map_plot.pdf",
       plot = p,
       device = cairo_pdf,
       path = "results",
       width = 27,
       height = 20,
       units = "cm")