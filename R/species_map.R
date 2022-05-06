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

map_data <- map_data %>% 
  left_join(map_data_specie_count) %>% 
  distinct()


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
             max.overlaps = Inf,
             box.padding = 0.5)+ 
  labs(title = "Map of the insect species origins.") +
  theme(panel.background = element_rect(fill = "aliceblue"), 
        plot.title = element_markdown(face = "bold"))

dir.create(path = "results")

ggsave(filename = "06_map_plot.pdf",
       plot = p,
       device = cairo_pdf,
       path = "results",
       width = 27,
       height = 20,
       units = "cm")