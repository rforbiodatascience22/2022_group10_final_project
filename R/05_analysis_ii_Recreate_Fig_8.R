# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
phys_data <- read_tsv(file = "data/02_physiological_data_clean.tsv")

view(phys_data)

# Wrangle data ------------------------------------------------------------

#phys_data <- 
phys_data %>% 
  view(cricket_id)

plot1 <- phys_data %>% 
  group_by(sex) %>% 
  filter(species == "ornatus") %>% 
  

plot2 <- phys_data %>% 
  filter(sex == "female") %>% 
  filter(species == "ornatus")

plot3 <- phys_data %>% 
  filter(sex == "male") %>% 
  filter(species == "ornatus")
# Model data --------------------------------------------------------------


# Visualise data ----------------------------------------------------------
ggplot(data = plot1,
       mapping = aes(x = frequency,
                     y = auditory_threshold,
                     color = sex)) +
 # geom_line(data = plot2) +
  geom_line(data = plot3) +
  geom_point()


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)