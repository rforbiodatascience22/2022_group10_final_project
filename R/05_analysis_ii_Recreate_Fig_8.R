# Load libraries ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(patchwork)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
phys_data <- read_tsv(file = "data/02_physiological_data_clean.tsv")

# Wrangle data ------------------------------------------------------------

data_plot <- phys_data %>% 
  group_by(frequency, sex, species) %>% 
  summarise(n = n(), 
            mean = mean(auditory_threshold,na.rm = TRUE), 
            sd = sd(auditory_threshold, na.rm = TRUE)) %>% 
  mutate(lower = mean - sd,
         upper = mean + sd)


# Make plot

data_plot %>% 
ggplot(aes(x = frequency, 
           y = mean,
           color = sex)) +
  geom_point() + 
  #geom_pointrange(aes(ymin = lower, ymax = upper)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +

  geom_line() + 
  facet_wrap(~species)

Trendplot <- function(data,
                      species,
                      title) {
  #Object for data points
  plot1 <- data %>% 
    group_by(sex) %>% 
    filter(species == species)
  
  #Object for female trend line
  plot2 <- data %>% 
    filter(sex == "female") %>% 
    filter(species == species) 
  
  #Object for male trend line
  plot3 <- data %>% 
    filter(sex == "male") %>% 
    filter(species == species)
  
  
  # Make plot
  ggplot(data = plot1,
         mapping = aes(x = frequency,
                       y = auditory_threshold,
                       color = sex)) +
    geom_smooth(data = plot2,
                se = FALSE) +
    geom_smooth(data = plot3,
                se = FALSE) +
    geom_point() +
    ggtitle(title)
  }

plot1 <- Trendplot(phys_data,"ornatus","Poecilimon ornatus")
plot2 <- Trendplot(phys_data,"modestior","Isophya modestior")
plot3 <- Trendplot(phys_data,"elegans","Poecilimon elegans")
plot4 <- Trendplot(phys_data,"ampliatus","Poecilimon ampliatus")

(plot1 + plot2) / (plot3 + plot4)
# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)