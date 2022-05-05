# Load libraries ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(patchwork)

# Define functions --------------------------------------------------------

Fig8plot <- function(data,
                     Species,
                     title) {data_plot <- data %>%
                       
                       # Wrangle data ------------------------------------------------------------
                     
                     group_by(frequency, sex) %>%
                       filter(species == Species) %>% 
                       summarise(n = n(),
                                 mean = mean(auditory_threshold,na.rm = TRUE),
                                 sd = sd(auditory_threshold, na.rm = TRUE)) %>%
                       mutate(lower = mean - sd,
                              upper = mean + sd) #%>% 
                     data_plot %>%
                       ggplot(aes(x = frequency, 
                                  y = mean,
                                  color = sex,
                       )) +
                       geom_point() + 
                       geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                                     position=position_dodge(.9)) +
                       geom_line() +
                       ggtitle(title) +
                       xlab("Frequency [KHz]") +
                       ylab("Auditory threshold \n [dB SPL]")
}

# Load data ---------------------------------------------------------------
phys_data <- read_tsv(file = "data/02_physiological_data_clean.tsv")


plot1 <- Fig8plot(phys_data,"ornatus","Poecilimon ornatus")
plot2 <- Fig8plot(phys_data,"modestior","Isophya modestior")
plot3 <- Fig8plot(phys_data,"elegans","Poecilimon elegans")
plot4 <- Fig8plot(phys_data,"ampliatus","Poecilimon ampliatus")

p <- (plot1 + plot2) / (plot3 + plot4)
# Write data --------------------------------------------------------------
dir.create(path = "results")

ggsave(filename = "05_auditory_threshold_vs_frequency_plot.pdf",
       plot = p,
       device = cairo_pdf,
       path = "results",
       width = 27,
       height = 20,
       units = "cm")

