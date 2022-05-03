# Load libraries ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(patchwork)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
phys_data <- read_tsv(file = "data/02_physiological_data_clean.tsv")


plot1 <- Fig8plot(phys_data,"ornatus","Poecilimon ornatus")
plot2 <- Fig8plot(phys_data,"modestior","Isophya modestior")
plot3 <- Fig8plot(phys_data,"elegans","Poecilimon elegans")
plot4 <- Fig8plot(phys_data,"ampliatus","Poecilimon ampliatus")

p <- (plot1 + plot2) / (plot3 + plot4)
# Write data --------------------------------------------------------------
ggsave(filename = "05_auditory_threshold_vs_frequency_plot.pdf",
       plot = p,
       device = cairo_pdf,
       path = "data/images/",
       width = 27,
       height = 20,
       units = "cm")