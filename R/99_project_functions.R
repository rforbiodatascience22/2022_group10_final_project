# Define project functions ------------------------------------------------
tidy_extract_physiological <- function(data,
                                       body_top,
                                       body_bottom,
                                       body_left,
                                       body_right) {
# Extract table body
  body <- data %>%
    select(all_of(body_left):all_of(body_right)) %>%
    slice(body_top:body_bottom)
  
# Extract table header  
  header <- data %>%
    select((all_of(body_left) + 1) :all_of(body_right)) %>%
    slice(body_top - 1) %>%
    as.character() %>%
    prepend("frequency")
  
# Extract table captions
  captions <- data %>%
    select(all_of(body_left): (all_of(body_left) + 1) ) %>%
    slice(body_top - 2)
  
# Set column names
  colnames(body) <- header

# Lengthen the data
  body <- body %>%
    pivot_longer(cols = !frequency,
                 names_to = "cricket_id",
                 values_to = "auditory_threshold")

# Add captions as new columns
  body %>%
    mutate(genus_species = captions %>%
             pull(1),
           sex = captions %>% 
             pull(2))
}

tidy_extract_meta <- function(data,
                              body_top,
                              body_bottom) {
# Extract table body
  body <- data %>%
    slice(body_top:body_bottom)
  
# Extract table header
  header <- data %>%
    slice(1) %>%
    as.character()

# Extract table captions
  captions <- data %>%
    select(1) %>%
    slice(body_top - 1)

# Set column names
  colnames(body) <- header

# Add captions as new columns
  body %>%
    mutate(species_group = captions %>%
             pull(1))
}

#Function for fig 8

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

                    