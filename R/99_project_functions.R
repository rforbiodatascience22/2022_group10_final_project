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

plot_mean_scatter <- function(data,
                         x,
                         y,
                         color,
                         shape = NULL,
                         title,
                         y_text,
                         x_text,
                         subtitle_main,
                         subtitle_end = NULL,
                         color_palette
                         ) {
  mean_scatter_plot <- data %>%
    ggplot(mapping = aes_string(x = str_c(x,
                                          "_mean"),
                                y = str_c(y,
                                          "_mean"),
                                color = color,
                                shape = shape)) +
    geom_pointrange(mapping = aes_string(ymin = str_c(y,
                                                      "_low"),
                                         ymax = str_c(y,
                                                      "_high")),
                    fill = "white") +
    geom_pointrange(mapping = aes_string(xmin = str_c(x,
                                                      "_low"),
                                         xmax = str_c(x,
                                                      "_high")),
                    fill = "white") +
    scale_color_manual(values = color_palette) +
    labs(title = title,
         subtitle = str_c(y_text,
                          " vs ",
                          x_text,
                          subtitle_main,
                          subtitle_end)) +
    theme(plot.title = element_markdown(face = "bold"),
          plot.subtitle = element_markdown(),
          plot.title.position = "plot",
          legend.position = "none",
          axis.title = element_blank())
  
  if(!is.null(shape)) {
    mean_scatter_plot <- mean_scatter_plot +
      scale_shape_manual(values = c(21,
                                    19))
  }
  
  mean_scatter_plot
}