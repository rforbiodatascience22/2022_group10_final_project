# Define project functions ------------------------------------------------
tidy_extract_physiological <- function(data,
                                        first_row,
                                        first_column,
                                        last_row,
                                        last_column){
  captions <- data %>%
    select(first_column: (first_column + 1) ) %>%
    slice(first_row)
  
  header <- data %>%
    select((first_column + 1) :last_column) %>%
    slice(first_row + 1) %>%
    as.character() %>%
    c("frequency",
      .)
  
  body <- data %>%
    select(first_column:last_column) %>%
    slice( (first_row + 2) :last_row)
  
  colnames(body) <- header
  
  body <- body %>%
    pivot_longer(cols = !frequency,
                 names_to = "cricket_id",
                 values_to = "auditory_threshold")
  
  body %>%
    mutate(genus_species = captions %>%
             pull(1),
           sex = captions %>% 
             pull(2))
}

tidy_extract_meta <- function(data,
                              first_row,
                              first_column,
                              last_row,
                              last_column){
  captions <- data %>%
    select(first_column) %>%
    slice(first_row)
  
  header <- data %>%
    select(1:6) %>%
    slice(1) %>%
    as.character()
  
  body <- data %>%
    select(first_column:last_column) %>%
    slice( (first_row + 1) :last_row)
  
  colnames(body) <- header
  
  body %>%
    mutate(group = captions %>%
             pull(1))
}