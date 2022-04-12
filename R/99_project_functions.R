# Define project functions ------------------------------------------------
tidy_subtable_physiological <- function(data,
                                        first_row,
                                        first_column,
                                        last_row,
                                        last_column){
  table_captions <- data %>%
    select(first_column: (first_column + 1) ) %>%
    slice(first_row)
  
  genus_species <- table_captions %>%
    pull(1)
  
  sex <- table_captions %>%
    pull(2)
  
  cricket_indicies <- data %>%
    select((first_column + 1) :last_column) %>%
    slice(first_row + 1) %>%
    as.character()
  
  subtable <- data %>%
    select(first_column:last_column) %>%
    slice( (first_row + 2) :last_row)
  
  colnames(subtable) <- c("frequency",
                          cricket_indicies)
  
  subtable <- subtable %>%
    pivot_longer(cols = !frequency,
                 names_to = "cricket_id",
                 values_to = "auditory_threshold")
  
  subtable %>%
    mutate(genus_species = genus_species,
           sex = sex)
}