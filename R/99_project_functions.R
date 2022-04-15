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

tidy_extract <- function(data,
                         body_top,
                         body_bottom,
                         body_left,
                         body_right,
                         header = TRUE,
                         header_row = NULL,
                         header_na_names = NULL,
                         caption_left = NULL,
                         caption_right = NULL,
                         caption_row = NULL,
                         caption_names = NULL,
                         pivot_longer_left = NULL,
                         pivot_longer_right = NULL,
                         names_to = "pivot_name",
                         values_to = "pivot_value"){
  body_ <- data %>%
    select(body_left:body_right) %>%
    slice(body_top:body_bottom)
  
  header_width <- body_right - body_left + 1
  header_generic <- str_c("header", 1:header_width)
  
  if ( isTRUE(header) ){
    if ( is.null(header_row) ){
      header_row <- body_top - 1
    }
    
    header <- data %>%
      select(body_left:body_right) %>%
      slice(header_row) %>%
      as.character()
    
    if ( is.null(header_na_names) ){
      header <- header %>%
        coalesce(header_generic)
    } else {
      header <- header %>%
        replace(is.na(header), header_na_names)
    }
    
    colnames(body_) <- header
  } else {
    colnames(body_) <- header_generic
  }
  
  if ( !is.null(pivot_longer_left) && !is.null(pivot_longer_right) ){
    body_ <- body_ %>%
      pivot_longer(cols = all_of(pivot_longer_left):all_of(pivot_longer_right),
                   names_to = names_to,
                   values_to = values_to)
  }
  
  if ( !is.null(caption_left) && !is.null(caption_right) && !is.null(header_row) ){
    caption_width <- caption_right - caption_left + 1
    
    if ( is.null(caption_names) ){
      caption_names <- str_c("caption", 1:caption_width)
    }
    
    captions <- data %>%
      select(caption_left:caption_right) %>%
      slice(caption_row) %>%
      as.character() %>%
      set_names(caption_names)
    
    body_ <- body_ %>%
      mutate(!!! captions)
  }
  
  return(body_)
}

extract_test <- tidy_extract(data = physiological_data_raw,
                 body_top = 21,
                 body_bottom = 35,
                 body_left = 11,
                 body_right = 19,
                 header = TRUE,
                 header_row = NULL,
                 header_na_names = "frequency",
                 caption_left = 11,
                 caption_right = 12,
                 caption_row = 19,
                 caption_names = c("genus_species",
                                   "sex"),
                 pivot_longer_left = 2,
                 pivot_longer_right = 9,
                 names_to = "cricket_id",
                 values_to = "auditory_threshold")