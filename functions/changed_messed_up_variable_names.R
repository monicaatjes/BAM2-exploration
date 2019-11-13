changed_messed_up_variable_names <- function(raw_data, cat){
  
  # Choose data to deal with
  data_to_deal <- raw_data %>% 
    dplyr::select(starts_with(cat)) %>%
    colnames()
  
  
  # Change the column names
  for (i in 1:length(data_to_deal)) {
    splitted <- data_to_deal[i] %>% str_split("_") %>% unlist()
    data_to_deal[i] <- paste0(cat, "_", splitted[length(splitted)], "_", splitted[length(splitted) - 1])
  }
  
  # Replace it in the raw data
  data_to_deal <- data_to_deal %>% as.matrix()
  first_position <- grep(paste0(cat, "_b1_p1") , colnames(raw_data))
  last_position <- grep(paste0(cat, "_b20_p9") , colnames(raw_data))
  for (i in first_position:last_position){
    names(raw_data)[i] <- data_to_deal[i + 1 - first_position]
  }
  # Return
  return(raw_data)
}
