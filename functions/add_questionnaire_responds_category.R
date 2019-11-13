add_questionaire_responds_category <- function(base_data, raw_data, cat) {
  cat_col <- paste0(cat, "_b")
  join_cols <- c("X1")
  if ("b_value" %in% colnames(base_data)) join_cols <- c(join_cols, "b_value")
  raw_data %>%
    dplyr::select(X1, dplyr::starts_with( cat_col)) %>%
    tidyr::gather(b_value, !! paste0(cat, "_value"), -X1) %>%
    dplyr::mutate(
      b_value := as.numeric(gsub(cat_col, "", b_value))
    ) %>%
    dplyr::arrange(X1, b_value) %>%
    dplyr::left_join(
      x = base_data,
      y = .,
      by = join_cols
    ) %>%
    return()
}