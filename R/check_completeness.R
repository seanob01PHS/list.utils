check_completeness <- function(df, key_cols = NULL, exclude_cols = NULL){

  df %>%
    select_key_cols(key_cols, exclude_cols) %>%
    {
      nrow(.) == (map(., unique) %>%
                    map(length) %>%
                    reduce(`*`))
    }
}
