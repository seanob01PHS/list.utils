#' Key combinations not present in input
#'
#' This function returns a dataframe of key column combinations that are not present in `df`.
#' Thus if `df` has no key duplicates and `nrow(absent_keys(df))==0` then `df` is complete in its keys.
#'
#' By default if neither `key_cols` or  `non_key_cols` are provided, all columns are interpreted as key columns.
#'
#' @param df Dataframe to inspect for incomplete key combinations.
#' @param key_cols Character vector of key columns.
#' @param non_key_cols Character vector of non-key columns. You cannot provide both `key_cols` and `non_key_cols`.
#'
#' @return Dataframe.
#' @export
#'
#' @examples
#' > a <- tibble(key_1 = c(1,1,2),
#'               key_2 = c(1,2,1))
#'
#' > absent_keys(a)
#' # A tibble: 1 × 2
#    key_1 key_2
#    <dbl> <dbl>
#1     2     2
absent_keys <- function(df, key_cols = NULL, non_key_cols = NULL){

  df_key_cols <- df %>% select_key_cols(key_cols, non_key_cols)
  df_key_cols_complete <- df_key_cols %>% cross_df()

  df_key_cols_complete %>%
    anti_join(df_key_cols, by = df_key_cols %>% colnames)

}
