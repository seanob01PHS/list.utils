#' Helper for selecting columns by inclusion or exclusion but not both. Useless to end user.
#'
#' Default behaviour if neither `key_cols` nor `non_key_cols` are provided is to select all columns.
#'
#' @param df Dataframe to select columns from.
#' @param key_cols Columns to select.
#' @param non_key_cols Columns to disclude. You cannot provide both `key_cols` and `non_key_cols`.
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' > a <- tibble(key_1 = numeric(),
#'               key_2 = numeric(),
#'               non_key = numeric())
#'
#' > select_key_cols(a, key_cols = c("key_1", "key_2"))
#' # A tibble: 0 × 2
#  … with 2 variables: key_1 <dbl>, key_2 <dbl>
#'
#' > select_key_cols(a, non_key_cols = "non_key")
#' # A tibble: 0 × 2
#' … with 2 variables: key_1 <dbl>, key_2 <dbl>
#'
#' > select_key_cols(a, key_cols = c("key_1", "key_2"), non_key_cols = "non_key")
#' Error in select_key_cols(a, key_cols = c("key_1", "key_2"), non_key_cols = "non_key") :
#'   You can define which variables should be checked for completeness by setting 'key_cols'
#'   or which variables should be discluded by setting 'non_key_cols'.
#'   You cannot set both.
select_key_cols <- function(df, key_cols = NULL, non_key_cols = NULL){
  if(!is.null(key_cols) & !is.null(non_key_cols)){
    stop("You can define which variables should be checked for completeness by setting 'key_cols'
  or which variables should be discluded by setting 'non_key_cols'.
  You cannot set both.")
  }

  if(!is.null(key_cols)){
    df_key_cols <- df %>% select(all_of(key_cols))
  } else {
    df_key_cols <- df %>% select(-any_of(non_key_cols))
  }

  df_key_cols
}
