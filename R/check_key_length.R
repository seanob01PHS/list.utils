#' Ckecks if length of dataframe could accomodate all possible key combinations.
#'
#' This function finds the number of unique values in each of the key columns and then checks if the number of rows in the dataframe provided is consistent with the predicate that the dataframe is complete in its keys.
#' Say the number of unique values for `key_col_1` and `key_col_2` were `nUnique_1` and `nUnique_2` etc. and there are `m` key columns.
#' If the dataframe was complete in those keys it would have exactly `nUninque_1 * nUnique_2 * ...* nUnique_m` rows.
#' This function verifies this, that df has as many rows as the product of the number of unique values in its keys.
#'
#' NOTE `check_key_length(df)` does NOT imply df is complete in its keys (there could be duplicates or garbage rows).
#' rather `!check_key_length(df)` does imply df is non-complete in its keys.
#'
#' By default if neither `key_cols` or  `non_key_cols` are provided, all columns are interpreted as key columns.
#'
#' @param df Dataframe input to check for key completeness.
#' @param key_cols Character vector of key columns.
#' @param non_key_cols Character vector of non-key columns. You cannot provide both `key_cols` and `non_key_cols`.
#'
#' @return Booolean vector.
#' @export
#'
#' @examples
#' # a IS complete in keys and thus satisfies check_key_length(a)
#' > a <- tibble(key_1 = c(1,1,2,2),
#'               key_2 = c(1,2,1,2))
#' > check_key_length(a)
#' TRUE
#'
#'# b IS NOT complete in keys but still satisfies check_key_length(b)
#' > b <- tibble(key_1 = c(1,1,2,2),
#'               key_2 = c(1,1,1,2))
#' > check_key_length(b)
#' TRUE
#'
#'# c fails check_key_length(c), thus it is not complete in its keys.
#' > c <- tibble(key_1 = c(1,1,2),
#'               key_2 = c(1,2,1))
#' > check_key_length(c)
#' FALSE
check_key_length <- function(df, key_cols = NULL, non_key_cols = NULL){

  df %>%
    select_key_cols(key_cols, non_key_cols) %>%
    {
      nrow(.) == (map(., unique) %>%
                    map(length) %>%
                    reduce(`*`))
    }
}
