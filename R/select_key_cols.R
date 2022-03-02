select_key_cols <- function(df, key_cols = NULL, exclude_cols = NULL){
  if(!is.null(key_cols) & !is.null(exclude_cols)){
    stop("You can define which variables should be checked for completeness by setting 'key_vars'
         or which variables should be discluded by setting 'exclude_vars'.
         You cannot set both.")
  }

  if(!is.null(key_cols)){
    df_key_cols <- df %>% select(all_of(key_cols))
  } else {
    df_key_cols <- df %>% select(-any_of(exclude_cols))
  }

  df_key_cols
}
