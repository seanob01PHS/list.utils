EASR <- function(df,
                 n_col = "n",
                 pop_col = "pop",
                 age_group_col = "age_group",
                 sex_col = "sex",
                 multiply = 1,
                 age_group_breaks = c(seq(0,90,5),Inf),
                 standard_pop_lookup = NULL,
                 verbose_colnames = TRUE,
                 include_crude_rate = TRUE){

  age_group_labels <- make_age_group_labels(age_group_breaks)

  default_standard_pop_lookup_filepath <- "/conf/linkage/output/lookups/Unicode/Populations/Standard/ESP2013_by_sex.sav"
  # Behaviour for gathering the ESP file (users can supply a custom standard population tibble)
  if(is.null(standard_pop_lookup)){
    standard_pop_lookup <- haven::read_sav(default_standard_pop_lookup_filepath) %>%
      mutate(age_group=factor(agegroup),
             sex=factor(sex, levels = 1:2),
             easr_standard_pop = ESP2013) %>%
      select(age_group, sex, easr_standard_pop)

    levels(standard_pop_lookup$age_group) <- age_group_labels

  } else if(!(is_tibble(standard_pop_lookup))){
    stop("argument 'standard_pop_lookup' must left default or be a tibble.")

  }

  join_by_vec <- c("age_group", "sex")
  names(join_by_vec) <- c(age_group_col, sex_col)

  df_with_standard_pops <- df %>%
    left_join(standard_pop_lookup, by=join_by_vec)

  df_with_all_rates <- df_with_standard_pops %>%
    mutate(crude_rate_in_agesex_group = n/pop) %>%
    mutate(n_in_easr_agesex_group = crude_rate_in_agesex_group * easr_standard_pop)

  easr_colname <- paste0("easr_", n_col)
  crude_rate_colname <- paste0("crude_rate_", n_col)
  n_pop_total_prefix <- "total_"
  if(!verbose_colnames){
    easr_colname <- "easr"
    crude_rate_colname <- "crude_rate"
    n_pop_total_prefix <- ""
  }


  df_accumulate <- df_with_all_rates %>%
    group_by(across(-any_of(c(n_col, pop_col, age_group_col, sex_col, "easr_standard_pop", "crude_rate_in_agesex_group", "n_in_easr_agesex_group")))) %>%
    summarise(total_n_in_easr_pop = sum(n_in_easr_agesex_group),
              total_easr_pop = sum(easr_standard_pop),
              across(all_of(c(n_col, pop_col)), sum, .names=paste0(n_pop_total_prefix, "{.col}")),
              .groups = "drop")

  if((df_accumulate %>% pull(total_easr_pop) %>% unique %>% length) != 1){
    stop("Something has gone wrong, most likely the supplied data is not complete in age_groups and sex")
  }


  # Had to tragically abandon dplyr for these ones. Already tried for >1 hour.
  df_accumulate[[easr_colname]] <- df_accumulate[["total_n_in_easr_pop"]] / df_accumulate[["total_easr_pop"]] * multiply
  if(include_crude_rate){
    df_accumulate[[crude_rate_colname]] <-
      df_accumulate[[paste0(n_pop_total_prefix, n_col)]] / df_accumulate[[paste0(n_pop_total_prefix, pop_col)]] * multiply

  }

  #return the final result
  df_accumulate %>%
    select(-total_easr_pop, -total_n_in_easr_pop)

}
