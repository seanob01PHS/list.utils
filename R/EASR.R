#' Generate EASR data from sex and age-group data.
#'
#' This takes a dataframe df with some sort of count variable split over sex and age-group columns with population data for each sex, age-group split.
#' These columns are grouped over and accumulated in to EASR (European Age Standardised Rate) and crude rate columns. Total count and total population variables are also provided in the output.
#' The EASR will be based on a dataframe that describes a typical (standard) European split of age-grouping and sex demographics.
#' The default for this dataframe is at `/conf/linkage/output/lookups/Unicode/Populations/Standard/ESP2013_by_sex.sav`.
#'
#'
#' If the user provides column names of `n_col`, `pop_col`, `age_group_col` or `sex_col`, they must be character vectors and not tidyselections.
#'
#' The user can provide the standard population dataframe using the `standard_pop_lookup` argument. This must have exactly these columns: `age_group`, `sex` and `easr_standard_pop`.
#' The format and values of `age_group_col` and `sex_col` in `df` must match that of the relevant columns in `standard_pop_lookup`.
#'
#' If using the default `standard_pop_lookup`, the age-group column must be in 5-year age groups and be formatted as follows `c("0-4", "5-9", "10-14", ..."90plus")`.
#' Similarly, using that default, the sex column must be a numeric vector where 1=Male, 2=Female.
#'
#' The output column names will depend on the value of `n_col` and `verbose_colnames`.
#' If `n_col='n_attendances'` and `verbose_colnames=TRUE`, then the EASR and crude rate column names will be `'easr_n_attendances'` and `'crude_rate_n_attendances'` respectively.
#'
#' If `verbose_colenames=FALSE` then the EASR and crude rate column names will be `easr` and `crude_rate`.
#'
#' @param df Dataframe containging `n_col`, `pop_col`, `age_group_col` and `sex_col` for each observation.
#' @param n_col Name of column containing the number of occurences of the measure. E.g. `'n_hospital_stays'`.
#' @param pop_col Name of column containing popultaion data.
#' @param age_group_col Name of column containing age group data.
#' @param sex_col Name of column containing sex data.
#' @param multiply Number to multiply EASR and crude rate by. E.g. `multiply=1000` will give rate per 1000 people.
#' @param standard_pop_lookup Tibble describing the standard population sex and age-group demographics.
#' @param verbose_colnames Option to include the value of `n_col` in the names of the EASR and Crude Rate output columns.
#' @param include_crude_rate Option to include the Crude Rate output column.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' > df <- tibble(indicator = rep("sample indicator", 38),
#'              sex = factor(c(rep(1, 19), rep(2, 19))),
#'              age_group = factor(rep(make_age_group_labels(c(seq(0,90,5),Inf)), 2)),
#'              pop = rep(c(100,200), 19),
#'              n = c(rep(c(1,2,3), 12), c(1,2))
#'              )
#'
#' > df
#' # A tibble: 38 × 5
#'    indicator          sex age_group   pop     n
#'    <chr>            <dbl> <chr>     <dbl> <dbl>
#'  1 sample indicator     1 0-4         100     1
#'  2 sample indicator     1 5-9         200     2
#'  3 sample indicator     1 10-14       100     3
#'  4 sample indicator     1 15-19       200     1
#'  5 sample indicator     1 20-24       100     2
#'  6 sample indicator     1 25-29       200     3
#'  7 sample indicator     1 30-34       100     1
#'  8 sample indicator     1 35-39       200     2
#'  9 sample indicator     1 40-44       100     3
#' 10 sample indicator     1 45-49       200     1
#' # … with 28 more rows
#'
#' > EASR(df)
#'# A tibble: 1 × 5
#'  indicator        total_n total_pop easr_n crude_rate_n
#'  <chr>              <dbl>     <dbl>  <dbl>        <dbl>
#'1 sample indicator      75      5700 0.0150       0.0132
EASR <- function(df,
                 n_col = "n",
                 pop_col = "pop",
                 age_group_col = "age_group",
                 sex_col = "sex",
                 multiply = 1,
                 standard_pop_lookup = NULL,
                 verbose_colnames = TRUE,
                 include_crude_rate = TRUE){



  default_standard_pop_lookup_filepath <- "/conf/linkage/output/lookups/Unicode/Populations/Standard/ESP2013_by_sex.sav"

  # Behaviour for gathering the ESP file (users can supply a custom standard population tibble)
  if(is.null(standard_pop_lookup)){
    #default age_group labels
    age_group_labels <- make_age_group_labels(age_group_breaks = c(seq(0,90,5),Inf))

    #default ESP
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


