#' Make character age-group labels from numeric age-group breaks.
#'
#' The age-group breaks are interpreted as open to the left and closed to the right.
#' See examples.
#'
#' @param age_group_breaks Numeric vector of age group breaks
#'
#' @return Character vector
#' @export
#'
#' @examples
#' > make_age_group_labels(c(0,5,13,Inf))
#' "0-4"    "5-12"   "13plus"
make_age_group_labels <- function(age_group_breaks){
  paste0(head(age_group_breaks,-1),
         "-",
         tail(age_group_breaks,-1) - 1) %>% gsub("-Inf","plus",.)
}
