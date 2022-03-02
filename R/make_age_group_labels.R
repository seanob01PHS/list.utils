make_age_group_labels <- function(age_group_breaks){
  paste0(head(age_group_breaks,-1),
         "-",
         tail(age_group_breaks,-1) - 1) %>% gsub("-Inf","plus",.)
}
