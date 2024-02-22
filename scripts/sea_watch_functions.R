
#' Functions returns cleaned columns with date and unique column
#' 
#' @inheritParams None
#' @return A dataframe with date and a unique value
#' @param name: A list of values to filter by
#' @export 
#' @examples Not yet implemented

envar_clean <- function(varname) {
  
  output <- envar %>% 
    filter(var == paste(varname)) %>% 
    select(date, stat) %>% 
    mutate(date = str_remove(date, "X"),
           date = str_replace_all(date, "\\.", "-"),
           date = mdy(date))
  
  colnames(output) <- c("date", paste(varname))

  return(output)
}




explore_model <- function(model) { #species = NULL, data) {
  
  try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
  
  # if(!is.null(species)) {
  #   dat <- data %>% 
  #     filter(group.code == paste(species))
  # } else {
  #   dat <- data
  # }
  
  
  qqnorm(resid(model))
  qqline(resid(model))
  
  
  print(plot(model))
  
  
  summary(model)
  
}