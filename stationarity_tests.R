stationarity_tests <- function(series){
  adf_results <- series%>%adf.test()
  pp_results <- series %>% pp.test()
  kpss_results <- series %>%kpss.test()
  return(list(adf_results,pp_results,kpss_results))
}