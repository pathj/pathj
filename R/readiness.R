
readiness <- function(options) {
  result <- list(reason = NULL, ready = TRUE, report = FALSE)

 mark(options$endogenous)
 mark(options$factors)
 mark(options$covs)
 
  if(length(options$endogenous) == 0) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need at least 1 endogenous variable")
    return(result)
  } 

  if((length(options$factors) == 0) & (length(options$covs) == 0)) {
    result$ready <- FALSE
    result$report <- TRUE
    result$reason <- glue::glue("we need at least 1 exogenous variable")
    return(result)
  } 
  
  return(result)
}
