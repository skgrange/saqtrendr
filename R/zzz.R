#' Squash the global variable notes when building a package. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    "trend", "remainder", "sub.labels", "weights", "observations", "decomposed",
    "trend_test"
  )
  
  # Squash the notes
  utils::globalVariables(variables)
  
}
