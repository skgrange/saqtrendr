#' Squash the global variable notes when building a package. 
#' 
if (getRversion() >= "2.15.1") {
  
  # What variables are causing issues?
  variables <- c(
    "trend", "remainder", "sub.labels", "weights", "observations", "decomposed",
    "trend_test", "trend_and_remainder", "date_start", "date_end", "p_value",
    "slope", "slope_lower", "slope_upper", "significant", "intercept", 
    "intercept_lower", "intercept_upper", "date_centre"
  )
  
  # Squash the notes
  utils::globalVariables(variables)
  
}
