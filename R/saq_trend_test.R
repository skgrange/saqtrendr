#' Function to conduct a robust trend test using the Theil-Sen estimator.
#' 
#' \code{saq_trend_test} can conduct the trend tests in parallel if a 
#' \strong{futures} backend has been registered. 
#' 
#' @param df Input data frame containing a time series. 
#' 
#' @param by Which variables should be used as grouping variables? 
#' 
#' @param decompose Should the time series be decomposed before the trend test? 
#' If this is \code{FALSE}, names in your input will be changed. 
#' 
#' @param window Span (in lags) of the loess window for seasonal extraction. 
#' This should be an odd number.
#' 
#' @param na_preserve Should observations which were missing with the values 
#' also be propagated into the deseasonalised components? 
#' 
#' @param alpha Confidence interval of the slope, default is 0.05 for 95 % 
#' confidence intervals. 
#' 
#' @param auto_correlation Should auto correlation be considered in the 
#' estimates?
#' 
#' @param period Period of input time series. Default is \code{"month"} but can
#' also be \code{"year"}. 
#' 
#' @param df_nest Nested tibble including a nested observational variable named
#' \code{observations}. 
#' 
#' @param progress Should a progress bar be displayed for the trend test 
#' calculations? 
#' 
#' @author Stuart K. Grange
#' 
#' @return Named list or nested tibble. 
#' 
#' @export
saq_trend_test <- function(df, by = as.character(), decompose = TRUE, 
                           window = 35, na_preserve = TRUE, alpha = 0.05, 
                           auto_correlation = FALSE, period = "month", 
                           progress = FALSE) {
  
  # Check value
  stopifnot("value" %in% names(df) && is.numeric(df$value))
  
  # Check period and switch decompose argument if needed
  stopifnot(period %in% c("month", "year"))
  decompose <- if_else(period == "year", FALSE, decompose)
  
  # Do the transformations and calculations in a nested tibble
  df_nest <- df %>% 
    dplyr::group_by_at(by) %>% 
    dplyr::group_nest(.key = "observations") 
  
  if (decompose) {
    
    df_nest <- df_nest %>% 
      mutate(
        decomposed = furrr::future_map(
          observations, 
          ~decompose_with_stlplus(
            ., 
            window = !!window, 
            na_preserve = !!na_preserve
          )
        ),
        trend_test = furrr::future_map(
          decomposed,
          ~smonitor::theil_sen_trend_test(
            ., 
            variable = "trend_and_remainder",
            deseason = FALSE,
            alpha = !!alpha,
            auto_correlation = !!auto_correlation,
            period = !!period
          ),
          future.seed = TRUE
        )
      )
    
    # Get pieces
    df_decomposed <- df_nest %>% 
      select(-observations,
             -trend_test) %>% 
      tidyr::unnest(decomposed)
    
  } else {
    
    # Use observations, not decomposed list
    df_nest <- df_nest %>% 
      mutate(
        trend_test = furrr::future_map(
          observations,
          ~smonitor::theil_sen_trend_test(
            ., 
            variable = "value",
            deseason = FALSE,
            alpha = !!alpha,
            auto_correlation = !!auto_correlation
          ),
          future.seed = TRUE
        )
      )
    
    # Get pieces, a bad name here, these data are not decomposed
    df_decomposed <- df_nest %>% 
      select(-trend_test) %>% 
      tidyr::unnest(observations) %>% 
      rename(trend_and_remainder = value)
    
  }
  
  # And the trend tests
  df_trend_tests <- df_nest %>% 
    select(-dplyr::matches("observations|decomposed")) %>% 
    tidyr::unnest(trend_test) %>% 
    mutate(decomposed = !!decompose) %>% 
    select(-deseason)
  
  # Build list for return
  list_trends <- list(
    observations = df_decomposed,
    trend_tests = df_trend_tests
  )
  
  return(list_trends)
  
}


#' @rdname saq_trend_test
#' @export
saq_trend_test_nested <- function(df_nest, decompose = FALSE, window = 35,
                                  na_preserve = TRUE, alpha = 0.05,
                                  auto_correlation = FALSE, 
                                  period = "month", progress = FALSE) {
  
  # Check input
  if (!"observations" %in% names(df_nest) && inherits(df_nest, "rowwise_df")) {
    cli::cli_abort("Nested input must contain a nested `observations` variable.")
  }
  
  # Apply the trend tests
  df_nest_tests <- df_nest %>% 
    pull(observations) %>% 
    purrr::map(
      ~saq_trend_test_nested_worker(
        .,
        decompose = decompose,
        window = window,
        na_preserve = na_preserve,
        alpha = alpha,
        auto_correlation = auto_correlation,
        period = period
      ), 
      .progress = progress
    ) %>% 
    purrr::list_rbind()
  
  # Bind the trend tests to the nested input
  df_nest <- dplyr::bind_cols(df_nest, df_nest_tests)
  
  return(df_nest)
  
    
}


saq_trend_test_nested_worker <- function(df, decompose, window, na_preserve, 
                                         alpha, auto_correlation, period) {

  # Check value
  stopifnot("value" %in% names(df) && is.numeric(df$value))

  # Check period and switch decompose argument if needed
  stopifnot(period %in% c("month", "year"))
  decompose <- if_else(period == "year", FALSE, decompose)

  # Decompose time series with stlplus if desired
  if (decompose) {
    df <- decompose_with_stlplus(df, window = window, na_preserve = na_preserve)
  } else {
    df <- rename(df, trend_and_remainder = value)
  }

  # Do the test, the deseasonalisation algorithm has been applied above if
  # desired
  df_tests <- df %>%
    smonitor::theil_sen_trend_test(
      variable = "trend_and_remainder",
      deseason = FALSE,
      alpha = alpha,
      auto_correlation = auto_correlation,
      period = period
    ) %>% 
    select(-deseason) %>% 
    mutate(decomposed = !!decompose)

  # Build nested tibble return
  df_nest <- tibble(
    trend_observations = list(df),
    trend_tests = list(df_tests)
  )

  return(df_nest)

}
