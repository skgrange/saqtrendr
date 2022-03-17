#' Function to build a trend plot from the output of \code{\link{saq_trend_test}}.
#' 
#' @param df Input data frame, generally containing deseasonalised observations
#' from \code{\link{saq_trend_test}}. 
#' 
#' @param df_tests Input data frame, generally containing trend tests calculated
#' by \code{\link{saq_trend_test}}.
#' 
#' @param label Should the plots have a trend magnitude label added? 
#' 
#' @param round If \code{label} is \code{TRUE}, how many digits should the trend
#' estimated be rounded too?  
#' 
#' @param y_location If \code{label} is \code{TRUE}, where should the label be
#' positioned? In a decimal of the maximum y value. 
#' 
#' @param include_n If \code{label} is \code{TRUE}, should the number of 
#' observations be included in the label? 
#' 
#' @param facet_variable What variable in \code{df} and \code{df_tests} should
#' be used for faceting? 
#' 
#' @param scales If \code{facet_variable} is \code{TRUE}, should the axes be 
#' free or fixed? 
#' 
#' @param ylim A vector with the length of two for the y-axis limits. By default,
#' the y-axis will start at zero. 
#' 
#' @param x_label_rotate Angle of the x-tick (date) labels. If overlapping 
#' labels are encountered, setting \code{x_angle} to \code{45} may help. 
#' 
#' @param colour Either a colour of the plot's point and lines, or a variable in
#' \code{df} which the points' and lines' colours will be mapped to. 
#' 
#' @param parse_facet_label Should the facet names be parsed as an expression? 
#' 
#' @author Stuart K. Grange
#' 
#' @return \strong{ggplot2} plot.
#' 
#' @export
saq_trend_plot <- function(df, df_tests, label = TRUE, round = 3, 
                           y_location = 1, include_n = TRUE, facet_variable = NA, 
                           scales = "fixed", ylim = c(0, NA), colour = "#FCA50A",
                           x_label_rotate = 0, parse_facet_label = FALSE) {
  
  # If a list is passed
  if (class(df) == "list" && 
      missing(df_tests) && 
      names(df) %in% c("observations", "trend_tests")) {
    
    # The order matters here due to df being overwritten
    df_tests <- df$trend_tests
    df <- df$observations
    
  }
  
  if (stringr::str_detect(scales, "free") && !is.na(facet_variable[1])) {
    df_value_label_y <- df %>% 
      dplyr::group_by(across(!!facet_variable)) %>% 
      summarise(value_label_y = max(trend_and_remainder, na.rm = TRUE),
                .groups = "drop")
  } else {
    value_label_y <- df %>% 
      pull(trend_and_remainder) %>% 
      max(na.rm = TRUE) %>% 
      `*` (y_location)
  }
  
  # Build labels that will be printed on the plot
  df_labels <- df_tests %>% 
    mutate(
      date_start = min(date_start),
      date_end = max(date_end),
      date_centre = threadr::date_centre(date_start, date_end),
      significant = if_else(p_value <= 0.05, "*", ""),
      label = stringr::str_c(
        format(round(slope, round), nsmall = round),
        " [",
        format(round(slope_lower, round), nsmall = round),
        ",",
        format(round(slope_upper, round), nsmall = round),
        "] ", 
        significant,
        " (n = ", n, ")"
      ),
      label = stringr::str_trim(label)
    )
  
  # Drop n text if desired
  if (include_n) {
    # Simple case here
    plot_caption <- expression(
      Slope ~ '[lower,' ~ 'upper]' ~ 95 ~ '%' ~ 'in' ~ units ~
        year ^ {-1} ~ '(count) and * indicates a sig. trend' 
    )
  } else {
    
    # Drop n from labels
    df_labels <- df_labels %>% 
      mutate(label = stringr::str_split_fixed(label, " \\(n", 2)[, 1],
             label = stringr::str_trim(label))
    
    # Remove the count text
    plot_caption <- expression(
      Slope ~ '[lower,' ~ 'upper]' ~ 95 ~ '%' ~ 'in' ~ units ~
        year ^ {-1} ~ 'and * indicates a sig. trend' 
    )
    
  }
  
  if (stringr::str_detect(scales, "free") && !is.na(facet_variable[1])) {
    df_labels <- left_join(df_labels, df_value_label_y, by = facet_variable)
  } else {
    df_labels <- mutate(df_labels, value_label_y = !!value_label_y)
  }
  
  # A name switch, this needs to be done if the trend tests have been applied
  # with smonitor
  if ("deseason" %in% names(df_tests)) {
    df_tests <- rename(df_tests, decomposed = deseason)
  }
  
  # For y-axes labels
  if (all(df_tests$decomposed)) {
    label_y_axis <- "Deseasonalised monthly means"
  } else {
    label_y_axis <- "Monthly means"
  }
  
  # Build the basic components of plot, some repetition here...
  if (colour %in% names(df)) {
    
    # Requires the use of aes_string
    plot <- ggplot(
      data = df, 
      ggplot2::aes_string("date", "trend_and_remainder", colour = colour)
    ) + 
      geom_line() + 
      geom_point(size = 1.5, pch = 1, na.rm = TRUE)
    
  } else {
    
    # A bit simpler
    plot <- ggplot(data = df, aes(date, trend_and_remainder)) +
      geom_line(colour = colour, na.rm = TRUE) +
      geom_point(size = 1.5, pch = 1, colour = colour, na.rm = TRUE)
    
  }
  
  # Add the extras
  plot <- plot + 
    geom_abline(
      data = df_tests,
      aes(slope = slope / threadr::seconds_in_a_year(), intercept = intercept)
    ) +
    geom_abline(
      data = df_tests,
      aes(
        slope = slope_lower / threadr::seconds_in_a_year(), 
        intercept = intercept_lower
      ),
      linetype = "dashed"
    ) +
    geom_abline(
      data = df_tests,
      aes(
        slope = slope_upper / threadr::seconds_in_a_year(), 
        intercept = intercept_upper
      ),
      linetype = "dashed"
    ) + 
    ylim(as.numeric(ylim)) + 
    threadr::theme_less_minimal() + 
    xlab("Date") +
    ylab(label_y_axis)
  
  if (label) {
    # Could use geom_label too? 
    plot <- plot +
      geom_text(
        data = df_labels,
        aes(label = label, x = date_centre, y = value_label_y),
        size = 3,
        colour = "black"
      ) +
      labs(caption = plot_caption)
  }
  
  # Facet
  if (!is.na(facet_variable[1])) {
    
    # For expressions
    if (parse_facet_label) {
      facet_label <- ggplot2::label_parsed
    } else {
      facet_label <- "label_value"
    }
    
    plot <- plot + 
      facet_wrap(facet_variable, scales = scales, labeller = facet_label) +
      threadr::theme_less_minimal(x_label_rotate = x_label_rotate)
      
  }
  
  return(plot)
  
}
