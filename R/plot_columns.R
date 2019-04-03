# Nurses' Health Study started in 1976.
# NHS1 is on even years, NHS2 is on odd years.
ORDERED_YEARS <- as.character(seq(1976, 2020, by = 1))
ORDERED_2_DIGIT_YEARS <- stringr::str_trunc(ORDERED_YEARS,
                                            width = 2,
                                            side = 'left',
                                            ellipsis = '')
ORDERED_PERIODS <- ORDERED_2_DIGIT_YEARS

# When a numeric-valued column has this many or fewer unique values, we'll
# assume its categorical data.
MAX_NUM_CATEGORIES <- 12

#' Given a set of columns of data in a time series plot the data over time.
#'
#' @param data_ The dataframe containing the columns to plot.
#' @param col_names Character vector of columns to plot.
#' @param col_prefix The prefix all the columns have in common. Defaults to the
#'     first column name with the final two characters removed.
#' @return a ggplot2 object for the columns that were checked
#'
#' @importFrom magrittr %>%
#'
#' @export
#' @md
#'
#' @examples
#' data_to_plot <- data.frame(smk80 = c('never', 'past', 'never'),
#'                            smk82 = c('past', 'never', 'past'),
#'                            smk84 = c('never', 'never', 'past'))
#' plot_columns(data_to_plot, generate_column_names('smk', from = 1980, to = 1984))
#'
#' # You can also capture the plot returned and modify it.
#' numeric_data_to_plot <- data.frame(
#'                            bmi80 = rnorm(200, mean = 30, sd = 3),
#'                            bmi82 = rnorm(200, mean = 31, sd = 2.8),
#'                            bmi84 = rnorm(200, mean = 32, sd = 2.5))
#' p <- plot_columns(numeric_data_to_plot,
#'                   generate_column_names('bmi', from = 1980, to = 1984))
#' # Show default plot
#' p
#' # Modify default plot to zoom in by changing the limits
#' library(ggplot2)
#' p + ylim(27, 35)
#'
#' # Modify the default plot to invert the X and Y axes and use
#' # a different theme.
#' p + coord_flip() + theme_minimal()
#'
plot_columns <- function(data_,
                         col_names,
                         col_prefix = stringr::str_sub(col_names[1], end = -3)) {
  periods_from_col_names <- stringr::str_replace(col_names, col_prefix, '')
  # Pivot the data from wide to long.
  long_data = data_ %>%
    dplyr::select(col_names) %>%
    tidyr::gather_(gather_cols = col_names, key_col = 'period', value_col = col_prefix) %>%
    dplyr::mutate(
      # Use the ordering from ORDERED_PERIODS to put a nice order on this factor.
      period = forcats::fct_relevel(stringr::str_replace(.data$period, col_prefix, ""),
                                    intersect(ORDERED_PERIODS, periods_from_col_names))
    )

  if (is.character(long_data[[col_prefix]])
      || MAX_NUM_CATEGORIES >= length(unique(long_data[[col_prefix]]))) {
    # Create a count of instances per categorical value and time period and then display a heatmap.
    long_data %>%
      dplyr::group_by_('period', col_prefix) %>%
      dplyr::summarize(count = dplyr::n()) %>%
      ggplot2::ggplot(ggplot2::aes_string(x='period', y=col_prefix, fill='count', label='count')) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(color = "white") +
      viridis::scale_fill_viridis() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 50, hjust = 1, vjust = 1)) +
      ggplot2::ggtitle(stringr::str_c(col_prefix, ' by time period'))
  } else {
    # Display a box plot of the continuous variable.
    long_data %>%
      ggplot2::ggplot(ggplot2::aes_string(x = 'period', y = col_prefix)) +
      ggplot2::geom_boxplot() +
      ggplot2::ggtitle(stringr::str_c(col_prefix, ' by time period'))
  }
}
