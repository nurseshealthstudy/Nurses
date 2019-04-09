#' Given a set of columns of data in a time series check whether they are all
#' of the same mode, class, and if factor or string-valued, the same collection of
#' strings.
#'
#' It also emits the output of skimr::skim and a plot so that the user can also
#' visually inspect additional aspects of the columns.
#'
#' Note that the returned plot can be captured and modified further to, for example,
#' change the X and Y limits.
#'
#' @param data_ The dataframe to check.
#' @param col_names Character vector of columns to check.
#' @return a ggplot2 object for the columns that were checked
#' @export
#' @md
#'
#' @examples
#' data_that_passes <- data.frame(smk80 = c('never', 'past', 'never'),
#'                                smk82 = c('past', 'never', 'past'),
#'                                smk84 = c('never', 'never', 'past'))
#' data_that_fails <- data.frame(smk80 = c('never smoker', 'past smoker', 'never smoker'),
#'                               smk82 = c('past', 'never', 'past'),
#'                               smk84 = c('never', 'never', 'past'))
#' check_columns(data_that_passes, generate_column_names('smk', from = 1980, to = 1984))
#' \dontrun{
#'
#' check_columns(data_that_fails, generate_column_names('smk', from = 1980, to = 1984))
#'
#' }
#'
#' # You can also capture the plot returned and modify it.
#' numeric_data_to_plot <- data.frame(
#'                            bmi80 = rnorm(200, mean = 30, sd = 3),
#'                            bmi82 = rnorm(200, mean = 31, sd = 2.8),
#'                            bmi84 = rnorm(200, mean = 32, sd = 2.5))
#' p <- check_columns(numeric_data_to_plot,
#'                    generate_column_names('bmi', from = 1980, to = 1984))
#' # Show default plot
#' p
#'
#' # Modify default plot to zoom in by changing the limits
#' library(ggplot2)
#' p + ylim(27, 35)
#'
#' # Modify the default plot to invert the X and Y axes and use
#' # a different theme.
#' p + coord_flip() + theme_minimal()
#'
check_columns <- function(data_, col_names) {
  # Check whether the mode and type are the same for all of the data.
  modes <- purrr::map(col_names, function(x) { mode(data_[[x]]) })
  classes <- purrr::map(col_names, function(x) { class(data_[[x]]) })
  stopifnot(1 == length(unique(modes)))
  stopifnot(1 == length(unique(classes)))

  if ('factor' == classes[1]) {
    # This data is a factor. Check that all columns contain the same set of levels.
    col_levels <- levels(data_[[col_names[1]]])
    stopifnot(length(col_levels) > 0)
    for (i in seq(2, length(col_names))) {
      message(stringr::str_glue(
        'Comparing levels in {col_names[1]} to {col_names[i]}.'))
      stopifnot(col_levels == levels(data_[[col_names[i]]]))
    }
    message(c('\nFactor Levels:\n\t', stringr::str_c(col_levels, collapse = '\n\t')))
  } else if ('character' == classes[1]) {
    # This data is string-valued. Check that all columns contain the same set of
    # unique strings.
    col_levels <- sort(unique(data_[[col_names[1]]]))
    stopifnot(length(col_levels) > 0)
    for (i in seq(2, length(col_names))) {
      message(stringr::str_glue(
        'Comparing unique values in {col_names[1]} to {col_names[i]}.'))
      stopifnot(col_levels == sort(unique(data_[[col_names[i]]])))
    }
    message(c('\nUnique values:\n\t', stringr::str_c(col_levels, collapse = '\n\t')))
  } else {
    # This data is something other than a factor or string-valued data. Be sure
    # to examine the subsetquent skim and plot.
    message(stringr::str_glue('All columns are of class {classes[1]}.'))
  }

  # As a convenience, output the skim of the data.
  print(skimr::skim(data_[col_names]))

  # As a convenience, return a default plot. Note that this return plot object
  # can be further modified (e.g., to change the X or Y limits).
  return(plot_columns(data_ = data_, col_names = col_names))
}
