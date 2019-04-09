#' Generate the column names from a prefix for our time periods of interest.
#'
#' @param prefix The prefix of the variable name.
#' @param from The starting year, as an integer, for our time periods of interest.
#' @param to The ending year, as an integer, for our time periods of interest.
#' @return A character vector of the column names.
#' @export
#' @md
#'
#' @examples
#' generate_column_names(prefix = 'irt', from = 1990, to = 2002)
generate_column_names <- function(prefix, from = 1976, to = 2018) {
  if (!is.numeric(to) | !is.numeric(from)) {
    stop('Use numeric years such as 1988 for the "to" and "from" parameters.')
  }
  every_two_years <- as.character(seq(from = from, to = to, by = 2))
  stringr::str_c(prefix, stringr::str_sub(every_two_years, 3))
}
