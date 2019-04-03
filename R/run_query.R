#' Convenience method to execute a SQL query optionally expressed template.
#'
#' This function is a wrapper for the [bigrquery::bq_project_query()] and
#' [bigrquery::bq_table_download()] functions from
#' the `bigrquery` package changing a few of the default values for parameters.
#' See the documentation and vignettes of that package to learn how to use bigrquery.
#' It also makes use of [stringr::str_glue()] to format and interpolate the SQL string
#' passed in, if any of it needed to be interpolated using values from the global environment.
#' See the documentation and vignettes of the `stringr` package for more detail.
#'
#' Troubleshooting tips
#' - If you get 'Error: Requested Resource Too Large to Return
#'   [responseTooLarge](https://cloud.google.com/bigquery/troubleshooting-errors)',
#'   reduce the value of the page_size parameter.
#'
#' - If you get 'Error: Exceeded rate limits: Your project: ### exceeded quota
#'   for tabledata.list bytes per second per project. [rateLimitExceeded](https://cloud.google.com/bigquery/troubleshooting-errors)' or
#'  'Error: Error encountered during execution. Retrying may solve the problem. [backendError](https://cloud.google.com/bigquery/troubleshooting-errors)'
#'  increase the value of the page_size parameter.
#'
#' @param query SQL query string optionally containing code to be interpolated
#' @param project The billing project name, a string.
#' @param page_size The number of rows returned per page. Make this smaller if you
#'   have many fields or large records and you are seeing a 'responseTooLarge' error.
#' @param max_pages Maximum number of results to retrieve. Defaults to Inf to retrieve all rows.
#'
#' @seealso [bigrquery::bq_project_query()], `browseVignettes("bigrquery")`
#' @seealso [bigrquery::bq_table_download()], `browseVignettes("bigrquery")`
#' @seealso [stringr::str_glue()], `browseVignettes("stringr")`
#' @export
#' @md
#' @examples
#' \dontrun{
#'
#' BILLING_PROJECT <- 'my-cloud-project'
#' TABLE <- 'genomics-public-data.1000_genomes.sample_info'
#' df <- run_query('SELECT Sample, Gender, Super_Population FROM `{TABLE}`',
#'                 project = BILLING_PROJECT)
#'
#' }
run_query <- function(query, project, page_size = 5000, max_pages = Inf) {
  interpolated_query <- stringr::str_glue(query, .envir = globalenv())
  message(interpolated_query)
  tbl <- bigrquery::bq_project_query(
    project,
    query = interpolated_query)
  result <- bigrquery::bq_table_download(
    tbl,
    page_size = page_size,
    max_results = max_pages)
  message('Number of rows returned: ', nrow(result))
  message('Number of columns returned: ', ncol(result))
  return(result)
}
