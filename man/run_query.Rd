% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_query.R
\name{run_query}
\alias{run_query}
\title{Convenience method to execute a SQL query optionally expressed template.}
\usage{
run_query(query, project, page_size = 5000, max_pages = Inf)
}
\arguments{
\item{query}{SQL query string optionally containing code to be interpolated}

\item{project}{The billing project name, a string.}

\item{page_size}{The number of rows returned per page. Make this smaller if you
have many fields or large records and you are seeing a 'responseTooLarge' error.}

\item{max_pages}{Maximum number of results to retrieve. Defaults to Inf to retrieve all rows.}
}
\description{
This function is a wrapper for the \code{\link[bigrquery:bq_project_query]{bigrquery::bq_project_query()}} and
\code{\link[bigrquery:bq_table_download]{bigrquery::bq_table_download()}} functions from
the \code{bigrquery} package changing a few of the default values for parameters.
See the documentation and vignettes of that package to learn how to use bigrquery.
It also makes use of \code{\link[stringr:str_glue]{stringr::str_glue()}} to format and interpolate the SQL string
passed in, if any of it needed to be interpolated using values from the global environment.
See the documentation and vignettes of the \code{stringr} package for more detail.
}
\details{
Troubleshooting tips
\itemize{
\item If you get 'Error: Requested Resource Too Large to Return
\href{https://cloud.google.com/bigquery/troubleshooting-errors}{responseTooLarge}',
reduce the value of the page_size parameter.
\item If you get 'Error: Exceeded rate limits: Your project: ### exceeded quota
for tabledata.list bytes per second per project. \href{https://cloud.google.com/bigquery/troubleshooting-errors}{rateLimitExceeded}' or
'Error: Error encountered during execution. Retrying may solve the problem. \href{https://cloud.google.com/bigquery/troubleshooting-errors}{backendError}'
increase the value of the page_size parameter.
}
}
\examples{
\dontrun{

BILLING_PROJECT <- 'my-cloud-project'
TABLE <- 'genomics-public-data.1000_genomes.sample_info'
df <- run_query('SELECT Sample, Gender, Super_Population FROM `{TABLE}`',
                project = BILLING_PROJECT)

}
}
\seealso{
\code{\link[bigrquery:bq_project_query]{bigrquery::bq_project_query()}}, \code{browseVignettes("bigrquery")}

\code{\link[bigrquery:bq_table_download]{bigrquery::bq_table_download()}}, \code{browseVignettes("bigrquery")}

\code{\link[stringr:str_glue]{stringr::str_glue()}}, \code{browseVignettes("stringr")}
}
