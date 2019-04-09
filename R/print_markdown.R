kInstalledPackages <- rownames(installed.packages())

print_markdown <- function(markdown, output_file) {
  if (methods::hasArg(output_file)) {
    if (tools::file_ext(output_file) != 'md') {
      warning(output_file, ' should instead end in ".md" because it will be a markdown file.')
    }
    message(
      stringr::str_c(
        '\n\nWriting to ',
        output_file,
        '\nTip:',
        '\n(0) Download this file from Jupyter.',
        '\n(1) Use Microsoft Word to open this Markdown file.',
        '\n(2) Use the "Save as..." command and choose "Word Document" from the Save as type field.')
    )
    cat(markdown, file = output_file, sep = '\n')
  }
  if ('IRdisplay' %in% kInstalledPackages) {
    IRdisplay::display_markdown(markdown)
  } else {
    print(markdown)
  }
}

#' Convenience method to output a summary_table object as markdown.
#'
#' Markdown is emitted to the console, and optionally to an external file.
#'
#' This function takes as input a [qwraps2::summary_table()] object`. See the
#' documentation and vignettes of that package to learn how to use qwraps2.
#'
#' @param summary_table The qwraps2::summary_table object to print.
#' @param title The title for the table.
#' @param caption The caption for the table.
#' @param output_file (optional) The file to which to write the markdown.
#'
#' @seealso [qwraps2::summary_table()], `browseVignettes('qwraps2')`
#' @export
#' @md
#' @examples
#' require(qwraps2)
#'
#' mtcars_summaries <-
#' list('Miles Per Gallon' =
#'        list('min' = ~ min(mpg),
#'        'mean' = ~ mean(mpg),
#'        'mean &plusmn; sd' = ~ qwraps2::mean_sd(mpg),
#'        'max' = ~ max(mpg)),
#'      'Weight' =
#'        list('median' = ~ median(wt)),
#'      'Cylinders' =
#'        list('4 cyl: n (%)' = ~ qwraps2::n_perc0(cyl == 4),
#'             '6 cyl: n (%)' = ~ qwraps2::n_perc0(cyl == 6),
#'             '8 cyl: n (%)' = ~ qwraps2::n_perc0(cyl == 8)))
#'
#' print_table1(summary_table = qwraps2::summary_table(mtcars, mtcars_summaries),
#'              title = '**mtcars summary table**',
#'              caption = 'example using mtcars')
print_table1 <- function(summary_table, title = '**Table 1**', caption = '', output_file) {
  orig_opt <- options()$qwraps2_markup
  options(qwraps2_markup = 'markdown')
  table <- stringr::str_c(utils::capture.output(print(summary_table,
                                                      rtitle = 'covariates',
                                                      markup = 'markdown')),
                          collapse = '\n')
  options(qwraps2_markup = orig_opt)
  markdown <- stringr::str_glue('\n{title}\n\n{table}\n\n{caption}\n')
  print_markdown(markdown, output_file)
}

#' Convenience method to output a model object as markdown.
#'
#' Markdown is emitted to the console, and optionally to an external file.
#'
#' This function takes as input a model object and applies [broom::tidy()] to
#' it to obtain a data frame. See the documentation and vignettes of broom
#' for more detail.
#'
#' @param model The model object to print.
#' @param title The title for the table.
#' @param caption The caption for the table.
#' @param output_file (optional) The file to which to write the markdown.
#'
#' @seealso [broom::tidy()], `browseVignettes('broom')`
#'
#' @importFrom magrittr %>%
#'
#' @export
#' @md
#' @examples
#' ## Annette Dobson (1990) 'An Introduction to Generalized Linear Models'.
#' ## Page 9: Plant Weight Data.
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' group <- gl(2, 10, 20, labels = c('Ctl','Trt'))
#' weight <- c(ctl, trt)
#' test_lm <- lm(weight ~ group)
#'
#' print_model_details(model = test_lm,
#'                     title = '**model markdown**',
#'                     caption = 'example using plant weight data')
print_model_details <- function(model, title = '**Model details**', caption = '', output_file) {
  model_details <- broom::tidy(model, exponentiate = TRUE) %>%
    dplyr::mutate(p_value = format(p.value, scientific = TRUE, digits = 3)) %>%
    dplyr::select(-std.error, -statistic, -p.value)
  model_markdown <- stringr::str_c(knitr::kable(model_details),
                                   collapse = '\n')
  markdown <- stringr::str_glue('\n{title}\n\n{model_markdown}\n\n{caption}\n')
  print_markdown(markdown, output_file)
}
