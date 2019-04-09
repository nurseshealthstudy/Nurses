test_that('print_table1', {

  mtcars_summaries <-
    list('Miles Per Gallon' =
        list('min' = ~ min(mpg),
        'mean' = ~ mean(mpg),
        'mean &plusmn; sd' = ~ qwraps2::mean_sd(mpg),
        'max' = ~ max(mpg)),
      'Weight' =
        list('median' = ~ median(wt)),
      'Cylinders' =
        list('4 cyl: n (%)' = ~ qwraps2::n_perc0(cyl == 4),
             '6 cyl: n (%)' = ~ qwraps2::n_perc0(cyl == 6),
             '8 cyl: n (%)' = ~ qwraps2::n_perc0(cyl == 8)))

  expect_equal(as.character(print_table1(
    summary_table = qwraps2::summary_table(mtcars, mtcars_summaries),
    title = '**summary table markdown**',
    caption = 'example using mtcars')),
    '**summary table markdown**



|covariates                    |mtcars (N = 32)     |
|:-----------------------------|:-------------------|
|**Miles Per Gallon**          |&nbsp;&nbsp;        |
|&nbsp;&nbsp; min              |10.4                |
|&nbsp;&nbsp; mean             |20.09062            |
|&nbsp;&nbsp; mean &plusmn; sd |20.09 &plusmn; 6.03 |
|&nbsp;&nbsp; max              |33.9                |
|**Weight**                    |&nbsp;&nbsp;        |
|&nbsp;&nbsp; median           |3.325               |
|**Cylinders**                 |&nbsp;&nbsp;        |
|&nbsp;&nbsp; 4 cyl: n (%)     |11 (34)             |
|&nbsp;&nbsp; 6 cyl: n (%)     |7 (22)              |
|&nbsp;&nbsp; 8 cyl: n (%)     |14 (44)             |

example using mtcars')
})
