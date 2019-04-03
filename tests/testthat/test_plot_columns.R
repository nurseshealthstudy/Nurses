test_that('plot continuous', {
  plot <- plot_columns(tibble::tibble(cont12 = c(1.0, 2.0, 3.0),
                                      cont14 = c(4.0, NA_real_, 5.0)),
                       c('cont12', 'cont14'))
  expect_is(plot, 'ggplot')
})

test_that('plot categorical', {
  plot <- plot_columns(tibble::tibble(cat12 = c('never smoker', 'past smoker', NA_character_),
                                      cat14 = c('past smoker', 'never smoker', 'past smoker')),
                       c('cat12', 'cat14'))
  expect_is(plot, 'ggplot')
})
