test_that('matching integer columns are okay', {
  plot <- check_columns(tibble::tibble(var80 = c(1, 2, 3),
                                       var82 = c(4, NA_integer_, 5)),
                        c('var80', 'var82'))
  expect_is(plot, 'ggplot')
})

test_that('matching float columns are okay', {
  plot <- check_columns(tibble::tibble(var80 = c(1.0, 2.0, 3.0),
                                       var82 = c(4.0, NA_real_, 5.0)),
                        c('var80', 'var82'))
  expect_is(plot, 'ggplot')
})

test_that('matching boolean columns are okay', {
  plot <- check_columns(tibble::tibble(var80 = c(TRUE, TRUE, NA),
                                       var82 = c(FALSE, TRUE, FALSE)),
                        c('var80', 'var82'))
  expect_is(plot, 'ggplot')
})

test_that('matching characters columns are okay', {
  plot <- check_columns(
    tibble::tibble(
      var80 = c('never smoker', 'past smoker', NA_character_),
      var82 = c('past smoker', 'never smoker', 'past smoker')),
    c('var80', 'var82'))
  expect_is(plot, 'ggplot')
})
test_that('matching factor columns are okay', {
  plot <- check_columns(
    data.frame(
      var80 = c('never smoker', 'past smoker', NA_character_),
      var82 = c('past smoker', 'never smoker', 'past smoker'),
      stringsAsFactors = TRUE),
    c('var80', 'var82'))
  expect_is(plot, 'ggplot')
})

test_that('integer columns together with character columns fail', {
  expect_error(check_columns(data.frame(var80 = c(1, 2, 3),
                                        var82 = c('4', NA_character_, '5')),
                             c('var80', 'var82')))
})

test_that('character columns with different levels fail', {
  expect_error(check_columns(tibble::tibble(var80 = c('1', '2', '3'),
                                            var82 = c('4', '5', '6')),
                             c('var80', 'var82')))
})

test_that('factor columns with different levels fail', {
  expect_error(check_columns(
    data.frame(
      var80 = c('never smoker', 'past smoker', NA_character_),
      var82 = c('past', 'never', 'past'),
      stringsAsFactors = TRUE),
    c('var80', 'var82')))
})
