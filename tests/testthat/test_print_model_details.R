test_that('print_model_details', {
  ## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
  ## Page 9: Plant Weight Data.
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  test_lm <- lm(weight ~ group)

  expect_equal(as.character(print_model_details(
    model = test_lm,
    title = '**model markdown**',
    caption = 'example using plant weight data')),
    '**model markdown**

|term        |    estimate|p_value  |
|:-----------|-----------:|:--------|
|(Intercept) | 153.2391848|9.55e-15 |
|groupTrt    |   0.6900439|2.49e-01 |

example using plant weight data')
})