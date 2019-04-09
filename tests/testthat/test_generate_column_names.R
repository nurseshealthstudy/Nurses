test_that('column names are generated for default to and from', {
  expect_equal(generate_column_names(prefix = 'irt'),
               c('irt76', 'irt78', 'irt80', 'irt82', 'irt84', 'irt86', 'irt88',
                 'irt90', 'irt92', 'irt94', 'irt96', 'irt98', 'irt00', 'irt02',
                 'irt04', 'irt06', 'irt08', 'irt10', 'irt12', 'irt14', 'irt16',
                 'irt18'))
})

test_that('column names are generated for specific to and/or from', {
  expect_equal(generate_column_names(prefix = 'irt', from = 1990, to = 2002),
               c('irt90', 'irt92', 'irt94', 'irt96', 'irt98', 'irt00', 'irt02'))
  expect_equal(generate_column_names(prefix = 'irt', from = 2004),
               c('irt04', 'irt06', 'irt08', 'irt10', 'irt12', 'irt14', 'irt16',
                 'irt18'))
  expect_equal(generate_column_names(prefix = 'irt', to = 1988),
               c('irt76', 'irt78', 'irt80', 'irt82', 'irt84', 'irt86', 'irt88'))
})

test_that('error is thrown when to and from are not numeric', {
  expect_error(generate_column_names(prefix = 'irt', from = '90', to = '02'))
})
