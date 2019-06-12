context('suite names')

test_that('suite names are not empty',{
  result <- list_suite_names()
  expect_gt(length(result), 0)
})
