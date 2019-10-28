context("testing cols for readr")

VA2_SPEK <- read_spek(get_spek_path('va2'))

test_that("extracts named list of readr collectors", {
  result <- cols_for_readr(VA2_SPEK)

  expect_type(result, "list")
  expect_length(names(result), 5)
  expect_s3_class(result[[1]], "collector")
})
