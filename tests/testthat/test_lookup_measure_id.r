context("testing spek extractors")

VA2_SPEK <- read_spek(get_spek_path('va2'))

test_that("id can be looked up from measure identifier", {
  short_result <- lookup_measure_id("Short-Stay", VA2_SPEK)
  long_result <- lookup_measure_id("Long-Term", VA2_SPEK)

  expect_identical(short_result, "_:m1")
  expect_identical(long_result, "_:m2")
})
