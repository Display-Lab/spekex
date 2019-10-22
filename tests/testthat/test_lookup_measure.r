context("testing spek extractors")

VA2_SPEK <- read_spek(get_spek_path('va2'))

test_that("measure can be looked up by id or value",{
  val_result <- lookup_measure("Short-Stay", VA2_SPEK)
  id_result <- lookup_measure("_:m1", VA2_SPEK)

  expect_true(length(val_result) > 0 && length(id_result) > 0)
  expect_identical(val_result, id_result)
})

test_that("id can be looked up from measure identifier", {
  short_result <- lookup_measure_id_by_value("Short-Stay", VA2_SPEK)
  long_result <- lookup_measure_id_by_value("Long-Term", VA2_SPEK)

  expect_identical(short_result, "_:m1")
  expect_identical(long_result, "_:m2")
})

test_that("no matching identifier raises warning",{
  expect_warning(lookup_measure("NotPresent", VA2_SPEK))
})

test_that("no matching identifier returns empty list",{
  result <- expect_warning(lookup_measure("NotPresent", VA2_SPEK))
  expect_identical(result, list())
})
