context("comparator lookup")

VA2_SPEK <- read_spek(get_spek_path('va2'))

test_that("comparator can be looked up by @id",{
  result <- lookup_comparator("_:m1001", VA2_SPEK)

  expect_identical(name_of_comparator(result), "peer_ave")
  expect(length(result) > 0)
})

test_that("no matching identifier raises warning",{
  expect_warning(lookup_measure("NotPresent", VA2_SPEK))
})

test_that("no matching identifier returns empty list",{
  result <- expect_warning(lookup_measure("NotPresent", VA2_SPEK))
  expect_identical(result, list())
})
