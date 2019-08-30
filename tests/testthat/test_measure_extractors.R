context("testing measure extractors")

# For convenience always use the same order: sham, mtx, va
SHAM_SPEK <- read_spek(get_spek_path('sham'))
MTX_SPEK <- read_spek(get_spek_path('mtx'))
VA_SPEK <- read_spek(get_spek_path('va'))
VA2_SPEK <- read_spek(get_spek_path('va2'))

COMPARATOR <- list( `@type` = "http://purl.obolibrary.org/obo/psdo_0000094",
                  `http://purl.org/dc/terms/title` = list(list(`@value` = "Comparator Title")),
                  `http://schema.org/name` = list(list(`@value` = "comp_name")),
                  `http://example.com/slowmo#ComparisonValue` = list(list( `@value` = 11L))
                )

MEASURE <- list(`@id` = "_:m1", `@type` = "http://purl.obolibrary.org/obo/psdo.owl/psdo_0000102",
                `http://purl.org/dc/terms/title` = list(list(`@value` = "Measure Title")),
                `http://schema.org/identifier` = list(list(`@value` = "M01")),
                `http://example.com/slowmo#WithComparator` = list(COMPARATOR)
               )

test_that("measure extractor returns correct number of measures", {
  all_speks <- list(SHAM_SPEK, MTX_SPEK, VA_SPEK, VA2_SPEK )
  results <- lapply(all_speks, FUN=measures_from_spek)
  results_lengths <- sapply(results, FUN=length)
  expect_equal(results_lengths, c(1,1,1,2))
})

test_that("measure extractor returns list representation of measures", {
  all_speks <- list(SHAM_SPEK, MTX_SPEK, VA_SPEK, VA2_SPEK)
  results <- lapply(all_speks, FUN=measures_from_spek)
  results_classes <- sapply(results, FUN=class)
  results_rdftypes <- sapply(results, function(x) x[[1]][['@type']])

  expect_true(all(results_rdftypes == SE$MEASURE_IRI))
  expect_true(all(results_classes == 'list'))
})

test_that("title can be extracted from measures", {
  result <- title_of_measure(MEASURE)
  expect_identical(result, "Measure Title")
})

test_that("identifier can be extracted from measure",{
  result <- identifier_of_measure(MEASURE)
  expect_identical(result, "M01")
})

test_that("id can be extracted from measure",{
  result <- id_of_measure(MEASURE)
  expect_identical(result, "_:m1")
})

test_that("comparator can be extracted from measure",{
  result <- comparator_of_measure(MEASURE)
  expect_identical(result, COMPARATOR)
})

test_that("name can be extracted from comparator",{
  result <- name_of_comparator(COMPARATOR)
  expect_identical(result, "comp_name")
})

test_that("title can be extracted from comparator",{
  result <- title_of_comparator(COMPARATOR)
  expect_identical(result, "Comparator Title")

})

test_that("value can be extracted from comparator",{
  result <- comparison_value_of_comparator(COMPARATOR)
  expect_identical(result, 11L)
})
