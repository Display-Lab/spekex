context("testing measure extractors")

# For convenience always use the same order: sham, mtx, va
SHAM_SPEK <- read_spek(get_spek_path('sham'))
MTX_SPEK <- read_spek(get_spek_path('mtx'))
VA_SPEK <- read_spek(get_spek_path('va'))
VA2_SPEK <- read_spek(get_spek_path('va2'))

COMP1_TITLE <- "Comparator One"
COMP1_NAME  <- "comp_one"
COMP2_TITLE <- "Comparator Two"
COMP2_NAME  <- "comp_two"

COMPARATOR1 <- list( `@type` = SE$GOAL_COMPARATOR_IRI,
                    `http://purl.org/dc/terms/title` = list(list(`@value` = COMP1_TITLE)),
                    `http://schema.org/name` = list(list(`@value` = COMP1_NAME)),
                    `http://example.com/slowmo#ComparisonValue` = list(list( `@value` = 11L))
                  )
COMPARATOR2 <- list( `@type` = SE$SOCIAL_COMPARATOR_IRI,
                    `http://purl.org/dc/terms/title` = list(list(`@value` = COMP2_TITLE)),
                    `http://schema.org/name` = list(list(`@value` = COMP2_NAME)),
                    `http://example.com/slowmo#ComparisonValue` = list(list( `@value` = 11L))
                  )
MEASURE <- list(`@id` = "_:m1", `@type` = SE$MEASURE_IRI,
                  `http://purl.org/dc/terms/title` = list(list(`@value` = "Measure Title")),
                  `http://schema.org/identifier` = list(list(`@value` = "M01")),
                  `http://example.com/slowmo#WithComparator` = list(COMPARATOR1, COMPARATOR2)
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

test_that("correct number of comparators are extracted from measure",{
  result <- comparators_of_measure(MEASURE)
  expect_type(result, "list")
  expect_length(result, 2)
})

test_that("comparators are extracted from measure",{
  result <- comparators_of_measure(MEASURE)
  expected <- list(COMPARATOR1, COMPARATOR2)
  expect_identical(result, expected)
})

test_that("type of comparator can be extracted from measure", {
  result <- comparator_types_of_measure(MEASURE)
  expect_type(result, "list")
  expect_length(result, 2)
  expect_identical(result, list(SE$GOAL_COMPARATOR_IRI,SE$SOCIAL_COMPARATOR_IRI))
})
