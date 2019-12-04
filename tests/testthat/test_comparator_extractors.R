context("testing comparator extractors")

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

test_that("name can be extracted from comparator",{
  result <- name_of_comparator(COMPARATOR1)
  expect_identical(result, COMP1_NAME)

  result <- name_of_comparator(COMPARATOR2)
  expect_identical(result, COMP2_NAME)
})

test_that("title can be extracted from comparator",{
  result <- title_of_comparator(COMPARATOR1)
  expect_identical(result, COMP1_TITLE)

})

test_that("value can be extracted from comparator",{
  result <- comparison_value_of_comparator(COMPARATOR1)
  expect_identical(result, 11L)
})

test_that("type can be extracted from comparator", {
  result <- type_of_comparator(COMPARATOR1)
  expect_identical(result, SE$GOAL_COMPARATOR_IRI)

  result <- type_of_comparator(COMPARATOR2)
  expect_identical(result, SE$SOCIAL_COMPARATOR_IRI)
})
