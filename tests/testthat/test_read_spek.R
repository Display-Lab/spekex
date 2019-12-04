context('read spek')

TOP_LEVEL_SPEK_KEYS <- c(SE$ABOUT_ORGANIZATION_IRI, SE$ABOUT_MEASURE_IRI,
                         SE$ABOUT_TEMPLATE_IRI, SE$INPUT_TABLE_IRI, "@type")

VA2_SPEK <- read_spek(get_spek_path('va2'))

test_that('Provides error when spek is missing.', {
  expect_error(read_spek(), regexp = SE$WARN_NO_SPEK, fixed=T)
})

test_that('Retuns named list with expected keys',{
  spek_file <- system.file('extdata', 'aspire-spek.json', package='spekex')
  result <- read_spek(spek_file)

  expect_type(result, 'list')
  expect_named(result)
  expect_true(all(TOP_LEVEL_SPEK_KEYS %in% names(result)))
})

test_that('Returns expected top level keys for all suites initial speks',{
  spek_files <- sapply(SE$SUITE_NAMES,
                       function(n){system.file('extdata', paste0(n,'-spek.json'), package='spekex')})
  results <- lapply(spek_files, FUN=read_spek)
  results_have_names <- sapply(results, function(r){ all(TOP_LEVEL_SPEK_KEYS %in% names(r)) })

  expect_true(all(results_have_names))
})

test_that('Fills blank node ids in measures that lack them',{
  measure_ids <- sapply(measures_from_spek(VA2_SPEK), `[[`, '@id')
  expect_false(any(is.null(measure_ids)))
})

test_that("Filled measure ids begin with expected prefix",{
  measure_ids <- sapply(measures_from_spek(VA2_SPEK), `[[`, '@id')
  patt <- paste0("^",SE$MEASURE_BNODE_PREFIX)
  expect_match(measure_ids, patt)
})

test_that('Fills blank node ids to comparators that lack them',{
  measures <- measures_from_spek(VA2_SPEK)
  comparators <- unlist(lapply(measures, comparators_of_measure), recursive = F)
  comparator_ids <- sapply(comparators, `[[`, '@id')
  expect_false(any(is.null(comparator_ids)))
})

test_that("Filled comparator ids begin with expected prefix",{
  measures <- measures_from_spek(VA2_SPEK)
  comparators <- unlist(lapply(measures, comparators_of_measure), recursive = F)
  comparator_ids <- sapply(comparators, `[[`, '@id')
  patt <- paste0("^",SE$MEASURE_BNODE_PREFIX)
  expect_match(comparator_ids, patt)
})
