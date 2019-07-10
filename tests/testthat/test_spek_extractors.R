context("testing spek extractors")

# For convenience always use the same order: sham, mtx, va
SHAM_SPEK <- read_spek(get_spek_path('sham'))
MTX_SPEK <- read_spek(get_spek_path('mtx'))
VA_SPEK <- read_spek(get_spek_path('va'))

EXPECTED_USE_COLNAMES <- list(numerators=c(NA, 'high_dose_scripts', 'documented'),
                          denominators=c(NA,'total_scripts','total'),
                          values=c('performance',NA,NA),
                          identities=c('performer','practice','sta6a'))


test_that("id column name extractor returns character vector", {
  all_speks <- list(SHAM_SPEK, MTX_SPEK, VA_SPEK)
  results <- sapply(all_speks, FUN=get_id_col_from_spek)
  expect_identical(results, EXPECTED_USE_COLNAMES$identities)
})

test_that("value column name extractor returns name of value column", {
  result <- get_value_or_numerator_col_from_spek(SHAM_SPEK)
  expect_identical(result, "performance")
})

test_that("numerator column name extractor returns name of numerator column",{
  all_speks <- list(SHAM_SPEK, MTX_SPEK, VA_SPEK)
  results <- sapply(all_speks, FUN=get_numerator_colname)
  expect_identical(results, EXPECTED_USE_COLNAMES$numerators)
})

test_that("denominator column name extractor returns name of denominator column",{
  all_speks <- list(SHAM_SPEK, MTX_SPEK, VA_SPEK)
  results <- sapply(all_speks, FUN=get_denominator_colname)
  expect_identical(results, EXPECTED_USE_COLNAMES$denominators)
})
