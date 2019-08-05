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

test_that("column list is extracted from spek",{
  col_list  <- get_column_list(VA_SPEK)
  expected_column_attr <- c(SE$COLUMN_USE_IRI, SE$COLUMN_NAME_IRI,
                            SE$COLUMN_DTYPE_IRI, SE$COLUMN_TITLE_IRI, SE$DESCRIPTION_IRI )
  col_attrs <- lapply(col_list, FUN=names)
  attrs_present <- sapply(col_attrs, FUN=setequal, expected_column_attr)

  expect_true(all(attrs_present))
  expect_equal(length(col_list), 4)
})

test_that("uses are extracted from column list",{
  col_list  <- get_column_list(VA_SPEK)
  col_use <- get_use_of_column(col_list[[1]])
  expect_type(col_use, 'character')
  expect_identical(col_use, "identifier")
})
