context('suite file providers')

test_that('spek paths exist for each suite.',{
  spek_paths <- lapply(list_suite_names(), get_spek_path)
  paths_exists <- sapply(spek_paths, file.exists)
  expect_true(all(paths_exists))
})

test_that('data suite paths exist for each suite.',{
  data_paths <- lapply(list_suite_names(), get_data_path)
  paths_exists <- sapply(data_paths, file.exists)
  expect_true(all(paths_exists))

})

test_that('annotations paths exist for each suite.',{
  anno_paths <- lapply(list_suite_names(), get_annotations_path)
  paths_exists <- sapply(anno_paths, file.exists)
  expect_true(all(paths_exists))
})

test_that('list suite names reads from package constants',{
  result <- list_suite_names()
  expect_identical(result, SE$SUITE_NAMES)
})

test_that('Error on bad suite name',{
  expect_error(get_annotations_path('foo'), SE$UNRECOGNIZED_NAME, fixed=T)
  expect_error(get_data_path('foo'), SE$UNRECOGNIZED_NAME, fixed=T)
  expect_error(get_spek_path('foo'), SE$UNRECOGNIZED_NAME, fixed=T)
})