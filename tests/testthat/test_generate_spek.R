context('generate spek')

test_that('Returns list with expected names.', {
  result <- generate_spek("sham")

  expect_true(result[['@id']] == "http://example.com/app#example-client")
  expect_true(result[['@type']] == "http://example.com/slowmo#spek")
})

test_that('Candidates stage returns spek with candidates', {
  skip('Pending issue 13')
  result <- generate_spek("mtx", stage="candidates")
  expect_true(SE$HAS_CANDIDATE_IRI %in% names(result))
})
