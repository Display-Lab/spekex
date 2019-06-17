context('generate spek')

test_that('Returns list with expected names.', {
  result <- generate_spek("sham")

  expect_true(result[['@id']] == "http://example.com/app#example-client")
  expect_true(result[['@type']] == "http://example.com/slowmo#spek")
})
