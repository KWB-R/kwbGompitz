context("test-parameter")

test_that("reading and writing the parameter file works", {
  
  file <- system.file("extdata/example_unix/param.txt", package = "kwbGompitz")
  
  # Read the example file param.txt that is shipped with GompitZ
  parameters <- kwbGompitz:::readParameters(file)

  # Check that the function that creates the content for param.txt
  # is able to recreate the lines that were read
  expect_equal(kwbGompitz:::parameterLines(parameters), readLines(file))
  
  # Write the parameters to a temporary file
  kwbGompitz:::writeParameters(parameters, (temp_file <- tempfile()))
  
  # Check that the file contents are identical
  testthat::expect_identical(readLines(file), readLines(temp_file))
})
