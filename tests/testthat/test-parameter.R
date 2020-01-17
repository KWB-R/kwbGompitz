context("test-parameter")

test_that("reading and writing the parameter file works", {
  
  file <- exampleFile("param.txt")
  
  # Read the example file param.txt that is shipped with GompitZ
  parameters <- readParameters(file)

  # Set number of exponent digits
  digits_exp = 3L
  
  # Check that the function that creates the content for param.txt
  # is able to recreate the lines that were read
  expect_equal(
    parameterLines(parameters, digits_exp = digits_exp), 
    readLines(file)
  )
  
  # Write the parameters to a temporary file
  writeParameters(
    parameters, (temp_file <- tempfile()), digits_exp = digits_exp
  )
  
  # Check that the file contents are identical
  expect_identical(readLines(file), readLines(temp_file))
})
