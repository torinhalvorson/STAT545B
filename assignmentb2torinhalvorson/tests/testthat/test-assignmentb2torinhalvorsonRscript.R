# Test that the function output is the same regardless of the order of the numbers in the vector
test_that("test1", {expect_equal(stdev(c(1,3,5,7,10)), stdev(c(10,7,5,3,1)))})

#Test that the function returns an error if the input is not numeric
test_that("test2", {expect_error(stdev(c("apple", "banana", "orange")))})

#Test that the function returns the same output regardless of missing values in the vector
test_that("test3", {expect_equal(stdev(c(1,3,5,NA,4,7)), stdev(c(1,3,5,4,7)))})
