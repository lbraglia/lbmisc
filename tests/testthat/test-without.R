context("%without%")

test_that("%without% does what it suggests", {

  ## integer
  x <- c(2L,4L)
  expect_that(x %without% 1L, is_identical_to(x))
  expect_that(x %without% 2L, is_identical_to(c(4L)))
  expect_that(x %without% NA, is_identical_to(x))

  ## double
  x <- as.double(x)
  expect_that(x %without% 1.0, is_identical_to(x))
  expect_that(x %without% 2.0, is_identical_to(c(4.0)))
  expect_that(x %without% NA, is_identical_to(x))

  ## characters
  x <- letters[1:5]
  expect_that(x %without% letters[6], is_identical_to(x))
  expect_that(x %without% letters[5], is_identical_to(letters[1:4]))
  expect_that(x %without% NA, is_identical_to(x))
  
  
})
