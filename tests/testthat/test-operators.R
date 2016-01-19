context("Operators")

## -----
## %nin%
## -----
test_that("nin does what it suggests", {

  ## integer
  expect_that(1L %nin% 2:4, is_true())
  expect_that(2L %nin% 2:4, is_false())
  expect_that(NA %nin% 2:4, is_true())

  ## double
  expect_that(1.0 %nin% c(2.0, 4.0), is_true())
  expect_that(2.0 %nin% c(2.0, 4.0), is_false())
  expect_that(NA  %nin% c(2.0, 4.0), is_true())

  ## characters
  expect_that("a" %nin% letters[2:4], is_true())
  expect_that("b" %nin% letters[2:4], is_false())
  expect_that(NA  %nin% letters[2:4], is_true())

})

test_that("nin is !in", {

  ## integer
  t2 <- c(1:5, NA)
  c2 <- c(4:6, NA)
  expect_that(t2 %nin% c2, is_identical_to(! t2 %in% c2))  

  ## integer
  t2 <- as.double(t2)
  c2 <- as.double(c2)
  expect_that(t2 %nin% c2, is_identical_to(! t2 %in% c2))  

  ## char
  t1 <- c(letters[1:5], NA)
  c1 <- c(letters[4:7], NA)
  expect_that(t1 %nin% c1, is_identical_to(! t1 %in% c1))

})

## ----------
## %in_range%
## ----------

test_that("%in_range% does what it suggests", {

  ## integer
  expect_that(1L %in_range% c(2L,4L), is_false())
  expect_that(3L %in_range% c(2L,4L), is_true())
  expect_that(NA %in_range% c(2L,4L), is_identical_to(NA))

  ## double
  expect_that(1.0 %in_range%  c(2.0, 4.0), is_false())
  expect_that(3.0 %in_range%  c(2.0, 4.0), is_true())
  expect_that(NA  %in_range%  c(2.0, 4.0), is_identical_to(NA))

})

## ---------
## %without%
## ---------

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
