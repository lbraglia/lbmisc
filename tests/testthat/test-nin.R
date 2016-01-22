context("%nin%")

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
