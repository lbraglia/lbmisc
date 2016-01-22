context("%in_range%")

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
