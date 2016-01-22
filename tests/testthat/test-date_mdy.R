context("date_mdy")

test_that("date_mdy", {

    expect_identical(date_mdy(3, 1, 2000), as.Date('2000-03-01'))

})
