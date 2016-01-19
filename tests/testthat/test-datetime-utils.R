context("Data/Time functions")

test_that("date_mdy", {

    expect_identical(date_mdy(1, 1, 2000), as.Date('2000-01-01'))

})

test_that("date_plus", {

    expect_identical(date_plus(as.Date('2000-01-01'), 1, 1, 1),
                     as.Date('2001-02-02'))

})
