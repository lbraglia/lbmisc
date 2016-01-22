context("date_plus")

test_that("date_plus works fine", {

    expect_identical(date_plus(as.Date('2000-01-01'), 1, 1, 1),
                     as.Date('2001-02-02'))

})
