context("Data management")

test_that("duplicated2 flag all the duplicates by default", {

    expect_identical(duplicated2(c(0, 1, 1, 2, 2, NA, NA)),
                     c(FALSE, rep(TRUE, 6))

})


test_that("is.unique works correctly", {

    expect_identical(is.unique(1:3), TRUE)
    expect_identical(is.unique(c('a','a','b'), FALSE)

})
