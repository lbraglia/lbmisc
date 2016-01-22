context("Duplicated")

test_that("duplicated2 flag all the duplicates by default", {

    expect_identical(duplicated2(c(0, 1, 1, 2, 2, NA, NA)),
                     c(FALSE, rep(TRUE, 6)))

})

