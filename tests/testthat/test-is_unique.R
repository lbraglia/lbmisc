context("Duplicated")

test_that("is.unique works correctly", {

    expect_true(is.unique(1:3))
    expect_false(is.unique(c('a', 'a', 'b')))

})
