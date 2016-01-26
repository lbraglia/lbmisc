context("rm_spaces")

test_that("rm_spaces space", {
    test<- '   Hello     world   '
    right <- 'Hello world'
    res <- rm_spaces(test)
    expect_identical(res, right)
})
