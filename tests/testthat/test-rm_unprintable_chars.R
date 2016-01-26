context("rm_unprintable_chars")

test_that("rm_unprintable_chars basic tests", {

    test <- c('\001')
    res <- rm_unprintable_chars(test)
    right <- c('')
    expect_identical(res, right)
    
})

