context("rm_accents")

test_that("rm_accents basic usage", {

    test <- c('à', 'è', 'é', 'ì', 'ò', 'ù')
    right <- c('a', 'e', 'e', 'i', 'o', 'u')
    res <- rm_accents(test)
    expect_identical(res, right)
    
})

test_that("rm_accents removes in full strings", {

    test <- c('àcsè')
    right <- c('acse')
    res <- rm_accents(test)
    expect_identical(res, right)
    
})

