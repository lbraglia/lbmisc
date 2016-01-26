context("preprocess_varnames")

test_that("preprocess_varnames basic usage", {

    test <- c('  (Vàriable-name.from  *Hell*)\U001  ')
    right <- 'variable_name_from_per_hell_per'
    res <- preprocess_varnames(test)
    expect_identical(res, right)
    
})

test_that("preprocess_varnames trimming", {

    test <- c('  (Vàriable-name.from  *Hell*)\U001  ')
    right <- 'variable_name'
    res <- preprocess_varnames(test, trim = 13L)
    expect_identical(res, right)
    
})

