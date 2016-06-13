context("pretty_pval")

test_that("pretty_pval normal usage", {

    pval <- c(1e-01, 1e-02, 1e-03, 1e-04)
    right <- c("= 0.100", "= 0.010", "= 0.001", "< 0.001")
    res <- pretty_pval(pval, space = TRUE, equal = TRUE)
    expect_identical(res, right)
        
    
})

test_that("pretty_pval strange input values: NA", {
    pval <- c(NA)
    right <- c(NA_character_)
    res <- pretty_pval(pval)
    expect_identical(res, right)
    
})


test_that("pretty_pval strange input values: out of range [0, 1]", {

    ## there must be a warning
    pval <- c(-0.01, 1.01)
    expect_warning(pretty_pval(pval))

    ## the function returns NAs
    right <- rep(NA_character_, 2)
    suppressWarnings(res <- pretty_pval(pval))
    expect_identical(res, right)
        
})


