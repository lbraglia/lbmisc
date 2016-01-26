context("to_00_char")

test_that("to_00_char basic usage", {

    test <- as.integer(c(1e00, 1e01, 1e02, 1e03))
    res <- to_00_char(test, clen = 5)
    right <- c('00001', '00010', '00100', '01000') 
    expect_identical(res, right)
    
})

test_that("to_00_char accepts only integers", {

    expect_warning(to_00_char(c(1e00, 1e01, 1e02, 1e03)))
        
})

