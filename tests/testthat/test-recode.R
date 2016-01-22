context("Recode")

## -------------
## Numeric input
## -------------

test_that("recode is ok with right from_to (numeric)", {

    test <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA)
    recode_m <- matrix(c( 1, 3,
                          2, 4,
                         NA, 1), ncol = 2, byrow = TRUE)
    right <- c(3, 4, 3, 4, 5, 6, 7, 8, 9, 10, 1)
    expect_equal(recode(test, recode_m), right)

})

test_that("recode raise errors when needed (with numbers)", {

    test <- c(1:10, NA)
    recode_m <- matrix(c(1, 5,
                          2, 6,
                          4, 7,
                          4, 8), ncol = 2, byrow = TRUE)
    expect_error(recode(test, recode_m))
})

test_that("recode works fine with vector recode directives", {

    test <-  c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, NA)
    right <- c(3, 4, 3, 4, 5, 6, 7, 8, 9, 10,  1)

    expect_equal(recode(test, c( 1, 3,
                                 2, 4,                        
                                NA, 1)),
                        right)
})


## ---------------
## character input
## ---------------

test_that("recode is ok with right from_to (character)", {

    test <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", NA)
    recode_m <- matrix(c('a', NA,
                         'b', 'B',
                         'c', 'C'), ncol = 2, byrow = TRUE)
    right <- c(NA, "B", "C", "d", "e", "f", "g", "h", "i", "j", NA)
    expect_identical(recode(test, recode_m), right)

})
