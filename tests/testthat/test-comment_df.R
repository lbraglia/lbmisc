context("comment_df")

## -----------
## Basic usage
## -----------

test_that("vector var_com input", {

    x <- data.frame("a" = 1:3, "b" = 4:6)

    a_comment <- 'Variable a'
    b_comment <- 'Variable b'
    
    var_com <- c('a', a_comment, 'b', b_comment)
    
    right <- {
        tmp <- x;
        comment(tmp$a) <- a_comment
        comment(tmp$b) <- b_comment
        tmp
    }
    
    expect_identical(comment_df(x = x, var_com = var_com), right)

})


test_that("matrix var_com input", {

    x <- data.frame("a" = 1:3, "b" = 4:6)

    a_comment <- 'Variable a'
    b_comment <- 'Variable b'
    
    var_com <- matrix(c('a', a_comment, 'b', b_comment),
                      ncol = 2, byrow = TRUE)
    
    right <- {
        tmp <- x;
        comment(tmp$a) <- a_comment
        comment(tmp$b) <- b_comment
        tmp
    }
    
    expect_identical(comment_df(x = x, var_com = var_com), right)

})
