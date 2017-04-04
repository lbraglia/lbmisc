context("compare_columns")

test_that("compare columns: test with < and numerics", {
    
    data <- data.frame(
        id = c('x', 'y', 'z'),
        a = c(1,2,3),
        b = c(0,1,4)
    )
    
    res <- compare_columns(db = data[, -1],
                           operator = '<',
                           row_id = data$id)
    
    right <- structure(list(id = c("x", "y"),
                            issue = c("'a' vs 'b'", "'a' vs 'b'")),
                       .Names = c("id", "issue"),
                       row.names = c(NA, -2L),
                       class = "data.frame")

    expect_identical(res, right)
    
})

test_that("compare columns: test with > and numerics", {

    data <- data.frame(
        id = c('x', 'y', 'z'),
        a = c(1,2,3),
        b = c(0,1,4)
    )
    
    res <- compare_columns(db = data[, -1],
                           operator = '>',
                           row_id = data$id)

    right <- structure(list(id = "z", issue = "'a' vs 'b'"),
                       .Names = c("id", "issue"),
                       row.names = c(NA, -1L), class = "data.frame")

    expect_identical(res, right)
    
})

