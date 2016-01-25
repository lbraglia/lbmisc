context("compare_columns")

test_that("compare columns: test with < and numerics", {

    data <- data.frame(a = c(1,2,3), b = c(0,1,4))
    id <- letters[1:3]
    res <- compare_columns(db = data, operator = '<', row_id = id)
    right <- list('a' = "a.vs.b", 'b' = "a.vs.b")
    expect_identical(res, right)
    
})

test_that("compare columns: test with > and numerics", {

    data <- data.frame(a = c(1,2,3), b = c(0,1,4))
    id <- letters[1:3]
    res <- compare_columns(db = data, operator = '>', row_id = id)
    right <- list('c' = 'a.vs.b')
    expect_identical(res, right)
    
})

