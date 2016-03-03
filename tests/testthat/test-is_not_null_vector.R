context("is.not_null_vector")

test_that("basic cases", {

    expect_that(is.not_null_vector(logical()), is_true())
    expect_that(is.not_null_vector(integer()), is_true())
    expect_that(is.not_null_vector(numeric()), is_true())
    expect_that(is.not_null_vector(complex()), is_true())
    expect_that(is.not_null_vector(character()), is_true())
    expect_that(is.not_null_vector(raw()), is_true())
    expect_that(is.not_null_vector(NULL), is_false())
    expect_that(is.not_null_vector(matrix(1:10, 2)), is_false())
    expect_that(is.not_null_vector(list()), is_false())
    expect_that(is.not_null_vector(expression()), is_false())
    expect_that(is.not_null_vector(pairlist()), is_false())
    expect_that(is.not_null_vector(pairlist(1)), is_false())
    
})

