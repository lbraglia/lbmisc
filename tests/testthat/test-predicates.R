context("is.vector2")

test_that("is.vector2 basic cases", {

    expect_true(is.vector2(logical()))
    expect_true(is.vector2(integer()))
    expect_true(is.vector2(numeric()))
    expect_true(is.vector2(complex()))
    expect_true(is.vector2(character()))
    expect_true(is.vector2(raw()))

    expect_false(is.vector2(NULL))
    expect_false(is.vector2(matrix(1:10, 2)))
    expect_false(is.vector2(list()))
    expect_false(is.vector2(expression()))
    expect_false(is.vector2(pairlist()))
    expect_false(is.vector2(pairlist(1)))
    
})


context('is.quantitative')

test_that("is.quantitative basic cases", {

    expect_false(is.quantitative(logical(3)))
    expect_false(is.quantitative(complex(3)))
    expect_false(is.quantitative(character(3)))
    expect_false(is.quantitative(factor(letters)))
    expect_false(is.quantitative(raw(3)))
    expect_false(is.quantitative(NULL))
    ## integer and numeric percentages
    expect_false(is.quantitative(c(0:1, NA_integer_)))
    expect_false(is.quantitative(c(0, 1, NA)))
    ## quantitative variables (integer and numeric)
    expect_true(is.quantitative(c(0:2, NA_integer_)))
    expect_true(is.quantitative(c(0, 1, 2, NA)))
    
})


test_that("is.percentage basic cases", {

    expect_false(is.percentage(logical(3)))
    expect_false(is.percentage(complex(3)))
    expect_false(is.percentage(character(3)))
    expect_false(is.percentage(factor(letters)))
    expect_false(is.percentage(raw(3)))
    expect_false(is.percentage(NULL))
    ## integer and numeric percentages
    expect_true(is.percentage(c(0:1, NA_integer_)))
    expect_true(is.percentage(c(0, 1, NA)))
    ## quantitative variables (integer and numeric)
    expect_false(is.percentage(c(0:2, NA_integer_)))
    expect_false(is.percentage(c(0, 1, 2, NA)))
    
})

test_that("is.qualitative", {

    expect_false(is.qualitative(logical(3)))
    expect_false(is.qualitative(complex(3)))
    expect_false(is.qualitative(character(3)))
    expect_true(is.qualitative(factor(letters)))
    expect_false(is.qualitative(raw(3)))
    expect_false(is.qualitative(NULL))
    ## integer and numeric qualitatives
    expect_false(is.qualitative(c(0:1, NA_integer_)))
    expect_false(is.qualitative(c(0, 1, NA)))
    ## quantitative variables (integer and numeric)
    expect_false(is.qualitative(c(0:2, NA_integer_)))
    expect_false(is.qualitative(c(0, 1, 2, NA)))
    
})
