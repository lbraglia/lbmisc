context("group_prog_id")

test_that("group_prog_id does what is expected", {

    expect_identical(group_prog_id(1:6), rep(1L, 6))
    expect_identical(group_prog_id(c('a', 'b', 'c', 'a', 'b', 'c')),
                     c(rep(1L, 3), rep(2L, 3)))
    expect_identical(group_prog_id(c(1:5, NA)), c(rep(1L, 5), NA_integer_))
    
})

