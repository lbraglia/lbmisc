context("NA_remove")

test_that("NA_remove send messages only if deletes rows in data.frame", {

    data1 <- data.frame(a = letters[1:3], b = c(1, 2, 3))
    data2 <- data.frame(a = letters[1:3], b = c(1, 2, NA))

    expect_silent(NA_remove(data1))
    expect_message(NA_remove(data2))
    
})
