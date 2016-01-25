context("NA_report")

test_that("NA_report basic usage", {
    data <- data.frame(name = c("john","mary","gregor"),
                       x    = c(     1,   NA,       NA),
                       y    = c(    NA,    1,        1),
                       z    = c(     1,    1,       NA))
    
    res <- NA_report(x = data, id_var = "name")
    right <- data.frame('name' = c("gregor", "gregor", "john", "mary"),
                        'variable' = c("x", "z", "y", "x"))
    expect_identical(res, right)                       
    
})

test_that("NA_report with 2 variables as id", {
    data <- data.frame(name    = c("john", "mary", "gregor"),
                       surname = c( "doe",  "foo",    "bar"),
                       x       = c(     1,   NA,         NA),
                       y       = c(    NA,    1,          1),
                       z       = c(     1,    1,         NA))
    res <- NA_report(x = data, id_var = c("name", "surname"))
    right <- data.frame('name' = c("gregor", "gregor", "john", "mary"),
                        'surname' = c("bar", "bar", "doe", "foo"),
                        'variable' = c("x", "z", "y", "x"))
    expect_identical(res, right)                       
    
})

