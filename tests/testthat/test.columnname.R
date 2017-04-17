require(sqltools)
context("enquote column names for sql query string")

test_that("MySQL is enquoted by ``", {
  expect_equal(my_columnnames(colnames(iris)), paste0("`", colnames(iris), "`"))
})

test_that("PostgreSQL is enquoted by \"\"", {
  expect_equal(pg_columnnames(colnames(iris)), paste0('"', colnames(iris), '"'))
})

test_that("MSSQL is enquoted by []", {
  expect_equal(ms_columnnames(colnames(iris)), paste0("[", colnames(iris), "]"))
})

