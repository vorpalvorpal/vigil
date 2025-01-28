library(testthat)
library(vigil)

# Create temporary script for testing
setup_test_env <- function() {
  tmp_dir <- tempdir()
  script_path <- file.path(tmp_dir, "test_callback.R")
  writeLines("x <- 1", script_path)
  return(list(
    tmp_dir = tmp_dir,
    script_path = script_path
  ))
}

test_that("validate_callback handles NULL correctly", {
  result <- validate_callback(NULL)
  expect_equal(result$type, "none")
  expect_null(result$value)
})

test_that("validate_callback accepts functions", {
  fn <- function(x) x + 1
  result <- validate_callback(fn)
  expect_equal(result$type, "expression")
  expect_equal(result$value, fn)
})

test_that("validate_callback accepts package functions", {
  result <- validate_callback("utils::head")
  expect_equal(result$type, "package")
  expect_equal(result$value, "utils::head")
})

test_that("validate_callback rejects invalid package functions", {
  expect_error(
    validate_callback("nonexistent::function"),
    "Invalid callback specification"
  )
})

test_that("validate_callback accepts R scripts", {
  env <- setup_test_env()
  result <- validate_callback(env$script_path)
  expect_equal(result$type, "script")
  expect_equal(result$value, fs::path_abs(env$script_path))
})

test_that("validate_callback rejects non-existent scripts", {
  expect_error(
    validate_callback("nonexistent.R"),
    "Invalid callback specification"
  )
})

test_that("validate_callback rejects invalid inputs", {
  expect_error(validate_callback(42))
  expect_error(validate_callback(list()))
  expect_error(validate_callback(matrix()))
})

test_that("null coalescing operator works correctly", {
  expect_equal(NULL %||% 1, 1)
  expect_equal(2 %||% 1, 2)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(FALSE %||% TRUE, FALSE)
  expect_equal(NA %||% 1, NA)
})
