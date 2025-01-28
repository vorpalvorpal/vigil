library(testthat)

# Tests for VBScript regex conversion
test_that("convert_regex_to_vbs handles standard patterns", {
  # Basic patterns
  expect_equal(convert_regex_to_vbs(".*"), ".*")
  expect_equal(convert_regex_to_vbs("[0-9]+"), "[0-9]+")
  expect_equal(convert_regex_to_vbs("abc|def"), "abc|def")

  # Character classes
  expect_equal(convert_regex_to_vbs("\\d+"), "[0-9]+")
  expect_equal(convert_regex_to_vbs("\\w+"), "[A-Za-z0-9_]+")
  expect_equal(convert_regex_to_vbs("\\s+"), "[ \\t\\n\\r]+")

  # Null input
  expect_equal(convert_regex_to_vbs(NULL), ".*")
})

test_that("convert_regex_to_vbs handles complex patterns", {
  # Capture groups
  expect_equal(
    convert_regex_to_vbs("(?<name>[A-Z]+)"),
    "([A-Z]+)"
  )

  # Multiple substitutions
  expect_equal(
    convert_regex_to_vbs("\\d+\\s+\\w+"),
    "[0-9]+[ \\t\\n\\r]+[A-Za-z0-9_]+"
  )

  # Groups and alternation
  expect_equal(
    convert_regex_to_vbs("(foo|bar)\\d+"),
    "(foo|bar)[0-9]+"
  )
})

test_that("convert_regex_to_vbs removes unsupported features", {
  # Lookahead
  expect_equal(
    convert_regex_to_vbs("abc(?=def)"),
    "abcdef"
  )

  # Lookbehind
  expect_equal(
    convert_regex_to_vbs("(?<=abc)def"),
    "def"
  )

  # Named capture group
  expect_equal(
    convert_regex_to_vbs("(?<year>\\d{4})-(?<month>\\d{2})"),
    "([0-9]{4})-([0-9]{2})"
  )
})

test_that("convert_regex_to_vbs validates input", {
  # Invalid regex
  expect_error(
    convert_regex_to_vbs("[invalid"),
    "Invalid regular expression pattern"
  )

  # Wrong type
  expect_error(
    convert_regex_to_vbs(123),
    "Must be of type 'character'"
  )

  # Unicode classes
  expect_error(
    convert_regex_to_vbs("\\p{L}+"),
    "Unicode character classes are not supported"
  )
})

# Tests for Unix shell pattern escaping
test_that("escape_shell_pattern handles special characters", {
  # Basic pattern
  expect_equal(escape_shell_pattern("[a-z]+"), "[a-z]+")

  # Pattern with single quotes
  expect_equal(
    escape_shell_pattern("file's*.txt"),
    "file'\\''s*.txt"
  )

  # Multiple single quotes
  expect_equal(
    escape_shell_pattern("'test'pattern'"),
    "'\\''test'\\''pattern'\\''")

  # NULL input
  expect_null(escape_shell_pattern(NULL))

  # Invalid input
  expect_error(
    escape_shell_pattern(123),
    "Must be of type 'character'"
  )
})

# Integration tests for script generation
test_that("create_windows_watcher_script properly handles patterns", {
  skip_on_unix()

  config <- list(
    id = "test",
    path = "C:/temp",
    pattern = "\\d+\\.txt",
    recursive = FALSE,
    watch_mode = "continuous"
  )

  script <- create_windows_watcher_script(config)

  # Check pattern conversion
  expect_match(
    script,
    "Pattern = \"[0-9]+\\.txt\"",
    fixed = TRUE
  )

  # Test with complex pattern
  config$pattern <- "(?<name>\\w+)_\\d+"
  script <- create_windows_watcher_script(config)

  expect_match(
    script,
    "Pattern = \"([A-Za-z0-9_]+)_[0-9]+\"",
    fixed = TRUE
  )
})

test_that("create_unix_watcher_script properly handles patterns", {
  skip_on_windows()

  config <- list(
    id = "test",
    path = "/tmp",
    pattern = "[a-z]+\\.txt",
    recursive = FALSE,
    watch_mode = "continuous"
  )

  script <- create_unix_watcher_script(config)

  # Check inotifywait pattern
  expect_match(
    script,
    "inotifywait -m --regexp '[a-z]+\\.txt'"
  )

  # Check fswatch pattern
  expect_match(
    script,
    "fswatch --regexp '[a-z]+\\.txt'"
  )

  # Test with pattern containing single quotes
  config$pattern <- "file's*.txt"
  script <- create_unix_watcher_script(config)

  expect_match(
    script,
    "inotifywait -m --regexp 'file'\\''s\\*\\.txt'"
  )
  expect_match(
    script,
    "fswatch --regexp 'file'\\''s\\*\\.txt'"
  )
})

test_that("script generation works without pattern", {
  # Windows
  skip_on_unix()
  config <- list(
    id = "test",
    path = "C:/temp",
    pattern = NULL,
    recursive = FALSE,
    watch_mode = "continuous"
  )

  script <- create_windows_watcher_script(config)
  expect_match(script, "Pattern = \".*\"", fixed = TRUE)

  # Unix
  skip_on_windows()
  config$path <- "/tmp"
  script <- create_unix_watcher_script(config)

  # Should not have --regexp flag
  expect_match(script, "inotifywait -m(?! --regexp)", perl = TRUE)
  expect_match(script, "fswatch(?! --regexp)", perl = TRUE)
})
