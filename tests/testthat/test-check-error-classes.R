test_that("empty dynamic definitions have the expected structure", {
  result <- empty_dynamic_definitions()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)

  expect_identical(
    names(result),
    c(
      "argument",
      "call",
      "file",
      "line",
      "expression"
    )
  )

  expect_type(result$argument, "character")
  expect_type(result$call, "character")
  expect_type(result$file, "character")
  expect_type(result$line, "integer")
  expect_type(result$expression, "character")
})


test_that("empty parse errors have the expected structure", {
  result <- empty_parse_errors()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)

  expect_identical(
    names(result),
    c("file", "message")
  )

  expect_type(result$file, "character")
  expect_type(result$message, "character")
})


test_that("empty base-only conditions have the expected structure", {
  result <- empty_base_only_conditions()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0L)

  expect_identical(
    names(result),
    c(
      "condition_type",
      "base_class",
      "call",
      "file",
      "line",
      "column",
      "reason"
    )
  )

  expect_type(result$condition_type, "character")
  expect_type(result$base_class, "character")
  expect_type(result$call, "character")
  expect_type(result$file, "character")
  expect_type(result$line, "integer")
  expect_type(result$column, "integer")
  expect_type(result$reason, "character")
})


test_that("empty file analysis contains all expected result tables", {
  result <- empty_file_analysis()

  expect_identical(
    names(result),
    c(
      "occurrences",
      "edges",
      "dynamic_definitions",
      "parse_errors",
      "base_only_conditions"
    )
  )

  expect_true(all(vapply(result, is.data.frame, logical(1L))))
  expect_true(all(vapply(result, nrow, integer(1L)) == 0L))
})

