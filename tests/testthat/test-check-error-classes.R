
# Tests for the helpers  =======================================================

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
      "base_only_conditions",
      "implicit_condition_signals"
    )
  )

  expect_true(all(vapply(result, is.data.frame, logical(1L))))
  expect_true(all(vapply(result, nrow, integer(1L)) == 0L))
})


# Tests for remove_consecutive_duplicates  =====================================

test_that("consecutive duplicate classes are removed", {
  classes <- c(
    "SpecificError",
    "SpecificError",
    "PackageError",
    "error",
    "condition"
  )

  result <- remove_consecutive_duplicates(classes)

  expect_identical(
    result,
    c(
      "SpecificError",
      "PackageError",
      "error",
      "condition"
    )
  )
})


test_that("non-consecutive duplicate classes are retained", {
  classes <- c(
    "SpecificError",
    "PackageError",
    "SpecificError"
  )

  result <- remove_consecutive_duplicates(classes)

  expect_identical(result, classes)
})


test_that("duplicate removal handles short vectors", {
  expect_identical(
    remove_consecutive_duplicates(character()),
    character()
  )

  expect_identical(
    remove_consecutive_duplicates("SpecificError"),
    "SpecificError"
  )
})


# Tests for expand_condition_classes  ==========================================

test_that("subclass vectors receive the registered suffix", {
  suffixes <- list(
    package_error_condition = c(
      "PackageError",
      "error",
      "condition"
    )
  )

  result <- expand_condition_classes(
    classes = "InvalidArgumentError",
    argument_name = "subclass",
    call_name = "package_error_condition",
    subclass_suffixes = suffixes
  )

  expect_identical(
    result,
    c(
      "InvalidArgumentError",
      "PackageError",
      "error",
      "condition"
    )
  )
})


test_that("class arguments do not receive a subclass suffix", {
  suffixes <- list(
    package_error_condition = c(
      "PackageError",
      "error",
      "condition"
    )
  )

  result <- expand_condition_classes(
    classes = c("CustomCondition", "condition"),
    argument_name = "class",
    call_name = "package_error_condition",
    subclass_suffixes = suffixes
  )

  expect_identical(
    result,
    c("CustomCondition", "condition")
  )
})


test_that("duplicate classes at suffix boundary are removed", {
  suffixes <- list(
    package_error_condition = c(
      "PackageError",
      "error",
      "condition"
    )
  )

  result <- expand_condition_classes(
    classes = c(
      "InvalidArgumentError",
      "PackageError"
    ),
    argument_name = "subclass",
    call_name = "package_error_condition",
    subclass_suffixes = suffixes
  )

  expect_identical(
    result,
    c(
      "InvalidArgumentError",
      "PackageError",
      "error",
      "condition"
    )
  )
})


# Tests for make_occurrence_rows  ==============================================

test_that("one occurrence row is created per class", {
  result <- make_occurrence_rows(
    classes = c(
      "InvalidArgumentError",
      "PackageError",
      "error",
      "condition"
    ),
    argument_name = "subclass",
    call_name = "package_error_condition",
    file = "R/conditions.R",
    line = 42L
  )

  expect_identical(
    result$class,
    c(
      "InvalidArgumentError",
      "PackageError",
      "error",
      "condition"
    )
  )

  expect_identical(result$position, 1:4)
  expect_true(all(result$argument == "subclass"))
  expect_true(all(result$call == "package_error_condition"))
  expect_true(all(result$file == "R/conditions.R"))
  expect_true(all(result$line == 42L))
})


# Tests for make_edge_rows  ====================================================

test_that("adjacent classes are converted into hierarchy edges", {
  result <- make_edge_rows(
    classes = c(
      "InvalidArgumentError",
      "PackageError",
      "error",
      "condition"
    ),
    argument_name = "subclass",
    call_name = "package_error_condition",
    file = "R/conditions.R",
    line = 42L
  )

  expect_identical(
    result$child,
    c(
      "InvalidArgumentError",
      "PackageError",
      "error"
    )
  )

  expect_identical(
    result$parent,
    c(
      "PackageError",
      "error",
      "condition"
    )
  )

  expect_true(all(result$source == "subclass"))
  expect_true(all(result$line == 42L))
})


test_that("one class produces no hierarchy edges", {
  result <- make_edge_rows(
    classes = "condition",
    argument_name = "class",
    call_name = "structure",
    file = "R/conditions.R",
    line = 42L
  )

  expect_identical(result, empty_edges())
})


# Tests for find_inconsistent_parents  =========================================

test_that("empty edges have no inconsistent parents", {
  result <- find_inconsistent_parents(
    empty_edges()
  )

  expect_identical(
    result,
    empty_edges()
  )
})


test_that("a valid hierarchy has no inconsistent parents", {
  edges <- make_edge_rows(
    classes = c(
      "InvalidArgumentError",
      "PackageError",
      "error",
      "condition"
    ),
    argument_name = "subclass",
    call_name = "package_error_condition",
    file = "R/conditions.R",
    line = 10L
  )

  result <- find_inconsistent_parents(edges)

  expect_identical(
    result,
    empty_edges()
  )
})


test_that("a class with two distinct parents is inconsistent", {
  first_edge <- make_edge_rows(
    classes = c(
      "InvalidArgumentError",
      "PackageError"
    ),
    argument_name = "subclass",
    call_name = "package_error_condition",
    file = "R/first.R",
    line = 10L
  )

  second_edge <- make_edge_rows(
    classes = c(
      "InvalidArgumentError",
      "AlternativePackageError"
    ),
    argument_name = "subclass",
    call_name = "alternative_error_condition",
    file = "R/second.R",
    line = 20L
  )

  edges <- rbind(
    first_edge,
    second_edge
  )

  result <- find_inconsistent_parents(edges)

  expect_identical(
    result$child,
    c(
      "InvalidArgumentError",
      "InvalidArgumentError"
    )
  )

  expect_identical(
    result$parent,
    c(
      "PackageError",
      "AlternativePackageError"
    )
  )
})


test_that("repeated identical relationships are not inconsistent", {
  first_edge <- make_edge_rows(
    classes = c(
      "InvalidArgumentError",
      "PackageError"
    ),
    argument_name = "subclass",
    call_name = "package_error_condition",
    file = "R/first.R",
    line = 10L
  )

  second_edge <- first_edge
  second_edge$file <- "R/second.R"
  second_edge$line <- 20L

  edges <- rbind(
    first_edge,
    second_edge
  )

  result <- find_inconsistent_parents(edges)

  expect_identical(
    result,
    empty_edges()
  )
})


# Tests for parse_source_file  =================================================

test_that("a valid R source file is parsed", {
  source_file <- tempfile(fileext = ".R")

  writeLines(
    c(
      "add <- function(x, y) {",
      "  x + y",
      "}"
    ),
    source_file
  )

  result <- parse_source_file(source_file)

  expect_type(result, "list")

  expect_identical(
    names(result),
    c("parsed_file", "parse_errors")
  )

  expect_true(is.expression(result$parsed_file))

  expect_identical(
    result$parse_errors,
    empty_parse_errors()
  )
})


test_that("a syntax error is returned as a parse-error row", {
  source_file <- tempfile(fileext = ".R")

  writeLines(
    "broken_function <- function(",
    source_file
  )

  result <- parse_source_file(source_file)

  expect_null(result$parsed_file)
  expect_s3_class(result$parse_errors, "data.frame")
  expect_equal(nrow(result$parse_errors), 1L)

  expect_identical(
    result$parse_errors$file,
    source_file
  )

  expect_type(
    result$parse_errors$message,
    "character"
  )

  expect_true(
    nzchar(result$parse_errors$message)
  )
})


test_that("an empty R source file parses successfully", {
  source_file <- tempfile(fileext = ".R")
  file.create(source_file)

  result <- parse_source_file(source_file)

  expect_true(is.expression(result$parsed_file))
  expect_length(result$parsed_file, 0L)

  expect_identical(
    result$parse_errors,
    empty_parse_errors()
  )
})


# Tests for new_call_location_cursor  ==========================================

test_that("call locations are consumed in order", {
  locations <- data.frame(call = c("package_error_condition",
                                   "package_error_condition"),
                          line = c(10L, 20L),
                          column = c(3L, 5L)
  )

  cursor <- new_call_location_cursor(locations)

  first <- cursor$consume("package_error_condition")

  second <- cursor$consume("package_error_condition")

  expect_identical(first, list(line = 10L, column = 3L) )

  expect_identical(second, list(line = 20L, column = 5L) )
})


test_that("call positions are maintained separately", {
  locations <- data.frame(
    call = c("error_constructor",
             "warning_constructor",
             "error_constructor",
             "warning_constructor"
    ),
    line = c(10L, 20L, 30L, 40L),
    column = c(1L, 2L, 3L, 4L)
  )

  cursor <- new_call_location_cursor(locations)

  first_error <- cursor$consume("error_constructor")

  first_warning <- cursor$consume("warning_constructor")

  second_error <- cursor$consume("error_constructor")

  second_warning <- cursor$consume("warning_constructor")

  expect_identical(first_error$line, 10L)
  expect_identical(first_warning$line, 20L)
  expect_identical(second_error$line, 30L)
  expect_identical(second_warning$line, 40L)
})


test_that("an exhausted cursor returns a missing location", {
  locations <- data.frame(call = "error_constructor", line = 10L, column = 3L)

  cursor <- new_call_location_cursor(locations)

  cursor$consume("error_constructor")

  result <- cursor$consume("error_constructor")

  expect_identical(result, list(line = NA_integer_, column = NA_integer_) )
})

test_that("an unknown call has no source location", {
  locations <- data.frame(call = "error_constructor", line = 10L, column = 3L)

  cursor <- new_call_location_cursor(locations)

  result <- cursor$consume("unknown_constructor")

  expect_identical(result, list(line = NA_integer_, column = NA_integer_) )
})

test_that("an empty cursor returns missing locations", {
  locations <- data.frame(call = character(),
                          line = integer(),
                          column = integer() )

  cursor <- new_call_location_cursor(locations)

  result <- cursor$consume("error_constructor")

  expect_identical(result, list(line = NA_integer_, column = NA_integer_) )
})

test_that("call location cursors have independent state", {
  locations <- data.frame(call = c("error_constructor", "error_constructor"),
                          line = c(10L, 20L),
                          column = c(1L, 2L)
  )

  first_cursor <- new_call_location_cursor(locations)

  second_cursor <- new_call_location_cursor(locations)

  first_cursor$consume("error_constructor")
  first_cursor$consume("error_constructor")

  result <- second_cursor$consume("error_constructor")

  expect_identical(result$line, 10L)
})


# Tests for get_call_locations  ================================================

test_that("call locations include constructors and condition signalers", {
  source_file <- tempfile(fileext = ".R")

  writeLines(c("example <- function(x) {",
               "  package_error_condition(message = \"first\")",
               "  stop(\"second\")",
               "  warning(\"third\")",
               "}"
  ), source_file)

  parsed <- parse_source_file(source_file)

  result <- get_call_locations(
    parsed_file = parsed$parsed_file,
    call_names = c("package_error_condition", "stop", "warning") )

  expect_identical(result$call,
    c("package_error_condition", "stop", "warning")
  )

  expect_identical(result$line, c(2L, 3L, 4L) )

  expect_identical(result$column, c(3L, 3L, 3L) )
})


test_that("call locations exclude unrequested functions", {
  source_file <- tempfile(fileext = ".R")

  writeLines(c("example <- function(x) {",
               "  stop(\"problem\")",
               "  print(x)",
               "  warning(\"warning\")",
               "}"
  ), source_file)

  parsed <- parse_source_file(source_file)

  result <- get_call_locations(parsed_file = parsed$parsed_file,
                               call_names = c("stop", "warning") )

  expect_identical(result$call, c("stop", "warning") )

  expect_false("print" %in% result$call)
})


# Tests for analyze_condition_argument  ========================================


test_that("a static class vector produces occurrences and edges", {
  argument <- quote(c("SpecificError", "PackageError", "error", "condition") )

  result <- analyze_condition_argument(
    argument = argument,
    argument_name = "class",
    call_name = "structure",
    file = "R/conditions.R",
    line = 10L,
    subclass_suffixes = NULL)

  expect_identical(
    result$occurrences$class,
    c("SpecificError", "PackageError", "error", "condition") )

  expect_identical(result$edges$child,
                   c("SpecificError", "PackageError", "error") )

  expect_identical(result$edges$parent,
                   c("PackageError", "error", "condition") )

  expect_null(result$dynamic_definitions)
})

test_that("a static subclass receives its constructor suffix", {
  argument <- quote("SpecificError")

  suffixes <- list(package_error_condition =
                     c("PackageError", "error", "condition") )

  result <- analyze_condition_argument(
    argument = argument,
    argument_name = "subclass",
    call_name = "package_error_condition",
    file = "R/conditions.R",
    line = 10L,
    subclass_suffixes = suffixes
  )

  expect_identical(result$occurrences$class,
                   c("SpecificError", "PackageError", "error", "condition") )

  expect_identical(result$edges$child,
                   c("SpecificError", "PackageError", "error") )

  expect_identical(result$edges$parent,
    c("PackageError", "error", "condition") )

  expect_null(result$dynamic_definitions)
})

test_that("a dynamic condition argument is recorded", {
  argument <- quote(subclass_name)

  result <- analyze_condition_argument(
    argument = argument,
    argument_name = "subclass",
    call_name = "package_error_condition",
    file = "R/conditions.R",
    line = 25L,
    subclass_suffixes = NULL
  )

  expect_null(result$occurrences)

  expect_null(result$edges)

  expect_equal(nrow(result$dynamic_definitions), 1L)

  expect_identical(
    result$dynamic_definitions$argument,
    "subclass"
  )

  expect_identical(result$dynamic_definitions$call, "package_error_condition")

  expect_identical(result$dynamic_definitions$file, "R/conditions.R")

  expect_identical(result$dynamic_definitions$line, 25L)

  expect_identical(result$dynamic_definitions$expression, "subclass_name")
})


test_that("a partially dynamic class vector is recorded as dynamic", {
  argument <- quote(c("SpecificError", computed_class) )

  result <- analyze_condition_argument(
    argument = argument,
    argument_name = "subclass",
    call_name = "package_error_condition",
    file = "R/conditions.R",
    line = 30L,
    subclass_suffixes = NULL
  )

  expect_null(result$occurrences)
  expect_null(result$edges)

  expect_s3_class(result$dynamic_definitions, "data.frame")

  expect_equal(nrow(result$dynamic_definitions), 1L)

  expect_match(result$dynamic_definitions$expression,
               "c(\"SpecificError\", computed_class)",
               fixed = TRUE
  )
})

test_that("one static class produces no hierarchy edge", {
  argument <- quote("condition")

  result <- analyze_condition_argument(
    argument = argument,
    argument_name = "class",
    call_name = "structure",
    file = "R/conditions.R",
    line = 10L,
    subclass_suffixes = NULL
  )

  expect_s3_class(result$occurrences,"data.frame")

  expect_equal(nrow(result$occurrences), 1L)

  expect_identical(result$occurrences$class,"condition")

  expect_identical(result$occurrences$position,1L)

  expect_null(result$edges)
  expect_null(result$dynamic_definitions)
})


# Tests for classify_condition_signal  =========================================


test_that("a character message is an implicit condition signal", {
  result <- classify_condition_signal(
    expr = quote(stop("problem")),
    subclass_suffixes = NULL,
    control_arguments = "call."
  )

  expect_identical(result$status, "implicit")
  expect_identical(result$expression, "\"problem\"")
  expect_identical(result$reason, "character message supplied directly")
})


test_that("a message-building call is an implicit condition signal", {
  result <- classify_condition_signal(
    expr = quote(stop(paste0("problem at ", i))),
    subclass_suffixes = NULL,
    control_arguments = "call."
  )

  expect_identical(result$status, "implicit")
  expect_identical(result$expression, "paste0(\"problem at \", i)")
  expect_identical(result$reason, "message-building expression supplied directly")
})


test_that("multiple message components form an implicit condition signal", {
  result <- classify_condition_signal(
    expr = quote(stop("problem at ", i)),
    subclass_suffixes = NULL,
    control_arguments = "call."
  )

  expect_identical(result$status, "implicit")
  expect_identical(result$expression, "\"problem at \", i")
  expect_identical(result$reason, "multiple message components supplied directly")
})

test_that("condition signal control arguments are not message components", {
  result <- classify_condition_signal(
    expr = quote(stop("problem", call. = FALSE)),
    subclass_suffixes = NULL,
    control_arguments = "call."
  )

  expect_identical(result$status, "implicit")
  expect_identical(result$expression, "\"problem\"")
  expect_identical(result$reason, "character message supplied directly")
})

test_that("a registered constructor forms an explicit condition signal", {
  suffixes <- list(
    package_error_condition = c("PackageError", "error", "condition") )

  result <- classify_condition_signal(
    expr = quote(
      stop(
        package_error_condition(message = "problem", subclass = "SpecificError")
      )
    ),
    subclass_suffixes = suffixes,
    control_arguments = "call."
  )

  expect_identical(result$status, "explicit")
  expect_identical(result$reason, "registered condition constructor")
})

test_that("a symbol forms an unknown condition signal", {
  result <- classify_condition_signal(
    expr = quote(stop(condition_object)),
    subclass_suffixes = NULL,
    control_arguments = "call."
  )

  expect_identical(result$status, "unknown")
  expect_identical(result$expression, "condition_object")
  expect_identical(result$reason, "expression cannot be resolved statically")
})

test_that("an unregistered call forms an unknown condition signal", {
  result <- classify_condition_signal(
    expr = quote(stop(make_error("problem"))),
    subclass_suffixes = NULL,
    control_arguments = "call."
  )

  expect_identical(result$status, "unknown")
  expect_identical(result$expression, "make_error(\"problem\")")
})



# Tests for inspect_condition_expression  ======================================

test_that("condition expression inspection records static classes", {
  expr <- quote(
    structure(list(message = "Problem"),
              class = c("SpecificError", "PackageError", "error", "condition"))
  )

  accumulator <- new_condition_analysis_accumulator()

  location_cursor <- new_call_location_cursor(
    data.frame(call = character(),
               line = integer(),
               column = integer())
  )

  inspect_condition_expression(expr = expr,
                               file = "R/conditions.R",
                               argument_names = c("class", "subclass"),
                               subclass_suffixes = NULL,
                               location_cursor = location_cursor,
                               accumulator = accumulator)

  expect_length(accumulator$occurrence_rows, 1L)

  expect_identical(accumulator$occurrence_rows[[1L]]$class,
                   c("SpecificError", "PackageError", "error", "condition") )

  expect_length(accumulator$edge_rows, 1L)

  expect_length(accumulator$dynamic_rows, 0L)
})


test_that("irrelevant expressions produce no findings", {
  expr <- quote(length(x))

  accumulator <- new_condition_analysis_accumulator()

  location_cursor <- new_call_location_cursor(
    data.frame(call = character(),
               line = integer(),
               column = integer())
  )

  inspect_condition_expression(expr = expr,
                               file = "R/example.R",
                               argument_names = c("class", "subclass"),
                               subclass_suffixes = NULL,
                               location_cursor = location_cursor,
                               accumulator = accumulator)

  expect_length(accumulator$occurrence_rows, 0L)
  expect_length(accumulator$edge_rows, 0L)
  expect_length(accumulator$dynamic_rows, 0L)
  expect_length(accumulator$base_only_condition_rows, 0L)
  expect_length(accumulator$implicit_condition_signal_rows, 0L)
})

test_that("condition expression inspection records dynamic classes", {
  expr <- quote(structure(list(message = "Problem"), class = computed_classes))

  accumulator <- new_condition_analysis_accumulator()

  location_cursor <- new_call_location_cursor(
    data.frame(call = character(),
               line = integer(),
               column = integer() ) )

  inspect_condition_expression(expr = expr,
                               file = "R/conditions.R",
                               argument_names = "class",
                               subclass_suffixes = NULL,
                               location_cursor = location_cursor,
                               accumulator = accumulator
  )

  expect_length(accumulator$occurrence_rows, 0L)
  expect_length(accumulator$edge_rows, 0L)
  expect_length(accumulator$dynamic_rows, 1L)

  expect_identical(accumulator$dynamic_rows[[1L]]$expression, "computed_classes")
})

test_that("registered constructor without subclass is recorded", {
  expr <- quote(package_error_condition(message = "Problem"))

  suffixes <- list(
    package_error_condition = c("PackageError", "error", "condition") )

  locations <- data.frame(call = "package_error_condition",
                          line = 42L,
                          column = 5L)

  accumulator <- new_condition_analysis_accumulator()
  location_cursor <- new_call_location_cursor(locations)

  inspect_condition_expression(expr = expr,
                               file = "R/conditions.R",
                               argument_names = c("class", "subclass"),
                               subclass_suffixes = suffixes,
                               location_cursor = location_cursor,
                               accumulator = accumulator)

  expect_length(accumulator$base_only_condition_rows, 1L)

  result <- accumulator$base_only_condition_rows[[1L]]

  expect_identical(result$condition_type, "error")

  expect_identical(result$base_class, "PackageError")

  expect_identical(result$line, 42L)
  expect_identical(result$column, 5L)

  expect_identical(result$reason, "subclass argument is absent")
})

test_that("non-call expressions produce no findings", {
  accumulator <- new_condition_analysis_accumulator()

  location_cursor <- new_call_location_cursor(
    data.frame(call = character(),
               line = integer(),
               column = integer())
  )

  result <- inspect_condition_expression(
    expr = quote(x),
    file = "R/example.R",
    argument_names = c("class", "subclass"),
    subclass_suffixes = NULL,
    location_cursor = location_cursor,
    accumulator = accumulator
  )

  expect_null(result)
  expect_length(accumulator$occurrence_rows, 0L)
  expect_length(accumulator$edge_rows, 0L)
  expect_length(accumulator$dynamic_rows, 0L)
})

test_that("implicit stop calls are recorded", {
  expr <- quote(stop(paste0("problem at ", i)))

  locations <- data.frame(call = "stop", line = 42L, column = 5L)

  accumulator <- new_condition_analysis_accumulator()
  location_cursor <- new_call_location_cursor(locations)

  inspect_condition_expression(
    expr = expr,
    file = "R/example.R",
    argument_names = c("class", "subclass"),
    subclass_suffixes = NULL,
    location_cursor = location_cursor,
    accumulator = accumulator
  )

  expect_length(accumulator$implicit_condition_signal_rows, 1L)

  result <- accumulator$implicit_condition_signal_rows[[1L]]

  expect_identical(result$condition_type, "error")
  expect_identical(result$implicit_class, "simpleError")
  expect_identical(result$call, "stop")
  expect_identical(result$line, 42L)
  expect_identical(result$column, 5L)
  expect_identical(result$expression, "paste0(\"problem at \", i)")
})

test_that("implicit warning calls are recorded", {
  expr <- quote(warning("problem"))

  locations <- data.frame(call = "warning", line = 12L, column = 3L)

  accumulator <- new_condition_analysis_accumulator()
  location_cursor <- new_call_location_cursor(locations)

  inspect_condition_expression(
    expr = expr,
    file = "R/example.R",
    argument_names = c("class", "subclass"),
    subclass_suffixes = NULL,
    location_cursor = location_cursor,
    accumulator = accumulator
  )

  result <- accumulator$implicit_condition_signal_rows[[1L]]

  expect_identical(result$condition_type, "warning")
  expect_identical(result$implicit_class, "simpleWarning")
  expect_identical(result$call, "warning")
})

test_that("unknown condition signals are not reported as implicit", {
  expr <- quote(stop(condition_object))

  locations <- data.frame(
    call = "stop",
    line = 12L,
    column = 3L
  )

  accumulator <- new_condition_analysis_accumulator()
  location_cursor <- new_call_location_cursor(locations)

  inspect_condition_expression(
    expr = expr,
    file = "R/example.R",
    argument_names = c("class", "subclass"),
    subclass_suffixes = NULL,
    location_cursor = location_cursor,
    accumulator = accumulator
  )

  expect_length(accumulator$implicit_condition_signal_rows, 0L)
})

test_that("explicit condition signals are not reported as implicit", {
  expr <- quote(
    stop(
      package_error_condition(
        message = "problem",
        subclass = "SpecificError"
      )
    )
  )

  suffixes <- list(
    package_error_condition = c("PackageError", "error", "condition")
  )

  locations <- data.frame(
    call = c("stop", "package_error_condition"),
    line = c(12L, 13L),
    column = c(3L, 5L)
  )

  accumulator <- new_condition_analysis_accumulator()
  location_cursor <- new_call_location_cursor(locations)

  inspect_condition_expression(
    expr = expr,
    file = "R/example.R",
    argument_names = c("class", "subclass"),
    subclass_suffixes = suffixes,
    location_cursor = location_cursor,
    accumulator = accumulator
  )

  expect_length(accumulator$implicit_condition_signal_rows, 0L)
})




# Test on  extract_condition_hierarchy  ========================================

test_that("extraction returns an empty implicit condition signal table", {
  package_path <- tempfile()
  source_path <- file.path(package_path, "R")

  dir.create(source_path, recursive = TRUE)
  writeLines(
    c(
      "add <- function(x, y) {",
      "  x + y",
      "}"
    ),
    file.path(source_path, "example.R")
  )

  result <- extract_condition_hierarchy(
    package_path = package_path
  )

  expect_s3_class(result, "condition_hierarchy")
  expect_true("implicit_condition_signals" %in% names(result))
  expect_identical(
    result$implicit_condition_signals,
    empty_implicit_condition_signals()
  )
})

test_that("extraction reports implicit condition signals", {
  package_path <- tempfile()
  source_path <- file.path(package_path, "R")

  dir.create(source_path, recursive = TRUE)

  writeLines(
    c(
      "example <- function(i, condition_object) {",
      "  stop(\"first problem\")",
      "  warning(paste0(\"problem at \", i))",
      "  stop(condition_object)",
      "}"
    ),
    file.path(source_path, "example.R")
  )

  result <- extract_condition_hierarchy(package_path = package_path)

  expect_equal(nrow(result$implicit_condition_signals), 2L)

  expect_identical(result$implicit_condition_signals$condition_type,
                   c("error", "warning") )

  expect_identical(result$implicit_condition_signals$implicit_class,
                   c("simpleError", "simpleWarning") )

  expect_identical(result$implicit_condition_signals$line, c(2L, 3L))
})


