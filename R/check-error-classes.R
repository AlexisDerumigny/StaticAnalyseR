# Static analysis utilities for custom R condition classes.
#
# This file parses the R source files of a package without executing them.
# It extracts condition class vectors, reconstructs inheritance edges,
# detects base-only conditions and inconsistent parents, and formats the
# resulting hierarchy as text or as a tree.


# Combine a list of data frames by row.
#
# Returns `empty_result` when the input list is empty and removes row names
# from the combined result.
bind_rows <- function(rows, empty_result = NULL) {
  if (length(rows) == 0L) {
    return(empty_result)
  }

  result = do.call(rbind, rows)
  row.names(result) <- NULL

  return (result)
}


# Extract the function name from a call expression.
#
# Handles both ordinary calls, such as `stop()`, and namespace-qualified
# calls, such as `rlang::abort()`. Returns `NA_character_` for non-calls
# or unsupported call heads.
get_call_name <- function(expr) {
  if (!is.call(expr)) {
    return(NA_character_)
  }

  call_head <- expr[[1L]]

  if (is.symbol(call_head)) {
    return(as.character(call_head))
  }

  # Handle namespace-qualified calls such as rlang::abort().
  if (
    is.call(call_head) &&
    length(call_head) == 3L &&
    as.character(call_head[[1L]]) %in% c("::", ":::")
  ) {
    return(paste0(
      as.character(call_head[[2L]]),
      as.character(call_head[[1L]]),
      as.character(call_head[[3L]])
    ))
  }

  return (NA_character_)
}


# Extract statically declared character strings from an expression.
#
# Supports individual strings and calls to `c()` containing only character
# literals. Returns `character()` when the expression is dynamic or cannot
# be resolved safely without evaluating code.
#
extract_character_literals <- function(expr) {
  if (is.character(expr)) {
    return(as.character(expr))
  }

  if (
    is.call(expr) &&
    identical(expr[[1L]], as.name("c"))
  ) {
    values <- lapply(
      as.list(expr)[-1L],
      extract_character_literals
    )

    # Reject partially dynamic vectors.
    if (any(lengths(values) == 0L)) {
      return(character())
    }

    return(unlist(values, use.names = FALSE))
  }

  return (character())
}


# Retrieve the source line attached to a parsed expression.
#
# Returns the first line recorded in the expression's `srcref` attribute,
# or `NA_integer_` when no source reference is available.
get_expression_line <- function(expr) {
  source_reference <- attr(expr, "srcref")

  if (is.null(source_reference)) {
    return(NA_integer_)
  }

  as.integer(source_reference[[1L]])
}


is_missing_argument <- function(x) {
  missing(x)
}


# Extract a named argument from a call expression.
#
# Returns the unevaluated argument expression, or `NULL` when the input is
# not a call, the argument is absent, or the argument is syntactically
# missing.
get_named_argument <- function(expr, argument_name) {
  if (!is.call(expr)) {
    return(NULL)
  }

  arguments <- as.list(expr)[-1L]
  argument_names <- names(arguments)

  if (is.null(argument_names)) {
    return(NULL)
  }

  position <- which(argument_names == argument_name)

  if (length(position) == 0L) {
    return(NULL)
  }

  position <- position[[1L]]

  if (is_missing_argument(arguments[[position]])) {
    return(NULL)
  }

  arguments[[position]]
}


# Determine whether a constructor call has no explicit subclass.
#
# Returns a descriptive character string when `subclass` is absent or
# syntactically missing. Returns `NULL` when a subclass argument is present.
classify_subclass_argument <- function(expr) {
  subclass <- get_named_argument(
    expr,
    argument_name = "subclass"
  )

  # The argument is absent, or is a syntactically missing argument:
  #
  #   constructor(message = "...")
  #   constructor(message = "...", subclass = )
  if (is.null(subclass)) {
    return("subclass argument is absent")
  }

  # NULL means that the subclass is not demonstrably empty.
  return (NULL)
}


# Recursively traverse a parsed R expression.
#
# Calls `callback` on the current expression and all nested calls,
# expression vectors, and pairlists. Missing arguments are skipped safely.
# Additional arguments supplied through `...` are forwarded unchanged to the
# callback and to every recursive invocation.
walk_expression <- function(expr, callback, file, ...) {
  if (missing(expr)) {
    return(invisible(NULL))
  }

  callback(expr, file, ...)

  if (is.call(expr) || is.expression(expr) || is.pairlist(expr))
  {
    number_of_components <- length(expr)

    if (number_of_components > 0L) {
      for (i in seq_len(number_of_components)) {
        walk_expression(
          expr[[i]],
          callback = callback,
          file = file,
          ...
        )
      }
    }
  }

  invisible(NULL)
}


# Helpers for `extract_condition_hierarchy()`  =================================


## Empty-result constructors ===================================================


# Create an empty occurrence table with the expected column types.
#
# The returned data frame has the same structure as the occurrence table
# produced by `extract_condition_hierarchy()`.
empty_occurrences <- function() {
  data.frame(
    class = character(),
    position = integer(),
    argument = character(),
    call = character(),
    file = character(),
    line = integer(),
    stringsAsFactors = FALSE
  )
}


# Create an empty hierarchy-edge table with the expected column types.
#
# Each non-empty row of this table represents a direct child-parent
# relationship between two condition classes.
empty_edges <- function() {
  data.frame(
    child = character(),
    parent = character(),
    source = character(),
    call = character(),
    file = character(),
    line = integer(),
    stringsAsFactors = FALSE
  )
}

# Create an empty dynamic-definition table with the expected column types.
#
# Each non-empty row describes a class or subclass argument that could not be
# resolved statically without evaluating R code.
empty_dynamic_definitions <- function() {
  data.frame(
    argument = character(),
    call = character(),
    file = character(),
    line = integer(),
    expression = character(),
    stringsAsFactors = FALSE
  )
}


# Create an empty parse-error table with the expected column types.
#
# Each non-empty row describes a source file that could not be parsed and
# records the corresponding parser error message.
empty_parse_errors <- function() {
  data.frame(
    file = character(),
    message = character(),
    stringsAsFactors = FALSE
  )
}


# Create an empty base-only-condition table with the expected column types.
#
# Each non-empty row describes a registered condition constructor called
# without an explicit subclass.
empty_base_only_conditions <- function() {
  data.frame(
    condition_type = character(),
    base_class = character(),
    call = character(),
    file = character(),
    line = integer(),
    column = integer(),
    reason = character(),
    stringsAsFactors = FALSE
  )
}


# Create an empty file-analysis result.
#
# The returned list contains one consistently typed empty table for each kind
# of finding produced while analyzing a single source file.
empty_file_analysis <- function() {
  list(
    occurrences = empty_occurrences(),
    edges = empty_edges(),
    dynamic_definitions = empty_dynamic_definitions(),
    parse_errors = empty_parse_errors(),
    base_only_conditions = empty_base_only_conditions()
  )
}


## Row-building helpers  =======================================================


# Remove consecutive duplicate values from a vector.
#
# Non-consecutive duplicates are retained because they may describe separate
# positions in a declared condition hierarchy.
remove_consecutive_duplicates <- function(x) {
  if (length(x) <= 1L) {
    return(x)
  }

  keep <- c(
    TRUE,
    x[-1L] != x[-length(x)]
  )

  result = x[keep]
  return (result)
}

# Expand a statically extracted condition class vector.
#
# When the argument is `subclass` and the current call is a registered
# constructor, append the constructor's known fixed class suffix. Consecutive
# duplicates introduced at the boundary are then removed.
expand_condition_classes <- function(classes,
                                     argument_name,
                                     call_name,
                                     subclass_suffixes)
{
  full_classes <- classes

  if (identical(argument_name, "subclass") &&
      !is.null(subclass_suffixes) &&
      !is.na(call_name) &&
      call_name %in% names(subclass_suffixes)
  ) {
    matching_suffix <- subclass_suffixes[[call_name]]

    if (!is.null(matching_suffix)) {
      full_classes <- c(full_classes, matching_suffix)
    }
  }

  result = remove_consecutive_duplicates(full_classes)
  return (result)
}


# Create one occurrence row for each class in a condition class vector.
make_occurrence_rows <- function(classes,
                                 argument_name,
                                 call_name,
                                 file,
                                 line
) {
  if (length(classes) == 0L) {
    return(empty_occurrences())
  }

  result = data.frame(class = classes,
                      position = seq_along(classes),
                      argument = rep(argument_name, length(classes)),
                      call = rep(call_name, length(classes)),
                      file = rep(file, length(classes)),
                      line = rep(line, length(classes)),
                      stringsAsFactors = FALSE
  )

  return (result)
}


# Create direct child-parent edges from a condition class vector.
#
# For a class vector c("A", "B", "C"), the resulting edges are A -> B and
# B -> C.
make_edge_rows <- function(classes,
                           argument_name,
                           call_name,
                           file,
                           line
) {
  if (length(classes) < 2L) {
    return(empty_edges())
  }

  number_of_edges <- length(classes) - 1L
  positions <- seq_len(number_of_edges)

  result = data.frame(child = classes[positions],
                      parent = classes[positions + 1L],
                      source = rep(argument_name, number_of_edges),
                      call = rep(call_name, number_of_edges),
                      file = rep(file, number_of_edges),
                      line = rep(line, number_of_edges),
                      stringsAsFactors = FALSE
  )

  return (result)
}


# Analyze one class-related argument of a condition call.
#
# Returns a list containing occurrence, edge, or dynamic-definition rows. Result
# components without findings are NULL to avoid repeatedly constructing empty
# data frames during AST traversal.
analyze_condition_argument <- function(argument,
                                       argument_name,
                                       call_name,
                                       file,
                                       line,
                                       subclass_suffixes)
{
  classes <- extract_character_literals(argument)

  if (length(classes) == 0L) {
    return(list(
      occurrences = NULL,
      edges = NULL,
      dynamic_definitions = data.frame(
        argument = argument_name,
        call = call_name,
        file = file,
        line = line,
        expression = paste(deparse(argument), collapse = " ") )
    ) )
  }

  full_classes <- expand_condition_classes(
    classes = classes,
    argument_name = argument_name,
    call_name = call_name,
    subclass_suffixes = subclass_suffixes
  )

  result <- list(
    occurrences = make_occurrence_rows(
      classes = full_classes,
      argument_name = argument_name,
      call_name = call_name,
      file = file,
      line = line
    ),
    edges = if (length(full_classes) >= 2L) {
      make_edge_rows(
        classes = full_classes,
        argument_name = argument_name,
        call_name = call_name,
        file = file,
        line = line
      )
    } else {
      NULL
    },
    dynamic_definitions = NULL
  )

  return(result)
}


# Create mutable storage for findings produced during AST traversal.
#
# Rows are stored only when an expression produces an actual finding. This
# avoids constructing empty data frames for the many irrelevant AST nodes.
new_condition_analysis_accumulator <- function() {
  accumulator <- new.env(parent = emptyenv())

  accumulator$occurrence_rows <- list()
  accumulator$edge_rows <- list()
  accumulator$dynamic_rows <- list()
  accumulator$base_only_condition_rows <- list()

  return(accumulator)
}


## Package-level finalization  =================================================


# Find classes associated with multiple direct parents.
#
# Returns all hierarchy edges whose child has more than one distinct parent.
# Repeated occurrences of the same child-parent relationship are not treated
# as inconsistencies.
find_inconsistent_parents <- function(edges) {
  if (nrow(edges) == 0L) {
    return(empty_edges())
  }

  parents_by_class <- split(edges$parent, edges$child)

  number_of_parents <- lengths(lapply(parents_by_class, unique))

  inconsistent_classes <- names(number_of_parents[number_of_parents > 1L])

  result <- edges[edges$child %in% inconsistent_classes, , drop = FALSE]

  return(result)
}


## Source-file discovery  ======================================================


# Find R source files in requested package directories.
#
# Converts directories relative to `package_path` into full paths, ignores
# requested directories that do not exist, and recursively finds files whose
# names end in `.R` or `.r`.
find_r_source_files <- function(package_path, source_directories)
{
  source_paths <- file.path(package_path, source_directories)

  source_paths <- source_paths[dir.exists(source_paths)]

  if (length(source_paths) == 0L) {
    stop(
      "None of the requested source directories exists.",
      call. = FALSE
    )
  }

  source_files <- unlist(
    lapply(
      source_paths,
      list.files,
      pattern = "\\.[Rr]$",
      recursive = TRUE,
      full.names = TRUE
    ),
    use.names = FALSE
  )

  if (length(source_files) == 0L) {
    stop(
      "No R source files were found.",
      call. = FALSE
    )
  }

  return(source_files)
}

## Source-file parsing ==========================================================


# Parse one R source file without evaluating it.
#
# Returns the parsed expression and an empty parse-error table when parsing
# succeeds. If parsing fails, returns NULL as the parsed expression and a
# one-row parse-error table describing the failure.
parse_source_file <- function(source_file) {
  parsed_file <- tryCatch(
    parse(file = source_file,
          keep.source = TRUE),
    error = identity
  )

  if (inherits(parsed_file, "error")) {
    return(list(
      parsed_file = NULL,
      parse_errors = data.frame(
        file = source_file,
        message = conditionMessage(parsed_file),
        stringsAsFactors = FALSE
      )
    ) )
  }

  result <- list(parsed_file = parsed_file,
                 parse_errors = empty_parse_errors()
  )

  return(result)
}



# Find source locations of registered condition constructor calls.
#
# Uses the parser token table to locate calls whose names occur in
# `constructor_names`. Returns their function names, lines, and columns
# in source order.
get_constructor_locations <- function(
    parsed_file,
    constructor_names
) {
  parse_data <- getParseData(parsed_file, includeText = TRUE)

  if (is.null(parse_data) || nrow(parse_data) == 0) {
    return(data.frame(call = character(),
                      line = integer(),
                      column = integer(),
                      stringsAsFactors = FALSE) )
  }

  locations <- parse_data[parse_data$token == "SYMBOL_FUNCTION_CALL" &
                            parse_data$text %in% constructor_names,
                          c("text", "line1", "col1")
  ]

  names(locations) <- c("call", "line", "column")

  locations <- locations[order(locations$line, locations$column), ,drop = FALSE]

  row.names(locations) <- NULL

  return (locations)
}


# Create a stateful cursor over constructor source locations.
#
# The cursor maintains a separate position for each constructor name because
# the same constructor can occur several times in one source file.
new_constructor_location_cursor <- function(locations) {
  next_positions <- new.env(parent = emptyenv())

  consume <- function(call_name)
  {
    if (is.na(call_name) || nrow(locations) == 0L) {
      return(list(line = NA_integer_, column = NA_integer_) )
    }

    current_position <- next_positions[[call_name]]

    if (is.null(current_position)) {
      current_position <- 1L
    }

    matching_rows <- which(locations$call == call_name)

    if (current_position > length(matching_rows)) {
      return(list(line = NA_integer_, column = NA_integer_) )
    }

    row_index <- matching_rows[[current_position]]

    next_positions[[call_name]] <- current_position + 1L

    result <- list(line = locations$line[[row_index]],
                   column = locations$column[[row_index]])

    return(result)
  }

  return (list(consume = consume) )
}


# Inspect one parsed expression for condition class information.
#
# Registered constructors are checked for base-only conditions, while literal
# `class` and `subclass` vectors are converted into occurrences and direct
# hierarchy edges. Actual findings are appended to `accumulator`.
inspect_condition_expression <- function(expr,
                                         file,
                                         argument_names,
                                         subclass_suffixes,
                                         location_cursor,
                                         accumulator)
{
  if (!is.call(expr)) { return(invisible(NULL)) }

  current_call <- get_call_name(expr)
  current_line <- get_expression_line(expr)
  current_column <- NA_integer_

  is_registered_constructor <- !is.null(subclass_suffixes) &&
    !is.na(current_call) &&
    current_call %in% names(subclass_suffixes)

  # Nested calls frequently do not have an srcref. For registered condition
  # constructors, obtain the location from the parser token table.
  if (is_registered_constructor) {
    constructor_location <- location_cursor$consume(current_call)

    current_line <- constructor_location$line
    current_column <- constructor_location$column
  }

  # Detect calls to registered base constructors where `subclass` is absent
  # or syntactically missing.
  if (is_registered_constructor) {
    base_only_reason <- classify_subclass_argument(expr)

    if (!is.null(base_only_reason)) {
      suffix <- subclass_suffixes[[current_call]]

      base_class <- if (length(suffix) > 0L) suffix[[1L]] else NA_character_

      condition_type <- if ("error" %in% suffix) {
        "error"
      } else if ("warning" %in% suffix) {
        "warning"
      } else if ("message" %in% suffix) {
        "message"
      } else {
        "condition"
      }

      row_index <- length(accumulator$base_only_condition_rows) + 1L

      accumulator$base_only_condition_rows[[row_index]] <-
        data.frame(condition_type = condition_type,
                   base_class = base_class,
                   call = current_call,
                   file = file,
                   line = current_line,
                   column = current_column,
                   reason = base_only_reason)
    }
  }

  for (argument_name in argument_names)
  {
    argument <- get_named_argument(expr, argument_name)

    if (is.null(argument)) { next }

    argument_analysis <- analyze_condition_argument(
      argument = argument,
      argument_name = argument_name,
      call_name = current_call,
      file = file,
      line = current_line,
      subclass_suffixes = subclass_suffixes
    )

    if (!is.null(argument_analysis$occurrences)) {
      row_index <- length(accumulator$occurrence_rows) + 1L

      accumulator$occurrence_rows[[row_index]] <- argument_analysis$occurrences
    }

    if (!is.null(argument_analysis$edges)) {
      row_index <- length(accumulator$edge_rows) + 1L

      accumulator$edge_rows[[row_index]] <- argument_analysis$edges
    }

    if (!is.null(argument_analysis$dynamic_definitions)) {
      row_index <- length(accumulator$dynamic_rows) + 1L

      accumulator$dynamic_rows[[row_index]] <- argument_analysis$dynamic_definitions
    }
  }

  return(invisible(NULL))
}



#' Extract a condition class hierarchy from package source files
#'
#' Parse R source files without executing them and extract class vectors
#' supplied through `class` and `subclass` arguments. The function constructs
#' direct child-parent relationships, records class occurrences, identifies
#' classes with inconsistent parents, and reports registered constructors
#' called without an explicit subclass.
#'
#' @param package_path Path to the root directory of the package.
#'
#' @param source_directories Character vector of source directories, relative
#' to `package_path`, that should be scanned.
#'
#' @param argument_names Character vector containing the argument names from
#' which condition classes should be extracted.
#'
#' @param subclass_suffixes Named list mapping constructor names to the fixed
#' class suffix appended to their `subclass` argument.
#'
#' @return A named list containing:
#' \describe{
#'   \item{edges}{Direct child-parent class relationships.}
#'   \item{occurrences}{Every statically detected condition class occurrence.}
#'   \item{inconsistent_parents}{Classes associated with multiple parents.}
#'   \item{dynamic_definitions}{Class expressions that could not be evaluated
#'     statically.}
#'   \item{parse_errors}{Source files that could not be parsed.}
#'   \item{base_only_conditions}{Registered base constructors called without
#'     an explicit subclass.}
#' }
#'
#' @export
extract_condition_hierarchy <- function(
    package_path = ".",
    source_directories = "R",
    argument_names = c("class", "subclass"),
    subclass_suffixes = NULL)
{
  source_files <- find_r_source_files(package_path = package_path,
                                      source_directories = source_directories)

  accumulator <- new_condition_analysis_accumulator()

  parse_error_rows <- list()
  parse_error_index <- 0L

  # Parse and inspect each source file independently.
  #
  # Constructor locations and their consumption counters are reset for every
  # file because parser line and column information is file-specific.
  for (source_file in source_files) {
    parsed <- parse_source_file(source_file)

    if (nrow(parsed$parse_errors) > 0L) {
      parse_error_index <- parse_error_index + 1L
      parse_error_rows[[parse_error_index]] <- parsed$parse_errors

      next
    }

    parsed_file <- parsed$parsed_file

    constructor_names <- if (is.null(subclass_suffixes)) {
      character()
    } else {
      names(subclass_suffixes)
    }

    constructor_locations <- get_constructor_locations(
      parsed_file = parsed_file,
      constructor_names = constructor_names)

    location_cursor <- new_constructor_location_cursor(constructor_locations)

    walk_expression(
      parsed_file,
      callback = inspect_condition_expression,
      file = source_file,
      argument_names = argument_names,
      subclass_suffixes = subclass_suffixes,
      location_cursor = location_cursor,
      accumulator = accumulator
    )
  }

  # Combine the collected rows into consistently structured result tables ======

  base_only_conditions <- unique(
    bind_rows(accumulator$base_only_condition_rows,
              empty_result = empty_base_only_conditions() ) )

  occurrences <- bind_rows(accumulator$occurrence_rows,
                           empty_result = empty_occurrences() )

  edges <- unique(
    bind_rows(accumulator$edge_rows, empty_result = empty_edges() ) )

  dynamic_definitions <- bind_rows(accumulator$dynamic_rows,
                                   empty_result = empty_dynamic_definitions() )

  parse_errors <- bind_rows(parse_error_rows, empty_result = empty_parse_errors())

  analysis <- list(occurrences = occurrences,
                   edges = edges,
                   dynamic_definitions = dynamic_definitions,
                   parse_errors = parse_errors,
                   base_only_conditions = base_only_conditions
  )

  analysis$inconsistent_parents = find_inconsistent_parents(analysis$edges)

  return (analysis)
}


#' Format condition hierarchy edges
#'
#' Convert each unique child-parent relationship into a string of the form
#' `"ChildClass -> ParentClass"`.
#'
#' @param edges A data frame containing `child` and `parent` columns.
#'
#' @return A single character string containing one relationship per line.
#'
#' @export
format_all_edges <- function(edges) {
  unique_edges <- unique( edges[c("child", "parent")] )

  formatted_edges <- paste(unique_edges$child, "->", unique_edges$parent)

  result = paste(formatted_edges, collapse = "\n")

  return (result)
}


#' Format a condition class hierarchy as a tree
#'
#' Traverse condition classes from a root class toward their subclasses and
#' format the hierarchy using indented tree connectors. Cycles are detected
#' and marked in the output.
#'
#' @param edges A data frame containing `child` and `parent` columns.
#' @param root Name of the class used as the tree root.
#' @param sort_children Whether sibling classes should be sorted
#'   alphabetically.
#'
#' @return A single character string containing the formatted hierarchy.
#'
#' @keywords internal
format_class_tree <- function(edges,
                              root = "condition",
                              sort_children = TRUE)
{
  edges <- unique(edges[c("child", "parent")])

  # Recursively format one node and all of its descendant classes.
  format_node <- function(
    node,
    prefix = "",
    connector = "",
    ancestors = character()
  ) {
    current_line <- paste0(prefix, connector, node)

    if (node %in% ancestors) {
      return(paste0(current_line, " [cycle detected]"))
    }

    children <- unique(edges$child[edges$parent == node])

    if (sort_children) {
      children <- sort(children)
    }

    if (length(children) == 0L) {
      return(current_line)
    }

    output <- current_line

    for (i in seq_along(children)) {
      is_last_child <- i == length(children)

      child_connector <- if (is_last_child) {
        "└─ "
      } else {
        "├─ "
      }

      child_prefix <- paste0(
        prefix,
        if (nzchar(connector)) {
          if (identical(connector, "└─ ")) {
            "   "
          } else {
            "│  "
          }
        } else {
          ""
        }
      )

      output <- c(
        output,
        format_node(
          node = children[[i]],
          prefix = child_prefix,
          connector = child_connector,
          ancestors = c(ancestors, node)
        )
      )
    }

    output
  }

  paste(
    format_node(root),
    collapse = "\n"
  )
}


# Find all condition classes reachable from a root class.
#
# Traverses child-parent edges from the root toward its descendants and
# returns each reachable class once. This is used to detect disconnected
# hierarchy branches.
find_reachable_classes <- function(edges, root = "condition")
{
  edges <- unique(edges[c("child", "parent")])

  visited <- character()
  pending <- root

  while (length(pending) > 0L) {
    current <- pending[[1L]]
    pending <- pending[-1L]

    if (current %in% visited) {
      next
    }

    visited <- c(visited, current)

    children <- unique(
      edges$child[edges$parent == current]
    )

    pending <- c(pending, children)
  }

  return (visited)
}


#' Print a condition class hierarchy
#'
#' Print an indented class tree beginning at a specified root and warn when
#' condition classes are not connected to that root.
#'
#' @param edges A data frame containing `child` and `parent` columns.
#' @param root Name of the root condition class. The default is
#'   `"condition"`.
#'
#' @return Invisibly returns a list with two character vectors:
#'   `reachable`, containing classes connected to the root, and
#'   `disconnected`, containing classes outside the printed tree.
#'
#' @export
print_condition_tree <- function(edges, root = "condition")
{
  all_classes <- unique(c(edges$child, edges$parent))

  reachable_classes <- find_reachable_classes(edges, root = root)

  disconnected_classes <- setdiff(all_classes, reachable_classes )

  cat(format_class_tree(edges, root = root), "\n")

  if (length(disconnected_classes) > 0L) {
    warning(
      paste0(
        "Classes not connected to '",
        root,
        "': ",
        paste(disconnected_classes, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(list(
    reachable = reachable_classes,
    disconnected = disconnected_classes
  ))
}

