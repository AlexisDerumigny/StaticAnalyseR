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
walk_expression <- function(expr, callback, file) {
  if (missing(expr)) {
    return(invisible(NULL))
  }

  callback(expr, file)

  if (
    is.call(expr) ||
    is.expression(expr) ||
    is.pairlist(expr)
  ) {
    number_of_components <- length(expr)

    if (number_of_components > 0L) {
      for (i in seq_len(number_of_components)) {
        walk_expression(
          expr[[i]],
          callback = callback,
          file = file
        )
      }
    }
  }

  invisible(NULL)
}


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
    subclass_suffixes = NULL
) {
  source_directories <- file.path(package_path, source_directories)

  source_directories <- source_directories[dir.exists(source_directories)]

  if (length(source_directories) == 0L) {
    stop("None of the requested source directories exists.",
         call. = FALSE)
  }

  source_files <- unlist(
    lapply(
      source_directories,
      list.files,
      pattern = "\\.[Rr]$",
      recursive = TRUE,
      full.names = TRUE
    ),
    use.names = FALSE
  )

  if (length(source_files) == 0L) {
    stop("No R source files were found.", call. = FALSE)
  }

  current_constructor_locations <- data.frame(
    call = character(),
    line = integer(),
    column = integer(),
    stringsAsFactors = FALSE
  )

  next_constructor_location <- list()

  occurrence_rows <- list()
  edge_rows <- list()
  dynamic_rows <- list()
  parse_error_rows <- list()
  base_only_condition_rows <- list()

  occurrence_index <- 0L
  edge_index <- 0L
  dynamic_index <- 0L
  parse_error_index <- 0L
  base_only_condition_index <- 0L

  # Return the next unused source location for a constructor call.
  #
  # A separate position is maintained for each constructor name because the
  # same constructor can occur several times in one source file.
  consume_constructor_location <- function(call_name) {
    if (
      is.na(call_name) ||
      nrow(current_constructor_locations) == 0L
    ) {
      return(list(
        line = NA_integer_,
        column = NA_integer_
      ))
    }

    current_index <- next_constructor_location[[call_name]]

    if (is.null(current_index)) {
      current_index <- 1L
    }

    matching_rows <- which(
      current_constructor_locations$call == call_name
    )

    if (current_index > length(matching_rows)) {
      return(list(
        line = NA_integer_,
        column = NA_integer_
      ))
    }

    row_index <- matching_rows[[current_index]]

    next_constructor_location[[call_name]] <<-
      current_index + 1L

    list(
      line = current_constructor_locations$line[[row_index]],
      column = current_constructor_locations$column[[row_index]]
    )
  }


  # Inspect one parsed expression for condition class information.
  #
  # Registered constructors are checked for base-only conditions, while
  # literal `class` and `subclass` vectors are converted into occurrences
  # and direct hierarchy edges.
  inspect_expression <- function(expr, file) {
    if (!is.call(expr)) {
      return(invisible(NULL))
    }

    current_call <- get_call_name(expr)

    current_line <- get_expression_line(expr)
    current_column <- NA_integer_

    # Nested calls frequently do not have an srcref. For registered
    # condition constructors, obtain the location from getParseData().
    if (!is.null(subclass_suffixes) &&
        !is.na(current_call) &&
        current_call %in% names(subclass_suffixes)
    ) {
      constructor_location <- consume_constructor_location(current_call)

      current_line <- constructor_location$line
      current_column <- constructor_location$column
    }

    # Detect direct calls to registered base constructors where subclass
    # is absent or explicitly empty.
    #
    # The names of the registered constructors come from
    # subclass_suffixes. This therefore works for both errors and warnings.
    if (
      !is.null(subclass_suffixes) &&
      !is.na(current_call) &&
      current_call %in% names(subclass_suffixes)
    ) {
      base_only_reason <- classify_subclass_argument(expr)

      if (!is.null(base_only_reason)) {
        suffix <- subclass_suffixes[[current_call]]

        base_class <- if (length(suffix) > 0L) {
          suffix[[1L]]
        } else {
          NA_character_
        }

        condition_type <- if ("error" %in% suffix) {
          "error"
        } else if ("warning" %in% suffix) {
          "warning"
        } else if ("message" %in% suffix) {
          "message"
        } else {
          "condition"
        }

        base_only_condition_index <<-
          base_only_condition_index + 1L

        base_only_condition_rows[[base_only_condition_index]] <<- data.frame(
          condition_type = condition_type,
          base_class = base_class,
          call = current_call,
          file = file,
          line = current_line,
          column = current_column,
          reason = base_only_reason,
          stringsAsFactors = FALSE
        )
      }
    }

    for (argument_name in argument_names) {
      argument <- get_named_argument(expr, argument_name)

      if (is.null(argument)) {
        next
      }

      classes <- extract_character_literals(argument)

      if (length(classes) == 0L) {
        dynamic_index <<- dynamic_index + 1L

        dynamic_rows[[dynamic_index]] <<- data.frame(
          argument = argument_name,
          call = current_call,
          file = file,
          line = current_line,
          expression = paste(
            deparse(argument),
            collapse = " "
          ),
          stringsAsFactors = FALSE
        )

        next
      }

      # If subclass is passed to a base constructor, append the
      # known base hierarchy.
      full_classes <- classes

      if (
        identical(argument_name, "subclass") &&
        !is.null(subclass_suffixes)
      ) {
        matching_suffix <- subclass_suffixes[[current_call]]

        if (!is.null(matching_suffix)) {
          full_classes <- c(
            full_classes,
            matching_suffix
          )
        }
      }

      # Remove only consecutive duplicates.
      if (length(full_classes) > 1L) {
        keep <- c(
          TRUE,
          full_classes[-1L] !=
            full_classes[-length(full_classes)]
        )

        full_classes <- full_classes[keep]
      }

      for (position in seq_along(full_classes)) {
        occurrence_index <<- occurrence_index + 1L

        occurrence_rows[[occurrence_index]] <<- data.frame(
          class = full_classes[[position]],
          position = position,
          argument = argument_name,
          call = current_call,
          file = file,
          line = current_line,
          stringsAsFactors = FALSE
        )
      }

      if (length(full_classes) >= 2L) {
        for (position in seq_len(length(full_classes) - 1L)) {
          edge_index <<- edge_index + 1L

          edge_rows[[edge_index]] <<- data.frame(
            child = full_classes[[position]],
            parent = full_classes[[position + 1L]],
            source = argument_name,
            call = current_call,
            file = file,
            line = current_line,
            stringsAsFactors = FALSE
          )
        }
      }
    }

    invisible(NULL)
  }

  # Parse and inspect each source file independently.
  #
  # Constructor locations and their consumption counters are reset for every
  # file because parser line and column information is file-specific.
  for (source_file in source_files) {
    parsed_file <- tryCatch(
      parse(
        file = source_file,
        keep.source = TRUE
      ),
      error = identity
    )

    if (inherits(parsed_file, "error")) {
      parse_error_index <- parse_error_index + 1L

      parse_error_rows[[parse_error_index]] <- data.frame(
        file = source_file,
        message = conditionMessage(parsed_file),
        stringsAsFactors = FALSE
      )

      next
    }

    constructor_names <- if (is.null(subclass_suffixes)) {
      character()
    } else {
      names(subclass_suffixes)
    }

    current_constructor_locations <- get_constructor_locations(
      parsed_file = parsed_file,
      constructor_names = constructor_names
    )

    next_constructor_location <- list()

    walk_expression(
      parsed_file,
      callback = inspect_expression,
      file = source_file
    )
  }

  # Combine the collected rows into consistently structured result tables ======

  base_only_conditions <- if (length(base_only_condition_rows) == 0L) {
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
  } else {
    unique(bind_rows(base_only_condition_rows))
  }


  occurrences <- if (length(occurrence_rows) == 0L) {
    empty_occurrences()
  } else {
    bind_rows(occurrence_rows)
  }

  edges <- if (length(edge_rows) == 0L) {
    empty_edges()
  } else {
    unique(bind_rows(edge_rows))
  }

  dynamic_definitions <- if (length(dynamic_rows) == 0L) {
    data.frame(
      argument = character(),
      call = character(),
      file = character(),
      line = integer(),
      expression = character(),
      stringsAsFactors = FALSE
    )
  } else {
    bind_rows(dynamic_rows)
  }

  parse_errors <- if (length(parse_error_rows) == 0L) {
    data.frame(
      file = character(),
      message = character(),
      stringsAsFactors = FALSE
    )
  } else {
    bind_rows(parse_error_rows)
  }

  # A class normally has one direct parent. Multiple parents may indicate
  # either a typo or an inconsistent hierarchy.
  parents_by_class <- split(edges$parent, edges$child)

  inconsistent_classes <- names(
    parents_by_class[
      lengths(lapply(parents_by_class, unique)) > 1L
    ]
  )

  inconsistent_parents <- edges[edges$child %in% inconsistent_classes,
                                ,
                                drop = FALSE]

  result = list(
    edges = edges,
    occurrences = occurrences,
    inconsistent_parents = inconsistent_parents,
    dynamic_definitions = dynamic_definitions,
    parse_errors = parse_errors,
    base_only_conditions = base_only_conditions
  )

  return (result)
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

