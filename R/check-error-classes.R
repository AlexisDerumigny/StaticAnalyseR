# Extract the hierarchy of condition classes from the R source files
# of a package.
#
# This performs static analysis: package code is parsed but not executed.


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

  NA_character_
}


# Extract values only when they are statically declared character strings.
#
# Supported:
#   class = "MyError"
#   class = c("MyError", "error", "condition")
#
# Not supported:
#   class = make_class_vector(x)
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

  character()
}


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


#' @export
extract_condition_hierarchy <- function(
    package_path = ".",
    source_directories = "R",
    argument_names = c("class", "subclass"),
    subclass_suffixes = NULL
) {
  source_directories <- file.path(
    package_path,
    source_directories
  )

  source_directories <- source_directories[
    dir.exists(source_directories)
  ]

  if (length(source_directories) == 0L) {
    stop(
      "None of the requested source directories exists.",
      call. = FALSE
    )
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

  inspect_expression <- function(expr, file) {
    if (!is.call(expr)) {
      return(invisible(NULL))
    }

    current_call <- get_call_name(expr)
    current_line <- get_expression_line(expr)

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

    walk_expression(
      parsed_file,
      callback = inspect_expression,
      file = source_file
    )
  }

  bind_rows <- function(rows, empty_result) {
    if (length(rows) == 0L) {
      return(empty_result)
    }

    row.names(do.call(rbind, rows)) <- NULL
    do.call(rbind, rows)
  }

  base_only_conditions <- if (
    length(base_only_condition_rows) == 0L
  ) {
    data.frame(
      condition_type = character(),
      base_class = character(),
      call = character(),
      file = character(),
      line = integer(),
      reason = character(),
      stringsAsFactors = FALSE
    )
  } else {
    unique(do.call(
      rbind,
      base_only_condition_rows
    ))
  }


  occurrences <- if (length(occurrence_rows) == 0L) {
    empty_occurrences()
  } else {
    do.call(rbind, occurrence_rows)
  }

  edges <- if (length(edge_rows) == 0L) {
    empty_edges()
  } else {
    unique(do.call(rbind, edge_rows))
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
    do.call(rbind, dynamic_rows)
  }

  parse_errors <- if (length(parse_error_rows) == 0L) {
    data.frame(
      file = character(),
      message = character(),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, parse_error_rows)
  }

  row.names(occurrences) <- NULL
  row.names(edges) <- NULL
  row.names(dynamic_definitions) <- NULL
  row.names(parse_errors) <- NULL
  row.names(base_only_conditions) <- NULL

  # A class normally has one direct parent. Multiple parents may indicate
  # either a typo or an inconsistent hierarchy.
  parents_by_class <- split(edges$parent, edges$child)

  inconsistent_classes <- names(
    parents_by_class[
      lengths(lapply(parents_by_class, unique)) > 1L
    ]
  )

  inconsistent_parents <- edges[
    edges$child %in% inconsistent_classes,
    ,
    drop = FALSE
  ]

  list(
    edges = edges,
    occurrences = occurrences,
    inconsistent_parents = inconsistent_parents,
    dynamic_definitions = dynamic_definitions,
    parse_errors = parse_errors,
    base_only_conditions = base_only_conditions
  )
}


#' @export
format_all_edges <- function(edges) {
  unique_edges <- unique(
    edges[c("child", "parent")]
  )

  formatted_edges <- paste(
    unique_edges$child,
    "->",
    unique_edges$parent
  )

  paste(formatted_edges, collapse = "\n")
}



format_class_tree <- function(
    edges,
    root = "condition",
    sort_children = TRUE
) {
  edges <- unique(edges[c("child", "parent")])

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

find_reachable_classes <- function(
    edges,
    root = "condition"
) {
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

  visited
}


#' @export
print_condition_tree <- function(
    edges,
    root = "condition"
) {
  all_classes <- unique(c(
    edges$child,
    edges$parent
  ))

  reachable_classes <- find_reachable_classes(
    edges,
    root = root
  )

  disconnected_classes <- setdiff(
    all_classes,
    reachable_classes
  )

  cat(
    format_class_tree(
      edges,
      root = root
    ),
    "\n"
  )

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

