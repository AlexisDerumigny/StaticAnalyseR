

#' Format condition hierarchy edges
#'
#' Convert each unique child-parent relationship into a string of the form
#' `"ChildClass -> ParentClass"`.
#'
#' @param edges A data frame containing `child` and `parent` columns.
#'
#' @return A single character string containing one relationship per line.
#'
#' @noRd
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
#' @noRd
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

