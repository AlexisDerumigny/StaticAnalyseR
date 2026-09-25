
#' @export
summarise_package_code <- function(
    package_path = ".",
    output_file = "package_code.txt",
    include_tests = TRUE,
    include_cpp = TRUE)
{
  package_path <- normalizePath(package_path, winslash = "/", mustWork = TRUE)

  # Directories containing files to include
  source_directories <- file.path(package_path, "R")

  if (include_tests) {
    source_directories <- c(
      source_directories, file.path(package_path, "tests", "testthat")
    )
  }

  if (include_cpp) {
    source_directories <- c(source_directories, file.path(package_path, "src"))
  }

  # Keep only directories that actually exist
  source_directories <- source_directories[
    dir.exists(source_directories)
  ]

  # File extensions to include
  extensions <- "R|r"

  if (include_cpp) {
    extensions <- paste(
      extensions,
      "c|cc|cpp|cxx|h|hh|hpp|hxx",
      sep = "|"
    )
  }

  source_files <- unlist(
    lapply(source_directories,
           list.files,
           pattern = paste0("\\.(", extensions, ")$"),
           full.names = TRUE,
           recursive = TRUE,
           ignore.case = TRUE
    ),
    use.names = FALSE
  )

  # Make the order reproducible
  source_files <- sort(unique(source_files))

  # Avoid accidentally including a previous output file
  normalized_output <- normalizePath(file.path(package_path, output_file),
                                     winslash = "/",
                                     mustWork = FALSE)

  source_files <- source_files[
    normalizePath(
      source_files, winslash = "/", mustWork = FALSE
    ) != normalized_output
  ]

  if (length(source_files) == 0L) {
    stop("No matching source files were found.")
  }

  # Use a connection so that the output is written incrementally
  output_path <- file.path(package_path, output_file)

  output_connection <- file(output_path, open = "w", encoding = "UTF-8")

  on.exit(close(output_connection), add = TRUE)

  for (source_file in source_files)
  {
    # Display paths relative to the package root
    relative_path <- substring(source_file, nchar(package_path) + 2L)

    header <- paste0("======= ", relative_path, " =======")

    source_code <- readLines(source_file, warn = FALSE, encoding = "UTF-8")

    writeLines(
      c(header, "\n", source_code, "\n\n"),
      con = output_connection,
      useBytes = TRUE
    )
  }

  message(
    "Wrote ", length(source_files), " files to: ",
    normalizePath(output_path, winslash = "/", mustWork = FALSE)
  )

  invisible(output_path)
}
