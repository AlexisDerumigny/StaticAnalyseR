
library(StaticAnalyseR)


hierarchy <- extract_condition_hierarchy(
  package_path = here::here(),
  source_directories = "R",
  subclass_suffixes = condition_suffixes
)


hierarchy$edges

cat(format_all_edges(hierarchy$edges), "\n")

cat("\n\n")

tree_check <- print_condition_tree(
  hierarchy$edges
)
