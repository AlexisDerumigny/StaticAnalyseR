
library(StaticAnalyseR)

base_path_pckgs = paste0(here::here(),  "\\..\\..\\")

condition_suffixes <- list(
  UniversalShrink_error_condition_base = c(
    "UniversalShrinkError",
    "error",
    "condition"
  ),
  UniversalShrink_warning_condition_base = c(
    "UniversalShrinkWarning",
    "warning",
    "condition"
  )
)

hierarchy <- extract_condition_hierarchy(
  package_path = paste0(base_path_pckgs,  "UniversalShrink\\UniversalShrink"),
  source_directories = "R",
  subclass_suffixes = condition_suffixes
)


hierarchy$edges

cat(format_all_edges(hierarchy$edges), "\n")

cat("\n\n")

print(hierarchy$base_only_conditions)

tree_check <- print_condition_tree(
  hierarchy$edges
)



condition_suffixes <- list(
  ICM_error_condition_base = c(
    "ICM_Error",
    "error",
    "condition"
  )
)

hierarchy <- extract_condition_hierarchy(
  package_path = paste0(base_path_pckgs, "\\ICM\\ICM"),
  source_directories = "R",
  subclass_suffixes = condition_suffixes
)

print_condition_tree(
  hierarchy$edges
)

condition_suffixes <- list(
  CondCopulas_warning_condition_base = c(
    "CondCopulasWarning",
    "warning",
    "condition"
  )
)

hierarchy <- extract_condition_hierarchy(
  package_path = paste0(base_path_pckgs, "CondCopulas\\CondCopulas"),
  source_directories = "R",
  subclass_suffixes = condition_suffixes
)

print_condition_tree(
  hierarchy$edges
)


