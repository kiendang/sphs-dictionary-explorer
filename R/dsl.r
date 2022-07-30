# library(purrr)
# library(dplyr)

make_filter_expr <- function(filters) {
  filters |>
    imap(\(incls, col) {
      expr(!!sym(col) %in% !!incls)
    }) |>
    reduce(\(x, y) expr(!!x & !!y), .init = expr(TRUE))
}


evaluate_filter <- function(data, filters = list()) {
  expr <- make_filter_expr(filters)
  data |> filter(!!expr)
}


# test_case <- list(
#   color = c("red", "blue"),
#   species = c("bird", "cat")
# )
#
#
# test_df <- data.frame(
#   color = c("red", "blue", "blue", "green", "blue"),
#   species = c("bird", "dog", "what", "cat", "cat")
# )
#
#
# make_filter_expr(test_case)
# evaluate_filter(test_df, test_case)
# evaluate_filter(test_df, list())
