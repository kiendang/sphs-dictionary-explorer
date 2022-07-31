# library(purrr)
# library(dplyr)

make_filter_expr <- function(filters) {
  filters |>
    imap(function(incls, col) {
      expr(!!sym(col) %in% !!incls)
    }) |>
    reduce(\(x, y) expr(!!x & !!y), .init = expr(TRUE))
}


evaluate_filter <- function(data, filters = list()) {
  expr <- make_filter_expr(filters)
  data |> filter(!!expr)
}


render_filter <- function(filters) {
  filters |>
    imap(function(incls, col) {
      options <- incls |>
        (\(x) sprintf("<strong>%s</strong>", x))()
      options <-
        if (length(options) == 1L) {
          sprintf("<strong>%s</strong> is (%s)", toupper(col), options)
        } else {
          options |>
            paste(collapse = ", ") |>
            (\(x) sprintf("<strong>%s</strong> in (%s)", toupper(col),  x))()
        }
    }) |>
    paste(collapse = " and ") |>
    (\(x) sprintf("SELECT %s",  x))()
}


# test_case <- list(
#   color = c("red", "blue"),
#   species = c("bird")
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
# render_filter(test_case)
