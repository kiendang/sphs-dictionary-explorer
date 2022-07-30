remove_na <- function(x) {
  out <- na.omit(x)
  attributes(out) <- NULL
  out
}


add_names <- function(x) setNames(x, x)
