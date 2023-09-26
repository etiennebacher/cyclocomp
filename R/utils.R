
what_expr <- function(expr) {
  if (is.call(expr)) {
    paste0(as.character(expr[[1]])[1], "()")

  } else {
    typeof(expr)
  }
}

## We need the tryCatch, because the string might contain invalid
## multi-byte characters. E.g.
## substring('\x93', 1, 10) and nchar('\x93') both fail

what_atomic <- function(expr) {
  if (is.character(expr)) {
    tryCatch(
      paste0("\"", substring(expr[1], 1, 10), "\""),
      error = function(e) "\"<string>\""
    )
  } else if (is.name(expr)) {
    as.character(expr)
  } else {
    "atomic"
  }
}


## From list2DF() (appeared in R 4.0)
## lengths() requires R 3.2.0
list2DF <- function(x = list(), nrow = 0L) {
  stopifnot(is.list(x), is.null(nrow) || nrow >= 0L)
  if (n <- length(x)) {
    if (length(nrow <- unique(lengths(x))) > 1L)
      stop("all variables should have the same length")
  }
  else {
    if (is.null(nrow))
      nrow <- 0L
  }
  if (is.null(names(x)))
    names(x) <- character(n)
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(nrow)
  x
}

.set_row_names <- function(n) {
  if (n > 0) c(NA_integer_, -n) else integer()
}
