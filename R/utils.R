empty <- function (df) {
  length(df) == 0 || nrow(df) == 0 || ncol(df) == 0
}
