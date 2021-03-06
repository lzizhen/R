copy <- function(x, header = FALSE) {
  if (is.vector(x))
    x <- list(x)
  data <- .Internal(paste(x, '\t', NULL))
  if (header && !is.null(names(x)))
    data <- c(paste(names(x), collapse = '\t'), data)
  writeClipboard(data)
}
