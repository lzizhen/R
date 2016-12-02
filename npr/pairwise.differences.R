pairwise.differences <- function(x, y, simplify = FALSE) {
  x <- sort(x)
  y <- sort(y)
  names(x) <- x
  names(y) <- y
  diff <- outer(x, y, `-`)
  if (simplify) as.vector(diff) else diff
}
