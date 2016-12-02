pairwise.averages <- function(x, simplify = FALSE) {
  x <- sort(x)
  names(x) <- x
  avg <- outer(x, x, function(a, b) { (a + b) / 2 })
  avg[lower.tri(avg)] <- NA
  if (simplify) avg[upper.tri(avg, diag = TRUE)] else avg
}
