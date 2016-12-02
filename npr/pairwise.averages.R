pairwise.averages <- function(x) {
  x <- sort(x)
  names(x) <- x
  avg <- outer(x, x, function(a, b) { (a + b) / 2 })
  avg[lower.tri(avg)] <- NA
  avg
}
