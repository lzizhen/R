pairwise.averages <- function(x, simplify = FALSE) {
  x <- sort(x)
  names(x) <- x
  avgs <- outer(x, x, `+`) / 2
  avgs[lower.tri(avgs)] <- NA
  if (simplify) avgs[!lower.tri(avgs)] else avgs
}
