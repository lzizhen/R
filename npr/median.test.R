median.test <- function(x, y, alternative = c("two.sided", "less", "greater")) {
  DNAME <- paste(deparse(substitute(x)), "and",
                 deparse(substitute(y)))
  alternative <- match.arg(alternative)
  M <- median(c(x, y))
  a <- sum(x > M)
  c <- sum(x < M)
  b <- sum(y > M)
  d <- sum(y < M)
  PVAL <-
    switch(alternative,
           "two.sided" = {
             2 * min(phyper(a, a + b, c + d, a + c), phyper(a - 1, a + b, c + d, a + c, lower.tail = FALSE))
           },
           "greater" = {
             phyper(a - 1, a + b, c + d, a + c, lower.tail = FALSE)
           },
           "less" = phyper(a, a + b, c + d, a + c))
  names(a) <- 'T'
  structure(list(statistic = a, parameter = NULL, alternative = alternative, p.value = PVAL, method = 'Median Test', data.name = DNAME), class = 'htest')
}
