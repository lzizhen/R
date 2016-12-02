runs.test = function(x, mu = NULL, alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)
  DNAME <- deparse(substitute(x))
  mu = if (is.null(mu)) median(x) else mu
  z = as.numeric(x > median(x))
  m = length(z[z == 0])
  n = length(z[z == 1])
  R = 0
  k = -1
  for (i in z) {
    if (i != k)
      R = R + 1
    k = i
  }
  pruns = function(q, m, n, lower.tail = TRUE) {
    p = 0
    for (r in 2:q) {
      if (r %% 2 == 0) {
        p = p + (2 * choose(m - 1, r / 2 - 1) * choose(n - 1, r / 2 - 1)) / choose(m + n, n)
      }
      else {
        p = p + (choose(m - 1, (r - 3) / 2) * choose(n - 1, (r - 1) / 2) + choose(m - 1, (r - 1) / 2) * choose(n - 1, (r - 3) / 2)) / choose(m + n, n)
      }
    }
    if (lower.tail)
      p
    else
      1 - p
  }
  PVAL <-
    switch(alternative,
           "two.sided" = {
             p <- if(R > (2 * m * n / (m + n) + 1))
               pruns(R - 1, m, n, lower.tail = FALSE)
             else
               pruns(R, m, n)
             min(2 * p, 1)
           },
           "greater" = {
             pruns(R - 1, m, n, lower.tail = FALSE)
           },
           "less" = pruns(R, m, n))
  names(R) = "R"
  parameters = c(mu, m, n)
  names(parameters) = c('K', 'below K: m', 'above K: n')
  structure(list(statistic = R, parameter = parameters, alternative = alternative, p.value = PVAL, method = "Runs Test", data.name = DNAME), class = "htest")
}
