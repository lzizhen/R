runs.test <- function(x, k = NA, alternative = c("two.sided", "less", "greater"), exact = TRUE, correct = TRUE) {
  alternative <- match.arg(alternative)
  DNAME <- deparse(substitute(x))
  if (length(intersect(match(x, c(0, 1)), NA)) == 1) {
    k <- if (is.na(k)) median(x) else k
    z <- as.numeric(x > k)
  } else {
    z <- x
  }
  m <- length(z[z == 0])
  n <- length(z[z == 1])
  R <- 0
  l <- -1
  for (i in z) {
    if (i != l)
      R <- R + 1
    l <- i
  }
  pruns <- function(q, m, n, lower.tail = TRUE) {
    p <- 0
    for (r in 2:q) {
      if (r %% 2 == 0) {
        p <- p + (2 * choose(m - 1, r / 2 - 1) * choose(n - 1, r / 2 - 1)) / choose(m + n, n)
      }
      else {
        p <- p + (choose(m - 1, (r - 3) / 2) * choose(n - 1, (r - 1) / 2) + choose(m - 1, (r - 1) / 2) * choose(n - 1, (r - 3) / 2)) / choose(m + n, n)
      }
    }
    if (lower.tail)
      p
    else
      1 - p
  }
  names(R) <- "R"
  parameters <- c(k, m, n)
  names(parameters) <- c('K', 'below K: m', 'above K: n')
  if (exact) {
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
  } else {
    M <- 2 * m * n / (m + n) + 1
    V <- 2 * m * n * (2 * m * n - m - n) / ((m + n) ^ 2 * (m + n - 1))
    AD <- 0
    if (correct) {
      AD <- if (R < M) 0.5 else if (R > M) -0.5 else 0
    }
    Z <- (R + AD - M) / sqrt(V)
    PVAL <-
      switch(alternative,
             "two.sided" = {
               2 * pnorm(abs(Z), lower.tail = FALSE)
             },
             "greater" = {
               pnorm(Z, lower.tail = FALSE)
             },
             "less" = pnorm(Z))
    parameters <- c(parameters, Z)
    names(parameters)[4] <- 'Z'
  }
  structure(list(statistic = R, parameter = parameters, alternative = alternative, p.value = PVAL, method = "Runs Test", data.name = DNAME), class = "htest")
}
