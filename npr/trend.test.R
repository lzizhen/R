trend.test <- function(x, alternative = c("two.sided", "less", "greater")) {
  DNAME <- deparse(substitute(x))
  alternative <- match.arg(alternative)
  n <- length(x)
  D <- -diff(x, lag = ceiling(n / 2))
  SP <- sum(D > 0)
  SN <- sum(D < 0)
  N <- SP + SN
  test <- binom.test(SP, N, alternative = alternative)
  parameters <- c(SP, SN, N)
  names(parameters) <- c('positive signs', 'negative signs', 'number of differences')
  alt <- switch (alternative,
    'two.sided' = 'there exists a trend',
    'less' = 'there is an increasing trend',
    'greater' = 'there is a decreasing trend'
  )
  names(SP) <- 'S'
  structure(list(statistic = SP, parameter = parameters, alternative = alt, p.value = test$p.value, method = 'Cox-Stuart Test', data.name = DNAME), class = 'htest')
}
