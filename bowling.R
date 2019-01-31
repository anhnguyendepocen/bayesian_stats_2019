# computes the x-th Fibonacci number without recursion and with vectorization
F <- function(x) {
  stopifnot(is.numeric(x), all(x == as.integer(x)))
  sqrt_5 <- sqrt(5) # defined once, used twice
  golden_ratio <- (1 + sqrt_5) / 2
  return(round(golden_ratio ^ (x + 1) / sqrt_5))
}
# probability of knocking down x out of n pins
Pr <- function(x, n = 10) return(ifelse(x > n, 0, F(x) / (-1 + F(n + 2))))

Omega <- 0:10 # 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
round(c(Pr(Omega), total = sum(Pr(Omega))), digits = 3)

x <- sample(Omega, size = 1, prob = Pr(Omega)) # realization of random variable
x