addcol_constant <- function(matrix, m = 1) {
  n <- nrow(matrix)
  z <- rep(m, n)
  cbind(matrix, z)
}

addcol_normal <- function(matrix, m = 1, sd = m/2) {
  n <- nrow(matrix)
  y <- rnorm(n, m, sd)
  z <- ifelse(y > 0, y, 0)
  cbind(matrix, z)
}

addcol_spiked <- function(matrix, m = 1, k = round(n/4)) {
  n <- nrow(matrix)
  x <- cumsum(sample(c(-1, 1), n, TRUE))
  y <- caTools::runquantile(x, k, probs = 0.5)
  z <- (ifelse(x-y >= 0, x-y, 0)) * m
  cbind(matrix, z)
}

addcol_peak <- function(matrix, row = 10, m = 1) {
  x <- numeric(nrow(matrix))
  x[row] <- 1 * m
  cbind(matrix, x)
}
