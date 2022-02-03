library(dplyr)
check_perc <- function(x, up, low) {
  within <- sum(x < up & x > low)
  within/length(x)*100
}

N <- 10000
mat_death <- list()
for (i in 1:N) { 
  mat_death[[i]] <- rnbinom(n = 10000, #samples
                            size = 5, #true dispersion aka size aka r
                            mu = 50 #true mean maternal deaths)
}

hist(mat_death[[1]])
ci <- tolerance::negbintol.int(
  x  = 50,
  n = 5, #this is r aka size aka success
  m = NULL,
  alpha = 0.2,
  P = 0.99,
  side = 2,
  method = c("CB")
)
ci

perc <- c()
for (i in 1:N) {
  perc <- check_perc(
    x = mat_death[[3]],
    up = ci$`2-sided.upper`,
    low = ci$`2-sided.lower`)
}
mean(perc)


ci <- tolerance::negbintol.int(
  x  = 50,
  n = 5, #this is r aka size aka success
  m = NULL,
  alpha = 0.2,
  P = 0.5,
  side = 2,
  method = c("CB")
)
ci

perc <- c()
for (i in 1:N) {
  perc <- check_perc(
    x = mat_death[[3]],
    up = ci$`2-sided.upper`,
    low = ci$`2-sided.lower`)
}
mean(perc)
