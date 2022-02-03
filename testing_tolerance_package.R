library(dplyr)
check_perc <- function(x, up, low) {
  within <- sum(x < up & x > low)
  within/length(x)*100
}

# making up some values for mus and dispersions
mat_death <- list()
N <- 5
mus <- runif(N, 2, 100) 
sizes <- runif(N, 5, 50)
for (i in 1:N) { 
  mat_death[[i]] <- rnbinom(n = 10000, #samples
                            size = sizes[i], #true dispersion aka size aka r
                            mu = mus[i]) #true mean maternal deaths)
}

t <- mat_death %>% unlist()
hist(t, breaks = 50) #makes sense looks uniform if you look at all dif populations created


cis <- list()
for (i in 1:N) { 
  cis[[i]] <- tolerance::negbintol.int(
    x  = mus[i],
    n = sizes[i], #this is r aka size aka success
    m = NULL,
    alpha = 0.2,
    P = 0.99,
    side = 2,
    method = c("CB")
  )
}

perc <- c()
for (i in 1:N) {
  perc[i] <- check_perc(
    x = mat_death[[i]],
    up = cis[[i]]$`2-sided.upper`,
    low = cis[[i]]$`2-sided.lower`)
}
mean(perc)

