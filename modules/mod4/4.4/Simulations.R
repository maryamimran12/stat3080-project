
## Monte Carlo simulation for Type I error

my.test <- function(x){
  samp <- rXXX(x, ...)
  ts <- (mean(samp) - mu)/(sigma/sqrt(x))
  p.val <- 2*pnorm(-abs(ts))
  p.val <= alpha
}

rep.test <- function(x){
  res <- replicate(K, my.test(x))
  sum(res)/K
}



#












## Monte Carlo simulation for bootstrapped confidence intervals

set.seed(5)

B<-10000
p2.5 <- B*0.025
p97.5 <- B*0.975

samps <- replicate(100, rchisq(7, df=4))

bootCI.func <- function(x){
  samp_data <- samps[,x]
  samp_mean <- mean(samp_data)
  
  boot_samp <- replicate(B, sample(samp_data, replace=T))
  boot_means <- apply(boot_samp,2,mean)
  boot_err_sort <- sort(boot_means - samp_mean)
  boot_ci <- samp_mean - boot_err_sort[c(p97.5,p2.5)]
  boot_ci[1] <= 4 & boot_ci[2] >= 4
}

res <- sapply(1:100, bootCI.func)
samps[, which(res == FALSE)]



#
















## Consider a Normal population with a mean of 73.2 and a standard 
## deviation of 4.9. Use Monte Carlo simulation with 10,000 
## repetitions to determine the empirical power of a correctly 
## conducted right-sided z-test for sample sizes 8, 23, and 52.

mu <- 73.2
sigma <- 4.9
alpha <- 0.05
K <- 10000
sizes <- c(8, 23, 52)

my.test <- function(x){
  samp <- rnorm(x, mean=mu, sd=sigma)
  ts <- (mean(samp) - mu.0)/(sigma/sqrt(x))
  p.val <- 2*pnorm(-abs(ts))
  p.val <= alpha
}

rep.test <- function(x){
  res <- replicate(K, my.test(x))
  sum(res)/K
}

mu.0 <- 67.8
sapply(sizes, rep.test)























