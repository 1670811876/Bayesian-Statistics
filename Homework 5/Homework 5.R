setwd("E:/Bayesian Statistics/Homework 5")

## 8.1
unnormpost <- function(pi, pr) {
  like <- pi^7 * (1-pi)^43
  like*pr
}

mcintegral <- 0

for (i in 1:1000) {
  mcinteg <- function() {
    endpoints <- c(0, 0.1, 0.2, 0.3, 0.4)
    prior <- c(2.5, 5.0, 2.0, 0.5)
    nrand <- 100  # number of random points in each interval
    integral <- 0  # initialize variable to accumalate total integral
    
    for (i in 1:4) {
      mypis <- runif(nrand, endpoints[i], endpoints[i+1])
      heights <- unnormpost(mypis, prior[i])
      integral <- integral + mean(heights) * (endpoints[i+1]-endpoints[i])
    }
    
    integral
    
  }
  
  mcintegral <- mcintegral + mcinteg()

}

mcintegral <- mcintegral/1000

expec <- function(pi, pr) {
  like <- pi^8 * (1-pi)^43
  like*pr
}

exintegral <- 0

for (i in 1:1000) {
  expecinteg <- function() {
    endpoints <- c(0, 0.1, 0.2, 0.3, 0.4)
    prior <- c(2.5, 5.0, 2.0, 0.5)
    nrand <- 100  # number of random points in each interval
    integral <- 0  # initialize variable to accumalate total integral
    
    for (i in 1:4) {
      mypis <- runif(nrand, endpoints[i], endpoints[i+1])
      heights <- expec(mypis, prior[i])
      integral <- integral + mean(heights) * (endpoints[i+1]-endpoints[i])
    }
    
    integral/mcintegral
    
  }
  
  exintegral <- exintegral + expecinteg()
}

exintegral/1000
