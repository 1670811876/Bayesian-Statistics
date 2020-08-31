
#P114
#
unnormpost<- function(pi, pr)
{
  like <- pi^7 * (1-pi)^43 #likelihood function
  like * pr
}

unnormpost( .15, 5 )

mypis <- seq(0.105, 0.195, by = .01)

mypis

unnormpost(mypis, 5)

mypis <- seq(0.105, 0.195, by = .01)
heights <- unnormpost(mypis, 5)
sum( heights * 0.01)



#P115

function( )
{
  endpoints <- c(0, 0.1, 0.2, 0.3, 0.4)
  prior <- c(2.5, 5.0, 2.0, 0.5)
  h <- 0.001 # width of subintervals
  integral <- 0 # initialize variable to accumulate total integral
  for( i in 1:4)
  {
    mypis <- seq( endpoints[i] + h/2, endpoints[i+1]
                  - h/2, by=h)
    heights <- unnormpost(mypis, prior[i])
    integral <- integral + sum( heights * h)
  }
  integral
}




#plot figure 8.2

endpoints1 <- c(0, 0.1, 0.2, 0.3, 0.4,1)
prior1 <- c(2.5, 5.0, 2.0, 0.5,0)
h <- 0.001 # width of subintervals

x<-c()
y<-c()
for( i in 1:5)
{
  mypis1 <- seq( endpoints1[i] + h/2, endpoints1[i+1]
                - h/2, by=h)
  heights1 <- unnormpost(mypis1, prior1[i])/integral
  x<-c(x,mypis1)
  y<-c(y,heights1)
}

plot(x, y, type="l", ylab="normalized posterior")


#P116

postmean<- function()
{
  expec <- function(p, pr)
  {
    hold <- p^8 *(1-p)^43 # Note p^8 instead of p^7
    pr * hold
  }
  normconst <- 8.126965e-10 # normalizing constants
  endpoints <- c(0, 0.1, 0.2, 0.3, 0.4)
  prior <- c(2.5, 5.0, 2.0, 0.5)
  h <- 0.001 # width of subintervals
  integral <- 0 # initialize variable to accumulate total integral
  for( i in 1:4)
  {
    mypis <- seq( endpoints[i] + h/2, endpoints[i+1]
                  - h/2, by=h)
    heights <- expec(mypis, prior[i])
    integral <- integral + sum( heights * h)/normconst
    # divide by normalizing const
  }
  integral
}
 

#P119
 set.seed(1234)
 mypis <- runif( 100, 0.1, 0.2 )
 heights <- unnormpost(mypis, 5) # defined as before
 mean(heights) * 0.1



#P119

mcinteg <- function()
{
  set.seed(1234)
  endpoints <- c(0, 0.1, 0.2, 0.3, 0.4)
  prior <- c(2.5, 5.0, 2.0, 0.5)
  nrand <- 100 # number of random points in each interval
  integral <- 0 # initialize variable to accumulate total integral
  for( i in 1:4)
  {
    mypis <- runif(nrand, endpoints[i], endpoints[i+1])
    heights <- unnormpost(mypis, prior[i])
    integral <- integral + mean( heights) * (endpoints
                                             [i+1] - endpoints[i])
  }
  integral
}



#P121
#draw samples from the posterior 

postsamp <- rbeta( 50, 8, 44 ) #the sample size is 50
mean(postsamp)
quantile( postsamp, c(0.025, 0.975))


postsamp <- rbeta( 500, 8, 44 ) #the sample size is 500
mean(postsamp)
quantile( postsamp, c(0.025, 0.975))

postsamp <- rbeta( 50000, 8, 44 ) #the sample size is 5000
mean(postsamp)
quantile( postsamp, c(0.025, 0.975))


# Example of a normal-inverse gamma model

 kappa <- 3
 mu0 <- -2.45
 n0 <- 27
 sigsq0 <- 0.402
 ybar <- -2.563
 ssq <- 0.385
n <- 21
sigsq <- 1/ rgamma( 1000, (n0+n)/2, (n0* sigsq0
                                     + (n-1) * ssq)/2 )
# In the next line of code, these 1000 values of sigsq appear in the
# standard deviation for drawing random normals.
# This means that each of the values of mu will be drawn from a normal
# density with a different standard deviation.
mu <- rnorm( 1000, (kappa * mu0 + n * ybar)
             /(kappa+n), sqrt( sigsq / (kappa+n) ) )

mean(sigsq)
quantile(sigsq, c(0.025, 0.975))
mean(mu)
quantile(mu, c(0.025, 0.975) )



## stable distribution
## P103 of Baysian computation  with R
T<-matrix(c(.5,.5,0,0,0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,
            0,0,.25,.5,.25,0,0,0,0,.25,.5,.25,0,0,0,0,.5,.5),
          nrow=6,ncol=6,byrow=TRUE)

s<-array(0,c(50000,1))

s[1]<-3

for (j in 2:50000){
  s[j]<-sample(1:6,size=1,prob=T[s[j-1],])}

m<-c(500,2000,8000,50000)

for (i in 1:4){
  print(table(s[1:m[i]])/m[i])
}


w<-matrix(c(.1,.2,.2,.2,.2,.1),nrow=1,ncol=6)
w%*%T





  