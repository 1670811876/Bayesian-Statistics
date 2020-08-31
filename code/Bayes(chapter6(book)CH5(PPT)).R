


# The numerical results for the poisson distribution 

a<-2 ; b<-1
# prior parameter s
n1<-111 ; sy1<-217
# data in group 1
n2<-44 ; sy2<-66
# data in group 2
(a+sy1)/(b+n1)
# posterior mean
(a+sy1-1)/(b+n1)
# posterior mode
qgamma(c(.025 , .975 ) , a+sy1 , b+n1 )
# posterior 95% CI
(a+sy2)/(b+n2)
(a+sy2-1)/(b+n2)
qgamma(c(.025, .975), a+sy2, b+n2)




# the mercury data
y<- c(-2.526, -1.715, -1.427, -2.12, -2.659,
-2.408, -3.219, -1.966,
-2.526, -1.833, -2.813, -1.772, -2.813, -2.526,
-3.219, -2.526, -2.813, -2.526, -3.507, -2.996, -3.912)


#figure 6.2

hist(y,axes=F)
hist(y)

#figure 6.3
x<-seq(-4,-1,length=100)
dy<-dnorm(x,-2.586,sqrt(1/60))
plot(x,dy,type='l',xlab='x',ylab='density')
#or 
curve(dnorm(x,-2.586,sqrt(1/60)),from=-4,to=-1)

