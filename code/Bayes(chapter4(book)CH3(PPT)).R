
###############################################
# P52
# inference by the frequentist approach

binom.test(7,50,conf.level=0.9)

# P53 
# inference by the frequentist approach

binom.test(7,50,p=0.1,alternative="greater")

# the prior is  an uniform distribution on [0,1]
library(LearnBayes)
triplot(c(1,1),c(7,43))

# a worst case
triplot(c(10,2),c(1,9))

######################################3
# P57
qbeta( c(0.025, 0.975), 17, 83 )


#fig 4.3

x<-seq(0.005,0.995,length=100)
y<-dbeta(x,17,83)
plot(x,y,main="Beta density\n alpha=17 beta=83",type='l',
     xlab="value",ylab="Density")
z<-qbeta(c(0.025,0.975),17,83)
lines(z,dbeta(z,17,83),type="h" )


########################################
library(LearnBayes)
pbetap( c( 8, 44), 25, 3:6 )


pprobs <- pbetap( c( 8, 44), 25, 3:6 )
cbind(3:6, pprobs)