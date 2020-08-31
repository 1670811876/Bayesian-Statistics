 #example 2

 p <- seq(0.05, 0.95, by = 0.1)
 prior <- c(2, 4, 8, 8, 4, 2, 1, 1, 1, 1)
 prior <- prior/sum(prior)
 plot(p, prior, type = "h", ylab="Prior Probability")
 
 ######
library(LearnBayes)
data <- c(11, 16)
post <- pdisc(p, prior, data)
cbind(p, prior, post)
plot(p, post, type = "h", ylab="Posterior Probability")
 


 ######
 p <- seq(0, 1, length = 500)
 a <- 3.4
 b <- 7.4
 s <- 11
 f <- 16
 prior<-dbeta(p,a,b)
 like<-dbeta(p,s+1,f+1)
 post<-dbeta(p,a+s,b+f)
 plot(p,post,type="l",ylab="Density",lty=2,lwd=3)
 lines(p,like,lty=1,lwd=3)
 lines(p,prior,lty=3,lwd=3)
 legend(.6,4,c("Prior","Likelihood","Posterior"),
        lty=c(3,1,2),lwd=c(3,3,3),cex=0.5) 
 
 (a+s)/(a+s+b+f)
 
 ######
 midpt <- seq(0.05, 0.95, by = 0.1)          
 prior <- c(2, 4, 8, 8, 4, 2, 1, 1, 1, 1)    
 prior <- prior/sum(prior)                   
 p <- seq(0, 1, length = 500)                
 plot(p,histprior(p,midpt,prior),type="l",  
      ylab="Prior density",ylim=c(0,.25))        
 
 like <- dbeta(p, s + 1, f + 1)
 post <- like * histprior(p, midpt, prior)
 post<-post/sum(post)

 plot(p, post, type = "l",ylab="Posterior density")
 
##post mean
sum(p*post)
 
 x <- seq(0.005,0.995,length=100)
 y <- dbinom(7, size=50, prob=x)
 plot(x,y,type='l',xlab='pi',ylab='likelihood')
 
 
 
 #fig 3.8 beta densities with different parameter values
 op <- par(mfrow=c(2,2))
 x <- seq(0.005,0.995,length=100)
 y1 <-dbeta(x,10,40)
 plot(x,y1,type='l',xlab='pi',ylab='prior density',xlim=c(0,1),ylim=c(0,8),main='Beta(10,40)')
 
 y2 <-dbeta(x,5,20)
 plot(x,y2,type='l',xlab='pi',ylab='prior density',xlim=c(0,1),ylim=c(0,8),main='Beta(5,20)')
 
 y3 <-dbeta(x,2.5,10)
 plot(x,y3,type='l',xlab='pi',ylab='prior density',xlim=c(0,1),ylim=c(0,8),main='Beta(2.5,10)')
 
 y4 <-dbeta(x,1.25,5)
 plot(x,y4,type='l',xlab='pi',ylab='prior density',xlim=c(0,1),ylim=c(0,8),main='Beta(1.25,5)')
 
 par(op)


 #fig3.9 
 
 op <- par(mfrow=c(2,2),cex.main=0.6)
 x <- seq(0.005,0.995,length=100)
 y1 <-dbeta(x,10,40)
 plot(x,y1,type='l',xlab='pi',ylab='prior density',xlim=c(0,1),ylim=c(0,8),main='Beta(10,40) \n 95% central intetval(0.102,0.32)')
 c1<-c(0.102,0.32)
 d1<-dbeta(c1,10,40)
 lines( c1, d1,type='h' ,col='red')
 
 y2 <-dbeta(x,5,20)
 plot(x,y2,type='l',xlab='pi',ylab='prior density',xlim=c(0,1),ylim=c(0,8),main='Beta(5,20)  \n 95% central intetval(0.071,0.374 )')
 c2 <- c(0.071,0.374 )
 d2<-dbeta(c2,5,20)
 lines( c2, d2,type='h' ,col='red')
 
 
 y3 <-dbeta(x,2.5,10)
 plot(x,y3,type='l',xlab='pi',ylab='prior density',xlim=c(0,1),ylim=c(0,8),main='Beta(2.5,10)\n 95% central intetval (0.038,0.451 )')
 c3 <- c(0.038,0.451 )
 d3<-dbeta(c3,2.5,10)
 lines( c3, d3,type='h' ,col='red')
 
 y4 <-dbeta(x,1.25,5)
 plot(x,y4,type='l',xlab='pi',ylab='prior density',xlim=c(0,1),ylim=c(0,8),main='Beta(1.25,5)\n 95% central intetval (0.012,0.56 )')
 c4 <- c(0.012,0.56 )
 d4<-dbeta(c4,1.25,5)
 lines( c4, d4,type='h' ,col='red')
 par(op)
 
 
 
 
 