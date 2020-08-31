
###Table 5.2 in textbook###

# 定义似然函数
likelifun<-function(p,y,n){
  s<-p^y*(1-p)^(n-y)
  return(s)
}

p.value<-seq(0.02,0.34,by=0.04)
p.prior<-rep(0.1,length(p.value))
n<-50
y<-7

p.like<-likelifun(p.value,y,n)

prod.prior.likef<-p.like*p.prior

p.posterior<-prod.prior.likef/sum(prod.prior.likef)

result<-data.frame(p=p.value,Prior=p.prior,Likelihood=p.like,
                   Product=prod.prior.likef,Posterior=p.posterior)

result



## Figure 5.1

par(mfrow=c(2,1))

plot(p.value,p.prior,type="p",pch=19,xlim=c(0,1),ylim=c(0,0.2),xlab="Value",ylab="Density")
curve(dbinom(y,n,x),from=0.01,to=0.99,add=TRUE)

plot(p.value,p.prior,type="p",pch=17,xlim=c(0,1),ylim=c(0,max(p.prior,p.posterior)),xlab="Value",ylab="Density")
points(p.value,p.posterior,pch=19)



# Figure 5.2

midpts<-c(.05,.15,.25,.35,0.7)
prob<-c(.25,.5,.2,.05,0)
p<-seq(.01,.99,by=.01)
plot(p,histprior(p,midpts,prob),type="l")





