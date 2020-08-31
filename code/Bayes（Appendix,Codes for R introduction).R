
#############Part I: some commands in R##################
library(ISwR)

#########chunk 1 ############
###An overgrown calculator###
2+2
exp(-2)
rnorm(150)
#

## Assignments#############
x<-2
x

###chunk 2 #################
####Vectorized arithmetic###
weight <- c(60, 72, 57, 90, 95, 72)
height <- c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
bmi <- weight/height^2
sum(weight)
sum(weight)/length(weight)
xbar <- sum(weight)/length(weight)
weight - xbar
(weight - xbar)^2
sum((weight - xbar)^2)
sqrt(sum((weight - xbar)^2)/(length(weight) - 1))

mean(weight)
sd(weight)


###chunk 3 #################
####Standard procedures(functions)######
t.test(bmi, mu=22.5)


###chunk 4 ############################
########  Graphics     ################
plot(height,weight)
plot(height, weight, pch=2)
hh <- c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
lines(hh, 22.5 * hh^2)
## An excellent package for plot is "ggplot2"#####


##########Part II: the essentials of R #############

######chunk 5 ####################
######Vectors####################

c("Huey","Dewey","Louie")

c('Huey','Dewey','Louie')

cat(c("Huey","Dewey","Louie") )


c(T,T,F,T)

bmi > 25



######Functions that create vectors######
c(42,57,12,39,1,3,4)
x <- c(1, 2, 3)
y <- c(10, 20)
c(x, y, 5)
x <- c(red="Huey", blue="Dewey", green="Louie")


c(FALSE, 3)
c(pi, "abc")
c(FALSE, "abc")


seq(4,9)
seq(4,10,2)
4:9


oops <- c(7,9,13)
rep(oops,3)
rep(oops,1:3)
rep(1:2,c(10,15))



######Matrices and arrays##########
x <- 1:12
dim(x) <- c(3,4)
matrix(1:12,nrow=3,byrow=T)
x <- matrix(1:12,nrow=3,byrow=T)
rownames(x) <- LETTERS[1:3]
t(x)
cbind(A=1:4,   B=5:8,C=9:12)
rbind(A=1:4,B=5:8,C=9:12)

############ Factors ###################
 pain <- c(0,3,2,2,1)
 fpain <- factor(pain,levels=0:3)
 levels(fpain) <- c("none","mild","medium","severe")
 
 
 ############list ###################
 
 intake.pre <- c(5260,5470,5640,6180,6390,
                  6515,6805,7515,7515,8230,8770)
 
 intake.post <- c(3910,4220,3885,5160,5645,
                   4680,5265,5975,6790,6900,7335)
 
 
 a<-c("zhangsan","lisi")
 b<-matrix(1:12,nrow=3)
 
 mylist <- list(before=intake.pre,after=intake.post,a,b)
 
 
 
 ##########  Data frames  #################
 
 intake.pre <- c(5260,5470,5640,6180,6390,
                 6515,6805,7515,7515,8230,8770)
 
 intake.post <- c(3910,4220,3885,5160,5645,
                  4680,5265,5975,6790,6900,7335)
 
 d <- data.frame(intake.pre,intake.post)
 d$intake.pre
 intake.pre[5]
 intake.pre[c(3,5,7)]
 v <- c(3,5,7)
 intake.pre[v]
 intake.pre[1:5]
 intake.pre[-c(3,5,7)]
 
 
 ###Conditional selection####
 
 intake.post[intake.pre > 7000]
 intake.post[intake.pre > 7000 & intake.pre <= 8000]
 intake.pre > 7000 & intake.pre <= 8000
 
 
 #####Indexing of data frames #########
 d <- data.frame(intake.pre,intake.post)
 
 d[5,1]
 d[5,]
 d[d$intake.pre>7000,]
 sel <- d$intake.pre>7000
 d[sel,]
 d[1:2,]
 
 
 ######Grouped data and data frames####
 library(ISwR)
 energy
 exp.lean <- energy$expend[energy$stature=="lean"]
 exp.obese <- energy$expend[energy$stature=="obese"]
 l <- split(energy$expend, energy$stature)
 
 
 
 
 ######Implicit loops######
 lapply(thuesen, mean, na.rm=T)
 sapply(thuesen, mean, na.rm=T)
 replicate(10,mean(rexp(20)))
 m <- matrix(rnorm(12),4)
 apply(m, 2, min)
 tapply(energy$expend, energy$stature, median)
 
 
 ##### An example #######################
 library(LearnBayes)
 
 
 ## Reading the Data into R
 
 
 setwd("E:/2.教学/1. 教学资源/1. 统计学专业课及论文指导/贝叶斯统计/贝叶斯统计（本科）/代码")
 studentdata <- read.table("studentdata.txt", sep = "\t", header = TRUE)
 teachingratings<-read.csv("teachingratings.csv",head=TRUE)
 ##   plot(teachingratings$beauty,teachingratings$eval)
 ##   abline(lm(teachingratings$eval~teachingratings$beauty))
 
 ##   library(ggplot2)
 ##   ggplot(teachingratings,aes(x=beauty,y=eval))+geom_point()+geom_smooth(method=loess)
 
 ##   library(car)
 ##   scatterplot(eval~beauty,data=teachingratings)
 
 
 
 # R Commands to Summarize and Graph a Single Batch
 
 
 data(studentdata)
 studentdata[1, ]
 
 attach(studentdata)
 
 t<-table(studentdata$Drink)
 barplot(t,xlab="Drink",ylab="Count")
 
 hours.of.sleep <- WakeUp - ToSleep
 
 summary(hours.of.sleep)
 
 hist(hours.of.sleep,main="")
 
 
 ##R Commands to Compare Batches
 boxplot(hours.of.sleep~Gender)
 title(ylab="Hours of Sleep")
 female.Haircut<-Haircut[Gender=="female"]
 male.Haircut<-Haircut[Gender=="male"]
 summary(female.Haircut)
 summary(male.Haircut)
 
 
 ##R Commands for Studying Relationships
 plot(ToSleep,hours.of.sleep)
 plot(jitter(ToSleep),jitter(hours.of.sleep))
 fit<-lm(hours.of.sleep~ToSleep)
 
 summary(fit)
 
 
 
 
 ##use the package car######

 # library(car)
 # scatterplot(ToSleep,hours.of.sleep)
 # 
 # ## use the package ggplot2##
 # library(ggplot2)
 # ggplot(data=studentdata,aes(x=ToSleep,y=hours.of.sleep))+geom_point()+geom_jitter()
 # 

 
 
 ###  an example for illustrating the simulation study in R 
 ###  
 tstatistic=function(x,y){
   m<-length(x)
   n<-length(y)
   sp<-sqrt(((m-1)*sd(x)^2+(n-1)*sd(y)^2)/(m+n-2))
   t<-(mean(x)-mean(y))/(sp*sqrt(1/m+1/n))
   return(t)
 }
 
 data.x<-c(1,4,3,6,5)
 
 data.y<-c(5,4,7,6,10)
 
 tstatistic(data.x, data.y)
 
 
 
 
 
 alpha<-.1; m<-10; n<-10 # sets alpha, m, n
 N<-10000 # sets the number of simulations
 n.reject<-0 # counter of num. of rejections
 for (i in 1:N)
 {
   ##try other distribution
  x<-rt(m,df=4)
   y<-rt(n,df=4)
   
   ##
   #x<-rexp(m,rate=1)
   #y<-rexp(n,rate=1)
   
   ##
   # x<-rnorm(m,mean=10,sd=2)
   # y<-rexp(n,rate=1/10)
   
   
   #x=rnorm(m,mean=0,sd=1) # simulates xs from population 1
   #y=rnorm(n,mean=0,sd=1) # simulates ys from population 2
   t=tstatistic(x,y) # computes the t statistic
   if (abs(t)>qt(1-alpha/2,n+m-2))
     n.reject=n.reject+1 # reject if |t| exceeds critical pt
 }
 true.sig.level<-n.reject/N # est. is proportion of rejections
 true.sig.level
 
 