setwd("E:\\Bayesian Statistics\\Homework 2")

# 3.1
a = 2
b = 7
# calculate the mean and mode
Mean = a / (a + b)
Mode = (a - 1) / (a + b - 2)
print(Mean)
print(Mode)
# calculate the median and a 90% central interval
x = seq(0, 1, length.out=1000)
y = dbeta(x, a, b)
Median = quantile(y, 0.5)
print(Median)
print(qbeta(c(0.05, 0.95), 2, 7))
# plot the density
plot(0, 0, main='probability density', xlim=c(0, 1), ylim=c(0, 4), 
     ylab='Beta(2, 7)', xlab='x')
lines(x, dbeta(x, a, b), col='red')
legend('top', legend='α=2, β=7', col='red', lwd=1)

# 3.2
plot(0, 0, main='probability density', xlim=c(0, 1), ylim=c(0, 4), 
     ylab='Beta Density', xlab='x')
lines(x, dbeta(x, 0.5, 0.5), col='red')
lines(x, dbeta(x, 10.2, 1.5), col='green')
lines(x, dbeta(x, 1.5, 10.2),col='pink')
lines(x, dbeta(x, 100, 6.2), col='orange')
legend('top', legend=c('α=0.5, β=0.5','α=10.2, β=1.5', 'α=1.5, β=10.2', 'α=100, β=6.2'), 
       col=c('red', 'green', 'pink', 'orange'), lwd=1)

# 3.3
x = seq(0, 1, length.out=1000)
plot(0, 0, main='probability density', xlim=c(0, 1), ylim=c(0, 4), 
     ylab='Beta(41, 81)', xlab='x')
lines(x, dbeta(x, a, b), col='red')
legend('top', legend='α=41, β=81', col='red', lwd=1)

plot(0, 0, main='probability density', xlim=c(0, 1), ylim=c(0, 4), 
     ylab='Beta(25, 68)', xlab='x')
lines(x, dbeta(x, a, b), col='red')
legend('top', legend='α=25, β=68', col='red', lwd=1)

# 4
a = 46
b = 106
Mean = a / (a + b)
Mode = (a - 1) / (a + b - 2)
print(Mean)
print(Mode)
plot(0, 0, main='the posterior density', xlim=c(0, 1), ylim=c(0, 12), 
     ylab='Beta(46, 106)', xlab='x')
lines(x, dbeta(x, a, b), col='red')
legend('right', legend='α=46, β=106', col='red', lwd=1)
triplot(c(40, 80), c(5, 25), "right")

# 4.1
a = 41
b = 81
y = 5
n = 30
# calculate the mean and mode
Mean = (a + y) / (a + b + n)
Mode = (a + y - 1) / (a + b + n - 2)
print(Mean)
print(Mode)
# 95% posterior interval for theta
print(qbeta(c(0.025, 0.975), a + y, b + n - y))
# Posterior probability that theta > 0.25
print(qbeta(0.25, a + y, b + n - y, lower.tail=FALSE))

# 4.2
binom.test(131, 327, p=0.5, alternative="less")
print(pbeta(0.5,132,197))
print(1-pbeta(0.5,132,197))

# 4.3
library(LearnBayes)
pbetap(c(132, 197), 20, 8)
prob = pbetap(c(132, 197), 20, 0:20)
prob1 = data.frame(0:20, prob)
prob1 = prob1[order(prob1$prob, decreasing = T),]
i = 0
while (i < nrow(prob1)) {
  if (sum(prob1[1:i, 2]) < 0.95) {
    i = i + 1
  }
  else {
    print(i)
    break
  }
}
prob1[1:i, 1]

# 5.1
a = 0.5
b = 0.5
y = 5
n = 30
# calculate the mean and mode
Mean = (a + y) / (a + b + n)
Mode = (a + y - 1) / (a + b + n - 2)
print(Mean)
print(Mode)
# 95% posterior interval for theta
print(qbeta(c(0.025, 0.975), a + y, b + n - y))
# Posterior probability that theta > 0.25
print(qbeta(0.25, a + y, b + n - y, lower.tail=FALSE))

a = 0
b = 0
y = 5
n = 30
# calculate the mean and mode
Mean = (a + y) / (a + b + n)
Mode = (a + y - 1) / (a + b + n - 2)
print(Mean)
print(Mode)
# 95% posterior interval for theta
print(qbeta(c(0.025, 0.975), a + y, b + n - y))
# Posterior probability that theta > 0.25
print(qbeta(0.25, a + y, b + n - y, lower.tail=FALSE))

# 5.2
a_1 = 41
b_1 = 81
a_2 = 25
b_2 = 68
a_3 = 1
b_3 = 1
a_4 = 0.5
b_4 = 0.5
a_5 = 0
b_5 = 0
y = 5
n = 30
# calculate the probability that π > 0.1 in case of y
print(qbeta(0.25, a_1 + y, b_1 + n - y, lower.tail=FALSE))
print(qbeta(0.25, a_2 + y, b_2 + n - y, lower.tail=FALSE))
print(qbeta(0.25, a_3 + y, b_3 + n - y, lower.tail=FALSE))
print(qbeta(0.25, a_4 + y, b_4 + n - y, lower.tail=FALSE))
print(qbeta(0.25, a_5 + y, b_5 + n - y, lower.tail=FALSE))
# calculate the mean
Mean_1 = (a_1 + y) / (a_1 + b_1 + n)
Mean_2 = (a_2 + y) / (a_2 + b_2 + n)
Mean_3 = (a_3 + y) / (a_3 + b_3 + n)
Mean_4 = (a_4 + y) / (a_4 + b_4 + n)
Mean_5 = (a_5 + y) / (a_5 + b_5 + n)
# calculate the 95% equal tail credible set
print(qbeta(c(0.025, 0.975), a_1 + y, b_1 + n - y))
print(qbeta(c(0.025, 0.975), a_2 + y, b_2 + n - y))
print(qbeta(c(0.025, 0.975), a_3 + y, b_3 + n - y))
print(qbeta(c(0.025, 0.975), a_4 + y, b_4 + n - y))
print(qbeta(c(0.025, 0.975), a_5 + y, b_5 + n - y))

# 5.4
x = seq(0, 20, length.out=100000)
y = x^11/1440*exp(-5*x)
plot(x, y, main='Likelihood Function', xlab='λ', ylab='L(λ)', col='blue')
poisson.test(x=sum(c(2, 5, 1, 0, 3)), T=5, alternative="two.sided", conf.level=0.95)

# 5.5.2
x = seq(0, 8, length.out=100)
y = dgamma(x, 4, 2)
plot(x, y, main="the Gamma Density Distribution", xlim=c(0,8), ylim=c(0,0.6), col="red", type="l")

# 5.3.4
x = seq(0, 8, length.out=100)
y = dgamma(x, 15, 7)
mean = 15 / 7
print(mean)
qgamma(c(0.025, 0.975), 15, 7)

# 5.6.3
mean = 11.5 / 5
print(mean)
qgamma(c(0.025, 0.975), 11.5, 5)

# 5.6.6
pgamma(2, 11.5, 5, lower.tail=FALSE)
# 5.6.7
pgamma(2, 15, 7, lower.tail=FALSE)
