
# uncentered covariate
x <- 1997:2010
brulemeanLogHgConcYr <- c(2.2952, 2.3435, 2.5512, 2.5531, 2.3918,
2.1546, 2.3596,
2.2431, 2.1725, 2.3162, 2.3504, 2.1926,
1.9638, 2.2025)
brulelmout <- lm(brulemeanLogHgConcYr~x)

# centered covariate
xcent <- x - mean(x)
brulelmout2 <- lm(brulemeanLogHgConcYr~xcent)

#ÄâºÏ²Ğ²îÍ¼
plot( brulelmout2$fitted.values,
brulelmout2$residuals)

qqnorm(brulelmout2$residuals)
qqline(brulelmout2$residuals)
or
library(car)
qqPlot(brulelmout2$residuals)




library(lmtest)
 dwout <- dwtest( brulelmout2,alternative="two.sided")
 dwout

summary(brulelmout)
confint(brulelmout)
summary(brulelmout2)
confint(brulelmout2)
