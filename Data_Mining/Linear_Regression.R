### Exercise 3.8
library(ISLR)
data(Auto)

#(a)
slr <- lm(mpg ~ horsepower, Auto)
summary(slr)
predict(slr, newdata=data.frame(horsepower=98), interval=c("confidence"))
predict(slr, newdata=data.frame(horsepower=98), interval=c("predict"))

#(b)
library(ggplot2)
ggplot(Auto) + geom_point(aes(x = horsepower, y = mpg)) +
  geom_abline(intercept = slr$coef[1], slope = slr$coef[2])

#(c)
par(mfrow=c(2, 2))
plot(slr)



### Exercise 3.9

#(a)
pairs(Auto[,-9])

#(b)
cor(Auto[,-9])

#(c)
mlr <- lm(mpg ~ . - name, Auto)
summary(mlr)
AIC(mlr)

#(d)
par(mfrow=c(2, 2))
plot(mlr)

#(e)
mlr2 <- lm(mpg ~ (. - name)*(. - name), Auto)
summary(mlr2)
AIC(mlr2)

#(f)
ggplot(Auto) + geom_point(aes(x=displacement, y=mpg))
ggplot(Auto) + geom_point(aes(x=log(displacement), y=mpg))
ggplot(Auto) + geom_point(aes(x=horsepower, y=mpg))
ggplot(Auto) + geom_point(aes(x=log(horsepower), y=mpg))
ggplot(Auto) + geom_point(aes(x=weight, y=mpg))
ggplot(Auto) + geom_point(aes(x=log(weight), y=mpg))

mlr3 <- lm(mpg ~ cylinders + log(displacement) + log(horsepower) + log(weight) +
                 acceleration + year + origin, Auto)
summary(mlr3)
AIC(mlr3)



### Exercise 3.13

#(a)
set.seed(1)
x <- rnorm(100)
mean(x); sd(x)

#(b)
eps <- rnorm(100, mean=0, sd=sqrt(0.25))
mean(eps); sd(eps)

#(c)
y <- -1 + 0.5*x + eps

#(d)
ggplot() + geom_point(aes(x = x, y = y))

#(e)
lm <- lm(y ~ x)
summary(lm)
AIC(lm)

#(f)
lty <- c("estimated", "true")
ggplot() + geom_point(aes(x = x, y = y)) +
  geom_abline(aes(intercept = lm$coef[1], slope = lm$coef[2], lty=lty)) +
  geom_abline(aes(intercept = -1, slope = 0.5, lty=lty), lty=2) +
  scale_linetype_manual(name = "", values = c(1, 2))

#(g)
lm.poly <- lm(y ~ x + I(x^2))
summary(lm.poly)
AIC(lm.poly)

#(h)
set.seed(2)
x2 <- rnorm(100)
mean(x2); sd(x2)

eps2 <- rnorm(100, mean=0, sd=sqrt(0.1))
mean(eps2); sd(eps2)

y2 <- -1 + 0.5*x2 + eps2

ggplot() + geom_point(aes(x = x2, y = y2)) + labs(x = "x", y = "y")

lm2 <- lm(y2 ~ x2)
summary(lm2)
AIC(lm2)

ggplot() + geom_point(aes(x = x2, y = y2)) + labs(x = "x", y = "y") +
  geom_abline(aes(intercept = lm2$coef[1], slope = lm2$coef[2], lty=lty)) +
  geom_abline(aes(intercept = -1, slope = 0.5, lty=lty), lty=2) +
  scale_linetype_manual(name = "", values = c(1, 2))


#(i)
set.seed(3)
x3 <- rnorm(100)
mean(x3); sd(x3)

eps3 <- rnorm(100, mean=0, sd=sqrt(0.4))
mean(eps3); sd(eps3)

y3 <- -1 + 0.5*x3 + eps3

ggplot() + geom_point(aes(x = x3, y = y3)) + labs(x = "x", y = "y")

lm3 <- lm(y3 ~ x3)
summary(lm3)
AIC(lm3)

ggplot() + geom_point(aes(x = x3, y = y3)) + labs(x = "x", y = "y") +
  geom_abline(aes(intercept = lm3$coef[1], slope = lm3$coef[2], lty=lty)) +
  geom_abline(aes(intercept = -1, slope = 0.5, lty=lty), lty=2) +
  scale_linetype_manual(name = "", values = c(1, 2))


#(j)
confint(lm)
confint(lm2)
confint(lm3)



### Exercise 3.14

#(a)
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

#(b)
cor(x1, x2)
ggplot() + geom_point(aes(x1, x2))

#(c)
lm.c <- lm(y ~ x1 + x2)
summary(lm.c)

#(d)
lm.d <- lm(y ~ x1)
summary(lm.d)

#(e)
lm.e <- lm(y ~ x2)
summary(lm.e)

#(g)
x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)

lm.c2 <- lm(y ~ x1 + x2)
summary(lm.c2)
plot(lm.c2)

lm.d2 <- lm(y ~ x1)
summary(lm.d2)
plot(lm.d2)

lm.e2 <- lm(y ~ x2)
summary(lm.e2)
plot(lm.e2)
