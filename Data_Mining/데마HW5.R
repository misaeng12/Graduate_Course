library(ggplot2)
library(ISLR)
library(MASS)
library(boot)

### Exercise 5.2

#(g)
n <- 1:100000
ggplot() + geom_point(aes(x=n, y=1-(1-1/n)^n)) + ylim(0, 1)
n <- 1:20; y <- 1-(1-1/n)^n
ggplot() + geom_point(aes(x=n, y=y)) + ylim(0, 1) + geom_line(aes(x=n, y=y))

#(h)
x <- c()
for(i in 1:10000){
  x[i] <- ( 4 %in% sample(100, replace=T) )
}
mean(x)


### Exercise 5.5

#(a)
glm <- glm(default ~ income + balance, Default, family="binomial")
summary(glm)

#(b)
set.seed(1)
n <- nrow(Default); train <- sample(n, n/2)
glm <- glm(default ~ income + balance, Default[train,], family="binomial")
glm.prob <- predict(glm, Default[-train,], type="response")
contrasts(Default$default)
glm.pred <- ifelse(glm.prob > 0.5, "Yes", "No")
mean(glm.pred != Default$default[-train])

#(c)
error <- c()
for(i in 1:3){
  train <- sample(n, n/2)
  glm <- glm(default ~ income + balance, Default[train,], family="binomial")
  glm.prob <- predict(glm, Default[-train,], type="response")
  glm.pred <- ifelse(glm.prob > 0.5, "Yes", "No")
  error[i] <- mean(glm.pred != Default$default[-train])
}
error

#(d)
set.seed(1)
train <- sample(n, n/2)
glm2 <- glm(default ~ income + balance + student, Default[train,], family="binomial")
glm.prob <- predict(glm2, Default[-train,], type="response")
glm.pred <- ifelse(glm.prob > 0.5, "Yes", "No")
mean(glm.pred != Default$default[-train])


### Exercise 5.7

#(a)
glm <- glm(Direction ~ Lag1 + Lag2, Weekly, family="binomial")
summary(glm)

#(b)
glm1 <- glm(Direction ~ Lag1 + Lag2, Weekly[-1,], family="binomial")
summary(glm1)

#(c)
glm1.pred <- predict(glm1, Weekly[1,], type="response")
ifelse(glm1.pred > 0.5, "Up", "Down") == Weekly$Direction[1]

#(d)
n <- nrow(Weekly); error <- c()
for(i in 1:n){
  glm <- glm(Direction ~ Lag1 + Lag2, Weekly[-i,], family="binomial")
  glm.pred <- predict(glm, Weekly[i,], type="response")
  error[i] <- ifelse(glm.pred > 0.5, "Up", "Down") != Weekly$Direction[i]
}

#(e)
mean(error)


### Exercise 5.9

#(a)
( mu <- mean(Boston$medv) )

#(b)
n <- nrow(Boston); sd(Boston$medv)/sqrt(n)

#(c)
mean.fn <- function(var, index){ mean(var[index]) }
( bs <- boot(Boston$medv, mean.fn, R=1000) )

#(d)
c(bs$t0 - 2*0.4135872, bs$t0 + 2*0.4135872)
t.test(Boston$medv)$conf.int

#(e)
median(Boston$medv)

#(f)
med.fn <- function(var, index){ median(var[index]) }
boot(Boston$medv, med.fn, R=1000)

#(g)
quantile(Boston$medv, 0.1)

#(h)
q0.1.fn <- function(var, index){ quantile(var[index], 0.1) }
boot(Boston$medv, q0.1.fn, R=1000)
