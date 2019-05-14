library(ggplot2)
library(e1071)
library(ISLR)

### Exercise 9.4

## Generating data set
set.seed(1)
x <- matrix(rnorm(100*2), ncol=2)
x[1:30,] <- x[1:30,] + 2
x[31:60,] <- x[31:60,] - 2
y <- c(rep(1, 60), rep(2, 40))
data <- data.frame(y=as.factor(y), x1=x[,1], x2=x[,2])
ggplot(data, aes(x=x1, y=x2, color=y, shape=y)) + geom_point(size=2)

set.seed(1)
train <- sample(100, 50)

svm.linear <- svm(y~., data[train,], kernel = "linear")

(tune.svm1 <- tune(svm, y~., data=data[train,], kernel = "poly",
                   ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                                 degree = c(2, 3, 4, 5, 6))))
svm.poly <- tune.svm1$best.model

(tune.svm2 <- tune(svm, y~., data=data[train,], kernel = "radial",
                   ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                                 gamma = c(0.01, 0.1, 0.5, 1, 2))))
svm.radial <- tune.svm2$best.model

plot(svm.linear, data[train,])
plot(svm.poly, data[train,])
plot(svm.radial, data[train,])

pred.linear <- predict(svm.linear, data)
pred.poly <- predict(svm.poly, data)
pred.radial <- predict(svm.radial, data)
(misclass <- table(predict=pred.linear[train], truth=data$y[train]))
(misclass <- table(predict=pred.poly[train], truth=data$y[train]))
(misclass <- table(predict=pred.radial[train], truth=data$y[train]))

plot(svm.linear, data[-train,])
plot(svm.poly, data[-train,])
plot(svm.radial, data[-train,])

(misclass <- table(predict=pred.linear[-train], truth=data$y[-train]))
(misclass <- table(predict=pred.poly[-train], truth=data$y[-train]))
(misclass <- table(predict=pred.radial[-train], truth=data$y[-train]))



### Exercise 9.5

#(a)
set.seed(1)
x <- matrix(rnorm(500*2), ncol=2)
x[1:150,] <- x[1:150,] + 2.5
x[151:300,] <- x[151:300,] - 2.5
y <- c(rep(1, 300), rep(2, 200))
data <- data.frame(y=as.factor(y), x1=x[,1], x2=x[,2])
#x1 <- runif(500) - 0.5
#x2 <- runif(500) - 0.5
#y <- 1*(x1^2-x2^2 > 0)
#data <- data.frame(y=as.factor(y), x1=x1, x2=x2)

#(b)
ggplot(data, aes(x=x1, y=x2, color=y, shape=y)) + geom_point(size=2) +
  ggtitle("Generated Data")

#(c)
glm <- glm(y~., data, family="binomial")

#(d)
set.seed(1)
train <- sample(500, 250)
glm.pred <- predict(glm, data[train,], type="response")
y.pred <- as.factor(ifelse(glm.pred>0.5, 1, 0))
ggplot(data[train,], aes(x=x1, y=x2, color=y.pred, shape=y.pred)) +
  geom_point(size=2) + ggtitle("Logistic Regression")

#(e)~(f)
glm2 <- glm(y ~ poly(x1, 2) + poly(x2, 2), data, family="binomial"); glm2
glm2.pred <- predict(glm2, data[train,], type="response")
y.pred2 <- as.factor(ifelse(glm2.pred>0.5, 1, 0))
ggplot(data[train,], aes(x=x1, y=x2, color=y.pred2, shape=y.pred2)) +
  geom_point(size=2) + ggtitle("Non-linear Logistic Regression")

#(g)
svm.linear <- svm(y~., data, kernel="linear")
svm1.pred <- predict(svm.linear, data[train,])
ggplot(data[train,], aes(x=x1, y=x2, color=svm1.pred, shape=svm1.pred)) +
  geom_point(size=2) + ggtitle("Linear Kernel SVM")

#(h)
(tune.svm <- tune(svm, y~., data=data, kernel = "radial",
                  ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                                gamma = c(0.01, 0.1, 0.5, 1, 2))))
svm.radial <- tune.svm$best.model
svm2.pred <- predict(svm.radial, data[train,])
ggplot(data[train,], aes(x=x1, y=x2, color=svm2.pred, shape=svm2.pred)) +
  geom_point(size=2) + ggtitle("Radial Kernel SVM")



### Exercise 9.8

#(a)
set.seed(1)
train <- sample(nrow(OJ), 800)

#(b)
svm.fit <- svm(Purchase~., OJ[train,], kernel="linear", cost=0.01)
summary(svm.fit)

#(c)
pred <- predict(svm.fit, OJ)
(mis.train <- table(predict=pred[train], truth=OJ$Purchase[train]))
(78+55)/800
(mis.test <- table(predict=pred[-train], truth=OJ$Purchase[-train]))
(31+18)/(nrow(OJ)-800)

#(d)
(tune.svm <- tune(svm, Purchase~., data=OJ[train,], kernel="linear",
                  ranges=list(cost=c(0.01, 0.1, 1, 10))))

#(e)
pred2 <- predict(tune.svm$best.model, OJ)
(mis.train <- table(predict=pred2[train], truth=OJ$Purchase[train]))
(73+54)/800
(mis.test <- table(predict=pred2[-train], truth=OJ$Purchase[-train]))
(33+19)/(nrow(OJ)-800)

#(f)
svm2.fit <- svm(Purchase~., OJ[train,], kernel="radial", cost=0.01)
summary(svm2.fit)
pred <- predict(svm2.fit, OJ)
(mis.train <- table(predict=pred[train], truth=OJ$Purchase[train]))
306/800
(mis.test <- table(predict=pred[-train], truth=OJ$Purchase[-train]))
(111)/(nrow(OJ)-800)

(tune.svm2 <- tune(svm, Purchase~., data=OJ[train,], kernel="radial",
                  ranges=list(cost=c(0.01, 0.1, 1, 10))))
pred2 <- predict(tune.svm2$best.model, OJ)
(mis.train <- table(predict=pred2[train], truth=OJ$Purchase[train]))
(77+39)/800
(mis.test <- table(predict=pred2[-train], truth=OJ$Purchase[-train]))
(28+18)/(nrow(OJ)-800)

#(g)
svm3.fit <- svm(Purchase~., OJ[train,], kernel="poly", cost=0.01, degree=2)
summary(svm3.fit)
pred <- predict(svm3.fit, OJ)
(mis.train <- table(predict=pred[train], truth=OJ$Purchase[train]))
306/800
(mis.test <- table(predict=pred[-train], truth=OJ$Purchase[-train]))
111/(nrow(OJ)-800)

(tune.svm3 <- tune(svm, Purchase~., data=OJ[train,], kernel="poly", degree=2,
                  ranges=list(cost=c(0.01, 0.1, 1, 10))))
pred3 <- predict(tune.svm3$best.model, OJ)
(mis.train <- table(predict=pred3[train], truth=OJ$Purchase[train]))
(72+44)/800
(mis.test <- table(predict=pred3[-train], truth=OJ$Purchase[-train]))
(31+19)/(nrow(OJ)-800)
