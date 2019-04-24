library(ggplot2)
library(gridExtra)
library(MASS)
library(class)


##### Exercise 4.10 #####

library(ISLR)
attach(Weekly)

#(a)
summary(Weekly)
pairs(Weekly[,-9])

g1 <- ggplot(Weekly) + geom_boxplot(aes(x=Direction, y=Lag1))
g2 <- ggplot(Weekly) + geom_boxplot(aes(x=Direction, y=Lag2))
g3 <- ggplot(Weekly) + geom_boxplot(aes(x=Direction, y=Lag3))
g4 <- ggplot(Weekly) + geom_boxplot(aes(x=Direction, y=Lag4))
g5 <- ggplot(Weekly) + geom_boxplot(aes(x=Direction, y=Lag5))
g6 <- ggplot(Weekly) + geom_boxplot(aes(x=Direction, y=Volume))
grid.arrange(g1, g2, g3, g4, g5, g6, nrow=2)

#(b)
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data=Weekly, family=binomial)
summary(glm.fit)

#(c)
glm.prob <- predict(glm.fit, type="response")
glm.pred <- ifelse(glm.prob > 0.5, "Up", "Down")
table(glm.pred, Direction)
mean(glm.pred == Direction)
 
#(d)
train <- (Year < 2009); test <- (!train)
glm.fit <- glm(Direction ~ Lag2, Weekly, family=binomial, subset=train)
glm.prob <- predict(glm.fit, Weekly[test,], type="response")
glm.pred <- ifelse(glm.prob > 0.5, "Up", "Down")
table(glm.pred, Direction[test])
mean(glm.pred == Direction[test])

#(e)
lda.fit <- lda(Direction ~ Lag2, data=Weekly, subset=train)
lda.pred <- predict(lda.fit, Weekly[test,])
table(lda.pred$class, Direction[test])
mean(lda.pred$class == Direction[test])

#(f)
qda.fit <- qda(Direction ~ Lag2, data=Weekly, subset=train)
qda.pred <- predict(qda.fit, Weekly[test,])
table(qda.pred$class, Direction[test])
mean(qda.pred$class == Direction[test])

#(g)
set.seed(1)
knn.pred <- knn(matrix(Lag2[train]), matrix(Lag2[test]), Direction[train], k=1)
table(knn.pred, Direction[test])
mean(knn.pred == Direction[test])

#(h)
K <- data.frame(k=1:30, accuracy=0)
for(k in 1:30){
  knn.pred <- knn(matrix(Lag2[train]), matrix(Lag2[test]), Direction[train], k=k)
  K$accuracy[k] <- mean(knn.pred == Direction[test])
}
ggplot(K, aes(x=k, y=accuracy)) + geom_point() + geom_line()


detach(Weekly)





##### Exercise 4.11 #####

attach(Auto)

#(a)
Auto$mpg01 <- as.factor(ifelse(mpg > median(mpg), 1, 0))
table(Auto$mpg01)

#(b)
g1 <- ggplot(Auto) + geom_boxplot(aes(x=mpg01, y=cylinders))
g2 <- ggplot(Auto) + geom_boxplot(aes(x=mpg01, y=displacement))
g3 <- ggplot(Auto) + geom_boxplot(aes(x=mpg01, y=horsepower))
g4 <- ggplot(Auto) + geom_boxplot(aes(x=mpg01, y=weight))
g5 <- ggplot(Auto) + geom_boxplot(aes(x=mpg01, y=acceleration))
g6 <- ggplot(Auto) + geom_boxplot(aes(x=mpg01, y=year))
grid.arrange(g1, g2, g3, g4, g5, g6, nrow=2)

origin2 <- factor(origin, labels=origin_name)
ggplot(Auto) + geom_boxplot(aes(x=origin2, y=mpg))

#(c)
set.seed(1)
n <- nrow(Auto); train <- sample(n, n*0.8)
Auto.train <- Auto[train,]; Auto.test <- Auto[-train,]
table(Auto.train$mpg01); table(Auto.test$mpg01)

#(d)
lda.fit <- lda(mpg01 ~ cylinders + displacement + horsepower + weight, Auto.train)
lda.pred <- predict(lda.fit, Auto.test)
mean(lda.pred$class != Auto.test$mpg01)

#(e)
qda.fit <- qda(mpg01 ~ cylinders + displacement + horsepower + weight, Auto.train)
qda.pred <- predict(qda.fit, Auto.test)
mean(qda.pred$class != Auto.test$mpg01)

#(f)
glm.fit <- glm(mpg01 ~ cylinders + displacement + horsepower + weight, Auto.train,
               family = binomial)
glm.prob <- predict(glm.fit, Auto.test, type="response")
glm.pred <- ifelse(glm.prob > 0.5, 1, 0)
mean(glm.pred != Auto.test$mpg01)

#(g)
X.train <- scale(Auto.train[,(2:5)])
X.test <- scale(Auto.test[,(2:5)])

K <- data.frame(k=1:16, error=0)
for(k in 1:16){
  knn.pred <- knn(X.train, X.test, Auto.train$mpg01, k=k)
  K$error[k] <- mean(knn.pred != Auto.test$mpg01)
}
ggplot(K, aes(x=k, y=error)) + geom_point() + geom_line()

knn.pred <- knn(X.train, X.test, Auto.train$mpg01, k=7)
mean(knn.pred != Auto.test$mpg01)


detach(Auto)
