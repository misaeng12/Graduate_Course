library(ISLR)
library(tree)
library(randomForest)
library(gbm)
library(ggplot2)

### Exercise 8.8

#(a)
n <- nrow(Carseats)
train <- sample(n, n/2)

#(b)
tr <- tree(Sales~., Carseats[train,])
plot(tr); text(tr)
tr.pred <- predict(tr, Carseats[-train,])
mean((tr.pred-Carseats$Sales[-train])^2)

#(c)
cv.tr <- cv.tree(tr)
best.size <- cv.tr$size[which.min(cv.tr$dev)]
prune.tr <- prune.tree(tr, best=best.size)
prune.tr.pred <- predict(prune.tr, Carseats[-train,])
mean((prune.tr.pred-Carseats$Sales[-train])^2)

#(d)
p <- ncol(Carseats)-1
bag <- randomForest(Sales~., Carseats[train,], mtry=p, importance=T)
bag.pred <- predict(bag, Carseats[-train,])
mean((bag.pred-Carseats$Sales[-train])^2)
importance(bag)

#(e)
rf <- randomForest(Sales~., Carseats[train,], mtry=9, importance=T)
rf.pred <- predict(rf, Carseats[-train,])
mean((rf.pred-Carseats$Sales[-train])^2)
importance(rf)



### Exercise 8.9

#(a)
train <- sample(nrow(OJ), 800)

#(b)
tr <- tree(Purchase~., OJ[train,])
summary(tr)

#(c)
tr

#(d)
plot(tr); text(tr)

#(e)
tr.pred <- predict(tr, OJ[-train,], type="class")
table(tr.pred, OJ$Purchase[-train])
(36+17)/nrow(OJ[-train,])

#(f)
cv.tr <- cv.tree(tr, FUN=prune.misclass)

#(g)
df <- data.frame(x=cv.tr$size, y=cv.tr$dev)
ggplot(df, aes(x, y)) + geom_point() + geom_line() +
  labs(x="Tree Size", y="CV Classification Error Rate")

#(i)
prune.tr <- prune.tree(tr, best=5)

#(j)
summary(prune.tr)

#(k)
prune.tr.pred <- predict(prune.tr, OJ[-train,], type="class")
table(prune.tr.pred, OJ$Purchase[-train])
(13+47)/nrow(OJ[-train,])



### Exercise 8.10

#(a)
Hitters <- Hitters[!is.na(Hitters$Salary),]
Hitters$Salary <- log(Hitters$Salary)

#(b)
train <- 1:200

#(c)~(d)
lambda <- c(0.01, 0.1, 0.2, 0.3, 0.4, 0.5)
MSE <- data.frame(lambda=rep(lambda, 2), MSE=rep(0,2*length(lambda)),
                  legend=rep(c("Training", "Test"), each=length(lambda)))
for(i in 1:length(lambda)){
  bst <- gbm(Salary~., Hitters[train,], distribution="gaussian",
             n.tree=1000, shrinkage=lambda[i])
  bst.pred.train <- predict(bst, n.trees=1000)
  bst.pred.test <- predict(bst, Hitters[-train,], n.trees=1000)
  MSE[i, 2] <- mean((bst.pred.train-Hitters$Salary[train])^2)
  MSE[i+length(lambda), 2] <- mean((bst.pred.test-Hitters$Salary[-train])^2)
}
ggplot(MSE, aes(lambda, MSE)) + geom_point() + geom_line(aes(lty=legend)) +
  labs(x="Shrinkage", y="MSE") + guides(lty=guide_legend(title=NULL))

#(e)
lm <-lm(Salary~., Hitters[train,])
lm.pred <- predict(lm, Hitters[-train,])
(lm.err <- mean((lm.pred-Hitters$Salary[-train])^2))

library(glmnet)
X <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

lambda.ridge <- cv.glmnet(X[train,], y[train], alpha=0)$lambda.min
ridge <- glmnet(X[train,], y[train], alpha=0, lambda=lambda.ridge)
ridge.pred <- predict(ridge, newx=X[-train,])
(ridge.err <- mean((ridge.pred-y[-train])^2))

#(f)
bst <- gbm(Salary~., Hitters[train,], distribution="gaussian", n.tree=1000, shrinkage=0.2)
summary(bst)

#(g)
bag <- randomForest(Salary~., Hitters[train,], mtry=ncol(Hitters)-1)
bag.pred <- predict(bag, Hitters[-train,])
(bag.err <- mean((bag.pred-Hitters$Salary[-train])^2))
