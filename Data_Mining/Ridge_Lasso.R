### Exercise 6.9
College <- read.csv("College.csv")
dim(College)

#(a)
n <- nrow(College)
set.seed(1)
train <- sample(n, 0.7*n); test <- (-train)

#(b)
College <- College[,-1]
lm <- lm(Apps~., College[train,])
lm.pred <- predict(lm, College[test,])
(lm.error <- mean((lm.pred-College$Apps[test])^2))

#(c)
library(glmnet)
X <- model.matrix(Apps~., College)[,-1]
y <- College$Apps

set.seed(2)
(lambda.ridge <- cv.glmnet(X[train,], y[train], alpha=0)$lambda.min)
ridge <- glmnet(X[train,], y[train], alpha=0, lambda=lambda.ridge)
ridge.pred <- predict(ridge, newx=X[test,])
(ridge.error <- mean((ridge.pred-y[test])^2))

#(d)
set.seed(2)
(lambda.lasso <- cv.glmnet(X[train,], y[train], alpha=1)$lambda.min)
lasso <- glmnet(X[train,], y[train], alpha=1, lambda=lambda.lasso)
lasso.pred <- predict(lasso, newx=X[test,])
(lasso.error <- mean((lasso.pred-y[test])^2))
predict(lasso, type="coefficients")

#(e)
library(pls)
set.seed(2)
pcr <- pcr(Apps~., data=College[train,], scale=TRUE, validation="CV")
summary(pcr)
pcr.pred <- predict(pcr, X[test,], ncomp=17)
(pcr.error <- mean((pcr.pred-y[test])^2))

#(f)
set.seed(2)
pls <- plsr(Apps~., data=College[train,], scale=TRUE, validation="CV")
summary(pls)
pls.pred <- predict(pls, X[test,], ncomp=17)
(pls.error <- mean((pls.pred-y[test])^2))



### Exercise 6.11
library(MASS)
dim(Boston)

X <- model.matrix(crim~., Boston)
y <- Boston$crim
n <- nrow(Boston); p <- ncol(Boston)-1

#(a)
set.seed(1)
train <- sample(n, 0.7*n); test <- (-train)

## best subset selection
library(leaps)

# cross-validation
k <- 10
set.seed(2)
folds <- sample(1:k, n, replace=TRUE)

cv.errors <- matrix(0, k, p)
for(i in 1:k){
  best.fit <- regsubsets(crim~., data=Boston[folds!=i,], nvmax=p)
  for(j in 1:p){
    pred <- predict(best.fit, Boston[folds==i,], id=j)
    cv.errors[i, j] <- mean((y[folds==i]-pred)^2)
  }
}
(K <- which.min(apply(cv.errors, 2, mean)))

best.subset <- regsubsets(crim~., Boston[train,], nvmax=p)
(coef <- coef(best.subset, id=K))
best.subset.pred <- X[test, names(coef)] %*% coef
(best.subset.error <- mean((best.subset.pred-y[test])^2))

X <- X[,-1]

## ridge
set.seed(2)
(lambda.ridge <- cv.glmnet(X[train,], y[train], alpha=0)$lambda.min)
ridge <- glmnet(X[train,], y[train], alpha=0, lambda=lambda.ridge)
ridge.pred <- predict(ridge, newx=X[test,])
(ridge.error <- mean((ridge.pred-y[test])^2))

## lasso
set.seed(2)
(lambda.lasso <- cv.glmnet(X[train,], y[train], alpha=1)$lambda.min)
lasso <- glmnet(X[train,], y[train], alpha=1, lambda=lambda.lasso)
lasso.pred <- predict(lasso, newx=X[test,])
(lasso.error <- mean((lasso.pred-y[test])^2))
predict(lasso, type="coefficients")

# pcr
set.seed(2)
pcr <- pcr(crim~., data=Boston[train,], scale=TRUE, validation="CV")
summary(pcr)
pcr.pred <- predict(pcr, X[test,], ncomp=13)
(pcr.error <- mean((pcr.pred-y[test])^2))

# pls
set.seed(2)
pls <- plsr(crim~., data=Boston[train,], scale=TRUE, validation="CV")
summary(pls)
pls.pred <- predict(pls, X[test,], ncomp=9)
(pls.error <- mean((pls.pred-y[test])^2))
