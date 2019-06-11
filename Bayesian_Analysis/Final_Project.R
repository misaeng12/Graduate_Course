library(dplyr)
library(ggplot2)
library(gridExtra)
library(rjags)
library(plyr)
library(MASS)

data <- read.csv("final.csv")

y <- data$y
X <- cbind(1, scale(model.matrix(y~., data)[,-1]))
n <- length(y)
k <- ncol(X)

# glm()
glm <- glm(y ~ ., family = binomial, data.frame(y, X[,-1]))
beta.mle <- glm$coef
var.beta.mle <- diag(vcov(glm))


### Gibbs Variable Selection

modelString="
model{
  for(j in 1:k){ gbeta[j] <- gamma[j]*beta[j] }

  for(i in 1:n){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- inprod(X[i, 1:k], gbeta[1:k])
  }

  for(j in 1:k){
    gamma[j] ~ dbern(0.5)
    beta[j] ~ dnorm(mu.beta[j], tau.beta[j])
    mu.beta[j] <- (1-gamma[j])*beta.mle[j]
    tau.beta[j] <- gamma[j]/(100*var.beta.mle[j])+(1-gamma[j])/var.beta.mle[j]
  }
}
"
writeLines(modelString, "model_GVS.txt")

dataList <- list(y=y, X=X, n=n, k=k, beta.mle=beta.mle, var.beta.mle=var.beta.mle)
initsList <- list(beta=beta.mle, gamma=rep(1, k))
nChains=3; nAdapt=500; nBurn=1000; nIter=10000

jagsModel = jags.model(file="model_GVS.txt", data=dataList, inits=initsList,
                       n.chains=nChains, n.adapt=nAdapt)
update(jagsModel, n.iter=nBurn)
codaSamples = coda.samples(jagsModel, variable.names=c("gamma", "beta"), n.iter=nIter)

1-rejectionRate(codaSamples)

## 가능한 gamma 조합의 사후확률 비교
gamma.samples <- as.matrix(codaSamples)[,1:k]
freq <- count(gamma.samples)
colnames(freq) <- c(colnames(X), "prob")
freq$prob <- freq$prob/nrow(gamma.samples)
post.prob <- arrange(freq, -prob); post.prob[1:10,]

cor(data$kospi, data$near_cafe)

i <- 7  # i = 1~7
freq[1:104,][apply(freq[1:104,], 1, function(x){ sum(x!=freq[i,])==3 }),] %>%
  apply(1, function(x){ c<-(x!=freq[i,]); paste(colnames(freq)[c], "=", x[c]) })

i <- 17  # i = 8~17
freq[1:104,][apply(freq[1:104,], 1, function(x){ sum(x!=freq[i,])==2 }),] %>%
  apply(1, function(x){ c<-(x!=freq[i,]); paste(colnames(freq)[c], "=", x[c]) })

freq[c(5, 37),]

# 최종 모델 => M5 (M37과 near_cafe, near_rest만 다르고 나머지는 동일)
freq[5,]


## MLE, stepwise 방법과 비교
summary(glm)
glm.step <- stepAIC(glm, direction="both")


## 유의한 변수 시각화
df1 <- group_by(data, y) %>% summarise(total_area=mean(total_area))
ggplot(df1, aes(as.factor(y), total_area)) + geom_col(width=0.5) +
  labs(title=expression(paste("평균 총면적 (", m^2, ")")), x="", y="") +
  scale_x_discrete(labels=c("1년 이상 영업", "1년 내 폐업"))

df2 <- group_by(data, floor) %>% summarise(y=mean(y))
ggplot(df2, aes(floor, y)) + geom_col(width=0.8) + labs(title="1년 내 폐업할 확률", y="") +
  scale_x_discrete(limits=arrange(df2, y)$floor, name="floor")

df3 <- group_by(data, gu) %>% summarise(y=mean(y))
ggplot(df3, aes(gu, y)) + geom_col() + labs(title="1년 내 폐업할 확률", y="") +
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_x_discrete(limits=arrange(df3, y)$gu, name="행정구")



### Modeling

data.final <- select(data, y, kospi, total_area, floor, franchise, gu,
                     living_pop, household, near_cafe, crosswalk, blog)
X <- cbind(1, scale(model.matrix(y~., data.final)[,-1]))
k <- ncol(X)

glm.out <- glm(y ~ X - 1, family=binomial, data.final)
beta.mle <- glm.out$coef
var.beta.mle <- diag(vcov(glm.out))

modelString="
model{
  for(i in 1:n){
    y[i] ~ dbern(p[i])
    logit(p[i]) <- inprod(X[i, 1:k], beta[1:k])
  }
  for(j in 1:k){ beta[j] ~ dnorm(mu.beta[j], tau.beta[j]) }
}
"
writeLines(modelString, "model.txt")

dataList <- list(y=y, X=X, n=n, k=k, mu.beta=beta.mle, tau.beta=1/(100*var.beta.mle))
initsList <- list(beta=beta.mle)
nChains=3; nAdapt=500; nBurn=1000; nIter=10000

jagsModel = jags.model(file="model.txt", data=dataList, inits=initsList,
                       n.chains=nChains, n.adapt=nAdapt)
update(jagsModel, n.iter=nBurn)
codaSamples = coda.samples(jagsModel, variable.names=c("beta"), n.iter=nIter)

1-rejectionRate(codaSamples)


## 수렴 진단
colnames(X)[1] <- "intercept"
par(mfrow=c(4, 2))
for(i in 1:k){ traceplot(codaSamples[,i], xlab=colnames(X)[i]) }
par(mfrow=c(4, 2))
for(i in 1:k){ acf(codaSamples[,i][[1]], plot=T, main=colnames(X)[i]) }
gelman.diag(codaSamples)$mpsrf

## 사후추론
MCMCSamples <- as.matrix(codaSamples)
beta.hat <- colMeans(MCMCSamples)
beta <- matrix(beta.hat, nrow=1); colnames(beta) <- colnames(X); beta
HPD <- round(apply(MCMCSamples, 2, quantile, probs=c(0.025, 0.975)), 4)
colnames(HPD) <- colnames(X); HPD
for(i in 1:k){ ggplot() + geom_density(aes(MCMCSamples[,i])) + xlab(colnames(X)[i]) }



