library(dplyr)
library(ggplot2)
library(gridExtra)
library(rjags)
library(plyr)
library(MASS)

setwd("C:/Users/user/Misaeng/수업/2019-1/베이지안통계특론/프로젝트")
data <- read.csv("final_floor0607.csv")
data2 <- read.csv("final_0606_2.csv")
data2 <- select(data2, road_name, near_rest=near, near_cafe=nearcafe,
                near_fran_cafe=near_franchise, kospi=kospi_avg_lag1)
final <- left_join(data, data2)

### 데이터 전처리
data <- filter(final, days > 30 & !grepl("행사", close_reason) &
                 !grepl("한시", close_reason) & !grepl("단기", close_reason) &
                 !grepl("착오", close_reason) & !grepl("박람회", close_reason))
data$y <- as.numeric(data$days <= 365)
data <- select(data, y, month, kospi, total_area, floor, total_floor, franchise,
               gu, floating_pop, living_pop, office_pop, female, income=income_grade,
               household=household_num, near_rest, near_cafe, near_fran_cafe,
               parking_lot, crosswalk, subway, blog, area_st_date)
summary(data)

data[str_length(data$floor)==0,]$floor <- NA
data$floor <- as.factor(as.character(data$floor))
data[str_length(data$gu)==0,]$gu <- NA
data$gu <- as.factor(as.character(data$gu))
data$floating_pop <- as.numeric(sub(",", "", data$floating_pop))
data$office_pop <- as.numeric(sub(",", "", data$office_pop))
data$income <- as.numeric(sub("분위", "", data$income))
summary(data)

data <- data[complete.cases(data),]                               
#data <- rbind(unique(filter(data, y==0)), filter(data, y==1))        # n = 13589
data <- unique(data)
data <- filter(data, as.Date(area_st_date) < "2018-05-01") %>% select(-area_st_date) # n=8116
table(data$y)


### train set / test set 나누기
#train.data <- filter(data, as.Date(area_st_date) < "2017-05-01") %>% select(-area_st_date)
#test.data <- filter(data, as.Date(area_st_date) >= "2017-05-01") %>% select(-area_st_date)
#(n.train <- nrow(train.data))                                        # n.train = 5443
#(n.test <- nrow(test.data))                                          # n.test = 2408
#c(sum(train.data$y==1)/n.train, sum(test.data$y==1)/n.test)          # y=1 비율
# oversampling
#train.data2 <- rbind(train.data, filter(train.data, y==1))



### Gibbs Variable Selection

y <- data$y
X <- cbind(1, scale(model.matrix(y~., data)[,-1]))
n <- length(y)
k <- ncol(X)

# glm()
glm <- glm(y ~ ., family = binomial, data.frame(y, X[,-1]))
#table(as.numeric(predict(glm, type="response")>0.5), data$y)
#table(as.numeric(predict(glm, type="response", test.data)>0.3), test.data$y)
beta.mle <- glm$coef
var.beta.mle <- diag(vcov(glm))

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
                       n.chains=nChains, n.adapt=nAdapt)  # 약 1시간
update(jagsModel, n.iter=nBurn)                           # 약 1시간 30분?
codaSamples = coda.samples(jagsModel, variable.names=c("gamma", "beta"), n.iter=nIter)
                                                          # 17시간 20분
#1-rejectionRate(codaSamples)  # beta(1) / gamma(0~0.24) ???

gamma.samples <- as.matrix(codaSamples)[,1:k]  # 30000*49
freq <- count(gamma.samples)
colnames(freq) <- c(colnames(X), "prob")
freq$prob <- freq$prob/nrow(gamma.samples)
post.prob <- arrange(freq, -prob); post.prob[1:10,]

cor(data$kospi, data$near_cafe)
#1,2 -> kospi, near_rest -> 상관관계 크지 않음
## 상관관계 큰 것: quarter와 month(0.97), floor와 total_floor(0.7 이상), near들(0.8 이상)
##                 중구와 office_pop, parking_lot, subway(0.5 이상)

i <- 7
freq[1:104,][apply(freq[1:104,], 1, function(x){ sum(x!=freq[i,])==3 }),] %>%
  apply(1, function(x){ c<-(x!=freq[i,]); paste(colnames(freq)[c], "=", x[c]) })
# (1, 207) => 0.0005 
# (5, 37) => 0.00053333

i <- 17  # i=8~17: prob=0.0002666667
freq[1:104,][apply(freq[1:104,], 1, function(x){ sum(x!=freq[i,])==2 }),] %>%
  apply(1, function(x){ c<-(x!=freq[i,]); paste(colnames(freq)[c], "=", x[c]) })

## 최종 모델 => M5 (37번 모형과 주변 카페 수와 주변 음식점 수만 다르고 나머지는 동일)
freq[5,]

# MLE, stepwise 방법과 비교
summary(glm)
glm.step <- stepAIC(glm, direction="both")

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
                       n.chains=nChains, n.adapt=nAdapt)                       # 32분
update(jagsModel, n.iter=nBurn)                                                # 1시간
codaSamples = coda.samples(jagsModel, variable.names=c("beta"), n.iter=nIter)  # 13시간

# 채택확률
1-rejectionRate(codaSamples)                                                   # 1 ???


### 수렴 진단
colnames(X)[1] <- "intercept"
par(mfrow=c(4, 2))
for(i in 1:k){ traceplot(codaSamples[,i], xlab=colnames(X)[i]) }
par(mfrow=c(4, 2))
for(i in 1:k){ acf(codaSamples[,i][[1]], plot=T, main=colnames(X)[i]) }
gelman.diag(codaSamples)$mpsrf  # Gelman 상수 < 1.1

### 사후추론 (중심화 돌려놓기..???)
MCMCSamples <- as.matrix(codaSamples)
beta.hat <- colMeans(MCMCSamples)
beta <- matrix(beta.hat, nrow=1); colnames(beta) <- colnames(X); beta
HPD <- round(apply(MCMCSamples, 2, quantile, probs=c(0.025, 0.975)), 4)
colnames(HPD) <- colnames(X); HPD

ggplot() + geom_density(aes(MCMCSamples[,i])) + xlab(colnames(X)[i])