```r
require(rjags)
library(data.table)
```

### 예 10.1 (Gibbs Variable Selection)

```r
#### Simulation Data ####
n=200; k=8
x=matrix(rnorm(n*k), n, k)
z=rnorm(n)
for(i in 1:4){ x[,i]=x[,i]+2*z }  # x1, ..., x4의 상관계수가 약 0.8이 되도록 만들기
beta0.true=0
beta.true=rep(0,k)
for(m in 1:k){beta.true[m]=0.5**(m-1)}
y = as.vector(beta0.true + x%*% beta.true + rnorm(n))
      
#### Model ####
modelString ="
model{ 

  for(j in 1:k){ gbeta[j] <- gamma[j]*beta[j] }
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], invsigsq)
    mu[i] <- gamma0*beta0 + inprod(x[i, 1:k], gbeta[1:k])
  }

  gamma0 ~ dbern(0.5)
  for(j in 1:k){ gamma[j] ~ dbern(0.5) }
          
  beta0 ~ dnorm(m.b0, tau.b0)
  m.b0 <- (1-gamma0)*mu.beta0
  tau.b0 <- gamma0*0.01 + (1-gamma0)/var.beta0

  for(j in 1:k){
    beta[j] ~ dnorm(m.b[j], tau.b[j])
    m.b[j] <- (1-gamma[j])*mu.beta[j]
    tau.b[j]<- gamma[j]*0.01 + (1-gamma[j])/var.beta[j]
  }

  invsigsq ~ dgamma(0.01, 0.01)

}
"
write(modelString, file="model_GVS.txt")

## 사전 모수 설정
lm.out=lm(y~x)
mu.beta0=lm.out$coef[1]
var.beta0=vcov(lm.out)[1,1]
mu.beta=lm.out$coef[2:(k+1)]
var.beta=diag(vcov(lm.out)[2:(k+1), 2:(k+1)])

dataList=list(n=n, k=k, y=y, x=x, mu.beta0=mu.beta0, var.beta0=var.beta0,
              mu.beta=mu.beta, var.beta=var.beta)
gammaInit=rep(1, k)
initsList=list(gamma0=1, gamma=gammaInit, beta0=mu.beta0, beta=mu.beta)
nChains=3; nIter=10000
jagsModel=jags.model(file="model_GVS.txt", data=dataList, inits=initsList,
                    n.chains=nChains, n.adapt=3000)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Graph information:
##    Observed stochastic nodes: 200
##    Unobserved stochastic nodes: 19
##    Total graph size: 2497
## 
## Initializing model
```

```r
update(jagsModel, n.iter=10000)
codaSamples=coda.samples(jagsModel, variable.names=c("gamma0", "gamma", "beta0", "beta"),
                         n.chains=nChains, n.iter=nIter)

## gamma 추정치 선택
K=k+1
m=as.matrix(codaSamples)[, (K+1):(K+K)]  #Samples of gamma
mm=as.data.table(m)[, .N, by = c(eval(paste0("gamma[", seq_len(k), "]")), "gamma0")]
mm.order=order(mm$N, decreasing=T)
mm$N=round(mm$N/(nIter*nChains), 4)
mm[mm.order[1:10]]
```

```
##     gamma[1] gamma[2] gamma[3] gamma[4] gamma[5] gamma[6] gamma[7]
##  1:        1        1        1        0        0        0        0
##  2:        1        1        0        0        0        0        0
##  3:        1        1        1        0        0        0        0
##  4:        1        1        0        1        0        0        0
##  5:        1        1        1        1        0        0        0
##  6:        1        1        1        0        1        0        0
##  7:        1        1        0        0        0        0        0
##  8:        1        1        1        0        0        0        1
##  9:        1        1        1        0        0        1        0
## 10:        1        1        1        0        0        0        0
##     gamma[8] gamma0      N
##  1:        0      0 0.7180
##  2:        0      0 0.1090
##  3:        1      0 0.0466
##  4:        0      0 0.0388
##  5:        0      0 0.0315
##  6:        0      0 0.0128
##  7:        1      0 0.0087
##  8:        0      0 0.0069
##  9:        0      0 0.0069
## 10:        0      1 0.0057
```

```r
gamma.hat=as.numeric(mm[which.max(mm$N)])[1:K]
gamma.hat
```

```
## [1] 1 1 1 0 0 0 0 0 0
```

### 예 10.2 (Spike-and-Slab)

```r
dd=read.table("baseball.txt", header=T, sep=",")
colnames(dd)[1]<-"y"
dd$y=log(dd$y)
y=dd$y
n=nrow(dd)
x=dd[,-1]
k=ncol(x)

modelString ="
model{ 
  for(i in 1:n){
    y[i] ~ dnorm(mu[i], invsigsq)
    mu[i] <- beta0 + inprod(x[i, 1:k], beta[1:k])
}

gamma0~ dbern(0.5)
for(j in 1:k){  gamma[j] ~ dbern(0.5) }

beta0~ dnorm( mu.b0, tau.b0)
mu.b0 <-0
tau.b0<- (1-gamma0)/0.0001 + gamma0/100
for(j in 1:k){
beta[j]~ dnorm( mu.b[j], tau.b[j])
mu.b[j] <- 0
tau.b[j]<- (1-gamma[j])/0.0001 + gamma[j]/100 
}
invsigsq ~ dgamma(0.01, 0.01)
}
"
write(modelString , file="model_SSVS.txt")

lm.out=lm(y ~ ., data=dd)
mu.beta0=lm.out$coef[1]
mu.beta=lm.out$coef[2:(k+1)]

dataList=list(n=n, k=k, y=y, x=x)
gammaInit=rep(1,k)
initsList=list(beta0=mu.beta0, beta=mu.beta, gamma0=1, gamma=gammaInit)

nChains=3; nIter=10000
jagsModel=jags.model(file="model_SSVS.txt", data=dataList, inits=initsList,
                    n.chains=nChains, n.adapt=3000)
```

```
## Compiling model graph
##    Resolving undeclared variables
##    Allocating nodes
## Graph information:
##    Observed stochastic nodes: 337
##    Unobserved stochastic nodes: 35
##    Total graph size: 6852
## 
## Initializing model
```

```r
update(jagsModel, n.iter=10000)
codaSamples=coda.samples(jagsModel, variable.names=c("gamma0", "gamma", "beta0", "beta"), n.chains=nChains, n.iter=10000)

gamma.hat=colMeans(as.matrix(codaSamples))[(k+2):(2*(k+1))]
gamma.hat
```

```
##    gamma[1]    gamma[2]    gamma[3]    gamma[4]    gamma[5]    gamma[6] 
## 0.150633333 0.072200000 0.001066667 0.001266667 0.001366667 0.003733333 
##    gamma[7]    gamma[8]    gamma[9]   gamma[10]   gamma[11]   gamma[12] 
## 0.001600000 0.001433333 0.001266667 0.001100000 0.001366667 0.001066667 
##   gamma[13]   gamma[14]   gamma[15]   gamma[16]      gamma0 
## 1.000000000 0.161866667 1.000000000 0.017266667 1.000000000
```
