### 4.1 깁스표본기법 
## 4.1.1 선형회귀모형 분석
#(예 4.1)

rmvnorm<-function(n, mu,Sig){
  p=length(mu)
  R=chol(Sig)
  z=matrix(rnorm(n*p),n,p)
  tt=z%*%R + matrix(mu, n, p, byrow=T)
  return(tt)
}

setwd("C:/Users/user/Misaeng/수업/2019-1/베이지안통계특론/필기&코드")
dat=read.csv("immigrants.csv")
y=dat$wage
n=length(y)
X=cbind(rep(1,n),dat$sp, dat$lit)
p=ncol(X)

a = 1
b = 1
XtX=t(X) %*% X
XtX.inv=solve(XtX)
Xty=t(X)%*%y
beta.hat=beta_lse = as.vector(XtX.inv %*% t(X) %*% y)
sigsq.hat=sum((y - X %*% beta_lse)^2)/(n-p)
beta0=beta_lse
Sig0=diag(diag(XtX.inv))*sigsq.hat*100
Sig0.inv=solve(Sig0)

N = 10000 ; nburn=1000
sigsq.samples = rep(0,N)
beta.samples = matrix(0, N, p)

beta.init=beta_lse
sigsq.init=sigsq.hat
beta=beta.init ; sigsq=sigsq.init

# start gibbs sampling
for (iter in 1:(N+nburn)) {
  Sig.beta = solve(Sig0.inv + XtX/sigsq)
  mu.beta = Sig.beta %*%(Sig0.inv%*%beta0 + 1/sigsq*Xty)
  beta = as.vector(rmvnorm(1, mu.beta, Sig.beta))
  
  SSR = sum((y - X %*% beta)^2)#/(n-p)
  sigsq = 1/rgamma(1, n/2 + a, 1/2 *SSR + b)
  
  if (iter > nburn) {
    beta.samples[iter-nburn,] = beta
    sigsq.samples[iter-nburn] = sigsq
  }
}

#### 95% HPD interval ####
(ci_beta = round(apply(beta.samples, 2, quantile, probs = c(0.025, 0.975)),4))
(ci_sigsq = round(quantile(sigsq.samples, c(0.025, 0.975)),4))

#### Figure 4.1 예 4.1 모수에 대한 경로그림 및 자기 상관 ####
par(mfrow=c(2,2))
for (i in 1:3) plot(beta.samples[,i], type="l",xlab=paste0("beta_",i), ylab="", col="blue")
plot(sigsq.samples,xlab="sigsq",ylab="", type="l", col="blue")

#### Figure 4.2 예 4.1 사후밀도함수와 95% HPD 구간 ####
par(mfrow=c(2,2))
for (i in 1:3) {
  plot(density(beta.samples[,i]), type="l",main="",xlab=paste0("beta_",i))
  abline(v=ci_beta[,i], col=2, lty=2) }
plot(density(sigsq.samples),main="",xlab="sigsq", type="l")
abline(v=ci_sigsq, col=2, lty=2)



## 4.1.2  제한된 다변량 정규분포로부터의 표본 추출
# (예 4.2)

install.packages("truncnorm")
library(truncnorm)

k = 5
mu = c(0,1,2,3,5)
Sig = matrix(0.7, k,k) + diag(k) * 0.3
A= solve(Sig)
m = 1000
N = 10000
theta.init = c(0,1,2,3,4)    ## theta.init[1] <= theta.init[2]<=.. <= theta.init[5]
theta.samples = matrix(0, N, k)
theta=c(1:k)
for (iter in 1:(m+N)) {
  for (i in 1:k) {
    vec.Ai = A[i,-i]
    vec.mi = (theta-mu)[-i]
    cond.mean = mu[i] - 1/A[i,i] * vec.Ai %*% vec.mi
    cond.sd = 1/sqrt(A[i,i])
    a = ifelse(i == 1, -Inf, theta[i-1])
    b = ifelse(i == k, Inf, theta[i+1])
    theta[i] = rtruncnorm(1 ,as.double(a), as.double(b), cond.mean, cond.sd)
  }
  if (iter > m) theta.samples[iter-m,] = theta
}

#### Figure 4.3 예 4.2 모수의 산점도 ####
par(mfrow=c(2,2))
plot(theta.samples[,c(1,2)], xlab="theta1", ylab="theta2", col="blue")
lines(theta.samples[,1],theta.samples[,1], type="l")
plot(theta.samples[,c(2,3)], xlab="theta2", ylab="theta3", col="blue")
lines(theta.samples[,2],theta.samples[,2], type="l")
plot(theta.samples[,c(3,4)], xlab="theta3", ylab="theta4", col="blue")
lines(theta.samples[,3],theta.samples[,3], type="l")
plot(theta.samples[,c(4,5)], xlab="theta4", ylab="theta5", col="blue")
lines(theta.samples[,4],theta.samples[,4], type="l")

#### Figure 4.4 예 4.2 모수의 사후 밀도 ####
plot(density(theta.samples[,1]),xlab="theta1", main="")
plot(density(theta.samples[,2]),xlab="theta2", main="")
plot(density(theta.samples[,3]),xlab="theta3", main="")
plot(density(theta.samples[,4]),xlab="theta4", main="")





### 4.2 메트로폴리스-헤스팅스
# (예 4.3)

# 1) 입력 및 준비단계 

mu0<-10; sigsq0<-25; a<-0.5; b<-1
x<-c(10,13,15,11,9,18,20,17,23,21)
dataList=list(x=x, mu0=mu0, sigsq0=sigsq0, a=a, b=b)

#### compute posterior kernel for Metropolis ####
post.normal_mu_sigsq=function(theta,dataList){
  #--- retrieve data from dataList ---#
  x=dataList$x
  mu0=dataList$mu0
  sigsq0=dataList$sigsq0
  a=dataList$a
  b=dataList$b
  
  mu=theta[1]; sigsq=theta[2]
  f=exp( -0.5*length(x)*log(sigsq) -0.5*sum((x-mu)^2)/sigsq-
           0.5*(mu-mu0)^2/sigsq0-(a+1)*log(sigsq) -b/sigsq )
  return(f)
}


# 2) MCMC 표본 추출 함수 작성 

#### Random Walk Metropolis Algorithm ###
Metropolis_normal_mu_sigsq=function(nsim,nburn,delta, dataList, initsList){
  
  #--- initial values of mu and log.sigsq
  mu=initsList$mu
  log.sigsq=log(initsList$sigsq)
  theta.curr=c(mu,log.sigsq)
  p=length(theta.curr)
  
  #==== Start iterations
  para.samples=matrix(0,nsim,p)
  for(iter in 1:(nsim+nburn)){
    z=rnorm(p,0,1)
    theta.prop=z*delta +theta.curr
    mu.curr=theta.curr[1] 
    sigsq.curr=exp(theta.curr[2]) 
    mu.prop=theta.prop[1] 
    sigsq.prop=exp(theta.prop[2]) 
    alpha=post.normal_mu_sigsq(c(mu.prop,sigsq.prop),dataList)/
      post.normal_mu_sigsq(c(mu.curr,sigsq.curr),dataList)*
      sigsq.prop/sigsq.curr 
    if(runif(1)<alpha) { theta.next<-theta.prop } else {theta.next<-theta.curr}
    
    theta.curr=theta.next
    
    if( iter > nburn ) para.samples[iter-nburn,]=c(theta.next[1],exp(theta.next[2]))
  }
  #=== End iterations
  return(para.samples)
  
}


# 3) 다중 체인 MCMC

#a) MCMC 준비단계의 반복수, 준비단계 이후의 MCMC 반복수, 다중체인의 수 선택
nChains=3
nsim=20000; nburn=5000; 
p=2 #num. of para.
mcmc.samples=array(0, dim=c(nsim,p,nChains)) # array to save samles

#b) 랜덤워크의 표준편차  선택
delta=1

#c) 초기치 선택
#### Generate random initial values ###
inits.random = function(x){
  resampledX = sample(x,replace=T)
  muInit = mean(resampledX)
  sigsqInit = var(resampledX)
  return(list(mu = muInit, sigsq = sigsqInit))
}

#d) MCMC 수행
#### Start iteration#####
for(ich in 1:nChains){
  initsList = inits.random(x)
  mcmc.samples[,,ich] = Metropolis_normal_mu_sigsq(nsim,nburn,delta,dataList,initsList)
}


# 4) 수렴진단 (convergence diagnostics)

#### Figure 4.5; mu 와 sigma의 경로그림과 사후밀도함수 ####

mu.samples=mcmc.samples[,1,]
sigsq.samples=mcmc.samples[,2,]


plot(mu.samples[1:2000,1], type="l", xlab="iteration",ylab=quote(mu))
lines(mu.samples[1:2000,2], col=2)
lines(mu.samples[1:2000,3], col=3)

plot(density(mu.samples[,1]), xlab=quote(mu), ylab="posterior density", main="")
lines(density(mu.samples[,2]), col=2)
lines(density(mu.samples[,3]), col=3)

plot(sigsq.samples[1:2000,1], type="l", xlab="iteration",ylab=quote(sigma^2))
lines(sigsq.samples[1:2000,2], col=2)
lines(sigsq.samples[1:2000,3], col=3)

plot(density(sigsq.samples[,1]), xlab=quote(sigma^2), ylab="posterior density", main="")
lines(density(sigsq.samples[,2]), col=2)
lines(density(sigsq.samples[,3]), col=3)


#### Gelman shrink factor ####
install.packages("coda")
require(coda)   
samples.1=mcmc(mcmc.samples[,,1])
samples.2=mcmc(mcmc.samples[,,2])
samples.3=mcmc(mcmc.samples[,,3])

codaSamples=mcmc.list(list(samples.1,samples.2,samples.3))
gelman=gelman.diag(codaSamples)
gelman


# 채택확률
Metro.draws=mcmc(mcmc.samples[,,1])
accept.rate=1-rejectionRate(Metro.draws)
accept.rate


# 5) 베이지안 사후 추론 

#### Posterior inference ####
mcmc.samples.combined=rbind(mcmc.samples[,,1],mcmc.samples[,,2],mcmc.samples[,,3])  
para.hat=apply(mcmc.samples.combined,2, mean)
HPD=apply(mcmc.samples.combined,2,function(x) quantile(x, c(0.025,0.975)) )

#### Figure 4.6 예 4.3에서  mu와 sigma의 주변 사후밀도함수와 95% HPD 구간 ##
par(mfrow=c(1,2))
plot(density(mcmc.samples.combined[,1]),xlab=quote(mu), ylab="", main="")
abline(v=HPD[ ,1], lty=2, col=2)

plot(density(mcmc.samples.combined[,2]),xlab=quote(sigma^2), ylab="", main="")
abline(v=HPD[ ,2], lty=2, col=2)