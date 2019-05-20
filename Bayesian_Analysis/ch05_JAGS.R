 ########## 5장 JAGS를 이용한 베이지안 추론 ##########

install.packages("rjags")
library(rjags)

modelString=" 
model
{
  # 데이터의 분포
  for(i in 1:n){
    x[i] ~ dnorm( mu, invsigsq)
  }

  # 사전분포
  mu ~ dnorm(mu0, invsigsq0)
  invsigsq ~ dgamma(a,b)

  # 모수의 변환
  sigsq <- 1/invsigsq

  # 상수값 지정
  mu0<- 10
  invsigsq0 <- 1/25
  a<- 0.5
  b<- 1
}
"
writeLines(modelString, "model_ex5_1.txt")

dataList = list(n=10, x=c(10,13,15,11,9,18,20,17,23,21))

#initsList = list(mu=10 , invsigsq=1/25)
#또는 아래와 같이 자료를 복원 재추출하여 랜덤하게 초기치를 결정할 수도 있다.
initsList = function(x){
  resampledX = sample(x, replace=T)
  muInit = mean(resampledX)
  invsigsqInit = (1/sd(resampledX))^2*0.999+0.01
  return(list(mu=muInit, invsigsq=invsigsqInit))
}


jagsModel=jags.model( file="model_ex5_1.txt", data=dataList, inits=initsList(x),
                      n.chains=3, n.adapt=500 )
update(jagsModel, n.iter=500)

codaSamples=coda.samples(jagsModel, variable.names=c("mu", "sigsq"), n.iter=5000)


#### Fig 5.4 & 5.5 표본의 경로그림과 자기상관 계수 ####
par(mfrow=c(1,2))
coda::traceplot( codaSamples[,"mu"] , main="" , ylab="mu" )
acf(codaSamples[,"mu"][[1]],plot=T, main="")  

coda::traceplot( codaSamples[,"sigsq"] , main="" , ylab="sigsq" )
acf(codaSamples[,"sigsq"][[1]],plot=T, main="")  


gelman=gelman.diag(codaSamples)
gelman.1=as.matrix(gelman$psrf)
if( max(gelman.1) > 1.1 ) cat ("Warning: Gelman Shrink Factor > 1.1", "\n")
gelman.2=gelman$mpsrf
if( gelman.2 > 1.1 ) cat ("Warning: Gelman Multivariate Shrink Factor > 1.1", "\n")


#--- Check MCMC efficiency ---#
mcmcSamples.combined=mcmc(codaSamples[[1]])
if(n.chains > 1) for(ich in 2:n.chains){ 
  mcmcSamples.combined=rbind(mcmcSamples.combined,mcmc(codaSamples[[ich]]))
}

ESS=effectiveSize(mcmcSamples.combined)
cat("Effective Sample size = ", ESS)


MuSamples=as.matrix(codaSamples[,"mu"])
SigSamples=as.matrix(codaSamples[,"sigsq"])  

#### Fig 5.6 주변사후밀도함수 ####
par(mfrow=c(1,2))
plot( density( MuSamples), main="",xlab=bquote(mu), ylab="posterior density")
plot( density( SigSamples), main="",xlab=bquote(sigma^2), ylab="posterior density")


AcceptRate=1-rejectionRate(codaSamples); AcceptRate





#### 예 5.2 ####

dat=read.csv("immigrants.csv")
y=dat$wage
n=length(y) 
X=cbind(rep(1, n), dat$sp, dat$lit)
p=ncol(X)

a = 1
b = 1
XtX=t(X) %*% X
XtX.inv=solve(XtX)
Xty=t(X) %*% y
beta.hat=as.vector(XtX.inv %*% Xty)
sigsq.hat=sum((y - X %*% beta.hat)^2)/(n-p)
beta0=beta.hat
Sig0=diag(diag(XtX.inv))*sigsq.hat*100
Sig0.inv=solve(Sig0)

modelString=" 
model
{
  for(i in 1:length(y)){
    y[i] ~ dnorm( inprod(X[i,], beta[]), invsigsq )
  }
  
  beta[1:length(beta0)] ~ dmnorm( beta0[], Sig0.inv[,]) 
  
  invsigsq ~ dgamma(a,b)
  sigsq = 1/invsigsq
  
}
"
writeLines(modelString, "model_reg.txt")

dataList = list(X=X, y=y, a=a, b=b, beta0=beta0, Sig0.inv=Sig0.inv)
initsList = list(beta=beta.hat, invsigsq=1/sigsq.hat) 
nChains=3

jagsModel=jags.model( file="model_reg.txt", data=dataList, inits=initsList,
                      n.chains=nChains, n.adapt=500)
update(jagsModel, n.iter=1000)
codaSamples=coda.samples(jagsModel, variable.names=c("beta","sigsq"), n.iter=30000)

para.names=variable.names(codaSamples[[1]])


#### Fig 5.7: 경로그림과 자기상관 ####
par(mfrow=c(4,2))
for(i in 1:4){
  coda::traceplot( codaSamples[,i], main="", ylab=para.names[i] )
  acf(codaSamples[,i][[1]], plot=T, main=para.names[i])  
}
#savePlot(file="Fig_5_7", type=c("bmp"), device=dev.cur())


#### Fig 5.8: 사후밀도함수와 95%사후구간 ####

MCMCSamples = as.matrix(codaSamples)
(HPD= round(apply(MCMCSamples, 2, quantile, probs = c(0.025, 0.975)),4))

par(mfrow=c(2,2))
for(i in 1:4) {
  plot( density( MCMCSamples[,i]), main="",xlab=para.names[i], col="blue")
  abline( v=HPD[,i],col=2)
}
#savePlot(file="Fig_5_8", type=c("bmp"), device=dev.cur())
