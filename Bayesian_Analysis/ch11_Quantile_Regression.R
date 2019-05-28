#1. Boston Housing 자료의 분위 회귀모형 분석에 잠재변수 기법 적용 후 0-1 기법과 컴퓨팅 시간 비교

#### Boston Data ####
Boston=read.table(file="Boston_data.txt", header=T)
x=as.matrix(Boston[c(-3)])
for(i in 1:ncol(x)) x[,i]=(x[,i]-mean(x[,i]))/sd(x[,i])
x=cbind(rep(1,nrow(x)), x)
x=data.matrix(x)
colnames(x)[1]="intercept"
head(x)
y=Boston[,3]
data.name="Boston"

lm.summary=lm(y~x-1) 
mu=lm.summary$coefficients 
Tau=solve(vcov(lm.summary))*0.01
Ones=rep(1,length(y))
initsList=list(beta=mu)
K=ncol(x)

nAdapt=1000; nUpdate=10000 ; nIter=30000; nChains=3
p.set=seq(0.1,0.9, length=9)
beta.hat=matrix(0, length(p.set),K)
beta.cl=beta.cu=matrix(0, length(p.set),K)

#### Model ####
modelString ="
model { 
for(i in 1:length(y)){
fd[i] <- p*(1-p)*exp( -check[i] )
check[i]<- e[i]*( p-( 1-step(e[i]) ) )
e[i]<- y[i] - inprod( x[i,1:K], beta[1:K] ) 
#--- use 0-1 trick ---#
Ones[i] ~ dbern( pi[i] ) 
pi[i] <- fd[i]/10000
}
beta[1:K] ~ dmnorm( mu[], Tau[,] )
}
"
writeLines(modelString, "model_BQR.txt")

modelString ="
model { 
 theta <- (1-2*p)/(p*(1-p))
 eta <- 2/(p*(1-p)) 
 for(i in 1:length(y)){
  w[i] ~ dexp(sigsq)
  y[i] ~ dnorm( mu.y[i], tau.y[i] )
  mu.y[i] <- inprod(x[i,1:K], beta[1:K]) +theta*w[i]
  tau.y[i] <- 1/(eta*w[i]*sigsq)
 }
 inv.sigsq ~ dgamma(a0,b0)
 sigsq <- 1/sqrt(inv.sigsq)

beta[1:K] ~ dmnorm( mu[], Tau[,] )
}
"
writeLines(modelString, "model_BQR_latent.txt")

require(rjags)

time <- system.time(
for(ip in 1:length(p.set)) {
  p=p.set[ip]
  
  #for model_BQR.txt
  #dataList=list(p=p, K=K, y=y, x=x, Ones=Ones, mu=mu, Tau=Tau)
  
  #for model_BQR_latent.txt
  dataList=list(p=p, K=K, y=y, x=x, mu=mu, Tau=Tau, a0=0.1, b0=0.1) 
  
  jagsModel=jags.model(file="model_BQR_latent.txt", data=dataList, 
                       inits=initsList, n.chains=3, n.adapt=nAdapt)
  update(jagsModel, n.iter=nUpdate)
  codaSamples=coda.samples(jagsModel, variable.names=c("beta"), 
                           thin=1, n.iter=nIter)
  #source(MyFunc.r)
  #convDiag(codaSamples)
  mcmcSamples=as.matrix(codaSamples)
  
  beta.hat[ip,1:K ]= apply( mcmcSamples[,1:K],2,quantile, 0.5)
  beta.cl[ip,1:K ]= apply( mcmcSamples[,1:K],2,quantile, 0.025)
  beta.cu[ip,1:K ]= apply( mcmcSamples[,1:K],2,quantile, 0.975)
}
)

beta.hat1=beta.hat
beta.cl1=beta.cl; beta.cu1=beta.cu
time1=time

#--- plot ---#
par(mfrow=c(3,5))
for(k in 2:16) {
  plot(p.set, beta.hat[,k],type="l", ylim=c(min( beta.cl[,k]), max( beta.cu[,k])),
       xlab="p", ylab=colnames(x)[k] ) 
  lines(p.set, beta.cl[,k], col=3)
  lines(p.set, beta.cu[,k], col=3)
  abline(h=0)
}


#2. 잠재변수 기법을 사용하여 변수선택 수행

modelString ="
model { 
 for (j in 1:K) { gbeta[j]<- gamma[j]*beta[j] }

 theta <- (1-2*p)/(p*(1-p))
 eta <- 2/(p*(1-p)) 
 for(i in 1:length(y)){
  w[i] ~ dexp(sigsq)
  y[i] ~ dnorm( mu.y[i], tau.y[i] )
  mu.y[i] <- inprod(x[i,1:K], gbeta[1:K]) +theta*w[i]
  tau.y[i] <- 1/(eta*w[i]*sigsq)
 }
 inv.sigsq ~ dgamma(a0,b0)
 sigsq <- 1/sqrt(inv.sigsq)

 for (j in 1:K) { gamma[j] ~ dbern(0.5) }

 for(j in 1:K) { 
   beta[j] ~ dmnorm( mu[j], tau[j] )
   mu[j] <- (1- gamma[j]) * pseudo.mean.beta[j]
   tau[j] <- gamma[j]/100 + (1-gamma[j])/pseudo.var.beta[j]
 }

}
"
writeLines(modelString, "model_BQR_latent_GVS.txt")

K=ncol(x)
p=0.5
library(quantreg)
rq.out=rq.fit.br(x,y,p, ci=T)
pseudo.mean.beta=rq.out$coefficients[1:K,1]
pseudo.sd.beta=(rq.out$coefficients[1:K,3]-rq.out$coefficients[1:K,2])/(2*1.96)
pseudo.var.beta=pseudo.sd.beta^2

dataList=list(p=p, K=K, y=y, x=x, a0=0.1, b0=0.1,
              pseudo.mean.beta=pseudo.mean.beta, pseudo.var.beta=pseudo.var.beta)
gammaInit=rep(0,K)
initsList=list(beta=pseudo.mean.beta, gamma=gammaInit) 

jagsModel=jags.model(file="model_BQR_latent_GVS.txt", data=dataList, inits=initsList,
                     n.chains=3, n.adapt=10000)
update(jagsModel, n.iter=10000)
codaSamples=coda.samples(jagsModel, variable.names=c("gamma"), thin=1, n.iter=15000)

require(plyr)
gamma.samples=as.matrix(codaSamples)
freq=count(gamma.samples) 
colnames(freq)=c(colnames(x), "prob")
freq[,K+1]=freq[,K+1]/nrow(gamma.samples)
rankModel= as.integer(rank( -freq[,K+1]))
topModel=which(rankModel==1)
freq[order(rankModel)[1:5], ]
freq[topModel,]
