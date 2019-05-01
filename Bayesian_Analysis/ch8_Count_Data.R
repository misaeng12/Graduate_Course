mers = read.table("mers.txt", header=T)
head(mers)

day = as.character(mers$day)
diag = mers$diag_new
y = diag; n = length(y)
n.day = c(1:n)
plot(n.day, y, xlab="day", ylab="n_diag", type="l")
points(n.day, y)

#모형설정
x1 = c(rep(1,19), rep(0, n-19))
x2 = c(c(1:19), rep(0,n-19))
x3 = c(rep(0,19), rep(1, n-19))
x4 = c(rep(0,19), c(1:(n-19)))
X= cbind(x1, x2, x3, x4) 
data = data.frame(y,X)
p=ncol(X)


modelString="
model
{
  for(i in 1:length(y)){
  y[i] ~ dpois(lambda[i])
  log(lambda[i]) <- inprod ( X[i,], beta[] )
  }
  for (i in 1:p){ beta[i] ~ dnorm( mu.beta[i], Tau.beta[i] )}
}
"
writeLines(modelString, "model_pois.txt")

#prior parameters
mu.beta = rep(0, p)
Tau.beta = rep(0.01, p)


dataList = list(p=p, y=y, X=X, mu.beta=mu.beta, Tau.beta=Tau.beta)
initsList = list(beta=mu.beta)

require(rjags)
jagsModel.pois = jags.model(file="model_pois.txt", data=dataList, inits=initsList, 
                            n.chains=3, n.adapt=1000)
update(jagsModel.pois, n.iter=3000)
codaSamples = coda.samples(jagsModel.pois, variable.names=c("beta"), 
                           thin=1, n.chains=3, n.iter=10000)

coda::gelman.diag(codaSamples)
summary(codaSamples)


#추정치와 관측치 비교
mcmcSamples = as.matrix(codaSamples)
beta.hat = apply(mcmcSamples, 2, mean)
lambda.hat = exp(X%*%beta.hat)
plot(n.day, y, xlab="day", ylab="n_diag", type="l")
lines(n.day, lambda.hat, col=2)
points(n.day, lambda.hat, col=2, pch='*')

#DIC 계산
dic.pois = dic.samples(jagsModel.pois, 10000); dic.pois

#day 변수의 제곱항 추가한 다음 DIC 계산
X = cbind(x1, x2, x2*x2, x3, x4, x4*x4)
p = ncol(X)
mu.beta = rep(0, p)
Tau.beta = rep(0.01, p)
dataList = list(p=p, y=y, X=X, mu.beta=mu.beta, Tau.beta=Tau.beta)
initsList = list(beta=mu.beta)
jagsModel.pois = jags.model(file="model_pois.txt", data=dataList, inits=initsList, 
                            n.chains=3, n.adapt=1000)
dic.pois = dic.samples(jagsModel.pois, 10000); dic.pois
