library(coda)

##### Read Data #####
n=20; xbar=4; sigma=1
mu0=0; sigma0=10
dataList=list(n=n,xbar=xbar,sigma=sigma, mu0=mu0, sigma0=sigma0)

#### Function To Compute Posterior Kernel ####
post.kernel=function(mu,dataList){
  post.kernel= exp( -0.5*( ((dataList$xbar-mu)*sqrt(dataList$n)/dataList$sigma )^2 +
                           ((mu-dataList$mu0)/dataList$sigma0 )^2 ))
  return(post.kernel)
}

##### Function To Perform Random Walk Metropolis ####
Metro=function(nsim, mu.init, delta, dataList){
  
  mu.samples=mu.init
  mu.curr=mu.init
  
  for (iter in 1:nsim){
    mu.prop = rnorm(1, mean=mu.curr, sd=delta)
    alpha = min(1, post.kernel(mu.prop,dataList)/post.kernel(mu.curr,dataList))
    u = runif(1)
    mu.next = mu.prop*(u<alpha) + mu.curr*(u>alpha)
    mu.samples = rbind(mu.samples, mu.next)
    mu.curr = mu.next
  }
  
  return(mu.samples)
  
}

##################################################

delta=0.2
nsim=10000; nwarm=500
n.chains=3

mu.Samples=matrix(0, nsim, n.chains)

for(i in 1:n.chains){
  mu.init = rnorm(1, mu0, 2)
  mu.Samples[, i] = Metro(nsim-1, mu.init, delta, dataList) 
}


#### Fig3.1 예 3.1에서 3개의 체인에 대한 경로그림 ####
plot(mu.Samples[(nwarm+1):nsim,1], xlab="iteration", ylab="sample",type="l",main="", sub="(b)")
lines(mu.Samples[(nwarm+1):nsim,2], col=2)
lines(mu.Samples[(nwarm+1):nsim,3], col=3)

savePlot(file="figure/Fig_3_1_traceplot", type=c("bmp"), device=dev.cur())

#### Fig3.2 예 3.1에서 3개의 체인으로부터 추정된 사후밀도함수 ####
par(mfrow=c(1,2))
nwarm=0; nsim=200
plot(density(mu.Samples[(nwarm+1):nsim,1]), xlab="mu", ylab="posterior density", main="",  sub="(a)")
lines(density(mu.Samples[(nwarm+1):nsim,2]),col=2)
lines(density(mu.Samples[(nwarm+1):nsim,3]),col=3)

nwarm=500; nsim=10000
plot(density(mu.Samples[(nwarm+1):nsim,1]), xlab="mu", ylab="posterior density", main="", sub="(b)")
lines(density(mu.Samples[(nwarm+1):nsim,2]),col=2)
lines(density(mu.Samples[(nwarm+1):nsim,3]),col=3)
savePlot(file="figure/Fig_3_2_postdensity", type=c("bmp"), device=dev.cur())

#### Fig3.3 예 3.1에서 반복수에 따른 Gelman 상수 ####
mu1.Samples=mcmc(mu.Samples[1:200,])
mu1.codaSamples=mcmc.list(list(mu1.Samples[,1],mu1.Samples[,2],mu1.Samples[,3]))
gelman.plot(mu1.codaSamples, col=c("black", "blue"))
savePlot(file="figure/Fig_3_3_a_Gelman", type=c("bmp"), device=dev.cur())

mu2.Samples=mcmc(mu.Samples[501:10000,])
mu2.codaSamples=mcmc.list(list(mu2.Samples[,1],mu2.Samples[,2],mu2.Samples[,3]))
gelman.plot(mu2.codaSamples, col=c("black", "blue"))

mu3.Samples=mcmc(mu.Samples[2001:10000,])
mu3.codaSamples=mcmc.list(list(mu3.Samples[,1],mu3.Samples[,2],mu3.Samples[,3]))
gelman.plot(mu3.codaSamples, col=c("black", "blue"))
savePlot(file="figure/Fig_3_3_b_Gelman", type=c("bmp"), device=dev.cur())


#### ACF값 ####
mu.postSamples=as.matrix(mu3.codaSamples)
aa=acf(mu.postSamples); aa

#### Fig 3.4 예 3.1 표본의 자기상관 그림 ####
par(mfrow=c(1,1))
plot(acf(mu.Samples[(nwarm+1):nsim,1]), main="Autocorrelation")
savePlot(file="figure/Fig_3_4_acf", type=c("bmp"), device=dev.cur())

#### ESS (효용표본수) ####
mu.postSamples=as.matrix(mu3.codaSamples)
aa=as.vector(unlist(acf(mu.postSamples)))
aa=as.numeric(aa[2:17])
ESS = (nsim-nwarm)/(1+2*sum(aa)); ESS
