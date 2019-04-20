######################################################
# Metropolis-Hastings Algorithm (Example: 예 2.2)
######################################################

theta=c(1:5)
prob=c(1,3,8,5,3)
prob.ratio = function(theta1, theta2, theta, prob) {
  ind1 = which(theta == theta1)
  ind2 = which(theta == theta2)
  return(prob[ind1]/prob[ind2])
}

N=50000
theta.curr=2
theta.Samples=c(1:N)*0
theta.Samples[1]=theta.curr


#----Start Simulation----#

for( iter in 1:(N-1) ) {
  
  #theta.prop = ifelse(runif(1)<0.5, theta.curr+1, theta.curr-1)
  #if (theta.prop<1 || theta.prop>5) theta.prop = theta.curr     # 1) theta의 후보값을 현재값의 왼쪽과 오른쪽에 국한
  
  #theta.prop = sample(theta,1)                                  # 2) (1, 2, 3, 4, 5) 중 임의의 값을 같은 확률로 선택
  
  prob.prop = c(0.1, 0.1, 0.2, 0.3, 0.3)                         # 3) (1, 2, 3, 4, 5) 에 대하여 동일한 확률이 아니라
  theta.prop = sample(theta, 1, prob=prob.prop)                  #   확률 (0.1, 0.1, 0.2, 0.3, 0.3)로 선택
  
  theta.prop = round(theta.prop, 0)
  #alpha.star = prob.ratio(theta.prop, theta.curr, theta, prob)
  alpha.star = prob.ratio(theta.prop, theta.curr,theta, prob) *      # (3 continued) 후보추출 확률비
               prob.ratio(theta.curr, theta.prop, theta, prob.prop)  # => 서로 다른 확률로 후보로 선택된 점을 보정
  alpha = min(1, alpha.star)
  theta.next = ifelse(runif(1)<alpha, theta.prop, theta.curr)
  theta.Samples[iter+1] = theta.next
  theta.curr = theta.next
}

#----End simulation----#


##### Fig 2.4 #####
par(mfrow=c(1,1))
Ntrace=100
plot(theta.Samples[1:Ntrace], type="l", xlab="iteration", ylab="theta")
points(c(1:Ntrace), theta.Samples[1:Ntrace], pch=19, col="blue")

##### Fig 2.5 #####
par(mfrow=c(1,2))
barplot(prob,names.arg=theta, xlab="theta", ylab="prob", col="skyblue", sub="(a)true probability")
aa = table(theta.Samples[501:N])/sum(table(theta.Samples[501:N]))
barplot(aa, names.arg=theta, xlab="theta", ylab="prob", ylim=c(0,0.4), col="sky blue", sub="(b) relative frequency of samples")





############################################
# Gibbs Sampling (Example: 예 2.3)
############################################

M<-3000; m<-500   # burn-in=500; nTotalIteration=3000
mu0<-10; sigsq0<-25; a<-0.5; b<-1
x<-c(10,13,15,11,9,18,20,17,23,21)
n<-length(x)
xbar<-mean(x); var.x<-var(x)
THETA<-matrix(nrow=M, ncol=2)
sigsq<-var.x   # initial value of sigsq

#---- Gibbs sampler ----#

for(nsim in 1:M){
  
  # generate mu
  condpost.var <- 1/(1/sigsq0+n/sigsq)
  condpost.mu <- condpost.var*(xbar/(sigsq/n)+mu0/sigsq0)
  mu <- rnorm(1, condpost.mu, sqrt(condpost.var))
  
  # generate sigsq
  condpost.a <- a+n/2
  condpost.b <- b+1/2*((n-1)*var.x+n*(xbar-mu)^2)
  sigsq <- 1/rgamma(1, condpost.a, condpost.b)
  
  # save
  THETA[nsim,] <- c(mu,sigsq)
}


#### Fig 2.7 ####
par(mfrow=c(1,3))

plot(THETA[1:5,], type="n", xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:5,], lty=2)
for(i in 1:5) text(THETA[i, 1], THETA[i, 2], i)

plot(THETA[1:15,], type="n", xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:15,], lty=2)
for(i in 1:15) text(THETA[i, 1], THETA[i, 2], i)

plot(THETA[1:100,], type="n", xlab=expression(mu),ylab=expression(sigma^2))
lines(THETA[1:100,], lty=2)
for(i in 1:100) text(THETA[i, 1], THETA[i, 2], I)

#### Fig 2.8 ####
par(mfrow=c(1,1))
plot(THETA[m:M, 1], THETA[m:M, 2], xlab=expression(mu), ylab=expression(sigma^2), main="sample of (mu, sigma^2)")

#### Fig 2.9 ####
par(mfrow=c(1,2))
plot(density(THETA[m:M, 1]), xlab=expression(mu), ylab="posterior", main="")
plot(density(THETA[m:M, 2]), xlab=expression(sigma^2), ylab="posterior", main="")
