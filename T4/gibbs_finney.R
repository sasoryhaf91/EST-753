
## ------------------------------------------------
library(mvtnorm)
# read in data
datos <- read.table("/Users/peser/Dropbox/Metodos Estadísticos Avanzados/datos/finney.txt",header=F)
colnames(datos) <- c("y","v","r")


## ------------------------------------------------
# Maximum likelihood results
X<- cbind(1,as.matrix(datos[,2:3]))
y <- datos[,1]
mle <- glm(y ~ X-1, family=binomial("probit"))
# Summary table 

summary(mle)
# Wald test is highly significant 
beta_mle<- mle$coef # maximum likelihood estimates


## ------------------------------------------------
# Prior parameters 
beta0<- rep(0,3)
Pbeta0<- 0.0001*diag(3)


## ------------------------------------------------
# Run Albert & Chib algorithm - output results in beta.out file
beta<- rep(0,3) # starting value of chain
n<- nrow(datos)   # number of subjects
z<- rep(0,n)    # initial values of underlying variables
G<- 30000        # number of MCMC iterations

# Run Gibbs sampler
gibbs.fun <- function(){
  eta<- X%*%beta # linear predictor
  # sample underlying normal variables from truncated normal 
  # full conditional posterior distributions
  z[y==0]<- qnorm(runif(sum(1-y),0,pnorm(0,eta[y==0],1)),eta[y==0],1)
  z[y==1]<- qnorm(runif(sum(y),pnorm(0,eta[y==1],1),1),eta[y==1],1)

  # sample betas from normal full conditional posterior distribution
  Vbeta<- solve(Pbeta0 + t(X)%*%X)
  Ebeta<- Vbeta%*%(Pbeta0%*%beta0 + t(X)%*%z)
  beta <<- c(rmvnorm(1,Ebeta,Vbeta))
}

out <- replicate(G,gibbs.fun())
#write(out,file="beta.out",ncol=7)


## ------------------------------------------------
library(coda)
beta_out<- as.mcmc(matrix(scan("beta.out"), ncol=7, byrow=T))
beta_out <- as.mcmc(t(out))
geweke.diag(beta_out, frac1=0.1, frac2=0.5)


## ------------------------------------------------

# Plot chains for beta[1] and beta[2]
par(mfrow=c(2,1))
plot(beta_out[,1],type="l",xlab="iteration", ylab="intercept (beta_1)",density = FALSE)
plot(beta_out[,2],type="l",xlab="iteration", ylab="slope (beta_2)",density = FALSE)	



## ------------------------------------------------
# Plot marginal posterior density of slope 
slp<- as.mcmc(beta_out[1001:10000,2])
par(mfrow=c(1,1))
par(mar=c(5,5,5,5))
plot(slp,type="l",xlab="slope (beta_1)",ylab="Posterior Density",
     cex=1.2,trace = FALSE)
abline(v=mean(slp))
abline(v=2.1181,lwd=2.5) # MLE
abline(v=2.1181 + 1.96*0.7177*c(-1,1),lwd=2.5,lty=2)
abline(v=quantile(slp,probs=c(0.025,0.975)),lty=2)


## ------------------------------------------------
# Plot estimated dose response curve for preterm birth
par(mfrow=c(1,1))
par(mar=c(5,5,5,5))
xg<- seq(0,3.5,length=100)  # grid of dde values
beta<- beta_out[1001:10000,] # discard burn-in
post<- matrix(0,100,4)
for(i in 1:100){
  post[i,1]<- mean(pnorm(beta[,1] + xg[i]*beta[,2]))
  post[i,2:3]<- quantile(pnorm(beta[,1] + xg[i]*beta[,2]),probs=c(0.025,0.975))
  post[i,4]<- pnorm(-5.1945 + xg[i]*2.1181)
}
#xtrue<- xg*sd(dde$xtrue) + mean(dde$xtrue) # back transform
plot(xg,post[,1],xlab="v",ylab="Vasocons.",cex=1.2,
     ylim=c(0,max(post)), type="l")
lines(xg,post[,2],lty=2)
lines(xg,post[,3],lty=2)
lines(xg,post[,4],lwd=2.5)


## ------------------------------------------------
# calculate posterior summaries of regression coefficients
table1<- matrix(0,3,5)
for(i in 1:3){
  table1[i,]<- c(mean(beta[,i]),median(beta[,i]),sqrt(var(beta[,i])),
                 quantile(beta[,i],probs=c(0.025,0.975)))
}
table1<- round(table1*100)/100
colnames(table1) <- c("Media","Mediana","Desv. Est.", "Inf.","Sup")
table1


v<- seq(0,3.5,length=100)  # grid of dde values
r<- seq(0,3.5,length=100)  # grid of dde values

beta<- beta_out[1001:10000,] # discard burn-in
surf<- function(v,r) mean(pnorm(beta[,1] + v*beta[,2]+r*beta[,3]))
prob <- outer(v,r,Vectorize(surf))
contour(v,r,prob)

id <- which(prob>=0.49&prob<=0.51,arr.ind=T)
idx <- unique(tmp[,1])
idy <- unique(tmp[,2])
cbind(v[idx],r[idy])

