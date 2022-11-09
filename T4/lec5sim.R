library(mvtnorm)

n <- 20
x <- runif(n)
y <- rnorm(n,-1+2*x,1)
fit <- lm(y~x)
summary(fit)
plot(y~x)
abline(fit)

X <- cbind(rep(1,n),x)
XTX <- t(X)%*%X
INVXTX <- solve(XTX)
betahat <- INVXTX%*%t(X)%*%y
M <- X%*%INVXTX%*%t(X)
ss <- t(y)%*%(diag(n)-M)%*%y/(n-length(betahat))

Q <- function(beta){t(beta-betahat)%*%XTX%*%(beta-betahat)}


# densidad condicional a posteriori de beta

fbeta.cond <- function(tau){as.vector(rmvnorm(1,as.vector(betahat),INVXTX/as.vector(tau)))}
ftau.cond <- function(beta){rgamma(1,(n+2)/2,(t(y)%*%(diag(n)-M)%*%y+Q(as.matrix(beta)))/2)^(-1)}
r <- 100
taui <-1/ss
beta.sample <- NULL
tau.sample <- NULL
for(i in 1:r){
betai <- fbeta.cond(taui)
taui <- ftau.cond(betai)
beta.sample <- rbind(beta.sample,betai)
tau.sample <- rbind(tau.sample,taui)
}

row.names(beta.sample)<-seq(1:r)
row.names(tau.sample)<-seq(1:r)

burn <- 1000 
apply(beta.sample[(burn+1):r,],2,mean)
mean(1/tau.sample[(burn+1):r])

plot(density(beta.sample[,1]))
plot(density(beta.sample[,2]))
plot(density(1/tau.sample))
 
hist(tau)
