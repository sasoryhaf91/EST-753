library(mvtnorm)
n <- 25
beta <- c(-1,2)
dosis <- runif(n)
X <- cbind(rep(1,n),dosis)
y <- rnorm(n,X%*%beta,1)
betahat.prior1 <- solve(t(X)%*%X)%*%t(X)%*%y
Sigma.prior2 <- solve(diag(2)+t(X)%*%X) 
betahat.prior2 <- Sigma.prior2%*%t(X)%*%y

# densidad a posteriori de beta_2 (a priori uniforme)
post1.beta2 <- function(beta2)dnorm(beta2,betahat[2],sqrt(solve(t(X)%*%X)[1,1]))

# densidad a posteriori (a priori N(0,1))
post2.beta2 <- function(beta2)dnorm(beta2,betahat.prior2[2],sqrt(Sigma.prior2[1,1]))

beta.grid <- seq(-4,4,0.01)
curve(dnorm(x),-4,4,ylab="densidad",xlab="beta_2",ylim=c(0,1.1))
lines(beta.grid,post1.beta2(beta.grid),lty=2)
lines(beta.grid,post2.beta2(beta.grid),lty=3)
abline(v=2)
legend(x=locator(1),legend=c("a priori N(0,1)",
"a posteriori (a priori uniforme)","a posteriori (a priori N(0,1))"),lty=c(1,2,3))

