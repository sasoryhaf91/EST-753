## Función objetivo

target = function(x){
  return(ifelse(x<0,0,exp(-x)))
}


## Algoritmo MH, para Q simetrica,

x = rep(0,10000)
x[1] = 3     #initialize; I've set arbitrarily set this to 3
for(i in 2:10000){
  current_x = x[i-1]
  proposed_x = current_x + rnorm(1,mean=0,sd=1)
  A = target(proposed_x)/target(current_x) 
  if(runif(1)<A){
    x[i] = proposed_x       # accept move with probabily min(1,A)
  } else {
    x[i] = current_x        # otherwise "reject" move, and stay where we are
  }
}

## Having run this code we can plot the locations visited by the Markov 
## chain x (sometimes called a trace plot).

par(mfrow=c(1,1))
plot(x,main="values of x visited by the MH algorithm")

## Remember that we designed this algorithm to sample from an exponential 
## distribution. This means that (provided we ran the algorithm for long enough!) 
## the histogram of x should look like an exponential distribution. Here we check this:

hist(x,xlim=c(0,10),probability = TRUE, 
     main="Histogram of values of x visited by MH algorithm")
xx = seq(0,10,length=100)
lines(xx,target(xx),col="red")

#####################################################



log_exp_target = function(x){
  return(dexp(x,rate=1, log=TRUE))
}


##

easyMCMC = function(log_target, niter, startval, proposalsd){
  x = rep(0,niter)
  x[1] = startval     
  for(i in 2:niter){
    currentx = x[i-1]
    proposedx = rnorm(1,mean=currentx,sd=proposalsd) 
    A = exp(log_target(proposedx) - log_target(currentx))
    if(runif(1)<A){
      x[i] = proposedx       # accept move with probabily min(1,A)
    } else {
      x[i] = currentx        # otherwise "reject" move, and stay where we are
    }
  }
  return(x)
}

z1=easyMCMC(log_exp_target, 1000,3,1)
z2=easyMCMC(log_exp_target, 1000,1,1)
z3=easyMCMC(log_exp_target, 1000,5,1)

plot(z1,type="l")
lines(z2,col=2)
lines(z3,col=3)


##################
prior.mu <- 0
mu.1 <- 
sig.1 <- prior.scale
fhat <- deriv3(~ mu^(y + a - 1) * 
              exp(-mu * (1 + 1/b)) 
               / ((1/(1+1/b))^(y+a) * 
              gamma(y + a)), "mu", function.arg = TRUE)











