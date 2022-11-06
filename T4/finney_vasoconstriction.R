library(arm)

# Datos Bernoulli: Finney's vasocostriction data (1947, Biometrika, 34)
finney.dat  <-read.table("T4/finney.txt",header=F)
colnames(finney.dat) <- c("y","v","r")

x1 <- finney.dat$v
x2 <- finney.dat$r
y <- finney.dat$y

X <- cbind(rep(1,39),log(x1),log(x2))
XXinv <- solve(t(X)%*%X)

mlefit <- glm(y~log(x1)+log(x2),family=binomial)
mean.as <-summary(mlefit)$coef[,1]
sd.as <- summary(mlefit)$coef[,2]


par(mfrow=c(1,3))
for(i in 1:3){
	curve(dnorm(x,mean.as[i],sd.as[i]),mean.as[i]-5*sd.as[i],mean.as[i]+5*sd.as[i])
	}



bayesfit.unif <- bayesglm(y~log(x1)+log(x2),family=binomial,prior.mean=0, prior.scale=Inf,prior.df=Inf)
summary(bayesfit.unif )$coef[,1:2]
sim.unif <-coef(sim(bayesfit.unif,n.sims=10000))
apply(sim.unif,2,quantile,probs=c(0.025,0.975))

par(mfrow=c(1,3))
for(i in 1:3){
	plot(density(sim.unif[,i]))
	}



bayesfit.norm1 <- bayesglm(y~log(x1)+log(x2),family=binomial,prior.mean=0, prior.scale.for.intercept=sqrt(diag(XXinv)[1]),prior.scale=sqrt(diag(XXinv)[2:3]),prior.df=Inf)

summary(bayesfit.norm1 )$coef[,1:2]
sim.norm1 <-coef(sim(bayesfit.norm1,n.sims=100000))
apply(sim.norm1,2,quantile,probs=c(0.025,0.975))

par(mfrow=c(1,3))
for(i in 1:3){
	plot(density(sim.norm1[,i]))
	}


bayesfit.norm2 <- bayesglm(y~log(x1)+log(x2),family=binomial,prior.mean=0, prior.scale.for.intercept=400*diag(XXinv)[1],prior.scale=400*diag(XXinv)[2:3],prior.df=Inf)


summary(bayesfit.norm2 )$coef[,1:2]
sim.norm2 <-coef(sim(bayesfit.norm2,n.sims=10000))
apply(sim.norm2,2,quantile,probs=c(0.025,0.975))

par(mfrow=c(1,3))
for(i in 1:3){
	plot(density(sim.norm2[,i]))
	}


# Con JAGS

library(rjags)
n <- length(y)
datos <- list(n=n,y=finney.dat$y,v=finney.dat$v,r=finney.dat$r)

logis <-"T4/logis.txt"  
file.show(logis)
iniciales <- list(b=mean.as)
jagsmod <- jags.model(logis,d=datos,inits=iniciales)
update(jagsmod, 10000)
cadena=coda.samples(jagsmod, "b", 
                     n.iter=10000,thin=50)
summary(cadena)
plot(cadena)



