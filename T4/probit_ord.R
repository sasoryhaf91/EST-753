rm(list=ls())

library(arm)

## Evaluación de estudiantes

SATM <- scan(nmax=30*2)
1 557
1 463
2 525
2 533
2 582
2 471
2 557
2 517
2 488
3 581
3 572
3 559
3 543
3 574
3 582
3 591
4 545
4 576
4 576
4 525
4 574
4 595
4 584
4 584
4 563
4 553
5 609
5 599
5 649
5 549 
####
SATM <- data.frame(matrix(SATM,ncol=2,byrow=T,dimnames=list(1:30,c("y","satm"))))
SATM$y <- factor(SATM$y)
str(SATM)
attach(SATM)
satmc <- satm-mean(satm)
fit.ej1 <- bayespolr(y ~ satmc, prior.scale=Inf, method="probit",data=SATM) 
display(fit.ej1) 

# The table classifies 1681 residents of twelve areas in Copenhagen in
# terms of the type of housing they had, their feeling of influence on
# apartment management, their degree of contact with other residents,
# and their satisfaction with housing conditions. We will treat housing
# satisfaction as an ordered response, with categories low, medium and
# high, and the other three factors as explanatory variables.

housing
fit.ej2 <- bayespolr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, prior.scale=Inf, prior.df=Inf) # Same as M1 display (M2) 
display(fit.ej2)