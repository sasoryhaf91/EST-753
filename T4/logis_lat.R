library(R2WinBUGS)

# Datos Bernoulli: Finney's vasocostriction data (1947, Biometrika, 34)

n <-39
y <- c(1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,0,1,0,0,0,0,1,0,1,0,1,0,1,0,0,1,1,1,0,0,1)
v <- c(3.7,3.5,1.25,.75,.8,.7,.6,1.1,.9,.9,.8,.55,.6,1.4,.75,2.3,3.2,.85,1.7,1.8,.4,.95,1.35,1.5,
1.6,.6,1.8,.95,1.9,1.6,2.7,2.35,1.1,1.1,1.2,.8,.95,.75,1.3)
r <- c(.825,1.09,2.5,1.5,3.2,3.5,.75,1.7,.75,.45,.57,2.75,3,2.33,3.75,1.64,1.6,1.415,1.06,
1.8,2,1.36,1.35,1.36,1.78,1.5,1.5,1.9,.95,.4,.75,.03,1.83,2.2,2,3.33,1.9,1.9,1.625)  
low <- c(-20,0)
high <- c(0,20)

datos <- list("n","y","v","r","low","high")

modelo <-"c:/glm/ejemplos/logis_lat.txt"  
# file.show(modelo)
# iniciales <- function(){list( b = rnorm(3, 0, 10^6))} # valores iniciales generados al azar 
# iniciales <- NULL     # valores iniciales generados por WinBUGS
# iniciales <-list(list(b=c(0,0,0)),list(b=c(0,0,0)),list(b=c(0,0,0)))
load("C:\\GLM\\ejemplos\\mleinits_log.Rdata")
iniciales <- list(list(b=mleinits),list(b=mleinits),list(b=mleinits))
parametros <- c("b")
logis.lat.sim <- bugs(datos, iniciales, parametros, modelo,n.iter = 10000,
bugs.directory ="C:\\Archivos de programa\\WinBUGS14\\", 
working.directory ="c:\\glm\\ejemplos\\",clearWD=T,codaPkg=F,debug=T)
print(logis.lat.sim) 
plot(logis.lat.sim)