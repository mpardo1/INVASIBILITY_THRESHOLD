rm(list = ls())
start_time <- Sys.time()
library("deSolve")
library("parallel")

#### Check numerically if the eigenvalue it is well computed:
a <- runif(1,0,3)
f <- runif(1,0,3)
deltaA <- runif(1,0,3)
dL <- runif(1,0,3)
dE <- runif(1,0,3)
deltaL <- runif(1,0,3)
h <- runif(1,0,3)
deltaE <- runif(1,0,3) 
K <- matrix(0,3,3)
K[1,] <- c(0,0,a*f/((dE*h)+deltaE))
K[2,] <- c((dE*h)/(dL+deltaL),0,0)
K[3,] <- c(0,dL/deltaA,0)

eig <- eigen(K)

R0 <- (f*(a/deltaA)*(dL/(dL+deltaL))*((dE*h)/(dE*h+deltaE)))^(1/3)
abs(as.double(max(Re(eig$values))) - as.double(R0)) < 1.e-8
R0 < 1

# Mosquito model:
mosq_ode <- function(time, y, parameters){
  ydot <- c()
  cc <- list()
  with(as.list(c(y, parameters)), {
    ydot[1] <- a*f*y[3] - (dE*h+deltaE)*y[1]
    ydot[2] <- dE*h*y[1] - dL*y[2] - (deltaL + (y[2]/K))*y[2]
    ydot[3] <- dL*y[2] - deltaA*y[3]
    return(list(ydot))
  })
}

# Parameters: 
time <- seq(0, 1000, by=0.01)
# a <- 2
# f <- 0.2
# dE <- 0.1
# h <- 0.5
# deltaE <- 0.2
# dL <- 0.2
# deltaL <- 0.2
# deltaA <- 0.2
K <- 10000

parameters <- list(a = a,  f=f, dE = dE,
                   h = h, deltaE = deltaE,
                   dL = dL,
                   deltaL = deltaL, deltaA = deltaA,
                   K = K)

state <- c(0.1,0,0)

### Integratin:
out <- ode(y = state, times = time, func = mosq_ode, 
           parms = parameters)
plot(out)

