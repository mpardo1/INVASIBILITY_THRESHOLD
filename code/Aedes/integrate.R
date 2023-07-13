rm(list = ls())
start_time <- Sys.time()
library("deSolve")
library("parallel")

#### Check numerically if the eigenvalue it is well computed:
a <- runif(1,0,3)
f <- runif(1,0,3)
lf <- runif(1,0,110)
dL <- runif(1,0,3)
dE <- runif(1,0,3)
deltaL <- runif(1,0,3)
h <- runif(1,0,3)
deltaE <- runif(1,0,3) 
K <- matrix(0,3,3)
K[1,] <- c(0,0,a*f/((dE*h)+deltaE))
K[2,] <- c((dE*h)/(dL+deltaL),0,0)
K[3,] <- c(0,dL*lf,0)

eig <- eigen(K)

R0 <- (f*(a*lf)*(dL/(dL+deltaL))*((dE*h)/(dE*h+deltaE)))^(1/3)
abs(as.double(max(Re(eig$values))) - as.double(R0)) < 1.e-8
R0 < 1

# Mosquito model:
mosq_ode <- function(time, y, parameters){
  ydot <- c()
  cc <- list()
  with(as.list(c(y, parameters)), {
    ydot[1] <- a*f*y[3] - (dE*h+deltaE)*y[1]
    ydot[2] <- dE*h*y[1] - dL*y[2] - (deltaL + (y[2]/K))*y[2]
    ydot[3] <- dL*y[2] - (1/lf)*y[3]
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
                   deltaL = deltaL, lf = lf,
                   K = K)

state <- c(0.1,0,0)

### Integratin:
out <- ode(y = state, times = time, func = mosq_ode, 
           parms = parameters)
plot(out)
max(Re(eig$values))
as.double(R0)


#####------With forcings by temperature and human density------#
# Hanski model:
Hanski <- function(time, y, parameters, signal){
  ydot <- c()
  cc <- list()
  with(as.list(c(y, parameters)), {
    for (i in c(1:dim)) {
      ydot[1] <- a*f*y[3] - (dE*h+deltaE)*y[1]
      ydot[2] <- dE*h*y[1] - dL*y[2] - (deltaL + (y[2]/K))*y[2]
      ydot[3] <- dL*y[2] - (1/lf)*y[3]
    }
    return(list(ydot, c = cc))
  })
}

## Forcing function
signal <- list(approxfun(x = c(0,1,3,5,10), 
                         y = c(0.1, 0.5,  0.1,  0.1, 0.9), 
                         method = "linear", rule = 2),
               approxfun(x = c(0,1,3,5,10), 
                         y = c(0.1, 0.5,  0.1,  0.5, 0.2), 
                         method = "linear", rule = 2))


# Parameters: 
dim <- 2
time <- seq(0, 10, by=0.01)
b <- 0.2
c <- 0.1
xi <- 0.5
alp <- 100
mc <- 0.2
area_muni <- matrix(runif(dim, 0,10000),1,dim)
dist <- matrix(runif(dim*dim, 0,10000),dim,dim)
comm <- matrix(runif(dim*dim, 0,10000),dim,dim)
parameters <- list(dim = dim,  b=b, c = c,
                   xi = xi, alp = alp,
                   area_muni = area_muni,
                   dist = dist, comm = comm,
                   mc = 0.01)

state <- c(0.1,0)

### Integratin:
out <- ode(y = state, times = time, func = Hanski, 
           parms = parameters, signal = signal)
plot(out)
out <- as.data.frame(out[,c(1:(dim+1))])
end_time <- Sys.time()
print(paste("time:", end_time-start_time))