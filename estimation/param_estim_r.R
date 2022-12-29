rm(list = ls())
start_time <- Sys.time()
library("deSolve")

# Hanski model:
Hanski <- function(time, y, parameters, signal){
  ydot <- c()
  cc <- list()
  with(as.list(c(y, parameters)), {
    for (i in c(1:dim)) {
      cc[[i]] <- signal[[i]](time)
      area <- area_muni
      area[i] <- 0
      d <- sum(exp(-alp*dist[i,])*area*y)
      h <- mc*sum(comm[i,]*y)
      a <- c*(xi*d + (1-xi)*h)
      ydot[i] <- a*(1-y[i]) - (b/cc[[i]])*y[i]
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
area_muni <- matrix(runif(dim, 0,10000),1,dim)
dist <- matrix(runif(dim*dim, 0,10000),dim,dim)
comm <- matrix(runif(dim*dim, 0,10000),dim,dim)
parameters <- list( b=b, c = c,
                xi = xi, alp = alp,
                area_muni = area_muni,
                dist = dist, comm = comm,
                mc = 0.01)

state <- c(0.1,0.1)


### Integratin:
out <- ode(y = state, times = time, func = Hanski, 
           parms = parameters, signal = signal)
plot(out)
end_time <- Sys.time()
print(paste("time:", end_time-start_time))


###
