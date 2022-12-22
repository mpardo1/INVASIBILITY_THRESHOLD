rm(list = ls())
start_time <- Sys.time()
library("deSolve")

# Functions:
Hanski <- function(time, y, parameters, signal){
  ydot <- c()
  with(as.list(c(y, parameters)), {
    for (i in c(1:dim)) {
      cc <- signal[[i]](time)
      ydot[i] <- a*(1-y[i]) - (b/cc)*y[i]
    }
    return(list(ydot, c = cc))
  })
}

signal <- list(approxfun(x = c(0,1,3,5,10), 
                    y = c(0.1, 0.5,  0.1,  0.1, 0.9), 
                    method = "constant", rule = 2),
            approxfun(x = c(0,1,3,5,10), 
                      y = c(0.1, 0.5,  0.1,  0.5, 0.2), 
                      method = "constant", rule = 2))


# Parameters: 
time <- seq(0, 10, by=0.01)
parameters <- c(a=0.33, b=0.2)
dim <- 2
state <- c(0.1,0.1)



out <- ode(y = state, times = time, func = Hanski, 
           parms = parameters, signal = signal)
plot(out)
end_time <- Sys.time()
print(paste("time:", end_time-start_time))

