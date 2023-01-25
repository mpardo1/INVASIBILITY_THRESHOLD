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
out <- as.data.frame(out[,c(2,3)])
end_time <- Sys.time()
print(paste("time:", end_time-start_time))


### Likelihood Function
ll_ode <- function(x, # vector con los parámetros
                   forcings, # forzamientos para el solver de la ode
                   y ){ # datos
                   
  va = 0.1
  if(x[1] < 0 | x[2] < 0 | x[3] < 0 | x[4] < 0 | x[5] < 0){
    res = -86829146000
  }else{
    pars <- c(c = x[1], # Colonization constant
              b = x[2], # Extinction constant
              xi = x[3], # Spread constant
              alp = x[4], # Spread constant
              va = x[5]) # Variance
    
    state <- c(0.1,0) #Vector inicial para ODE
    signal <- forcings
    parameters <- list(dim = dim,  b=b, c = c,
                       xi = xi, alp = alp,
                       area_muni = area_muni,
                       dist = dist, comm = comm,
                       mc = mc)
    
    z <- ode(y = state, times = time, func = Hanski, 
                    parms = parameters, signal = signal) #Aquí corre el ODE
    z <- as.data.frame(z[,c(1:3)])
    z <- z[-1, ]
    
    res <- 0
    for(i in c(1:(dim-1))){
      res <- res +  sum(dnorm(y[,i+1], mean = z[,i+1], sd = va, log = T))
    }
  }
  return(res)
}

# sims <- 2 #Número de combinaciones paramétricas a explorar
seeds <- matrix( 0, ncol = 300, nrow = 5)
seeds[1,] <- runif(300,0,1)
seeds[2,] <- runif(300,0,1)
seeds[3,] <- runif(300,0,1)
seeds[4,] <- runif(300,90,150)
seeds[5,] <- runif(300,0,1)

sols <- NA #Pre-aloco el número de combinaciones paramétricas en 2 unidades de LL de la mejor
best <- -999999999 #LL inicial a mejorar
set.seed(476468713)

condition <- T 
#Esta condición se hará falsa en el bucle de después, si en las rondas la LL no
#mejora más de 2 puntos y hay más de 1000 combinaciones paramétricas a menos de
#2 puntos de loglikelihood de la mejor solución.

round <- 1
sims <- ncol(seeds) #Número de combinaciones paramétricas a explorar

# Cores <- 1
Cores <- parallel::detectCores()#Numero de cores a utilizar.
it <- 0
while(condition){
  start_time <- Sys.time()
  print(paste0("Iteration: ", it))
  #Ahora viene la paralelización
  parall <- mclapply(1:sims, mc.cores = Cores, mc.preschedule = F,function(k){
    
    print(paste("k:",k))
    start_time1 <- Sys.time()
    
    fit <- optim(par = seeds[, k], fn = ll_ode, forcings = signal, y = out, 
                 control = list(fnscale = -1, maxit = 500, parscale = seeds[, k]))
  
    
    if((k %% 1000) == 0) {
      cat("This was seed no. ", k, "\n")
      cat("This fit: ", fit$value, "\n")
    }
    
    fit
  })
  end_time <- Sys.time()
  diff_time <- end_time - start_time
  print("Execution time iteration i:")
  print(diff_time)
  it <- it + 1
  lhs <- parall
  
  rm(parall) #Para evitar fugas de memoria
  
  filename <- paste0("~/INVASIBILITY_THRESHOLD/output/param_HANSKI_1core_900it_fulldata",Sys.Date(), "_",round, ".RData") #Salva cada ronda de optimizaciones, por si acaso
  save(lhs, file = filename)
  
  # Ahora, recuperamos la loglikelihood de cada combinación de parámetros
  logl <- rep(NA, sims)
  for(i in 1:sims) logl[i] <- lhs[[i]]$value
  
  # Evaluamos las condiciones para parar el bucle
  best2 <- max(logl, na.rm = T)
  pc1 <- best < best2
  pc2 <- best > (best2 - 2)
  
  sols <- sum(logl > (max(logl, na.rm = T) - 2), na.rm = T)
  pc3 <- sols > 1000
  
  condition <- pc1 * pc2 * pc3
  condition <- !condition
  
  # Seleccionamos las mejores combinaciones de parámetros para mandar una nueva
  # ronda, cogemos las combinaciones que estén a 2 unidades de distancia de la
  # mejor, o en su defecto, las 250 mejores combinaciones.
  if(sols < 250){
    index <- order(logl, decreasing = T)[1:250]
  } else {
    index <- order(logl, decreasing = T)[1:sols]
  }
  
  n <- 1
  parmat <- matrix(NA, nrow = length(index), ncol = 5)
  for(i in index){
    parmat[n, ] <- lhs[[i]]$par
    n <- n + 1
  }
  
  # Muestreamos las mejores combinaciones de parámetros, cada uno de ellos
  # independientemente. De esta forma estamos rompiendo las posibles
  # correlaciones entre los parámetros, y en algún sentido, hacemos la
  # aproximación menos bayesiana al no samplear la distribución multidimensional
  # de los parámetros.
  seeds <- t(apply(X = parmat, MARGIN = 2, sample, size = 100000, replace = T))
  
  rm(parmat)
  rm(lhs)
  
  print(paste0("Round: ", round, ";  Best likelihood: ", best2, 
               "; Solutions: ", sols, "\n"))
  
  best <- best2
  round <- round + 1
} 
print("The optimization finished.")