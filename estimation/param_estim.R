rm(list = ls())
start_time <- Sys.time()

library("parallel")
library("tidyverse")
library("deSolve")

# Función de c que corre la ODE -------------------------------------------

# Esta función está adaptada para su uso con el paquete deSolve, el estándar en
# R para las ODE
Path = "~/INVASIBILITY_THRESHOLD/estimation/"
# Path = paste(PC,Path, sep="")

setwd(Path)
system("R CMD SHLIB model.c")
dyn.load("model.so")

gam1 = 0.0250265
gam2 = 0.146925
gam3 = 0.482396
trueSD = 1

# We create a vector with the constant parameters.
parms = c(gam1,gam2,gam3)
# We set the initial conditions to zero.
Y  <- matrix(0, nrow = 1, ncol=2000)
Path = "~/INVASIBILITY_THRESHOLD/estimation/Downloads_2378.data"
down <- data.frame(t(read.table(Path, header=FALSE)))
colnames(down) <- c("time", "down")

# List with the data frames of the forcings, sort as the c code.
forcs_mat <- list(data.matrix(down))

min_t <- min(down$time)
max_t <- max(down$time)
times <- seq(min_t,max_t, 1)
out <- ode(Y, times, func = "derivs",
           parms = parms, dllname = "model_2000eq",
           initfunc = "initmod", nout = 1,
           outnames = "Sum", initforc = "forcc",
           forcings = down, 
           fcontrol = list(method = "constant")) 


ode_o <- data.frame(out)
ode_o$Sum <- NULL
head(ode_o)

saveRDS(ode_o, file = "~/INVASIBILITY_THRESHOLD/estimation/ode_out.rds")

# Likelihood function
likelihood <- function(y, #datos
                       x, # vector con los parámetros
                       forcings) # forzamientos para el solver de la ode
{ 
  print("x:")
  print(x)
  if(x[1] < 0 | x[2] < 0 | x[3] < 0 ){
    print("Negative param")
    res = -86829146000
  }else{
    pars <- c(gam1 = x[1], # death rate group 1
              gam2 = x[2],
              gam3 = x[3]) # death rate group 2
    
    sd <- x[4]
    
    population <- matrix(0,nrow = 1,ncol=2000)
    
    forcs_mat <- list(data.matrix(forcings))
    
    z <- ode(y = population,
             times = 0:nrow(y), func = "derivs", method = "ode45",
             dllname = "model_2000eq", initfunc = "initmod", nout = 0, 
             parms = pars, initforc = "forcc", forcings = forcs_mat, 
             fcontrol = list(method = "constant")) #Aquí corre el ODE
    
    
    z <- as.data.frame(z)
    z <- z[-1, ]
    
    res <- 0
    for(i in c(1:2000)){
      res <- res +  sum(dnorm( y[,i+1], mean = z[,i+1], sd = sd, log = T))
    }
  }
  
  # print("res:")
  # print(res)
  return(res)
}


# Registrations:
Path = "~/INVASIBILITY_THRESHOLD/estimation/R0_municipio.data"
down <- data.frame(t(read.table(Path, header=FALSE)))
colnames(down) <- c("time", "down")
head(down)
forcs_mat <- data.matrix(down)

# Read Observed data:
ob_data <-read.table("~/INVASIBILITY_THRESHOLD/estimation/presence_absence.data", header=FALSE, sep= " ")
ob_data <- t(ob_data[,1:2000])

# Pseudo Data to check the oprimization method.
# ob_data <- readRDS(file = "~/MAD_MODEL/SUR_MODEL/Code/ode_pseudo_2000eq.rds")
y <- ob_data
# Example: plot the likelihood profile of the slope.
slopevalues <- function(par){
  return(likelihood(ob_data,c(par[1],par[2],par[3], trueSD),forcs_mat))
} 

length(seq(0,0.5,0.01))
mat <- as.matrix(expand.grid(seq(0,0.5,0.01), seq(0,0.5,0.01),seq(0,0.5,0.01)))
slopelikelihoods <- apply(mat,1, slopevalues)
mat <- as.data.frame(mat)
mat$LL <- slopelikelihoods

filename <- paste0("~/MAD_MODEL/SUR_MODEL/Code/LL_3D_2000eq_",Sys.Date(),".RData") 
save(mat, file = filename)

#---------------------------------------------------------------------------------------------#
# R para las ODE
Path = "~/MAD_MODEL/SUR_MODEL/Code/"
# Path = paste(PC,Path, sep="")

setwd(Path)
system("R CMD SHLIB model_2000eq.c")
dyn.load("model_2000eq.so")

true1 = 0.0250265
true2 = 0.146925
true3 = 0.482396
trueSD = 1
# Likelihood function
likelihood <- function(y, #datos
                       x, # vector con los parámetros
                       forcings) # forzamientos para el solver de la ode
{ 
  # print("x:")
  # print(x)
  if(x[1] < 0 | x[2] < 0 | x[3] < 0 | x[4] == 0){
    print("Negative param")
    res = -86829146000
  }else{
    pars <- c(gam1 = x[1], # death rate group 1
              gam2 = x[2],
              gam3 = x[3]) # death rate group 2
    
    sd <- x[4]
    
    population <- matrix(0,nrow = 1,ncol=2000)
    
    forcs_mat <- list(data.matrix(forcings))
    
    z <- ode(y = population,
             times = 0:nrow(y), func = "derivs", method = "ode45",
             dllname = "model_2000eq", initfunc = "initmod", nout = 0, 
             parms = pars, initforc = "forcc", forcings = forcs_mat, 
             fcontrol = list(method = "constant")) #Aquí corre el ODE
    
    
    z <- as.data.frame(z)
    z <- z[-1, ]
    
    # Sum the columns of the ode integration:
    sum_group1 <- rowSums(z[,2:31])
    sum_group2 <- rowSums(z[,31:648])
    sum_group3 <- rowSums(z[,648:2000])
    
    # Remove the df of solutions z:
    rm(z)
    
    # Compute the LL:
    res <- sum(dnorm( y$sum1, mean = sum_group1, sd = sd, log = T)) + 
      sum(dnorm( y$sum2, mean = sum_group2, sd = sd, log = T)) + 
      sum(dnorm( y$sum3, mean = sum_group3, sd = sd, log = T)) 
    
  }
  
  print("res:")
  print(res)
  return(res)
}


# Registrations:
Path = "~/MAD_MODEL/SUR_MODEL/data/Downloads_2378.data"
down <- data.frame(t(read.table(Path, header=FALSE)))
colnames(down) <- c("time", "down")
head(down)
forcs_mat <- data.matrix(down)

# Read Observed data:
ob_data <-read.table("~/MAD_MODEL/SUR_MODEL/Code/Observed_data_2300.data", header=FALSE, sep= " ")
ob_data <- t(ob_data[,1:2000])
sum_1 <- rowSums(ob_data[,2:31])
sum_2 <- rowSums(ob_data[,31:648])
sum_3 <- rowSums(ob_data[,648:2000])

ob_data <- data.frame(time = ob_data[,1], sum1 = sum_1 , sum2 = sum_2 , sum3 = sum_3 )

y <- ob_data
# Example: plot the likelihood profile of the slope.
slopevalues <- function(par){
  return(likelihood(ob_data,c(par[1],par[2],par[3], trueSD),forcs_mat))
} 

length(seq(0,0.5,0.01))
mat <- as.matrix(expand.grid(seq(0,0.5,0.01), seq(0,0.5,0.01),seq(0,0.5,0.01)))
slopelikelihoods <- apply(mat,1, slopevalues)
mat <- as.data.frame(mat)
mat$LL <- slopelikelihoods

filename <- paste0("~/MAD_MODEL/SUR_MODEL/Code/LL_3D_sum_",Sys.Date(),".RData") 
save(mat, file = filename)

print("LL Computation finish")