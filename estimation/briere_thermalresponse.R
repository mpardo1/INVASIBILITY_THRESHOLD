rm(list = ls())
start_time <- Sys.time()
library("deSolve")
library("parallel")
###-----------Briere---------------##
Path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/japonicus_temp_developmenttime.csv"
developL <- read.csv(Path)
head(developL)
developL$First_instar_mean <- 1/as.numeric(gsub(",", ".",developL$First_instar_mean))
developL <- developL[,c("Temp","First_instar_mean")]
plot_dE <- ggplot(developL) + 
  geom_point(aes(Temp,First_instar_mean)) + theme_bw()
plot_dE

# Main functions 
Briere_func <- function(cte, tmin, tmax, temp){
  outp <- temp*cte*(temp - tmin)*(tmax - temp)^(1/2)
  # if(outp < 0 | is.na(outp)){
  #   outp <- 0
  # }
  return(outp)
}

### Likelihood Function
ll_ode <- function(data,x){ # data: df with data, x: param
  # Penalize parameters not positive                
  c = x[1] # Colonization constant
  c1 = x[2] # Extinction constant
  c2 = x[3] # Spread constant
  colnames(data)  <- c("Temp","Develop" )
  va  <- var(data$Develop)
  
  Bf <- function(temp){Briere_func(c,c1,c2,temp)}
  z <- sapply(data$Temp,Bf)
  
  res <- sum(dnorm(data$Develop, mean = z, sd = 0.01, log = T))
  
  if(x[2] >= x[3] | x[3] < max(data$Temp)){
    print("penalized")
    res <- -20000000
  }else{
    # print("Work")
  }
  
  return(res)
}

x = c(0.00035,9.5,36)
data = developL
ll_ode(developL,c(0.00035,9.5,36))

number_it <- 100
num_param <- 3
seeds <- matrix( 0, ncol = number_it, nrow = num_param)
seeds[1,] <- runif(number_it,0,0.0004)
seeds[2,] <- runif(number_it,8,10)
seeds[3,] <- runif(number_it,34,37)

sols <- NA #Pre-aloco el número de combinaciones paramétricas en 2 unidades de LL de la mejor
best <- -999999999 #LL inicial a mejorar

set.seed(476468713)

condition <- T 
round <- 1
sims <- ncol(seeds) #Número de combinaciones paramétricas a explorar

Cores <- 1
# Cores <- parallel::detectCores()#Numero de cores a utilizar.
it <- 0
while(condition){
  start_time <- Sys.time()
  # print(paste0("Iteration: ", it))
  #Ahora viene la paralelización
  parall <- mclapply(1:sims, mc.cores = Cores, mc.preschedule = F,function(k){
    # print(paste("k:",k))
    start_time1 <- Sys.time()
    fit <- optim(par = seeds[,k], fn = ll_ode,
                 data = developL,
                 control = list(fnscale = -1,
                                maxit = 500,
                                parscale = seeds[, k]))
    
    
    if((k %% 1000) == 0) {
      cat("This was seed no. ", k, "\n")
      cat("This fit: ", fit$value, "\n")
    }
    
    fit
  })
  
  end_time <- Sys.time()
  diff_time <- end_time - start_time
  # print(paste0("Execution time iteration i:",it))
  print(diff_time)
  it <- it + 1
  lhs <- parall
  rm(parall) #Para evitar fugas de memoria
  
  filename <- paste0("~/INVASIBILITY_THRESHOLD/output/estimation/param_Briere",Sys.Date(), "_",round, ".RData") #Salva cada ronda de optimizaciones, por si acaso
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
  if(sols < 50){
    index <- order(logl, decreasing = T)[1:50]
  } else {
    index <- order(logl, decreasing = T)[1:sols]
  }
  
  n <- 1
  parmat <- matrix(NA, nrow = length(index), ncol = num_param)
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

### Read output
Path = "~/INVASIBILITY_THRESHOLD/output/estimation/param_Briere2023-05-10_10.RData"
load(Path)

lhs[[1]]$par
i = 10
mod <- function(te){
  c <- 0.00035#as.numeric(lhs[[i]]$par[1])
  c1 <- 9.5#as.numeric(lhs[[i]]$par[2])
  c2 <- 36#as.numeric(lhs[[i]]$par[3])
  Briere_func(c, c1, c2, te)
}

vec <- seq(0,45,0.001)
df_out_dE  <- data.frame(temp_ae = vec,
                         dE_jap <- sapply(vec, mod))

colnames(df_out_dE) <- c("temp_ae","dE_jap")
df_out_dE[which(df_out_dE$dE_jap < 0),2] <- 0

plotdE <- ggplot(df_out_dE) +
  geom_line(aes(temp_ae,dE_jap), size = 0.7) +
  geom_point(data = developL,aes(Temp,First_instar_mean), size = 0.9, color = "red") +
  xlim(c(0,45)) + 
  ylab("Develop rate from Egg to Larva") + xlab("Temperature (Cº)") +
  theme_bw()
plotdE

######c(0.00035,9.5,36) this looks good
Path <- "~/INVASIBILITY_THRESHOLD/data/japonicus/japonicus_temp_developmenttime.csv"
developL <- read.csv(Path)
head(developL)
developL$First_instar_mean <- 1/as.numeric(gsub(",", ".",developL$First_instar_mean))

plot_dE <- ggplot(developL) + 
  geom_point(aes(Temp,First_instar_mean)) + theme_bw()
plot_dE

Fitting_dE <- nls(First_instar_mean ~ cont*Temp*(Temp-cont1)*(cont2-Temp)^(1/2) ,
                  data = developL,
                  start = list(cont = 0.00035, cont1 = 9.5, cont2 = 36))

summary(Fitting_dE)

mod <- function(te){
  c <- as.numeric(Fitting_dE$m$getPars()[1])
  c1 <- as.numeric(Fitting_dE$m$getPars()[2])
  c2 <- as.numeric(Fitting_dE$m$getPars()[3])
  c*te*(te-c1)*(c2-te)^(1/2)
}

vec <- seq(0,45,0.001)
df_out_dE  <- data.frame(temp_ae = vec,
                         dE_jap <- sapply(vec, mod))
colnames(df_out_dE) <- c("temp_ae","dE_jap")
df_out_dE[which(df_out_dE$dE_jap < 0),2] <- 0

plotdE <- ggplot(df_out_dE) +
  geom_line(aes(temp_ae,dE_jap), size = 0.7) +
  geom_point(data = developL,aes(Temp,First_instar_mean), size = 0.9, color = "red") +
  xlim(c(0,45)) + 
  ylab("Develop rate from Egg to Larva") + xlab("Temperature (Cº)") +
  theme_bw()
plotdE
