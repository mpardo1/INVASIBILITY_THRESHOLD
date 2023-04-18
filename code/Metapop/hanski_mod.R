theme_set(ggplot2::theme_bw() +
            ggplot2::theme(axis.title = element_text(size  = 15, color = 'black'),
                           axis.text = element_text(size = 15, color = 'black'),
                           legend.text = element_text(size = 15, color = 'black'),
                           legend.title = element_text(size = 15, color = 'black')))

library("ggplot2")
library("latex2exp")
library("tidyverse")
library("parallel")
library("deSolve")
library("ggplot2")

##### FUNCTIONS #####

###### DISCRETE #######
##### Hanski: system of equations #####

hanski_DS <- function(N,end_time, ic, param) {
  p <- matrix(0, nrow = end_time, ncol = N)
  p[1,] <- ic
  c <- param[1]
  e <- param[2]
  for (k in c(2:end_time)) {
      p[k,] <- c*(1-p[k-1,]) - e*p[k-1,]
  }
  return(p)
}

N_p <- 1
init <- runif(N_p,0,1)
# init <- c(0.2,0)
end_t <- 50
cc <- 1
ee <- 9
ee <- ee-1
out <- hanski_DS(N_p,end_t,init, c(cc,ee))

# Loop to see the paramater space biologically meaningfull
p_vec <- seq(0,1,0.1)
c_vec  <- seq(0.01,1,0.005)
e_vec  <- seq(0.01,1,0.005)
l <- length(p_vec)
out_df <- data.frame(c = 0, e =0, p = 0, sig = 0)
for(i in (1:length(c_vec))){
  for(j in (1:length(c_vec))){
    f <- function(init){hanski_DS(N_p,end_t,init, c(c_vec[i],e_vec[j]))}
    p_out <- sapply(p_vec,f)
    vec_sign <- ifelse(colSums(p_out<0) >0,1,0)
    df <- data.frame(c = rep(c_vec[i],l), e = rep(e_vec[j],l), p = p_vec, sig =  vec_sign)
    out_df <- rbind(out_df, df)
  }
}

df_filt <- out_df[which(out_df$sig == 0),]
ggplot(df_filt) +
  geom_point(aes(c,e, color = p), size = 0.5)

#----------------------------------------------------------------------------#
###### CONTINUOUS #######

library("tidyverse")
library("parallel")
library("deSolve")
library("ggplot2")

##### FUNCTIONS #####

##### 1.SIR: system of equations #####

hanski_DS <- function(t, y, parameters) {
  with(as.list(c(y, parameters)),{
    dy <- c()
    for(i in c(1:dime)){
      dy[i] <- col*(1 - y[i]) - ext*y[i]    
      }
    list(dy)
  })
}

##### 2.integrate the system #####

int <- function(N, c, e, init_p, end_time){
  # create vector of parameters for ode function:
  parameters <- list(
    dime = N,
    col = c,
    ext = e)
  
  # time steps for integration:
  times = seq(0,end_time, 0.1)
  
  # initial values:
  pops <- c(init_p)
  
  # run integration:
  z <- ode(pops, times, hanski_DS, parameters)
  return(z)
}


# Example of integration
dim <- 2
c <- 0.2
e <- 0.6
eq <- c/(c + e)
out <- as.data.frame(int(dim, c, e, c(0.1,0.2), 10))
colnames(out) <- c("time", "p1", "p2")
ggplot(out) +
  geom_line(aes(time,p1)) + 
  geom_hline(yintercept = eq, linetype = "dashed", color = "red")
