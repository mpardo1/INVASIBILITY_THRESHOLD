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
c_vec  <- seq(0,10,0.1)
e_vec  <- seq(0,10,0.1)
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

