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

N_p <- 4
init <- runif(N_p,0,1)
# init <- c(0.2,0)
end_t <- 50
cc <- 0.7
ee <- 0.5
ee <- ee-1
out <- hanski_DS(N_p,end_t,init, c(cc,ee))
out
