rm(list = ls())
library("ggplot2")
library("latex2exp")
library("tidyverse")
library("parallel")
library("deSolve")
library("ggplot2")
theme_set(ggplot2::theme_bw() +
            ggplot2::theme(axis.title = element_text(size  = 15, color = 'black'),
                           axis.text = element_text(size = 15, color = 'black'),
                           legend.text = element_text(size = 15, color = 'black'),
                           legend.title = element_text(size = 15, color = 'black')))



Path <- "~/INVASIBILITY_THRESHOLD/output/Estimation/"
listfile <- list.files(Path)
l <- length(listfile)
df_out <- data.frame(c = numeric(),b= numeric(),xi= numeric(),
                     alp= numeric(),va= numeric(), likeli = numeric(), conv = numeric())
for(j in c(1:l)){
  load(paste0(Path,listfile[[j]]))
  for (i in c(1:300)) {
    if(length(lhs[[i]]) == 5){
      df_out[i*j,] <- c(lhs[[i]]$par, lhs[[i]]$value, lhs[[i]]$convergence)
    }else{
      df_out[i*j,] <- c(0,0,0,0,0,0,0)
    }
    
  }
}

df_conv <- df_out[which(df_out$conv == 0),]
df_conv$diff <- abs(df_conv$c - 0.1) +  abs(df_conv$b - 0.2) +
  abs(df_conv$xi - 0.5) +  abs(df_conv$alp - 100) 
