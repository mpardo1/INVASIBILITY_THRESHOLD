### Code to do a validation between female traps counts and R0
rm(list=ls())
library(mcera5)
library(mapSpain)
library(sf)
library(ggplot2)
library(tidyverse)
library(parallel)
library(data.table)
library("viridis")
library("gganimate")

# -------------------------Functions R0-------------------------#
# Main functions 
Briere_func <- function(cte, tmin, tmax, temp){
  outp <- temp*cte*(temp - tmin)*(tmax - temp)^(1/2)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Quad_func <- function(cte, tmin, tmax, temp){
  outp <- -cte*(temp - tmin)*(temp - tmax)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Lin_func <- function(cte, cte1, temp){
  outp <- temp*cte + cte1
  if(outp < 0 | is.na(outp)){
    outp <- 0.00001
  }
  return(outp)
}


Quad <- function(cte, cte1,cte2, temp){
  outp <- cte*temp^2 + cte1*temp + cte2
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

QuadN_func <- function(cte, c1, c2, temp){
  outp <- cte*temp^2 + c1*temp + c2
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}
### Incorporating rain and human density:
h_f <- function(hum, rain){
  # Constants: 
  erat = 0.5
  e0 = 1.5
  evar = 0.05
  eopt = 8
  efac = 0.01
  edens = 0.01
  
  
  hatch <- (1-erat)*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) +
    erat*(edens/(edens + exp(-efac*hum)))
  return(hatch)
}

#### -------------------------- Albopictus ------------------------- ####
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pLA_f_alb <- function(temp){Quad_func(0.002663,6.668,38.92,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){Briere_func(0.00006881,8.869,35.09,temp)} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  a <- a_f_alb(Te)
  f <- (1/2)*TFD_f_alb(Te)
  deltaa <- lf_f_alb(Te)
  dE <- dE_f_alb(Te)
  probla <- pLA_f_alb(Te)
  h <- h_f(hum,rain)
  deltaE = 0.1
  #R0 <- ((0.3365391*f*a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  R0 <- ((f*a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  return(R0)
}

#---------------------------------------------------------------#

## Data Catu 
Path <- "~/INVASIBILITY_THRESHOLD/data/Datos_Catu/gi_min_model_pred.RData"
load(Path)
unique(gi_min_model_pred$trap_name)

# Process data from Catu with trap data
trap_data <- setDT(gi_min_model_pred[,c("trap_name", "province",
                                        "city", "start_date","end_date",
                                        "females","precipitation", "mean_temperature",
                                        "population", "pred", "l7precipitation",
                                        "l14precipitation", "l21precipitation", 
                                        "trapping_effort", "pred")])
rm(gi_min_model_pred)
trap_data$prec7 <- trap_data$l7precipitation/7
trap_data$prec14 <- trap_data$l14precipitation/14
trap_data$trapping_effort <- as.numeric(trap_data$trapping_effort)
trap_data$females_daily <- trap_data$females/trap_data$trapping_effort
trap_data[which(trap_data$city == "La Bisbal de l'Empordà"), "population"] <- 10859
trap_data[, R0_alb := mapply(R0_func_alb, mean_temperature, prec7, population)]
trap_data$female_norm <- trap_data$females/max(trap_data$females)
trap_data$pred_norm <- trap_data$pred/max(trap_data$pred)

# Compare model sampling effort versus count females:
ggplot(trap_data) +
  geom_point(aes(start_date, female_norm), color = "red")+
  geom_point(aes(start_date, pred_norm), color = "blue")

# Normalized R0 between 0 and 1 to compare with counts
trap_data$R0_alb_norm <- trap_data$R0_alb/max(trap_data$R0_alb)
unique(trap_data$city)

library(RColorBrewer)
name_pal = "Dark2"
pal = brewer.pal(3, name_pal)

## Function with the plots
plotcorr <- function(city_name, year){
  trap_data_filt <- trap_data[which(trap_data$city == city_name),] %>%
    group_by(city, start_date) %>% 
    summarise(start_date, female_norm = mean(female_norm),
              R0_alb_norm = min(R0_alb_norm))
  plot_df <- reshape2::melt(trap_data_filt[, c("start_date",
                                               "female_norm",
                                               "R0_alb_norm")], id.vars = "start_date")
  
  # Add a zero female at the beggining of each year in order to show
  # a nice plot other wise the last year number of females join the first record 
  # next year
  plot_df$year <- lubridate::year(plot_df$start_date)
  plot_df$start_date <- as.Date(plot_df$start_date)
  year_date <- plot_df %>%
    group_by(year) %>% summarize(min_date = min(start_date))
  for(i in c(1:nrow(year_date))){
    plot_df[(nrow(plot_df)+1),1] <- as.Date(year_date$min_date[i]) -1
    plot_df[(nrow(plot_df)),2] <- "R0_alb_norm"
    plot_df[(nrow(plot_df)),3:4] <-c(0, lubridate::year(year_date$min_date[i]))
  }
  
  if(class(year) == "numeric"){
    ## Plot the results 
    plot1 <- ggplot(plot_df[which(lubridate::year(as.Date(plot_df$start_date)) == year),]) + 
      geom_line(aes(start_date, value, colour = variable)) +
      ggtitle(paste0(city_name," ", year)) +
      scale_color_manual(name ="",
                         labels = c("Number female normalized",
                                    "Relative R_M"),
                         values = c(pal[1], pal[2]))  +
      xlab("Date") +
      theme_bw() + 
      theme(legend.position = "bottom")
  }else{
    ## Plot the results 
    plot1 <- ggplot(plot_df) + 
      geom_line(aes(start_date, value, colour = variable)) +
      ggtitle(paste0(city_name)) +
      scale_color_manual(name ="",
                         labels = c("Number of females normalized",
                                    TeX("Relative $R_M$")),
                         values = c(pal[1], pal[2]))  +
      xlab("Date") +
      theme_bw() + 
      theme(legend.position = "bottom")
  }

  
  library("latex2exp")
  pearson <- cor.test(trap_data_filt$R0_alb_norm,
                      trap_data_filt$female_norm,
                      method = "pearson", conf.level = 0.95)
  
  plot2 <- ggplot(data = trap_data_filt, 
         mapping = aes(x = R0_alb_norm, 
                       y = female_norm)) +
    geom_point(color = '#0f993d', size = 1) +
    geom_smooth(method=lm, se=FALSE) + 
    ggtitle(paste0(" R=", round(as.numeric(pearson$estimate),3),
                   ", p =", format(as.numeric(pearson$p.value), scientific = TRUE, big.mark = ","))) +
    xlab(TeX("Relative $R_M$")) +
    ylab("Normalized number of females") +
    theme_bw()
  
  return(list(plot1,plot2))
}

# Compute plots 
unique(trap_data$city)
city = unique(trap_data$city)[1]
## If instead of a year there is a character the plot it is with all the data
# with all the years avaliable

year = "2020"
plots <- plotcorr(city, year)
ggarrange(plots[[1]],NULL, plots[[2]],
          nrow = 1, widths = c(1.2, 0.05, 0.8),
          common.legend = TRUE)
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/Corr_PA_",city,".pdf")
dev.copy2pdf(file=Path, width = 7, height = 3)

