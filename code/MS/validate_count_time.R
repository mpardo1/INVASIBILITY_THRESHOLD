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

source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")
#---------------------------------------------------------------#

## Data Catu ------------------------------------------------------
Path <- "~/INVASIBILITY_THRESHOLD/data/Datos_Catu/gi_min_model_pred.RData"
load(Path)
unique(gi_min_model_pred$trap_name)

# Process data from Catu with trap data ----------------------------
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

# Compare model sampling effort versus count females ----------------
ggplot(trap_data) +
  geom_point(aes(start_date, female_norm), color = "red")+
  geom_point(aes(start_date, pred_norm), color = "blue")

# Normalized R0 between 0 and 1 to compare with counts --------------
trap_data$R0_alb_norm <- trap_data$R0_alb/max(trap_data$R0_alb)
unique(trap_data$city)
library("latex2exp")
library(RColorBrewer)
name_pal = "Dark2"
pal = brewer.pal(3, name_pal)

## Function with the plots
plotcorr <- function(city_name, year){
  trap_data_filt <- trap_data[which(trap_data$city == city_name),] %>%
    group_by(city, start_date) %>% 
    summarise(start_date,
              female = sum(females),
              R0_alb = mean(R0_alb))
  trap_data_filt$female_norm <- trap_data_filt$female/max(trap_data_filt$female)
  trap_data_filt$R0_alb_norm <- trap_data_filt$R0_alb/max(trap_data_filt$R0_alb)
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
      geom_line(aes(start_date, value,
                    colour = variable)) +
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
  
  # Exponential --------------------------------------------------------
  Fitting_fem <- nls(female_norm ~ exp(cont1*R0_alb_norm)*cont,
                         data = trap_data_filt,
                         start = list(cont = 0.001, cont1 = 0))
  
  summary(Fitting_fem)
  
  mod <- function(te){
    t0 <- as.numeric(Fitting_fem$m$getPars()[1])
    tm <- as.numeric(Fitting_fem$m$getPars()[2])
    t0*exp(tm*te)
  }
  
  # Plot with exponential fit -----------------------------------------------
  vec <- seq(0,1,0.01)
  df_out_fem <- data.frame(temp_ae = vec, fem <- sapply(vec, mod))
  plot3 <- ggplot(data = trap_data_filt, 
                  mapping = aes(x = R0_alb_norm, 
                                y = female_norm)) +
    geom_point(color = '#0f993d', size = 1) +
    geom_line(data = df_out_fem, aes(temp_ae , fem)) + 
    ggtitle(paste0("lambda:",
                   as.numeric(Fitting_fem$m$getPars()[2]))) +
    xlab(TeX("Relative $R_M$")) +
    ylab("Normalized number of females") +
    theme_bw()
  
  return(list(plot1,plot2, plot3))
}

# Compute plots 
unique(trap_data$city)
city = unique(trap_data$city)[1]
## If instead of a year there is a character the plot it is with all the data
# with all the years avaliable

year = "2020"
plots <- plotcorr(city, year)
library(ggpubr)
ggarrange(plots[[1]],NULL, plots[[3]],
          nrow = 1, widths = c(1.2, 0.05, 0.8),
          common.legend = TRUE)
Path <- paste0("~/Documentos/PHD/2023/INVASIBILITY/Plots/MS/Validation/Time/Corr_PA_",city,".pdf")
dev.copy2pdf(file=Path, width = 8,
             height = 3)

# DF of specific cities ---------------------------------------------------
list_cit <- list("Blanes", "Lloret de Mar", "Tordera", "Palafolls")
trap_data_filt <- trap_data[which(trap_data$city %in% list_cit)] %>%
  group_by(city, start_date) %>% 
  summarise(female = sum(females),
            R0_alb = mean(R0_alb))

# DF compute maximum -------------------------------------------------------
trap_citi_max <- trap_data_filt %>%
  group_by(city) %>% 
  summarise(female_max= max(female),
            R0_alb_max = max(R0_alb))

# DF compute norm female and R0  --------------------------------------------
trap_data_filt <- trap_data_filt %>% left_join(trap_citi_max)
trap_data_filt$female_norm <- trap_data_filt$female/trap_data_filt$female_max
trap_data_filt$R0_alb_norm <- trap_data_filt$R0_alb/trap_data_filt$R0_alb_max


ggplot(trap_data_filt) +
  geom_point(aes(R0_alb_norm,female_norm, color = city))

# Fit each dots by city -------------------------------------------------------
df_out_fem <- data.frame()
for(i in c(1:length(list_cit))){
  Fitting_fem <- nls(female_norm ~ exp(cont1*R0_alb_norm)*cont,
                     data = trap_data_filt[which(trap_data_filt$city == list_cit[[i]]),],
                     start = list(cont = 0.001, cont1 = 0))
  
  summary(Fitting_fem)
  
  mod <- function(te){
    t0 <- as.numeric(Fitting_fem$m$getPars()[1])
    tm <- as.numeric(Fitting_fem$m$getPars()[2])
    t0*exp(tm*te)
  }
  
  vec <- seq(0,1,0.01)
  df_aux <- data.frame(temp_ae <- vec,
                       fem <- sapply(vec, mod))
  df_aux$cit <- list_cit[[i]]
  colnames(df_aux) <- c("vec", "out", "cit")
  df_out_fem <- rbind(df_aux, df_out_fem)
  colnames(df_out_fem) <- c("vec", "out", "cit")
}

# Plot with dots and fit exp ---------------------------------------------
name_pal = "Set1"
display.brewer.pal(length(list_cit), name_pal)
pal <- rev(brewer.pal(length(list_cit), name_pal))
sizelet = 14
plot3 <- ggplot(data = trap_data_filt) +
          geom_point(aes(x = R0_alb_norm, 
                         y = female_norm, color = city), size = 0.8, alpha = 0.6) +
          geom_line(data = df_out_fem,
                    aes(vec , out, color = cit),
                    lwd = 0.8) +
          scale_color_manual(values = pal) +
          xlab(TeX("Relative $R_M$")) +
          ylab("Normalized number of females") +
          theme_bw() + 
          theme(text = element_text(size = sizelet))
plot3

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
