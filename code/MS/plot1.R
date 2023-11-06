## Code that Compare the PA data for albopictus in comparison with
# the number o months in which R0>1 and the avg R0
rm(list=ls())
library(mapSpain)
library(ggplot2)
library("ggpubr")
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R")

# R_M ---------------------------------------------------------------------
vec <- seq(5,40,0.001)
aegypti <- sapply(vec,R0_func_aeg, hum = 500,rain = 8)
albopictus <- sapply(vec,R0_func_alb, hum = 500,rain = 8) 
japonicus <- sapply(vec,R0_func_jap, hum = 500,rain = 8) 

df_out <- data.frame(vec, aegypti = aegypti,
                     albopictus = albopictus,
                     japonicus = japonicus)
df_out <- reshape2::melt( df_out, id.vars = "vec")

library(RColorBrewer)
name_pal = "Set1"
display.brewer.pal(3, name_pal)
pal <- brewer.pal(3, name_pal)
letsize = 16
library("latex2exp")
plot_temp <- ggplot(df_out) + 
  geom_line(aes(vec,value, color=variable), size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ylab(TeX("$R_M$")) + 
  scale_color_manual(name = "", values =pal,
                     labels = c(expression(italic("Ae. aegypti")),
                                expression(italic("Ae. albopictus")),
                                expression(italic("Ae. japonicus")))) +
  xlab("Temperature") +
  scale_x_continuous(breaks = seq(5,41,4)) +
  theme_bw() + theme(legend.position = c(0.18,0.75),
                     text = element_text(size = letsize),
                     legend.text.align = 0)
# 
# ggplot(df_out[which(df_out$variable == "albopictus"),]) + 
#   geom_line(aes(vec,value), size = 1) +
#   geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
#   ylab(TeX("$R_M$")) + theme_bw() +
#   theme(legend.position = c(0.18,0.75),
#         text = element_text(size = letsize),
#         legend.text.align = 0) + 
#   ggtitle(expression(italic("Ae. albopictus")))
# checks for the text :
esp = "albopictus"
min(df_out[which(df_out$variable == esp &
                   df_out$value >1), "vec"])
max(df_out[which(df_out$variable == esp &
                   df_out$value >1), "vec"])
max_r <- max(df_out[which(df_out$variable == esp &
                   df_out$value >1), "value"])
df_out[which(df_out$value == max_r), "vec"]

# rm as a function of rainfall ------------------------------------------
vec <- seq(0,16,0.001)
rain <- sapply(vec,R0_func_aeg, hum = 0, Te = 15.5)
df_rain <- data.frame(vec, rain)
plot_rain <- ggplot(df_rain) + 
  geom_line(aes(vec,rain)) +
  xlab("Rainfall, mm") + ylab(TeX("$R_M$")) + 
  theme_bw() + theme(text = element_text(size = letsize))

# rm as a function of human density ------------------------------------------
vec <- seq(0,1000,0.1)
hum <- sapply(vec,R0_func_aeg, rain = 0, Te = 15.5)
df_hum <- data.frame(vec, hum)
plot_hum <- ggplot(df_hum) + 
  geom_line(aes(vec,hum)) +
  xlab("Human density") + ylab(TeX("$R_M$")) + 
  theme_bw() + theme(text = element_text(size = letsize))

# join all the plots ------------------------------------------------------
ggarrange(plot_temp + ggtitle("A"),
          plot_rain + rremove("ylab")+ ggtitle("B"),
          plot_hum + rremove("ylab")+ ggtitle("C"),
          ncol = 3,
          widths = c(1,0.7,0.7))

