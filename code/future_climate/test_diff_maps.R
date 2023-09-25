# see in details the diff in the difference map why negative numbers
rm(list=ls())
# load df for 2004 and 2060
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/2020/monthly_clim_2060.Rds")
df_2060 <- readRDS(Path)
Path <- paste0("~/INVASIBILITY_THRESHOLD/output/ERA5/temp/R0_clim_monthly2004.Rds")
df_2004 <- readRDS(Path)[,c("NATCODE", "month", "tmean","prec", "R0_mon_alb")]

# see the str of the df
head(df_2060)
head(df_2004)

# select a natcode to check that it has diff<0 
test_NATCODE <- "34053838007"
f_2060 <- df_2060[df_2060$NATCODE == test_NATCODE,]
f_2004 <- df_2004[df_2004$NATCODE == test_NATCODE,]

# check if the correct variables have been filter
head(f_2060)
head(f_2004)

# transform the month variable in the same format
mymonths <- c("Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov","Dec")
f_2004$month <- mymonths[as.numeric(f_2004$month)]

# join two df
f_2004 <- f_2004[,c("NATCODE", "month","R0_mon_alb","tmean")] 
f_2060 <- f_2060[,c("NATCODE", "month","R0_alb","tmean")]
colnames(f_2004) <- c("NATCODE", "month","R0_alb_2004","tmean_2004")
df_join <- f_2004 %>% left_join(f_2060)

# load spain municipalities map
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
can_box <- esp_get_can_box()
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can[which(esp_can$NATCODE == test_NATCODE), ]
