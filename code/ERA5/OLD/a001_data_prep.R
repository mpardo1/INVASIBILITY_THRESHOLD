# Preparation of trap data ####
# Written in R 4.0.3

rm(list=ls()) # clearing memory

# Dependencies ####
library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(lubridate)
library(RcppRoll)
library(suncalc) 
library(data.table)


# loading trap data ####
D_albopictus = Moschato_Tavros_bg_2018_2019 <- read_excel("data/raw/traps/Moschato_Tavros_bg_2018_2019.xlsx", sheet = "Aedes albopictus females") %>% mutate(start_date = as_date(Start_date), end_date = as_date(End_date), trapping_effort = end_date - start_date) %>% filter(!is.na(trap_name))

min(D_albopictus$trapping_effort, na.rm=TRUE)
max(D_albopictus$trapping_effort, na.rm=TRUE)

traps = Moschato_Tavros_bg_2018_2019 <- read_excel("data/raw/traps/Moschato_Tavros_bg_2018_2019.xlsx", sheet = "trap_info") 

traps$dlon = unlist(lapply(traps$Longitude, function(x) as.numeric(str_split(x, " ")[[1]][1]) + as.numeric(str_split(x, " ")[[1]][2])/60 + as.numeric(str_replace(str_split(x, " ")[[1]][3], ",", "."))/3600))

traps$dlat = unlist(lapply(traps$Latitude, function(x) as.numeric(str_split(x, " ")[[1]][1]) + as.numeric(str_split(x, " ")[[1]][2])/60 + as.numeric(str_replace(str_split(x, " ")[[1]][3], ",", "."))/3600))

traps = traps %>% st_as_sf(coords = c("dlon", "dlat"), crs=4326, remove = FALSE)

# quick visualization of locations:
# leaflet() %>% addProviderTiles("Esri.WorldImagery") %>% addCircleMarkers(data = traps, color = "yellow", radius = 5)

# quick visualization of counts
# ggplot(D_albopictus, aes(x=end_date, y=nrperspecies, color=trap_name)) + geom_line()

# loading ERA weather data ####
trap_locations = traps %>% select(`trap name`) %>% distinct()

sf_use_s2(FALSE) # turning off spherical geometry to avoid errors

gadm1 = st_make_valid(st_read("~/research/EuroTiger/data/external_data/gadm1-4_shapes/gadm1.shp") %>% filter(country == "Greece"))

# ggplot(gadm1) + geom_sf()

these_gid_1s = gadm1$gid_1

gadm2 = st_make_valid(st_read("~/research/EuroTiger/data/external_data/gadm1-4_shapes/gadm2_all_planet.shp") %>% filter(gid_1 %in% these_gid_1s)) %>% st_filter(trap_locations) 

names(gadm2)

weather_era5 = fread(file = "~/research/EuroTiger/data/proc/weather_gadm2.csv.gz") %>% as_tibble() %>% drop_na() %>% distinct() %>% filter(gid_2 == gadm2$gid_2)



# loading trap weather sensor data ####

BG_2_a_hobo_8_8_2018_19_12_2018 = read_csv("data/raw/traps/BG_2_a_hobo_8.8.2018_19.12.2018.csv", col_types = cols(`#` = col_skip(), `Coupler Detached (LGR S/N: 20363442)` = col_skip(), `Coupler Attached (LGR S/N: 20363442)` = col_skip(),`Host Connected (LGR S/N: 20363442)` = col_skip(), `End Of File (LGR S/N: 20363442)` = col_skip()), skip = 1) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363442, SEN S/N: 20363442, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363442, SEN S/N: 20363442, LBL: 100)") %>% mutate(date_time = as_datetime(date_time, "%m/%d/%y %I:%M:%S %p", tz = "Europe/Athens"), trap_name = "BG_2_T_2")

BG_2_b_hobo_1_1_19 = read_excel("data/raw/traps/BG_2_b_hobo_1_1_19 10_10_19.xlsx", skip=1, col_types = c("skip", "date", "numeric", "numeric", rep("skip", 4))) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363442, SEN S/N: 20363442, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363442, SEN S/N: 20363442, LBL: 100)") %>% mutate(date_time = as_datetime(date_time), trap_name = "BG_2_T_2") 

tz(BG_2_b_hobo_1_1_19$date_time) = "Europe/Athens"

BG_2_c_hobo_5_10_19_8_4_2020 = read_excel("data/raw/traps/BG_2_c_hobo_5_10_19 8_4_2020.xlsx", skip=1, col_types = c("skip", "date", "numeric", "numeric", rep("skip", 4))) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363442, SEN S/N: 20363442, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363442, SEN S/N: 20363442, LBL: 100)") %>% mutate(date_time = as_datetime(date_time), trap_name = "BG_2_T_2") 

tz(BG_2_c_hobo_5_10_19_8_4_2020$date_time) = "Europe/Athens"

BG_3_a_hobo_8_8_2018_19_12_2018 <- read_csv("data/raw/traps/BG_3_a_hobo_8.8.2018_19.12.2018.csv", col_types = cols(`#` = col_skip(), `Coupler Detached (LGR S/N: 20363443)` = col_skip(), `Coupler Attached (LGR S/N: 20363443)` = col_skip(),`Host Connected (LGR S/N: 20363443)` = col_skip(), `End Of File (LGR S/N: 20363443)` = col_skip()), skip = 1) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363443, SEN S/N: 20363443, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363443, SEN S/N: 20363443, LBL: 100)") %>% mutate(date_time = as_datetime(date_time, "%m/%d/%y %I:%M:%S %p", tz = "Europe/Athens"), trap_name = "BG_3_T_3")

BG_3_b_hobo_1_1_19_10_10_19_ = read_excel("data/raw/traps/BG_3_b_hobo_1_1_19 10_10_19..xlsx", skip=1, col_types = c("skip", "date", "numeric", "numeric", rep("skip", 4))) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363443, SEN S/N: 20363443, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363443, SEN S/N: 20363443, LBL: 100)") %>% mutate(date_time = as_datetime(date_time), trap_name = "BG_3_T_3") 


BG_4_a_hobo_8_8_2018_19_12_2018 = read_csv("data/raw/traps/BG_4_a_hobo_8.8.2018_19.12.2018.csv", col_types = cols(`#` = col_skip(), `Coupler Detached (LGR S/N: 20363444)` = col_skip(), `Coupler Attached (LGR S/N: 20363444)` = col_skip(),`Host Connected (LGR S/N: 20363444)` = col_skip(), `End Of File (LGR S/N: 20363444)` = col_skip()), skip = 1) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363444, SEN S/N: 20363444, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363444, SEN S/N: 20363444, LBL: 100)") %>% mutate(date_time = as_datetime(date_time, "%m/%d/%y %I:%M:%S %p", tz = "Europe/Athens"), trap_name = "BG_4_M_1")


BG_4_b_hobo_1_1_19_10_10_19 = read_excel("data/raw/traps/BG_4_b_hobo_1_1_19 10_10_19.xlsx", skip=1, col_types = c("skip", "date", "numeric", "numeric", rep("skip", 4))) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363444, SEN S/N: 20363444, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363444, SEN S/N: 20363444, LBL: 100)") %>% mutate(date_time = as_datetime(date_time), trap_name = "BG_4_M_1") 

BG_4_c_hobo_5_10_19_8_4_2020 = read_excel("data/raw/traps/BG_4_c_hobo_5_10_19 8_4_2020.xlsx", skip=1, col_types = c("skip", "date", "numeric", "numeric", rep("skip", 4))) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363444, SEN S/N: 20363444, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363444, SEN S/N: 20363444, LBL: 100)") %>% mutate(date_time = as_datetime(date_time), trap_name = "BG_4_M_1") 


BG_5_a_hobo_8_8_2018_19_12_2018 = read_csv("data/raw/traps/BG_5_a_hobo_8.8.2018_19.12.2018.csv", col_types = cols(`#` = col_skip(), `Coupler Detached (LGR S/N: 20363431)` = col_skip(), `Coupler Attached (LGR S/N: 20363431)` = col_skip(),`Host Connected (LGR S/N: 20363431)` = col_skip(), `End Of File (LGR S/N: 20363431)` = col_skip()), skip = 1) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363431, SEN S/N: 20363431, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363431, SEN S/N: 20363431, LBL: 100)") %>% mutate(date_time = as_datetime(date_time, "%m/%d/%y %I:%M:%S %p", tz = "Europe/Athens"), trap_name = "BG_5_M_2")

BG_5_b_hobo_1_1_19_10_10_19 = read_excel("data/raw/traps/BG_5_b_hobo_1_1_19 10_10_19.xlsx", skip=1, col_types = c("skip", "date", "numeric", "numeric", rep("skip", 4))) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363431, SEN S/N: 20363431, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363431, SEN S/N: 20363431, LBL: 100)") %>% mutate(date_time = as_datetime(date_time), trap_name = "BG_5_M_2") 

BG_5_c_hobo_5_10_19_8_4_2020_ = read_excel("data/raw/traps/BG_5_c_ hobo_5_10_19 8_4_2020..xlsx", skip=1, col_types = c("skip", "date", "numeric", "numeric", rep("skip", 4))) %>% rename(date_time = "Date Time, GMT+03:00", temp_c = "Temp, °C (LGR S/N: 20363431, SEN S/N: 20363431, LBL: 100)", RH_perc = "RH, % (LGR S/N: 20363431, SEN S/N: 20363431, LBL: 100)") %>% mutate(date_time = as_datetime(date_time), trap_name = "BG_5_M_2") 



hobos = bind_rows(BG_2_c_hobo_5_10_19_8_4_2020, BG_2_b_hobo_1_1_19, BG_2_a_hobo_8_8_2018_19_12_2018, BG_3_a_hobo_8_8_2018_19_12_2018, BG_3_b_hobo_1_1_19_10_10_19_, BG_4_a_hobo_8_8_2018_19_12_2018, BG_4_b_hobo_1_1_19_10_10_19, BG_4_c_hobo_5_10_19_8_4_2020, BG_5_a_hobo_8_8_2018_19_12_2018, BG_5_b_hobo_1_1_19_10_10_19, BG_5_c_hobo_5_10_19_8_4_2020_) %>% mutate(date = as_date(date_time), time = hms::as_hms(date_time))

names(hobos)

names(hobos)  
ggplot(hobos, aes(x=date_time, y=RH_perc)) + geom_line() + facet_grid(trap_name~.)

trap_locations = traps %>% st_drop_geometry()

library(parallel)
ncores = 12

sundata = bind_rows(mclapply(unique(hobos$date), function(this_date) {
  bind_rows(lapply(1:nrow(trap_locations), function(i){
    as_tibble(getSunlightTimes(date = this_date, lat = trap_locations$dlat[i], lon = trap_locations$dlon[i], tz = "EET")[, c("sunrise", "sunset")]) %>% mutate(date = this_date, trap_name = trap_locations$`trap name`[i])
  }))
  
}, mc.cores=ncores))

hobos = hobos %>% left_join(sundata) %>% mutate(time_since_sunrise = difftime(date_time, sunrise, units = "hours"), time_to_sunset = difftime(sunset, date_time, units="hours"), albotime = (time_since_sunrise < 1 | time_to_sunset < 1))

hobos %>% select(date_time, sunset, time_to_sunset, albotime)


# loading METEO weather data ####

these_dates = seq.Date(as_date("2018-06-01"), as_date("2019-12-01"), by="month")

this_date = these_dates[1]

meteo = bind_rows(lapply(these_dates, function(this_date){
  
  this_month = as.character(lubridate::month(this_date, label = TRUE, abbr = FALSE))
  
  this_year = year(this_date)
  
  max_days = as.integer(days_in_month(this_date))
  
  D = read_delim(paste0("data/raw/weather/", this_month, " ", this_year, ".txt"), delim=" ", trim_ws = TRUE, skip=11,n_max=max_days, col_names = c("DAY", "temp_mean",  "temp_high",   "time_temp_high",   "temp_low",    "time_temp_low",   "heat_deg_days",  "cool_deg_days",  "rain",  "wind_speed_ave", "wind_speed_high",   "time_wind_speed_high",    "wind_dom_direction")) %>% mutate(date = this_date + DAY - 1, FW_means = as.integer(wind_speed_ave <= (6*3.6)), FW_maxs = as.integer(wind_speed_high <= (6*3.6)), FT_means = case_when(temp_mean<=15~0, temp_mean> 30~0, (temp_mean > 15 & temp_mean <= 20)~ (.2*temp_mean)-3, (temp_mean>20 & temp_mean<=25)~1, (temp_mean>25 & temp_mean <= 30)~ (-.2*temp_mean)+6)) %>% select(-DAY)
  
}))

ggplot(meteo, aes(x = date, y=FW_maxs)) + geom_line()

head(hobos)

#### TEMP Meteo out ####

names(weather_gadm2)
hobos_era5 = hobos %>% mutate(month_int = as.integer(month(date)), year = year(date)) %>% rename(RH_perc_hobo = RH_perc) %>% left_join(weather_era5 %>% select(month_int, year, RH_perc = relative_humidity))

ggplot(hobos_era5, aes(x = date_time, y = RH_perc)) + geom_line() + geom_line(aes(x=date_time, y=era5_relative_humidity), color="red") + facet_wrap(.~trap_name)

hobos_daily = hobos_era5 %>% filter(albotime) %>% mutate(FH = case_when(RH_perc < 40~0, RH_perc >95~0, (RH_perc >=40 & RH_perc <= 95)~((RH_perc/55)-(40/55)) ), FT = case_when(temp_c<=15~0, temp_c>30~0, (temp_c>15 & temp_c <=20)~ (.2*temp_c)-3, (temp_c>20 & temp_c<=25)~1, (temp_c>25 & temp_c <= 30)~ (-.2*temp_c)+6)) %>% group_by(date, trap_name) %>% summarise(FT = max(FT), FH = max(FH), RH_perc = max(RH_perc), RH_perc_hobo = max(RH_perc_hobo), temp_c = max(temp_c)) %>% ungroup() %>% mutate(FH_m = case_when(RH_perc < 40~0, RH_perc >95~0, (RH_perc >=40 & RH_perc <= 95)~((RH_perc/55)-(40/55)) ), FT_m = case_when(temp_c<=15~0, temp_c>30~0, (temp_c>15 & temp_c <=20)~ (.2*temp_c)-3, (temp_c>20 & temp_c<=25)~1, (temp_c>25 & temp_c <= 30)~ (-.2*temp_c)+6)) #%>% left_join(meteo %>% select(date, FW_means, FW_maxs)) %>% mutate(mwi_maxwinds = FW_maxs*FH*FT, mwi = FW_means*FH*FT)

ggplot(hobos_daily, aes(x=date, y=FT)) + geom_line() + facet_grid(trap_name~.)

ggplot(hobos_daily, aes(x=date, y=FT_m)) + geom_line() + facet_grid(trap_name~.)

ggplot(hobos_daily, aes(x=date, y=RH_perc)) + geom_line() + facet_grid(trap_name~.)


ggplot(hobos_daily, aes(x=date, y=mwi_maxwinds)) + geom_line() + facet_grid(trap_name~.)

ggplot(hobos_daily, aes(x=date, y=mwi_meanwinds)) + geom_line() + facet_grid(trap_name~.)

names(hobos_daily)

hobos_weekly = hobos_daily %>% pivot_longer(cols=-c(date, trap_name), names_to = "weather_type", values_to="value") %>% group_by(weather_type, trap_name) %>% arrange(date) %>% mutate(mean_past_7days = roll_mean(value, n = 7, align = "right", fill=NA, normalize = FALSE, na.rm=TRUE)) %>% select(-value) %>% pivot_wider(names_from = weather_type, values_from = mean_past_7days) %>% ungroup() 


ggplot(hobos_weekly, aes(x=date, y=mwi)) + geom_line()

D = D_albopictus %>% left_join(hobos_weekly, by=c("end_date" = "date", "trap_name" = "trap_name"))


day_lengths = sundata %>% mutate(day_length = as.numeric(difftime(sunset, sunrise, units = "hours")))

D = D %>% left_join(day_lengths, by = c("end_date"= "date", "trap_name" = "trap_name"))

# names(D)

# ggplot(D, aes(x=end_date, y = RH_perc, color=trap_name)) + geom_line() + geom_line(data = D, aes(x=end_date, y= temp_c), color="red")

# ggplot(hobos_daily, aes(x=RH_perc_hobo, y=temp_c)) + geom_point()

# D %>% select(temp_c, RH_perc) %>% drop_na %>% cor


ggplot(D, aes(x=day_length, y = nrperspecies, color=trap_name)) + geom_point()

ggplot(D, aes(x=RH_perc, y = nrperspecies, color=trap_name)) + geom_point()


ggplot(D, aes(x=FH, y = nrperspecies, color=trap_name)) + geom_point()

ggplot(D, aes(x=FT, y = nrperspecies, color=trap_name)) + geom_point()
ggplot(D, aes(x=temp_c, y = nrperspecies, color=trap_name)) + geom_point()


write_rds(D, file="data/proc/albopictus_weather_prepared.Rds")