library(ncdf4)
era5_temp <- nc_open("~/INVASIBILITY_THRESHOLD/data/ERA5/adaptor.mars.external-1680271509.6859422-29720-9-a7d5dfd1-4c9f-4d22-be1b-389929d0079c.nc")
t2m_data <- ncvar_get(era5_temp, "t2m")

# Define the time and spatial scales of interest
start_date <- "2020-01-01"
end_date <- "2021-12-31"
lon_min <- -9.28 # Spain coordinates
lon_max <- 3.34
lat_min <- 36.00
lat_max <- 43.82

# Subset the data
temp_mean <- rclimate32::get_slice()(era5_temp, start_date, end_date,
                       c("2m_temperature"), lat_min, lat_max, lon_min, lon_max, aggregation = "mean")
temp_max <- rclimate32::get_slice()(era5_temp, start_date, end_date, 
                      c("maximum_2m_temperature_since_previous_post_processing"), 
                      lat_min, lat_max, lon_min, lon_max, aggregation = "max")
temp_min <- rclimate32::get_slice()(era5_temp, start_date, end_date,
                      c("minimum_2m_temperature_since_previous_post_processing"), 
                      lat_min, lat_max, lon_min, lon_max, aggregation = "min")
# Convert the data to Celsius
temp_mean <- temp_mean - 273.15
temp_max <- temp_max - 273.15
temp_min <- temp_min - 273.15