library(terra)
rast_list <- readRDS("~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/monthly_aggregated_raster.Rds")
raster_stack <- terra::stack(raster_list)
# Save the raster stack to a new file
terra::writeRaster(raster_stack, "~/INVASIBILITY_THRESHOLD/data/ERA5/Europe/monthly_aggregated_raster.tif", overwrite = TRUE)
