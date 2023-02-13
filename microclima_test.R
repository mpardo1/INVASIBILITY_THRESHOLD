rm(list=ls())

library(microclima)
library(raster)
head(microfitdata)
fitmicro(microfitdata)
# = = = = Example: topographic effects on shortwave radiation = = = = = =
# =================================
# Extract data for 2010-05-24 11:00
# =================================
dni <-dnirad[,,3444]
dif <-difrad[,,3444]
# ===========================
# Resample to 100m resolution
# ===========================
dnir <- raster(dni, xmn = -5.40, xmx = -5.00, ymn = 49.90, ymx = 50.15)
difr <- raster(dif, xmn = -5.40, xmx = -5.00, ymn = 49.90, ymx = 50.15)
crs(dnir) <- '+init=epsg:4326'
crs(difr) <- '+init=epsg:4326'
dnir <- projectRaster(dnir, crs = "+init=epsg:27700")
difr <- projectRaster(difr, crs = "+init=epsg:27700")
dni <- resample(dnir, dtm100m)
dif <- resample(difr, dtm100m)
sv <- skyviewtopo(dtm100m) # calculates skyview correction factor
jd <- julday(2010, 5, 24) # calculates astronomical julian day
ha <- mean_slope(dtm100m) # calculates mean slope to horizon
# ================================================================
# Calculate and plot net shortwave radiation for 2010-05-24 11:00
# ================================================================
netshort100m <- shortwavetopo(dni, dif, jd, 11, dtm = dtm100m,
                              svf = sv, ha = ha)
plot(mask(netshort100m, dtm100m),
     main = expression("Net shortwave radiation" ~ (MJ ~ m^{-2} ~
                                                      hour^{-1})))
