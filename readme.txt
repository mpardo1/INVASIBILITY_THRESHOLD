Code R0 paper
- Sensitivity analysis:
   * code/Sensitivity_analysis/sensitivy_analysis.R: code to compute the derivatives, also there is a comparison between each parameter between the species. Also, compute min, max and peak.
   * code/Sensitivity_analysis/sensitivy_analytic.R: Check if the numerical derivatives work.
- Thermal Responses fit:
   * code/Aedes/prob_LA.R : computes the fits for the literature data for the missing variables for aegypti and albopictus.
   * code/Aedes/Japonicus/japonicus_thermal.R : Computes the fits from the literature data for the variables related to Japonicus.
- Weather extraction:
   * code/ERA5/mcera5_weather.R: extract the weather for an specific year  from ERA5 using the mcera5 package.
   * code/ERA5/weather_ERA5_mcera5.R: process the nc file from mcera5_weather.R with the weather data in order to obtain hourly weather estimates. Very	 						   heavy use mcera5_dt_agg_daily.R to aggregate daily.
   * code/ERA5/mcera5_dt_agg_daily.R: process file ERA5_daily_mcera_2022.Rds from weather_ERA5_mcera5.R and aggregate daily.
   * code/ERA5/comp_ERA5_vs_mcera5.R: Compare the weather output from mcera5 and directly from era5.
   * code/ERA5/era5_temp.R: extract climate data from ERA5:
	https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=form
   * code/climatic data/ERA5.R: download with NATCODE at municipality level temp.
   * code/ERA5/agg_daily.R: aggregate in one data set temp (CERRA ERA5) and rainfall (mcera5 R).
   * code/ERA5/temp_CERRA.R : extract the weather as a RDS with NATCODE, date and temporal variables from CERRA data sets.
 - Maps:
   * code/MS/maps_mcera5_group.R: create maps from the weather files
   * code/Maps/diff_years.R: create maps where color defines the diff in sum months for two years.
   * code/Maps/maps_no_rainfall.R : Create maps with no hatching rate.
   * code/Maps/europe_map.R: Create the map for Europe as a raster.
   * /code/Europe/cl_agg_cerra.R: aggregate the raster hourly data to monthly data for Europe.
 - Validation:
   * code/MS/validate_count_time.R: Validation of the data of each trap weekly
   * code/MS/validate_PA.R: Validation of the PA data
   * code/europe/jap_val.R: code to do the validation for japonicus for europe.
 - Panel:
   * code/MS/panel_validation.R: panel with validation figures.
   * code/MS/panel_thermal.R: panel with thermal responses.
   * code/MS/plot1.R: the three plots, first RM depending on temp, second rain and third human density.
   * Year Arrival:
   * code/MS/arrivalmonth.R: code to extract the year of arrival of Aedes albopictus  per region, ccaa.