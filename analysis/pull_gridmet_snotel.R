## ---------------------------
##
## Project: FS Wilderness Lakes
## Repository: https://github.com/mtnlimnolab/FS-wilderness-lakes
## Script name: pull_gridmet_snotel.R
## Purpose of script: Pull GridMET and SNOTEL data based on given coordinates (clean, standalone version that can be used for any site)
## Authors: MJ Farruggia 
## Date Created: Nov 2024
## Last Updated: Sept 2025
##
## ---------------------------

# install.packages(c("snotelr", "tidyverse","geosphere"))
# 
# library(tidyverse)
# library(snotelr)
# library(geosphere)



# SNOTEL -----------------------------------------------------------------------------
#  load in snotel data and match lakes to nearest snotel  
# uses the package snotelr. See https://bluegreen-labs.github.io/snotelr/index.html 

#to use the shiny GUI for data exploration:
  # install.packages(c("DT","shinydashboard", "plotly", "leaflet"))
   # snotel_explorer()


#define sites
site_locations <- data.frame(
  site_name = c("wolf creek summit"), # lake names, in quotes like c("Lake_1", "Lake_2", "Lake_3")
  lat_DD = c(37.48),   # latitude, decimal degrees
  lon_DD = c(-106.80)  # longitude, decimal degrees
)


#limit geographic scope from which we're pulling from (makes it run a little faster, otherwise it searches all of US)
#right now it icludes CO and WY; can change states as needed
#can also just use this df to view/scroll through all the snotel stations in the states you define here
snotel_sites <- snotel_info()[snotel_info()$state %in% c("CO","WY"), ] 

#Match lakes to nearest snotel site and pull out snotel site ID

calculate_distances <- function(lake_lat, lake_lon, sites_df) {
  distances <- distGeo(c(lake_lon, lake_lat), cbind(sites_df$lon, sites_df$lat))
  return(distances)
}

#can check the nearest_snotel df to make sure it's the station you want/expect
nearest_snotel <- site_locations %>%
  rowwise() %>%
  mutate({
    distances <- calculate_distances(lat_DD, lon_DD, snotel_sites)
    nearest_site_index <- which.min(distances)
    nearest_site = snotel_sites$site_name[nearest_site_index]
    distance_to_nearest_site = distances[nearest_site_index] / 1000
    tibble(nearest_site, distance_to_nearest_site)
  })

# Add site_id from snotel_sites 
nearest_snotel <- nearest_snotel %>%
  left_join(snotel_sites %>% select(site_name, site_id), by = c("nearest_site" = "site_name"))

unique_snotel_site_ids <- unique(nearest_snotel$site_id)

#get snotel data for each of the unique snotel siteIDs from above
snotel_data_combined <- data.frame()

# Loop through each snotel site_id, download using snotel_download, combine into one df
for (site in unique_snotel_site_ids) {
  snotel_data <- snotel_download(site_id = site, network = "sntl", path = tempdir(), internal = TRUE)
  
  snotel_data <- snotel_data %>%
    mutate(date = as.Date(date)) %>%
    mutate(snotel_site_id = site) %>%  
    select(snotel_site_id, site_name,latitude, longitude, elev, date, 
           snow_water_equivalent, snow_depth, precipitation_cumulative, 
           temperature_max, temperature_min, temperature_mean, 
           precipitation)  
  
  snotel_data_combined <- bind_rows(snotel_data_combined, snotel_data)
}


#if having download issues, test outside of loop here to check things. could be 1 troublesome site or something that's breaking the loop
#snotel_data <- snotel_download(site_id = 1251, network = "sntl", path = tempdir(), internal = TRUE)


#calculate snotel phenology for each site in unique_snotel_site_ids
      #note: the function snotel_phenology is from the snotelr package. calculates snow phenology from SWE data

snow_phenology_df <- data.frame()

for (site in unique_snotel_site_ids) {
  snotel_data <- snotel_download(site_id = site, network = "sntl", path = tempdir(), internal = TRUE)
  phenology_results <- snotel_phenology(snotel_data, threshold = 0, offset = 210)
  phenology_results <- phenology_results %>%
    mutate(snotel_site_id = site)
  snow_phenology_df <- bind_rows(snow_phenology_df, phenology_results)
}


#final SNOTEL dfs: snotel_data_combined (does not have calculated values, just raw) and snow_phenology_df (has calculated values)




# gridMET -----------------------------------------------------------------------------
#pull gridMET data based on given site coords.

#gridmet citation: https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.3413
#gridmet website: https://www.climatologylab.org/gridmet.html 
#this code uses the package climateR (p good documentation on this github repo): https://github.com/mikejohnson51/climateR


# remotes::install_github("mikejohnson51/AOI")
# remotes::install_github("mikejohnson51/climateR")
# install.packages("sf")
# 
# library(AOI)
# library(climateR)
# library(sf)



#define sites
site_locations <- data.frame(
  site_name = c("Turkey Creek Lake"), # lake names, in quotes like c("Lake_1", "Lake_2", "Lake_3")
  lat_DD = c(37.47798),   # latitude, decimal degrees
  lon_DD = c(-107.01783)  # longitude, decimal degrees
)

#convert to a spatial object (required for the climateR:getGridMET function)
site_locations_sf <- st_as_sf(
  site_locations,
  coords = c("lon_DD", "lat_DD"),  
  crs = 4326                       # EPSG:4326 = WGS84 geographic coordinate system
)



#specify what data to pull from gridmet
start_date <- "1980-01-01"
end_date <- "2023-12-31"
parameters <- c("pr", "tmmx", "tmmn", "pdsi")  # 'pr' for precipitation, 'tmmx' and 'tmmn' for max and min temperature. pdsi is a drought index.

#PDSI:
#product is a derived variable - 10-day Palmer Drought Severity Index
#5.0 or more (extremely wet), 4.0 to 4.99 (very wet), 3.0 to 3.99 (moderately wet),
#2.0 to 2.99 (slightly wet), 1.0 to 1.99 (incipient wet spell), -0.99 to 0.99 (near normal),
#-1.99 to -1.00 (incipient dry spell), -2.99 to -2.00 (mild drought), -3.99 to -3.00
#(moderate drought), -4.99 to -4.00 (severe drought), or -5.0 or less (extreme drought).


    #this loop can take a while to run, just FYI!
all_lake_gridmet <- list()
# Loop through each lake and download data for the nearest grid cell
for (i in 1:nrow(site_locations_sf)) {
  lake_location <- site_locations_sf[i, ]  
  
  # pull the gridMET data 
  gridmet_data <- climateR::getGridMET(
    AOI = lake_location,
    varname = parameters,
    startDate = start_date,
    endDate = end_date
  )
  
  # Add lake id to the gridMET data
  gridmet_data$site_name <- site_locations_sf$site_name[i]
  
  # Combine 
  all_lake_gridmet[[i]] <- gridmet_data  
}
gridmet_data <- bind_rows(all_lake_gridmet)



#clean up the columns:
  
  #gridmet temp is in K; convert Kelvin to Celsius
gridmet_data$tmmn <- gridmet_data$tmmn - 273.15
gridmet_data$tmmx <- gridmet_data$tmmx - 273.15
  
  # pdsi.x is PDSI (less variable; longer term) and pdsi.y is Z-index (more variable; shorter term ~30 days)
  #rename the columns for clarity
  colnames(gridmet_data)[colnames(gridmet_data) == "pdsi.x"] <- "pdsi"
  colnames(gridmet_data)[colnames(gridmet_data) == "pdsi.y"] <- "pdsi_zindex"
  
  #change pr to precip just because i like it better like this
  colnames(gridmet_data)[colnames(gridmet_data) == "pr"] <- "precip"


#final df:
# View(gridmet_data)
  
  
  

# optional - change gridmet precip to snow based on air temp -------------------------------------------------
# this is code originally from MJ's Emerald Lake zoop chapter ("04_gridmet_data_snow_calculations.R"). 
  #It works very well in the Emerald Basin based on comparisons to measured SWE. 
  #Would suggest doing some checks before using with confidence on other mtn ranges.

#add a mean temp column (calc based on min and max temp)
  gridmet_data_snow <- gridmet_data %>%
  mutate(mean_temp = round(rowMeans(select(., tmmn, tmmx), na.rm = TRUE), 2))


#calculate snow based on mean air temp (citation: Dingman 2002 - Physical Hydrology). Used in Adrianne's winter climate controls summer regimes paper: https://agupubs.onlinelibrary.wiley.com/doi/full/10.1029/2021JG006277
  #values based on a temp-based regression model. 
  #This calculation makes it so that:
    #if mean temp <0, all precip is snow
    #if mean temp 0-6C, snow = total_precip -0.1678 * mean air temp
    # if mean temp >6 then it's rain

# Create a new column for snowfall, calculated based on the above 
  gridmet_data_snow <- gridmet_data_snow %>%
  mutate(snowfall = case_when(
    mean_temp < 0 ~ precip,
    mean_temp >= 0 & mean_temp <= 6 ~ pmax(0, precip - 0.1678 * mean_temp), 
    mean_temp > 6 ~ 0
  ))


#add a column for water year (Oct 1 to Sept 31)
  gridmet_data_snow <- gridmet_data_snow %>%
  mutate(date = as.Date(date),
         month = as.numeric(format(date, "%m")),
         day = as.numeric(format(date, "%d")),
         year = as.numeric(format(date,"%Y")))
  #define water_year here
  gridmet_data_snow <- gridmet_data_snow %>%
  mutate(water_year = case_when(
    month >= 10 ~ year + 1,  # October–December = Next year
    TRUE ~ year              # January–September = Same year
  ))

#calculate total winter precip for each site based on "water_year" as defined above
gridmet_data_snow <- gridmet_data_snow %>%
  group_by(site_name, water_year) %>%
  mutate(total_WY_snowfall = sum(snowfall, na.rm = TRUE)) %>%
  ungroup()
  
  
  #get a df of just annual snow totals, based on water year
  
total_WY_snowfall <- gridmet_data_snow %>%
    group_by(site_name, water_year) %>%
    summarize(total_snowfall = sum(snowfall, na.rm = TRUE), .groups = "drop")

  
#final dfs: gridmet_data (this one has no snow or calculated values); gridmet_data_snow (has snow estimated from precip and temp, and mean temp estimated from min/max); total_WY_snowfall (just estimated snowfall for each water year/site)


