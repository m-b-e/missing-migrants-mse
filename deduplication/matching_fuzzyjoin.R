#===================================================================================================== 
#
#  >>>>>>> Estimating Migrants Deaths at Southern US-Border Using Multiple Systems Estimation. <<<<<<<<<<<<<
#
#
#           --------------Creating Match Candidates Using the Fuzzyjoin Package---------------
#                  1) Settings on the loading and processing of data
#                  2) Loading the data for three lists
#                  3) Preprocessing
#                  4) Performing Matching between Lists A, B, B, C, and A,C Using Fuzzyjoin Package
#                  5) Plotting
#
# Author: Max Eckert, MSc Data Science for Public Policy
#
# Version date:	  09/03/2024
#=======================================================================================================


# ===========================================================================
#   1) Settings on the loading and processing of data
# ===========================================================================


# Housekeeping ------------------------------------------------------------
rm(list = ls())

# Packages ----------------------------------------------------------------

# Packages to load
pckg_to_load <- c("dplyr", "fuzzyjoin", "readxl", "sf") 

# Load packages silently
suppressPackageStartupMessages(
  invisible(lapply(pckg_to_load, library, character.only = TRUE))
)

# ===========================================================================
#   2) Loading the data for three lists
# ===========================================================================

setwd("./data")
mmp <- read_excel("MMP_SDPCOME_2014_2017.xlsx", sheet = 1)
foia <- read_excel("USBP_FOIA_Cleaned.xlsx", sheet = 1)
sdpcome <- read_excel("PCOME_SD_2014_2017.xlsx")

# ===========================================================================
#   3) Preprocessing
# ===========================================================================

# Ensure Uniform Classes Across Columns
dfs <- c(mmp, sdpcome, foia)
dfs_class <- lapply(dfs, function(df) lapply(df, class))

# ===========================================================================
#   4) Matching
# ===========================================================================

#Matching mmp and foia on date +/- 3 days (measured in seconds so max_dist=259,200)
close_dates_mmp_foia <- difference_inner_join(mmp, foia, by = "date", max_dist = 259200, distance_col = "dist")
#Matching mmp and foia on date +/- 3 days
close_dates_mmp_sdpcome <- difference_inner_join(mmp, sdpcome, by = "date", max_dist = 259200, distance_col = "dist")
#Matching foia and pcome on date +/- 3 days
close_dates_foia_sdpcome <- difference_inner_join(foia, sdpcome, by = "date", max_dist = 259200, distance_col = "dist")

#Ensuring Lat/Lon are saved as numeric
close_dates_mmp_sdpcome$lat.y <- close_dates_mmp_sdpcome$lat.y %>% as.numeric()
close_dates_mmp_sdpcome$lon.y <- close_dates_mmp_sdpcome$lon.y %>% as.numeric()
close_dates_foia_sdpcome$lat.y <- close_dates_foia_sdpcome$lat.y %>% as.numeric()
close_dates_foia_sdpcome$lon.y <- close_dates_foia_sdpcome$lon.y %>% as.numeric()

#Setting the maximum distance between Longitude x and Longitude y, as well as Latitude x and Latitude y to 0.2 degrees 
threshold_distance <- 0.2

#Subsetting date matches by lat/lon: MMP/FOIA
matches_mmp_foia <- close_dates_mmp_foia %>%
  filter(abs(close_dates_mmp_foia$lat.x - close_dates_mmp_foia$lat.y) <= threshold_distance &
           abs(close_dates_mmp_foia$lon.x - close_dates_mmp_foia$lon.y) <= threshold_distance) %>% 
  mutate(lat_diff = abs(matches_mmp_foia$lat.x - matches_mmp_foia$lat.y),
         lon_diff = abs(matches_mmp_foia$lon.x - matches_mmp_foia$lon.y))

#Subsetting date matches by lat/lon: MMP/PCOME
matches_mmp_sdpcome <- close_dates_mmp_sdpcome %>%
  filter(abs(close_dates_mmp_sdpcome$lat.x - close_dates_mmp_sdpcome$lat.y) <= threshold_distance &
           abs(close_dates_mmp_sdpcome$lon.x - close_dates_mmp_sdpcome$lon.y) <= threshold_distance) %>% 
  mutate(lat_diff = abs(matches_mmp_sdpcome$lat.x - matches_mmp_sdpcome$lat.y),
         lon_diff = abs(matches_mmp_sdpcome$lon.x - matches_mmp_sdpcome$lon.y))


#Subsetting date matches by lat/lon: FOIA/SDPCOME
matches_foia_sdpcome <- close_dates_foia_sdpcome %>%
  filter(abs(close_dates_foia_sdpcome$lat.x - close_dates_foia_sdpcome$lat.y) <= threshold_distance &
           abs(close_dates_foia_sdpcome$lon.x - close_dates_foia_sdpcome$lon.y) <= threshold_distance) %>% 
  mutate(lat_diff = abs(matches_foia_sdpcome$lat.x - matches_foia_sdpcome$lat.y),
         lon_diff = abs(matches_foia_sdpcome$lon.x - matches_foia_sdpcome$lon.y))

# ===========================================================================
#   5) Plotting
# ===========================================================================

# MMP/FOIA: Generating Two Dataframes: Candidates X with Lat/Long X and Candidates Y with Lat/Lon Y
mmp_foia_candidates.x <- st_as_sf(matches_mmp_foia[,c(1:5,8,9)], coords = c("lon.x", "lat.x"), crs = 4326)
mmp_foia_candidates.y <- st_as_sf(matches_mmp_foia[,c(12:13,16:17,19:21)], coords = c("lon.y", "lat.y"), crs = 4326)

mapview::mapview(mmp_foia_candidates.x, color = "blue") + mapview::mapview(mmp_foia_candidates.y, color="red")

# FOIA/SDPCOME: Generating Two Dataframes: Candidates X with Lat/Long X and Candidates Y with Lat/Lon Y

sdpcome_foia_candidates.x <- st_as_sf(matches_foia_sdpcome[,c(1:2,5:10)], coords = c("lon.x", "lat.x"), crs = 4326)
sdpcome_foia_candidates.y <- st_as_sf(matches_foia_sdpcome[,c(12:15,17,21:22)], coords = c("lon.y", "lat.y"), crs = 4326)

mapview::mapview(sdpcome_foia_candidates.x, color = "blue") + mapview::mapview(sdpcome_foia_candidates.y, color="red")

