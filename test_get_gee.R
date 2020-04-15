################
### TEST GEE ###
################

library(reticulate)
use_condaenv('gee_interface', conda = "auto", required = TRUE) # point reticulate to the conda environment created in GEE_setup.sh
ee <- import("ee")          # Import the Earth Engine library
ee$Initialize()             # Trigger the authentication
np <- import("numpy")       # Import Numpy        needed for converting gee raster to R raster object
pd <- import("pandas")      # Import Pandas       ditto the above

###### TEST REGION TYPES
# vector of 1 points
region <- c(6.224986,58.986239)
# matrix of 1 point
region <- cbind(6.224986,58.986239)
# data frame of 1 point
region <- data.frame(cbind(6.224986,58.986239))
# matrix of > 1 points
region <- cbind(c(6.0,6.1,6.5),c(58.80,58.90,58.85))
# data frame of > 1 points
region <- data.frame(cbind(c(6.0,6.1,6.5),c(58.80,58.90,58.85)))
# extent object
region <- raster::extent(sp::SpatialPoints(cbind(c(6.0,6.1,6.5),c(58.80,58.90,58.85)),proj4string=sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")))
### test
testRegion <- get_gee(region = region)
testRegion <- get_gee(region = region, buffer.width = 0)   # For multiple points
testRegion <- get_gee(region = region, shape = "circle")   # For single points

###### TEST OTHER REGION INPUTS
region <- cbind(c(6.0,6.1,6.5),c(58.80,58.90,58.85))
testInputs <- get_gee(region = region, buffer.width = 0, max.error = 5, scale = 30, max.pixels = 1e10)

###### TEST ELEVATION RASTER
region <- raster::extent(sp::SpatialPoints(cbind(c(6.0,6.1),c(58.80,58.90)),proj4string=sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")))
testALOS <- get_gee(region = region)
testSRTM <- get_gee(region = region, dem = "USGS/SRTMGL1_003", dem.band = "elevation")
testGMTED <- get_gee(region = region, dem = "USGS/GMTED2010", dem.band = "be75")  

###### TEST RGB RASTER
region <- data.frame(cbind(6.224986,58.986239))
testHansen <- get_gee(region = region,
                      rgb = "UMD/hansen/global_forest_change_2018_v1_6", red.band = "last_b30", green.band = "last_b40", blue.band = "last_b50", rgbfunc = NULL, rgbtype = "image")

