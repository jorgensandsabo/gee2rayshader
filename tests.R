# Load libraries
library(reticulate)
use_condaenv('gee_interface', conda = "auto", required = TRUE) # point reticulate to the conda environment created in GEE_setup.sh
ee <- import("ee")          # Import the Earth Engine library
ee$Initialize()             # Trigger the authentication
np <- import("numpy")       # Import Numpy        needed for converting gee raster to R raster object
pd <- import("pandas")      # Import Pandas       ditto the above

################
### TEST GEE ###
################

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
# Should not work
region <- c(5.7,8.4,-73.4)
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
testHansen <- get_gee(region = region, buffer.width = 5000,
                      rgb = "UMD/hansen/global_forest_change_2018_v1_6", red.band = "last_b30", green.band = "last_b40", blue.band = "last_b50", rgbfunc = NULL, rgbtype = "image")


###########################
#### TEST prep_rasters ####
###########################
#prep_rasters <- function(rasterElev, rasterRGB, labels = NULL,
#                         projection = "albersequalarea", resolution = 30.922080775909325,
#                         lightness = 1, stretch.method = NA, stretch.quantiles = c(0.02,0.98))
# Rectangular/circle shape
region <- raster::extent(sp::SpatialPoints(cbind(c(6.0,6.1),c(58.80,58.90)),proj4string=sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")))
testSRTM <- get_gee(region = region, dem = "USGS/SRTMGL1_003", dem.band = "elevation")
prepSRTM <- prep_rasters(rasterElev = testSRTM$rasterElev, rasterRGB = testSRTM$rasterRGB)

region <- data.frame(cbind(6.224986,58.986239))
testHansen <- get_gee(region = region, buffer.width = 10000, shape = "circle",
                      rgb = "UMD/hansen/global_forest_change_2018_v1_6", red.band = "last_b30", green.band = "last_b40", blue.band = "last_b50", rgbfunc = NULL, rgbtype = "image")
prepHansen <- prep_rasters(rasterElev = testHansen$rasterElev, rasterRGB = testHansen$rasterRGB)

# Projection change
region <- cbind(c(6.0,6.1,6.5),c(58.80,58.90,58.85))
testALOS <- get_gee(region = region,buffer.width=0)
prepProj1 <- prep_rasters(rasterElev = testHansen$rasterElev, rasterRGB = testHansen$rasterRGB, projection = NA)
prepProj2 <- prep_rasters(rasterElev = testHansen$rasterElev, rasterRGB = testHansen$rasterRGB, projection = "+init=epsg:32632")
SHOULDNOTWORK <- prep_rasters(rasterElev = testHansen$rasterElev, rasterRGB = testHansen$rasterRGB, projection = "+init=epsg:1234")

# Projection resolution change
prepRes1 <- prep_rasters(rasterElev = testHansen$rasterElev, rasterRGB = testHansen$rasterRGB, resolution = 20)
prepRes2 <- prep_rasters(rasterElev = testHansen$rasterElev, rasterRGB = testHansen$rasterRGB, resolution = 500)

# Different projections
region <- data.frame(cbind(6.224986,58.986239))
testALOS <- get_gee(region = region,buffer.width=5000,shape="circle")
testALOS[[1]] <- raster::projectRaster(testALOS[[1]],crs="+init=epsg:32632",res=20)
testALOS[[2]] <- raster::projectRaster(testALOS[[2]],crs="+init=epsg:3857",res = 30)
prepProj3 <- prep_rasters(testALOS$rasterElev,testALOS$rasterRGB)

######################
### TEST Rayshader ###
######################
library(rayshader)

testprep <- prepProj3
zscale <- 30.922080775909325
#zscale <- 500
heightmap <- testprep$elevation_m
overlay <- testprep$RGB_m
lab_pts <- testprep$lab_m

rgl::clear3d()
heightmap %>%
  sphere_shade(texture = "desert",zscale=zscale) %>%
  add_overlay(overlay, alphacolor = NULL, alphalayer = 0.99) %>%
  add_shadow(ray_shade(heightmap, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(heightmap),0.5) %>%
  plot_3d(heightmap, zscale = zscale, fov = 30, lineantialias = TRUE, theta = 45, phi = 35, zoom = 0.8)

#for(i in 1:nrow(lab_pts)){
#  render_label(heightmap = heightmap, 
#               x = dim(heightmap)[1]*(lab_pts[i,1] - raster::extent(alos_ls_prep$rasterElev)[1])/(raster::extent(alos_ls_prep$rasterElev)[2] - raster::extent(alos_ls_prep$rasterElev)[1]),
#               y = dim(heightmap)[2]*(lab_pts[i,2] - raster::extent(alos_ls_prep$rasterElev)[3])/(raster::extent(alos_ls_prep$rasterElev)[4] - raster::extent(alos_ls_prep$rasterElev)[3]), 
#               z = 1500, linewidth = 1, zscale = zscale, text = rownames(lab_pts)[i],relativez = T,
#               linecolor="red",textcolor="red",freetype=F)
#}

#render_depth(focus = 0.5, focallength = 10, clear = TRUE,filename="test.png")
render_snapshot(filename = "test.png")
#render_highquality(filename = "test.png")
rgl::clear3d()






  