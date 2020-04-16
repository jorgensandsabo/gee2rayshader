#### Function to download elevation and cloud-reduced median landsat imagery for a region of interest
# JS test change to commit

#### Accepts vector/matrix/dataframe of single point coordinates, matrix/dataframe of multiple coordinates or a raster::extent object
#### Square/circle around single point
#### Rectangular boundary around vectors of points

#### Downloads AW3D30v2.2 elevation data by default
#### Downloads landsat imagery cloud-reduced using the simplecomposite function (percentile: 30, cloudScoreRange: 1) by default

get_gee <- function(region, shape = "square", buffer.width = 1000, max.error = 1, scale = 30.922080775909325, max.pixels = 1e20,
                    dem = "JAXA/ALOS/AW3D30/V2_2", dem.band = "AVE_DSM", demfunc = NULL, demtype = "image",
                    rgb = "LANDSAT/LC08/C01/T1", red.band = "B4", green.band = "B3", blue.band = "B2", rgbfunc = landsatCF, rgbtype = "imagecollection"){

  # Get geometry of region of interest
  if(is.vector(region) & length(region) == 2){
    coords <- list(cbind(region[1],region[2])[1,])
    if(shape == "circle"){
      roi <- ee$Geometry$Point(coords[[1]])$buffer(buffer.width,max.error)
    }else{
      roi <- ee$Geometry$Point(coords[[1]])$buffer(buffer.width,max.error)$bounds()
    }
  }else if(is.matrix(region) | is.data.frame(region)){
    if(ncol(region) == 2 & nrow(region) >= 1){
      coords <- lapply(1:nrow(region),function(x)cbind(region[,1],region[,2])[x,])
      if(nrow(region) == 1){
        if(shape == "circle"){
          roi <- ee$Geometry$Point(coords[[1]])$buffer(buffer.width,max.error)
        }else{
          roi <- ee$Geometry$Point(coords[[1]])$buffer(buffer.width,max.error)$bounds()
        }
      }else if(nrow(region) > 1){
        roi <- ee$Geometry$MultiPoint(coords)$buffer(buffer.width,max.error)$bounds()
        if(buffer.width > 0){
         print("Buffer applied. If points represent exact region boundary, set buffer.width = 0") 
        }
      }
    }else{
      stop("region must be either a 2-element vector, 2-column matrix or dataframe, or an extent object")
    }
  }else if(class(region) == "Extent"){
    coords <- list(c(region[1],region[3]),c(region[2],region[4]))
    roi <- ee$Geometry$MultiPoint(coords)$bounds()
  }else{
    stop("region must be either a 2-element vector, 2-column matrix or dataframe, or an extent object")
  }
  
  # Get elevation raster from Earth Engine
  if(demtype == "imagecollection"){
    DEM_full <- ee$ImageCollection(dem)
    if(!is.null(demfunc)){
      DEM_full <- demfunc(DEM_full)
    }
  }else if(demtype == "image"){
    DEM_full <- ee$Image(dem)
  }
  DEM <- DEM_full$select(list(dem.band),list("elevation"))
  
  # Get cloud-free landsat raster from Earth Engine
  if(rgbtype == "imagecollection"){
    RGB_full <- ee$ImageCollection(rgb)
    if(!is.null(rgbfunc)){
      RGB_full <- rgbfunc(RGB_full)
    }
  }else if(rgbtype == "image"){
    RGB_full <- ee$Image(rgb)
  }
  RGB <- RGB_full$select(list(red.band,green.band,blue.band),list("red","green","blue"))
  
  # Reduce rasters to roi
  latlng <- ee$Image$pixelLonLat()$addBands(DEM)$addBands(RGB)
  latlng <- latlng$reduceRegion(reducer = ee$Reducer$toList(),
                                geometry = roi,
                                maxPixels = max.pixels,
                                scale=scale)
  
  # Convert to arrays
  lats <- np$array((ee$Array(latlng$get("latitude"))$getInfo()))
  lngs <- np$array((ee$Array(latlng$get("longitude"))$getInfo()))
  elevs <- np$array((ee$Array(latlng$get("elevation"))$getInfo()))
  
  red <- np$array((ee$Array(latlng$get("red"))$getInfo()))
  green <- np$array((ee$Array(latlng$get("green"))$getInfo()))
  blue <- np$array((ee$Array(latlng$get("blue"))$getInfo()))
  
  # Convert to elevation raster
  elevation <- data.frame(x = lngs, y = lats, elevation = elevs)
  rasterElev <- raster::rasterFromXYZ(elevation,crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  # Convert landsat to rasterBrick
  rasterRGB <- raster::brick(list(raster::rasterFromXYZ(data.frame(x = lngs, y = lats, red = red),crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"),
                                  raster::rasterFromXYZ(data.frame(x = lngs, y = lats, green = green),crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"),
                                  raster::rasterFromXYZ(data.frame(x = lngs, y = lats, blue = blue),crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")))
  
  # Return elevation and landsat rasters
  return(list(rasterElev = rasterElev, rasterRGB = rasterRGB))
}

