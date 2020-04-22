########## Function to prepare the rasters for rayshader:
########## Change lightness and stretch values of RGB raster
########## Reproject to a common projection
########## Convert to matrices

########## rasterElev should be a one-band RasterLayer of elevation values
########## rasterRGB should be a three-band RasterBrick or RasterStack of RGB values
########## labels should be a spatialPoints object or a 2-entry vector or a 2-column matrix or dataframe

### Add scaling of RGB rasters?

prep_rasters <- function(rasterElev, rasterRGB, labels = NULL,
                         projection = "albersequalarea", resolution = 30,
                         lightness = 1, stretch.method = "lin", stretch.quantiles = c(0.02,0.98)){

  # Change visuals of RGB raster (light + stretch)
  raster::values(rasterRGB) <- raster::values(rasterRGB)^(1/lightness)
  
  if(stretch.method %in% c("lin","hist","log","sqrt")){
    for(i in 1:3){
      raster::values(rasterRGB[[i]]) <- RGBstretch(raster::values(rasterRGB[[i]]), method = stretch.method, quantiles = stretch.quantiles)
    }
  }
  
  # Labels to SpatialPoints
  if(is.null(labels)){
    lab_m <- NULL
  }else if(class(labels) == "SpatialPoints"){
    lpts <- labels
  }else if(is.vector(labels) & length(labels) == 2){
    lpts <- sp::SpatialPoints(cbind(labels[1],labels[2]),proj4string=sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }else if(is.matrix(labels) | is.data.frame(labels)){
    lpts <- sp::SpatialPoints(labels,proj4string=sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  }else{
    stop("Labels must be a 2-entry vector or 2-column matrix or data frame or sp::spatialPoints object. Use NULL for no labels.")
  }

  # Choose projection
  if(!is.na(projection)){
    if(projection == "albersequalarea"){
      WSG84extent <- raster::extent(raster::projectRaster(rasterElev[[1]],crs="+init=epsg:4326"))
      avglat <- mean(c(WSG84extent[4],WSG84extent[3]))
      proj <- raster::crs(paste("+proj=aea +lat_1=",avglat[1]-1," +lat_2=",avglat[1]+1,sep=""))
    }else if(class(raster::crs(projection)) == "CRS"){
      print("reprojecting rasters to provided crs")
      proj <- raster::crs(projection)
    }else{
      stop("Projection must be NA to reproject rgb/labels to RGB layer, or a character string describing a projection and datum on the PROJ.4 format (see ?raster::crs), or the character string albersequalarea")
    }
  }

  # Reproject to a common projection
  if(is.na(projection)){
    print("No projection provided: reprojecting dem and labels the projection of RGB layer")
    rasterElev <- raster::projectRaster(from = rasterElev,to = rasterRGB)
    if(!(is.null(labels))){
      lpts_reproj <- sp::spTransform(lpts, proj)
      lpts <- data.frame(lpts_reproj,row.names = row.names(lpts))
    }
  }else{
    if(projection == "albersequalarea"){
      print(paste("reprojecting rasters to albers equal area, centered on mean latitude of rasterElev (",proj,")",sep=""))
    }else{
      print(paste("reprojecting to ",proj,sep=""))
    }
    rasterRGB <- raster::projectRaster(rasterRGB,crs = proj,res = resolution)
    rasterElev <- raster::projectRaster(from = rasterElev,to = rasterRGB)
    
    if(!(is.null(labels))){
      lpts_reproj <- sp::spTransform(lpts, proj)
      lpts_df <- data.frame(lpts_reproj,row.names = row.names(lpts))
    }
  }
  
  # Remove non-overlapping values between rasters
  rasterRGB_NA <- rasterRGB ; rasterRGB_NA[which(raster::values(!is.na(rasterRGB_NA)))] <- 1
  rasterElev_NA <- rasterElev ; rasterElev_NA[which(raster::values(!is.na(rasterElev_NA)))] <- 1
  rElev <- rasterElev * rasterRGB_NA[[1]] ; names(rElev) <- "elevation"
  rRGB <- rasterRGB * rasterElev_NA ; names(rRGB) <- c("red","green","blue")
  
  # Trim rasters to same extent
  rasterElev <- raster::trim(rElev)
  rasterRGB <- raster::trim(rRGB)
  
  # Convert to matrices
  elevation_m = rayshader::raster_to_matrix(rasterElev$elevation)
  
  RGB_m <- array(NA,dim=c(dim(rasterRGB)[1],dim(rasterRGB)[2],3))
  for(i in 1:3){
    RGB_m[,,i] <- matrix(raster::values(rasterRGB[[i]]),nrow=dim(rasterRGB[[i]])[1],ncol=dim(rasterRGB[[i]])[2],byrow=TRUE)
  }
  
  if(!is.null(labels)){
    lab_longs = sapply(1:nrow(lpts_df),function(i) dim(elevation_m)[1]*(lpts_df[i,1] - raster::extent(rasterElev)[1])/(raster::extent(rasterElev)[2] - raster::extent(rasterElev)[1]))
    lab_lats = sapply(1:nrow(lpts_df),function(i) dim(elevation_m)[2]*(lpts_df[i,2] - raster::extent(rasterElev)[3])/(raster::extent(rasterElev)[4] - raster::extent(rasterElev)[3]))
    lab_m <- data.frame(cbind(lab_longs,lab_lats), row.names = row.names(lpts_df))
  }
  
  # Return objects
  return(list(elevation_m = elevation_m,
              RGB_m = RGB_m,
              lab_m = lab_m))
}
