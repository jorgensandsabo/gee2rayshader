########## Function to prepare the rasters for rayshader:
########## Change lightness and stretch values of RGB raster
########## Reproject to a common projection
########## Convert to matrices

########## rasterElev should be a one-band RasterLayer of elevation values
########## rasterRGB should be a three-band RasterBrick or RasterStack of RGB values
########## labels should be a spatialPoints object or a ???. Labels included as rownames or ??? will be retained
prep_rasters <- function(rasterElev, rasterRGB, labels = NULL,
                         projection = "albersequalarea", resolution = 30.922080775909325,
                         lightness = 1, stretch.method = "lin", stretch.quantiles = c(0.02,0.98)){

  # Change visuals of RGB raster (light + stretch) ## ADD SCALING
  raster::values(rasterRGB) <- raster::values(rasterRGB)^(1/lightness)
  
  if(stretch.method %in% c("lin","hist","log","sqrt")){
    for(i in 1:3){
      raster::values(rasterRGB[[i]]) <- RGBstretch(raster::values(rasterRGB[[i]]), method = stretch.method, quantiles = stretch.quantiles)
    }
  }
  
  # Choose projection
  if(!is.na(projection)){
    if(projection == "albersequalarea"){
      print("reprojecting rasters to albers equal area, centered on mean latitude of rasterElev")
      WSG84extent <- raster::extent(raster::projectRaster(testALOS[[1]],crs="+init=epsg:4326"))
      avglat <- mean(c(WSG84extent[4],WSG84extent[3]))
      proj <- raster::crs(paste("+proj=aea +lat_1=",avglat[1]-1," +lat_2=",avglat[1]+1,sep=""))
    }else if(class(raster::crs(projection)) == "CRS"){
      print("reprojecting rasters to provided crs")
      proj <- raster::crs(projection)
    }else{
      stop("Projection must be NA to reproject rgb/labels to RGB layer, or a character string describing a projection and datum on the PROJ.4 format (see ?raster::crs), or the character string albersequalarea")
    }
  }else if(is.na(projection)){
    print("No projection provided: reprojecting dem and labels the projection of RGB layer")
  }
  
  # Labels
  if(is.null(labels)){
    lab_m <- NULL
  }else if(NULL){
    lpts <- labels
    lpts_df <- data.frame(lpts)
  }else if(is.vector(labels)){
    #lpts <- sp::SpatialPoints(cbind(lab_longs,lab_lats),proj4string=sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    lpts_df <- data.frame(lpts)
  }else if(is.matrix(labels)){
    lpts <- NA
    lpts_df <- data.frame(lpts)
  }else if(is.data.frame(labels)){
    lpts <- NA
    lpts_df <- data.frame(lpts)
  }else{
    stop("Labels must be ???. Use NULL for no labels.")
  }
  
  # Reproject to a common projection
  if(is.na(projection)){
    rasterElev <- raster::projectRaster(from = rasterElev,to = rasterRGB)
    
    if(!(is.null(labels))){
      lpts_reproj <- sp::spTransform(from = lpts, to = rasterRGB)
      lpts <- data.frame(lpts_reproj,row.names = lab_names)
    }
    
  }else{
    rasterRGB <- raster::projectRaster(rasterRGB,crs = proj,res = resolution)
    rasterElev <- raster::projectRaster(from = rasterElev,to = rasterRGB)
    
    if(!(is.null(labels))){
      lpts_reproj <- sp::spTransform(from = lpts,to = rasterRGB)
      lpts_df <- data.frame(lpts_reproj,row.names = lab_names)
    }
  }
  
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
