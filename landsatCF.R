#### Function to generate cloud-reduced median landsat imagery, used in get_gee
landsatCF <- function(landsat_image_collection,percentile = 30,cloudScoreRange = 1,
                      firstyear = NULL,lastyear = NULL,firstmonth = NULL,lastmonth = NULL){
  
  if(!is.null(firstyear) & !is.null(lastyear)){
    landsat_image_collection <- landsat_image_collection$filter(ee$Filter$calendarRange(firstyear,lastyear,'year'))
    if(!is.null(firstmonth) & !is.null(lastmonth)){
      landsat_image_collection <- landsat_image_collection$filter(ee$Filter$calendarRange(firstmonth,lastmonth,'month'))
    }
  }else if(!is.null(firstmonth) & !is.null(lastmonth)){
    landsat_image_collection <- landsat_image_collection$filter(ee$Filter$calendarRange(firstmonth,lastmonth,'month'))
  }
  
  LScf <- ee$Algorithms$Landsat$simpleComposite(
    collection = landsat_image_collection,
    percentile = percentile,
    cloudScoreRange = cloudScoreRange);
  return(LScf)
}