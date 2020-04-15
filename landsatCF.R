#### Function to generate cloud-reduced median landsat imagery, used in get_gee
landsatCF <- function(landsat_image_collection){
  LScf <- ee$Algorithms$Landsat$simpleComposite(
    collection = landsat_image_collection,
    percentile = 30,
    cloudScoreRange = 1);
  return(LScf)
}
