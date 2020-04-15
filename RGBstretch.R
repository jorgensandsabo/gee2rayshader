########## Stretch functions, stolen from RStoolbox


RGBstretch <- function (x, method = "lin", quantiles = c(0.02,0.98), band = NULL) {
  if(method == "lin"){
    if(length(quantiles) == 1) quantiles <- c(0,1) + c(quantiles, -quantiles)/100
    v <- quantile(x, quantiles, na.rm = TRUE)
    if(diff(v)==0) {
      ## sometimes lowr and upr quantile can be identical, which breaks the color calculation --> enforce a minimal distance by adding ~0
      v[2] <- v[2] + 1e-9
    }
    temp <-  (x - v[1])/(v[2] - v[1])
    temp[temp < 0] <- 0
    temp[temp > 1] <- 1
    return(temp)
  } 
  
  if(method == "hist"){
    ecdfun <- ecdf(x)
    return(ecdfun(x))
  } 
  
  if(method == "log"){
    x <- log(x + 1)
    x <- x - min(x,na.rm=TRUE)
    return(x / max(x,na.rm=TRUE))         
  }
  
  if(method == "sqrt"){
    x <- sqrt(x)
    x <- x - min(x,na.rm=TRUE)
    return(x /max(x,na.rm=TRUE))
  }
}
