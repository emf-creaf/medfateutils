#' Modifies soil properties estimated from SoilGrids
#'
#' @param soildata A data frame of soil physical parameters or a list of data frames
#' @param soildepth An estimate of soil depth (in mm) or a vector of soil depths if `soildata` is a list
#'
#' @details
#' These functions are intended to correct the amount of water that can be extracted from a soil. Each of them is based on different assumptions:
#' \itemize{
#'   \item{Function \code{modifySoilDepth} trims the set of input layers to soil depth, while keeping a final rocky layer.}
#'   \item{Function \code{modifySoilRockContent} Modifies the rock content of soil layers from an estimate of surface rock percent, assuming that rock content increases with depth.}
#'   \item{Function \code{modifySoilRockContentFromSoilDepth} Modifies the rock content of soil layers from an estimate of soil depth, assuming that rock content increases with depth beyond soil depth.}
#' }
#'
#'
#' @return A data frame of modified physical parameters or a list of modified data frames
#' @name modifySoilParams
#' @export
#'
#' @examples
#'
#' \dontrun{
#' library(sf)
#' coords_sf <- st_sfc(st_point(c(-5.6333, 42.6667)), crs = 4326)
#' foo_1 <- soilgridsParams(coords_sf, widths = c(300, 700, 1000, 2000))
#' foo_1
#'
#' # trimming to 90 cm depth
#' foo_2 <- modifySoilDepth(foo_1, 900)
#' foo_2
#'
#' # 20 % rocks in the surface
#' foo_3 <- modifySoilRockContent(foo_1, 20)
#' foo_3
#'
#' # soil depth has been estimated as 50 cm
#' foo_4 <- modifySoilRockContentFromSoilDepth(foo_1, 500)
#' foo_4
#' }
modifySoilDepth<-function(soildata, soildepth) {
  modifySoilDepthOne <-function(soildf, soildepthone) {
    soildepthone = pmax(50,soildepthone)
    nl = nrow(soildf)
    oridepths = c(0, cumsum(soildf$widths[1:(nl-1)]))
    findepths = cumsum(soildf$widths[1:nl])
    orimaxdepth = sum(soildf$widths)
    w = which(findepths[-nl]>soildepthone)
    if(length(w)>0) {
      w1 = w[1]
      soildf$widths[w1] = soildepthone - oridepths[w1]
      if(soildf$widths[w1]==0) {
        soildf = soildf[c(1:(w1-1),nl),]
      } else {
        soildf = soildf[c(1:w1,nl),]
      }
      row.names(soildf) = NULL
      nl = nrow(soildf)
      soildf$widths[nl] = orimaxdepth - soildepthone
    }
    soildf$rfc[nl] = 97.5
    return(soildf)
  }
  if(inherits(soildata, "data.frame")) {
    if(length(soildepth)>1) stop("'soildepth' has to be of length one when 'soildata' is a data.frame")
    return(modifySoilDepthOne(soildata, soildepth))
  } else if(inherits(soildata, "list")) {
    if(length(soildepth)!=length(soildata)) stop("Vector 'soildepth' has to be of the same length as 'soildata'")
    for(i in 1:length(soildata)) {
      if(inherits(soildata[[i]], "data.frame")) {
        soildata[[i]] = modifySoilDepthOne(soildata[[i]], soildepth[i])
      } else {
        stop("Wrong class for soil data. Has to be a data.frame or a list of data.frame objects")
      }
    }
    return(soildata)
  } else {
    stop("Wrong class for soil data. Has to be a data.frame or a list of data.frame objects")
  }
}

#' @param surfacerock An estimate of surface rock content (in percent) or a vector of such values if `soildata` is a list
#'
#' @export
#' @rdname modifySoilParams
modifySoilRockContent<-function(soildata, surfacerock) {
  rock_from_surface<-function(surfacerock, depth) {
    coef = 1+3^(depth/1000)
    return(100*(1 - exp(-(max(10,surfacerock)/100)*coef)))
  }
  modifySoilRockContentOne <-function(soildf, surfacerockone) {
    nl = nrow(soildf)
    for(l in 1:nl) {
      upper = 0
      if(l>1) upper = sum(soildf$widths[1:(l-1)])
      lower = sum(soildf$widths[1:l])
      middepth = 0.5*(upper+lower)
      soildf$rfc[l] = min(97.5, max(soildf$rfc[l], rock_from_surface(surfacerockone, middepth)))
    }
    return(soildf)
  }
  if(inherits(soildata, "data.frame")) {
    if(!is.null(surfacerock)) if(length(surfacerock)>1) stop("'surfacerock' has to be of length one when 'soildata' is a data.frame")
    return(modifySoilRockContentOne(soildata, surfacerock))
  } else if(inherits(soildata, "list")) {
    if(!is.null(surfacerock)) if(length(surfacerock)!=length(soildata)) stop("Vector 'surfacerock' has to be of the same length as 'soildata'")
    for(i in 1:length(soildata)) {
      if(inherits(soildata[[i]], "data.frame")) {
        soildata[[i]] = modifySoilRockContentOne(soildata[[i]], surfacerock[i])
      } else {
        stop("Wrong class for soil data. Has to be a data.frame or a list of data.frame objects")
      }
    }
    return(soildata)
  } else {
    stop("Wrong class for soil data. Has to be a data.frame or a list of data.frame objects")
  }
}


#' @param soildepthrock An estimate of rock content (in percent) at soil depth, or a vector of such values if `soildata` is a list
#'
#' @export
#' @rdname modifySoilParams
modifySoilRockContentFromSoilDepth<-function(soildata, soildepth, soildepthrock = 70) {
  rock_from_soildepth<-function(soildepth, soildepthrock, depth) {
    coef = 1+3^((depth-soildepth)/1000)
    return(100*(1 - exp(-(max(10,soildepthrock)/100)*coef)))
  }
  modifySoilRockContentFromSoilDepthOne <-function(soildf, soildepthone, soildepthrockone = 70) {
    nl = nrow(soildf)
    for(l in 1:nl) {
      upper = 0
      if(l>1) upper = sum(soildf$widths[1:(l-1)])
      lower = sum(soildf$widths[1:l])
      middepth = 0.5*(upper+lower)
      prop = 1 - max(0, min(1, (soildepthone - upper)/(lower - upper)))
      rfc_est = min(97.5, max(soildf$rfc[l], rock_from_soildepth(soildepthone, soildepthrockone, middepth)))
      soildf$rfc[l] = prop*rfc_est + (1 - prop)*soildf$rfc[l]
    }
    return(soildf)
  }
  if(inherits(soildata, "data.frame")) {
    if(!is.null(soildepth)) if(length(soildepth)>1) stop("'soildepth' has to be of length one when 'soildata' is a data.frame")
    if(!is.null(soildepthrock)) if(length(soildepthrock)>1) stop("'soildepthrock' has to be of length one when 'soildata' is a data.frame")
    return(modifySoilRockContentFromSoilDepthOne(soildata, soildepth, soildepthrock))
  } else if(inherits(soildata, "list")) {
    if(!is.null(soildepth)) if(length(soildepth)!=length(soildata)) stop("Vector 'soildepth' has to be of the same length as 'soildata'")
    if(!is.null(soildepthrock)) if(length(soildepthrock)!=length(soildata)) stop("Vector 'soildepthrock' has to be of the same length as 'soildata'")
    for(i in 1:length(soildata)) {
      if(inherits(soildata[[i]], "data.frame")) {
        soildata[[i]] = modifySoilRockContentFromSoilDepthOne(soildata[[i]], soildepth[[i]], soildepthrock[i])
      } else {
        stop("Wrong class for soil data. Has to be a data.frame or a list of data.frame objects")
      }
    }
    return(soildata)
  } else {
    stop("Wrong class for soil data. Has to be a data.frame or a list of data.frame objects")
  }
}
