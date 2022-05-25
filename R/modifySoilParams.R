#' Modifies soil properties estimated from SoilGrids
#'
#' @param soildata A data frame of soil physical parameters or a list of data frames
#' @param soildepth An estimate of soil depth (in mm) or a vector of soil depths if `soildata` is a list
#' @param surfacerock An estimate of surface rock content (in percent) or a vector of such values if `soildata` is a list
#'
#' @return A data frame of modified physical parameters or a list of modified data frames
#' @export
#'
#' @examples
modifySoilParams<-function(soildata, soildepth, surfacerock = NULL) {
  rock_from_surface<-function(surfacerock, depth) {
    coef = 1+3^(depth/1000)
    return(100*(1 - exp(-(max(10,surfacerock)/100)*coef)))
  }
  modifySoilOne <-function(soildf, soildepthone, surfacerockone) {
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
    if(!is.null(surfacerockone)) {
      for(l in 1:(nl-1)) {
        upper = 0
        if(l>1) upper = sum(soildf$widths[1:(l-1)])
        lower = sum(soildf$widths[1:l])
        middepth = 0.5*(upper+lower)
        soildf$rfc[l] = min(97.5, max(soildf$rfc[l], rock_from_surface(surfacerockone, middepth)))
      }
    }
    soildf$rfc[nl] = 97.5
    return(soildf)
  }
  if(inherits(soildata, "data.frame")) {
    if(length(soildepth)>1) stop("'soildepth' has to be of length one when 'soildata' is a data.frame")
    if(!is.null(surfacerock)) if(length(surfacerock)>1) stop("'surfacerock' has to be of length one when 'soildata' is a data.frame")
    return(modifySoilOne(soildata, soildepth, surfacerock))
  } else if(inherits(soildata, "list")) {
    if(length(soildepth)!=length(soildata)) stop("Vector 'soildepth' has to be of the same length as 'soildata'")
    if(!is.null(surfacerock)) if(length(surfacerock)!=length(soildata)) stop("Vector 'surfacerock' has to be of the same length as 'soildata'")
    for(i in 1:length(soildata)) {
      if(inherits(soildata[[i]], "data.frame")) {
        if(!is.null(surfacerock)) soildata[[i]] = modifySoilOne(soildata[[i]], soildepth[i], surfacerock[i])
        else soildata[[i]] = modifySoilOne(soildata[[i]], soildepth[i])
      } else {
        stop("Wrong class for soil data. Has to be a data.frame or a list of data.frame objects")
      }
    }
    return(soildata)
  } else {
    stop("Wrong class for soil data. Has to be a data.frame or a list of data.frame objects")
  }
}
