#' Estimate Rooting Depth Parameters
#'
#' Estimates rooting depth parameters for forest objects following Cabon et al. (2018).
#'
#' @param forest An object of class \code{\link{forest}}.
#' @param soil An object of class \code{\link{soil}} or a data frame with soil parameters.
#' @param PET_summer Average (climatic) potential evapotranspiration (mm) corresponding to summer months.
#' @param P_summer Average (climatic) precipitation (mm) corresponding to summer months.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param fillMissing A boolean flag to indicate that missing estimates should be filled with mean values.
#'
#' @details If \code{PET_summer} or \code{P_summer} are of length one, their elements are repeated for all elements in \code{forestlist}. Similarly, if \code{soillist} is an object of class \code{\link{soil}} or a data frame with soil parameters, the same soil parameters are repeated for all elements in \code{forestlist}.
#'
#' @return Function \code{estimateRootingDepth} returns object of class \code{\link{forest}} with modified values for \code{Z50} and \code{Z95}. Function \code{estimateRootingDepthList} returns a list of forest objects with modified root distribution parameters.
#'
#' @export
#'
#' @references Cabon, A., Martinez-Vilalta, J., de Aragon, J.M., Poyatos, R., De Caceres, M., 2018. Applying the eco-hydrological equilibrium hypothesis to model root distribution in water-limited forests. Ecohydrology 11, e2015. https://doi.org/10.1002/eco.2015
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @seealso \code{\link[medfate]{forest}}
#'
#' @examples
#'
#' @name estimateRootingDepth
estimateRootingDepth<-function(forest, soil, PET_summer, P_summer, SpParams,
                               fillMissing = FALSE) {
  Z50.LDR <- function(V,Z,Z95){
    a <- log(V/(1-V))/2.94
    Z50 <- (Z/Z95^a)^(1/(1-a))
    return(Z50)
  }
  if(inherits(soil, "soil")) {
    widths = soil$dVec
    rfc = soil$rfc
  } else if(inherits(soil, "data.frame")) {
    widths = soil$widths
    rfc = soil$rfc
  } else {
    stop("Wrong class for 'soil'")
  }

  rfc = sum(rfc[1:2]*widths[1:2])/sum(widths[1:2])
  lai = sum(plant_LAI(forest, SpParams), na.rm = TRUE) #plot's lai

  if(lai>0) {
    psiE = abs(plant_parameter(forest, SpParams, "Psi_Extract"))*1000 ## from MPa to Pa
    if(fillMissing) psiC[is.na(psiE)] = mean(psiE, na.rm=T)
    # Water potential corresponding to 50% PLC
    VCstem_c = plant_parameter(forest, SpParams, "VCstem_c")
    VCstem_d = plant_parameter(forest, SpParams, "VCstem_d")
    psi_critic = rep(NA, length(VCstem_c))
    for(i in 1:length(VCstem_c)) psi_critic[i] = hydraulics_xylemPsi(0.5, 1.0, VCstem_c[i], VCstem_d[i])
    psiC = pmax(1.5*psiE, abs(psi_critic)*1000) ## from MPa to Pa
    if(fillMissing) psiC[is.na(psiC)] = mean(psiC, na.rm=T)
    newDF = data.frame(psi_extr = psiE, psi_crit = psiC, rock = rfc, LAI = lai,
                       summerPET = PET_summer,
                       summerPrecipitation = P_summer)
    newDF$Zopt = pmin(4000, pmax(500, exp(predict(reg.Z.2, newdata = newDF)))) # Zopt should be limited to 300.1 in order to be consistent with the optimization procedure
    newDF$V = pmin(1, pmax(0.05,predict(reg.V.2, newdata = newDF)))
    newDF$Z50 = pmin(0.9*newDF$Zopt, pmax(100,Z50.LDR(newDF$V,300, newDF$Zopt)))
    if(nrow(forest$treeData)>0) {
      forest$treeData$Z50 =  newDF$Z50[1:nrow(forest$treeData)]
      forest$treeData$Z95 = newDF$Zopt[1:nrow(forest$treeData)]
      if(nrow(forest$shrubData)>0){
        forest$shrubData$Z50 = pmin(newDF$Z50[-(1:nrow(forest$treeData))],250)
        forest$shrubData$Z95 = pmin(newDF$Zopt[-(1:nrow(forest$treeData))],1500)
      }
    } else {
      if(nrow(forest$shrubData)>0){
        forest$shrubData$Z50 = pmin(newDF$Z50,250)
        forest$shrubData$Z95 = pmin(newDF$Zopt,1500)
      }
    }
  } else {
    warning("LAI = 0")
  }
  if(fillMissing) {
     forest$treeData$Z95[is.na(forest$treeData$Z95)] = mean(forest$treeData$Z95, na.rm=T)
     forest$treeData$Z50[is.na(forest$treeData$Z50)] = mean(forest$treeData$Z50, na.rm=T)
     forest$shrubData$Z95[is.na(forest$shrubData$Z95)] = mean(forest$shrubData$Z95, na.rm=T)
     forest$shrubData$Z50[is.na(forest$shrubData$Z50)] = mean(forest$shrubData$Z50, na.rm=T)
  }
  return(forest)
}
#' @rdname estimateRootingDepth
#' @param forestlist A list of objects of class \code{\link{forest}}.
#' @param soillist A list with objects of class \code{\link{soil}} or data frames with soil parameters.
#'
#' @export
estimateRootingDepthList<-function(forestlist, soillist, PET_summer, P_summer, SpParams,
                                   fillMissing = FALSE) {

  n = length(forestlist)
  if(length(PET_summer)==1) PET_summer = rep(PET_summer, n)
  if(length(P_summer)==1) P_summer = rep(P_summer, n)
  if(inherits(soillist, "soil") || inherits(soillist, "data.frame")) {
    l = vector("list", n)
    for(i in 1:n) l[[i]] = soillist
    soillist = l
  }
  for(i in 1:n) {
    forestlist[[i]] = estimateRootingDepth(forestlist[[i]], soillist[[i]], PET_summer[i], P_summer[i],
                                           SpParams, fillMissing)
  }
  return(forestlist)
}
