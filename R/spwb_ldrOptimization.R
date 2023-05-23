# Function to calculate Z50 knowing Z95 and the cumulated root proportion V at a given depth Z
.root_ldrZ50 <- function(V,Z,Z95){
  if(sum(V >= 0.9497887)>0) {stop("The function is not defined for V >= 0.9497887")}
  if(sum(Z == Z95)>0) {stop("The function is not defined for Z = Z95")}
  a <- log(V/(1-V))/2.94
  Z50 <- (Z/Z95^a)^(1/(1-a))
  return(Z50)
}

# Convenience function for finding the inverse solution of a given function
.inverse <- function (f, lower, upper) {
  function (y) uniroot((function (x) f(x) - y), lower = lower, upper = upper, extendInt = "yes")[1]
}


#' @rdname spwb_ldrOptimization
#'
#' @param x An object of class \code{\link{spwbInput}}.
#' @param meteo A data frame with daily meteorological data series (see \code{\link{spwb}}).
#' @param cohorts A character string with the names of cohorts to be explored. If \code{NULL} then all cohorts are explored.
#' @param RZmin The minimum value of RZ (the rooting depth) to be explored (in mm)
#' @param RZmax The maximum value of RZ (the rooting depth) to be explored (in mm)
#' @param V1min The minimum value of V1 (the root proportion in the first soil layer) to be explored
#' @param V1max The maximum value of V1 (the root proportion in the first soil layer) to be explored
#' @param resolution An integer defining the number of values to obtain by discretization of the root parameters RZ and V1. The number of parameter combinations and therefore the computation cost increases increase with the square of resolution
#' @param transformation Function to modify the size of Z intervals to be explored (by default, bins are equal).
#' @param heat_stop An integer defining the number of days during to discard from the calculation of the optimal root distribution. Usefull if the soil water content initialization is not certain
#' @param ... Additional parameters to function \code{\link{spwb}}.
#'
spwb_ldrExploration<-function(x, meteo, cohorts = NULL,
                              RZmin = 301, RZmax = 4000, V1min = 0.01, V1max = 0.94, resolution = 10,
                              heat_stop = 0, transformation = "identity", verbose = FALSE,
                              ...) {
  # define the days to keep in the analysis
  op_days <- (heat_stop+1):nrow(meteo)

  # Define the values of Z50 and Z90 to be explored
  trans <- function(x) do.call(transformation, list(x))
  inverse_trans <- .inverse(trans, lower = 0.01, upper = 100) # inverse of the function used for the transformation

  if(RZmax > x$soil$SoilDepth){
    if(verbose) cat("\n RZmax is larger than soil depth\n")
  }

  RZ_trans <- seq(trans(RZmin), trans(RZmax), length.out = resolution)
  RZ <- as.numeric(unlist(sapply(RZ_trans, FUN = inverse_trans)))

  # the case where RZ = Z1 will create problems when using the LDR model -> remove if it exists
  Z1 <- x$soil$dVec[1]
  if(sum(RZ == Z1) > 0){
    if(verbose) cat("\nThe function to derive the root proportion in each soil layer is not defined for RZ = Z1 (depth of the first soil layer)\n",
                    "This value is removed\n",
                    paste("Resolution is now\n", resolution-1))
    RZ <- RZ[-which(RZ == Z1)]
  }

  if(V1max >= 0.9497887){
    if(verbose) cat("\nThe function to derive the root proportion in each soil layer is only defined for V1 c ]0,0.949[\nV1max is set to 0.94\n")
    V1max <- 0.94
  }
  if(V1min <= 0){
    if(verbose) cat("\nThe function to derive the root proportion in each soil layer is only defined for V1 c ]0,0.949[\nV1min is set to 0.01\n")
    V1min <- 0.001
  }

  V1 <- seq(V1min,V1max,length.out = length(RZ)) # the proportion of root in the first soil layer
  # Create a matrix with V1 as rows and RZ as column, filled with logical values to indicate the parameter combinations to explore
  mExplore <- matrix(T, nrow = length(V1), ncol = length(RZ), dimnames = list(V1 = V1, RZ = RZ))
  # mExplore[lower.tri(mExplore, diag = T)] <- F

  # Calculate Z50
  Z50 <- .root_ldrZ50(V = array(V1,dim = dim(mExplore)), Z = array(Z1, dim = dim(mExplore)), Z95 = t(array(RZ, dim = dim(mExplore))))
  dimnames(Z50) <- dimnames(mExplore)
  # Prepare array for V
  V <- array(dim = c(length(x$soil$dVec),length(V1), length(RZ)),
             dimnames = list(layer = 1:length(x$soil$dVec), V1 = V1, RZ = RZ))

  # Sum LAI of all species
  x$above$LAI_live <- sum(x$above$LAI_live)
  x$above$LAI_expanded <- sum(x$above$LAI_expanded)
  x$above$LAI_dead <- sum(x$above$LAI_dead)

  # Data outputs
  if(is.null(cohorts)) cohorts = row.names(x$cohorts)
  An<-E <- PsiMin <- array(dim = c(length(cohorts), length(V1), length(RZ)), dimnames = list(cohort = cohorts, V1 = V1, RZ = RZ))

  # Start loop
  cc <- which(mExplore == T, arr.ind = T)

  # Reset input
  resetInputs(x)

  for(ci in 1:length(cohorts)){
    coh = cohorts[ci]
    sp = which(row.names(x$cohorts)==coh)

    cat(paste("Exploring root distribution of cohort", coh,"(", x$cohorts$Name[sp],"):\n"))

    x_1sp <- x
    x_1sp$cohorts <- x$cohorts[sp,,drop = FALSE]
    x_1sp$above <- x$above[sp,,drop = FALSE]
    x_1sp$below <- x$below
    x_1sp$belowLayers$V <- x$belowLayers$V[sp,,drop = FALSE]
    x_1sp$paramsInterception <- x$paramsInterception[sp,,drop = FALSE]
    x_1sp$paramsTransp <- x$paramsTransp[sp,,drop = FALSE]
    x_1sp$Transpiration <- x$Transpiration[sp,drop = FALSE]
    x_1sp$Photosynthesis <- x$Photosynthesis[sp,drop = FALSE]
    if(x_1sp$control$transpirationMode=="Granier") {
      x_1sp$PLC <- x$PLC[sp,drop = FALSE]
    } else {
      x_1sp$belowLayers$VGrhizo_kmax <- x$belowLayers$V[sp,,drop = FALSE]
      x_1sp$belowLayers$VCroot_kmax <- x$belowLayers$V[sp,,drop = FALSE]
      x_1sp$paramsAnatomy <- x$paramsAnatomy[sp,,drop = FALSE]
      x_1sp$paramsWaterStorage <- x$paramsWaterStorage[sp,,drop = FALSE]
      x_1sp$StemPLC <- x$StemPLC[sp,drop = FALSE]
      x_1sp$Einst <- x$Einst[sp,drop = FALSE]
      x_1sp$RhizoPsi <- x$RhizoPsi[sp,,drop = FALSE]
      x_1sp$RootCrownPsi <- x$RootCrownPsi[sp,drop = FALSE]
      x_1sp$StemSympPsi <- x$StemSympPsi[sp,drop = FALSE]
      x_1sp$StemPsi1 <- x$StemPsi1[sp,drop = FALSE]
      x_1sp$StemPsi2 <- x$StemPsi2[sp,drop = FALSE]
      x_1sp$LeafSympPsi <- x$LeafSympPsi[sp,drop = FALSE]
      x_1sp$LeafPsi <- x$LeafPsi[sp,drop = FALSE]
    }
    x_1sp$control$verbose <- F

    pb <- txtProgressBar(max = nrow(cc), style = 3)
    for(row in 1:nrow(cc)){
      i <- cc[row,1]
      j <- cc[row,2]

      # Update the depth of the different soil layer to match RZ
      s. <- x$soil
      s.$SoilDepth <- RZ[j]
      dCum <- cumsum(s.$dVec)
      layersWithinRZ <- dCum < RZ[j]
      layersWithinRZ <- c(T,layersWithinRZ[-length(layersWithinRZ)])
      s.$dVec <- s.$dVec[layersWithinRZ] # remove the layers not included
      nl <- length(s.$dVec) #new number of layers
      s.$dVec[nl] <- s.$dVec[nl]-dCum[nl]+RZ[j] # adjust the width of the last layer
      # s.$Water_FC[nl] = soil$Water_FC[nl]*(s.$dVec[nl]/soil$dVec[nl]) #Adjust volume of the last layer
      # Adjust the other soil parameters to the new number of layers
      s.[["sand"]] <- s.[["sand"]][1:nl]
      s.[["clay"]] <- s.[["clay"]][1:nl]
      s.[["om"]] <- s.[["om"]][1:nl]
      s.[["rfc"]] <- s.[["rfc"]][1:nl]
      s.[["macro"]] <- s.[["macro"]][1:nl]
      s.[["W"]] <- s.[["W"]][1:nl]
      s.[["Temp"]] <- s.[["Temp"]][1:nl]
      s.[["VG_alpha"]] <- s.[["VG_alpha"]][1:nl]
      s.[["VG_theta_res"]] <- s.[["VG_theta_res"]][1:nl]
      s.[["VG_theta_sat"]] <- s.[["VG_theta_sat"]][1:nl]
      s.[["Ksat"]] <- s.[["Ksat"]][1:nl]

      V[,i,j] <- 0
      x_1sp$belowLayers$V = x$belowLayers$V[sp,1:nl,drop = FALSE]
      x_1sp$belowLayers$V[1,] <- root_ldrDistribution(Z50 = Z50[i,j], Z95 = RZ[j], d=s.$dVec)
      V[1:length(x_1sp$belowLayers$V),i,j] <- x_1sp$belowLayers$V

      x_1sp[["soil"]] <- s.
      s_res <- spwb(x = x_1sp, meteo = meteo, ...)

      # Outputs
      years <- substr(as.Date(rownames(meteo)), start = 1, stop = 4)
      ma <- function(x,n=10){
        f = filter(x,rep(1/n,n), method = "convolution", sides = 2)
        f = f[!is.na(f)]
        # print(sum(is.na(f)))
        f
      }
      if(x_1sp$control$transpirationMode=="Granier") {
        psi <- s_res$Plants$PlantPsi[op_days]
      } else {
        psi <- s_res$Plants$StemPsi[op_days]
      }
      PsiMin[ci,i,j] <- mean(aggregate(psi,
                                       by = list(years[op_days]),
                                       FUN = function(x) {
                                         m <- ma(x)
                                         if(sum(!is.na(m)>0)) return(min(m, na.rm=TRUE))
                                         return(NA)
                                       })$x)
      # if(verbose) print(s_res$spwbInput)
      E[ci,i,j] <- mean(s_res$Plants$Transpiration[op_days], na.rm=TRUE)
      if(x_1sp$control$transpirationMode=="Granier") {
        An[ci,i,j] <- mean(s_res$Plants$GrossPhotosynthesis[op_days], na.rm=TRUE)
      } else {
        An[ci,i,j] <- mean(s_res$Plants$NetPhotosynthesis[op_days], na.rm=TRUE)
      }
      setTxtProgressBar(pb, row)
    }
    cat("\n")
  }
  res <-list(cohorts = cohorts, RZ = RZ, V1 = V1, Z50 = Z50, E = E, An = An, PsiMin = PsiMin)
  class(res)<-list("spwb_ldrExploration","list")
  return(res)
}

#' Optimization of root distribution
#'
#' Functions \code{spwb_ldrExploration} and \code{spwb_ldrOptimization} are used to
#' find optimum the species root distribution within \code{spwb}, given the arguments
#' \code{x}, \code{meteo} and \code{psi_crit}.
#'
#' @param y The result of calling \code{spwb_ldrExploration}.
#' @param psi_crit A numerical vector of length iqual to the number of species in the plot containing the species values of water potential inducing hydraulic failure (in MPa). Use \code{NA} values to skip optimization for particular plant cohorts.
#' @param opt_mode Optimization mode:
#'     \itemize{
#'       \item{\code{opt_mode = 1} maximizes transpiration along the line of stress equal to \code{psi_crit} (Cabon et al. 2018). The optimization is based on the eco-hydrological equilibrium hypothesis (Eagleson, 1982), which is formulated here as the root distribution for which plant transpiration is maximized while the plant water potential is close to the species-defined critical value \code{psi_crit} (Cabon et al.,2018).}
#'       \item{\code{opt_mode = 2} maximizes transpiration among combinations with stress according to \code{psi_crit}).}
#'       \item{\code{opt_mode = 3} maximizes photosynthesis among combinations with stress according to \code{psi_crit}).}
#'       \item{\code{opt_mode = 4} maximizes transpiration, subject to root construction constrains, among combinations with stress according to \code{psi_crit}).}
#'       \item{\code{opt_mode = 5} maximizes photosynthesis, subject to root construction constrains, among combinations with stress according to \code{psi_crit}).}
#'     }
#' @param verbose A logical value. Print the internal messages of the function?
#'
#' @details
#' For each combination of the parameters RZ and V1 the function \code{spwb_ldrExploration} runs \code{spwb},
#' setting the total soil depth equal to RZ. The root proportion in each soil layer is derived from V1,
#' the depth of the first soil layer and RZ using the LDR root distribution model (Schenk and Jackson, 2002)
#' and assuming that the depth containing 95 percent of the roots is equal to RZ.
#' Function \code{spwb_ldrOptimization} takes the result of the exploration and tries
#' to find optimum root distribution parameters. \code{psi_crit}, the species specific
#' water potential inducing hydraulic failure, can be approached by the water potential
#' inducing 50 percent of loss of conductance for the and gymnosperms and 88 percent for
#' the angiosperms (Urli et al., 2013, Brodribb et al., 2010). Details of the hypothesis
#' and limitations of the optimization method are given in Cabon et al. (2019).
#'
#' @return
#' Function \code{spwb_ldrExploration} returns a list containing a list containing
#' the explored RZ and V1 combinations as well as arrays with the values of average daily plant transpiration,
#' average daily net photosynthesis and the minimum plant water potential for each cohort and parameter combination.
#'
#' Function \code{spwb_ldrOptimization}  returns a data frame with containing
#' the species index used in medfate, \code{psi_crit} and the optimized values of V1
#' and the LDR parameters Z50 and Z95 (see \code{\link{root_ldrDistribution}})
#' and as many rows as the number of species.
#'
#' @references
#' Brodribb, T.J., Bowman, D.J.M.S., Nichols, S., Delzon, S., Burlett, R., 2010. Xylem function and growth rate interact to determine recovery rates after exposure to extreme water deficit. New Phytol. 188, 533–542. doi:10.1111/j.1469-8137.2010.03393.x
#'
#' Cabon, A., \enc{Martínez-Vilalta}{Martinez-Vilalta}, J., Poyatos, R., \enc{Martínez de Aragón}{Martinez de Aragon}, J., De \enc{Cáceres}{Caceres}, M. (2018) Applying the eco-hydrological equilibrium hypothesis to estimate root ditribution in water-limited forests. Ecohydrology 11: e2015.
#'
#' Druel, A., Martins, N., Cochard, H., De Caceres, M., Delzon, S., Mencuccini, M., Torres-Ruiz, J., and Ruffault, J.: European forest vulnerability to hydraulic failure: an ecohydrological approach, EGU General Assembly 2023, Vienna, Austria, 24–28 Apr 2023, EGU23-17068, https://doi.org/10.5194/egusphere-egu23-17068, 2023.
#'
#' Eagleson, P.S., 1982. Ecological optimality in water-limited natural soil-vegetation systems: 1. Theory and hypothesis. Water Resour. Res. 18, 325–340. doi:10.1029/WR018i002p00325
#'
#' Schenk, H.J., Jackson, R.B., 2002. The Global Biogeography of Roots. Ecol. Monogr. 72, 311. doi:10.2307/3100092
#'
#' Urli, M., Porte, A.J., Cochard, H., Guengant, Y., Burlett, R., Delzon, S., 2013. Xylem embolism threshold for catastrophic hydraulic failure in angiosperm trees. Tree Physiol. 33, 672–683. doi:10.1093/treephys/tpt030
#'
#' @author
#' Antoine Cabon, WSL
#'
#' \enc{Arsène}{Arsene} Druel, URFM-INRAE
#'
#' Nicolas Martin-StPaul, URFM-INRAE
#'
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#'
#' @seealso \code{\link{spwb}}, \code{\link{soil}}, \code{\link{root_ldrDistribution}}
#'
#' @examples
#' \donttest{
#' #Load example daily meteorological data
#' data(examplemeteo)
#'
#' #Load example plot plant data
#' data(exampleforestMED)
#'
#' #Default species parameterization
#' data(SpParamsMED)
#'
#' #Initialize soil with default soil params
#' examplesoil <- soil(defaultSoilParams(2))
#'
#' #Initialize control parameters
#' control <- defaultControl("Granier")
#'
#' #Initialize input
#' x <- forest2spwbInput(exampleforestMED,examplesoil, SpParamsMED, control)
#'
#' #Run exploration (weather subset for faster computation)
#' y <- spwb_ldrExploration(x = x, meteo = examplemeteo[1:50,],
#'                         elevation = 100, latitude = 41.82592)
#'
#' #Optimization under different modes
#' spwb_ldrOptimization(y = y, psi_crit = c(-2,-3,-4), opt_mode = 1)
#' }
#'
#' @name spwb_ldrOptimization
spwb_ldrOptimization<-function(y, psi_crit, opt_mode = 1) {


  E = y$E
  An = y$An
  PsiMin = y$PsiMin
  V1 = y$V1
  RZ = y$RZ
  Z50 = y$Z50
  cohorts = y$cohorts

  if(length(psi_crit)!= length(cohorts)) stop("The length of 'psi_crit' must be equal to the number of cohorts in 'y'.")

  optim <- data.frame(psi_crit = psi_crit, Z50 = NA, Z95 = NA, V1 = NA)
  row.names(optim) = cohorts
  for (i in 1:length(cohorts)){
    psimin <- PsiMin[i,,]
    e <- E[i,,]
    an <- An[i,,]
    if(opt_mode==1) {
      # emax <- max(e)
      # e[e >= emax-0.05*emax] <- emax - 0.05*emax
      # cost <- (matrix(z, ncol = 1)%*%(1-v) + matrix(300, ncol = 1, nrow = length(z))%*%v)^(3/2)
      supinf <- matrix(0, ncol = ncol(psimin), nrow = nrow(psimin))
      supinf[psimin >= psi_crit[i]] <- 1
      subselb <- rbind(supinf[-nrow(supinf),]-supinf[-1,], rep(0, ncol(supinf)))
      subselt <- rbind(rep(0, ncol(supinf)), supinf[-1,]-supinf[-nrow(supinf),])
      subsell <- cbind(rep(0, nrow(supinf)), supinf[,-1]-supinf[,-ncol(supinf)])
      subselr <- cbind(supinf[,-ncol(supinf)]-supinf[,-1], rep(0, nrow(supinf)))
      sel <- matrix(F, ncol = ncol(psimin), nrow = nrow(psimin))
      sel[subselb == 1 | subselt == 1 | subsell == 1 | subselr == 1] <- T
      if(length(e[sel])==0) {
        warning(paste("Psi value", psi_crit[i],"for cohort ",row.names(cohorts)[i],"not reached for any combination."))
        optim[i,] <- NA
      } else {
        point <- which(sel & e == max(e[sel]), arr.ind = T)
        optim$Z50[i] <- Z50[point[1], point[2]]
        optim$V1[i] <- V1[point[1]]
        optim$Z95[i] <- RZ[point[2]]
      }
    }
    else if(opt_mode==2) {
      selPsi = (psimin > psi_crit[i]) # Select combinations with less stress than psi_crit
      if(sum(selPsi)==0) selPsi = (psimin == max(psimin)) # If none, select combination of minimum stress
      maxE = max(e[selPsi]) # Find maximum transpiration (among combinations selected)
      sel2 = selPsi & (e == maxE) # Select combination with maximum transpiration
      point = which(sel2, arr.ind = TRUE)
      optim$Z50[i] <- Z50[point[1], point[2]]
      optim$V1[i] <- V1[point[1]]
      optim$Z95[i] <- RZ[point[2]]
    }
    else if(opt_mode==3) {
      selPsi = (psimin > psi_crit[i]) # Select combinations with less stress than psi_crit
      if(sum(selPsi)==0) selPsi = (psimin == max(psimin)) # If none, select combination of minimum stress
      maxAn = max(an[selPsi]) # Find maximum transpiration (among combinations selected)
      sel2 = selPsi & (an == maxAn) # Select combinations with maximum photosynthesis
      point = which(sel2, arr.ind = TRUE)
      optim$Z50[i] <- Z50[point[1], point[2]]
      optim$V1[i] <- V1[point[1]]
      optim$Z95[i] <- RZ[point[2]]
    }
    else if(opt_mode==4) {
      selPsi = (psimin > psi_crit[i]) # Select combinations with less stress than psi_crit
      if(sum(selPsi)==0) selPsi = (psimin == max(psimin)) # If none, select combination of minimum stress
      maxE = max(e[selPsi]) # Find maximum transpiration (among combinations selected)
      sel2 = selPsi & (e >= maxE*0.95) # Select combinations with > 95% of maximum transpiration
      points = as.data.frame(which(sel2, arr.ind = TRUE))
      minZ = min(points$RZ, na.rm=T) # Minimum rooting depth
      maxV1 = max(points$V1[points$RZ==minZ], na.rm=T) # Maximum V1
      point = c(maxV1, minZ)
      optim$Z50[i] <- Z50[point[1], point[2]]
      optim$V1[i] <- V1[point[1]]
      optim$Z95[i] <- RZ[point[2]]
    }
    else if(opt_mode==5) {
      selPsi = (psimin > psi_crit[i]) # Select combinations with less stress than psi_crit
      if(sum(selPsi)==0) selPsi = (psimin == max(psimin)) # If none, select combination of minimum stress
      maxAn = max(an[selPsi]) # Find maximum transpiration (among combinations selected)
      sel2 = selPsi & (an >= maxAn*0.95) # Select combinations with > 95% of maximum photosynthesis
      points = as.data.frame(which(sel2, arr.ind = TRUE))
      minZ = min(points$RZ, na.rm=T) # Minimum rooting depth
      maxV1 = max(points$V1[points$RZ==minZ], na.rm=T) # Maximum V1
      point = c(maxV1, minZ)
      optim$Z50[i] <- Z50[point[1], point[2]]
      optim$V1[i] <- V1[point[1]]
      optim$Z95[i] <- RZ[point[2]]
    }
  }
  return(optim)
}

# Function for plotting the outputs of spwb_ldrOptimization
# works with the libraries ggplot2, reshape and viridis
# x is the output of the function spwb_ldrOptimization with explore_out = T
# .plot.ldrOptimization <- function(x, SP = 1, raster_var = "E", contour_var = "E", special_breaks_var = "Psi",
#                              legend_pos = c(1,1), xaxis_pos = "bottom", yaxis_pos = "left", special_breaks = 0, axis_trans = "identity"){
#
#   Psi.xyz <- melt(x$explore_out$PsiMin[SP,,])
#   E.xyz <- melt(x$explore_out$E[SP,,]*365)
#   xy <- Psi.xyz[,c("V1", "RZ")]
#
#   # Raster layer
#   if(raster_var == "Psi"){
#     leg_title <- expression(paste(Psi[min],"(MPa)"))
#     data_raster <- Psi.xyz
#   }
#   if(raster_var == "E"){
#     leg_title <- expression(paste("E (mm ", yr^{-1}, ")"))
#     data_raster <- E.xyz
#   }
#
#   # Contour layer
#   if(contour_var == "Psi"){
#     data_contour <- Psi.xyz
#     bw1 <- 1
#     bw2 <- 0.2
#   }
#   if(contour_var == "E"){
#     data_contour <- E.xyz
#     bw1 <- 50
#     bw2 <- 10
#   }
#
#   # Add special break
#   if(special_breaks_var == "Psi"){
#     data_special_breaks <- Psi.xyz
#   }
#   if(special_breaks_var == "E"){
#     data_special_breaks <- E.xyz
#   }
#
#   # Optimized parameters
#   x$optim$RZ <- x$optim$Z95
#
#   # Plot
#   p <- ggplot(xy, aes(x = RZ, y = V1))+
#     geom_raster(data = data_raster, aes(fill = value))+
#     geom_contour(data = data_contour, aes(z = value), colour = "white", binwidth = bw1, size = 1)+
#     geom_contour(data = data_contour, aes(z = value), colour = "white", binwidth = bw2, size = 0.5)+
#     geom_contour(data = data_special_breaks, aes(z = value), colour = "red", breaks = special_breaks, size = 1)+
#     geom_point(data = x$optim[SP,], aes(x = RZ, y = V1), color = "black", fill = "red", shape = 21, size = 4, inherit.aes = F)+
#     scale_fill_viridis(name = leg_title)+
#     coord_cartesian(expand = F)+
#     ylab(expression(paste(V[1])))+
#     xlab(expression(paste(RZ, "(mm)")))+
#     theme_bw()+
#     theme(legend.position = legend_pos, legend.justification = legend_pos, legend.background = element_rect(fill = rgb(1,1,1,0.7)))+
#     scale_x_continuous(position = xaxis_pos, trans = axis_trans)+
#     scale_y_continuous(position = yaxis_pos, trans = "identity")
#
#   return(p)
# }

# make nls model
.tryNLSmodel <- function(ResAnalysis) {
  if ( any(ResAnalysis[-1,2]>= 40 & ResAnalysis[-1,2] <= 60) ) {
    Pval = ResAnalysis[-1,1][which(ResAnalysis[-1,2]>= 40 & ResAnalysis[-1,2] <= 60)[1]]
  } else if ( any(ResAnalysis[-1,2]>= 15 & ResAnalysis[-1,2] < 40) && any(ResAnalysis[-1,2]> 60 & ResAnalysis[-1,2] <= 85) ) {
    Pval = mean(max(ResAnalysis[-1,1][which(ResAnalysis[-1,2]>= 15 & ResAnalysis[-1,2] < 40)]),min(ResAnalysis[-1,1][which(ResAnalysis[-1,2]> 60 & ResAnalysis[-1,2] <= 85)]))
  } else {
    Pval = c(80,160,320,640,960,1280,40,20)
  }

  modelOK = FALSE
  for ( iPval in Pval ) {
    if ( !modelOK ) {
      # if (iPval <50) {
      #   slopeVal = -5
      if (iPval <100) {
        slopeVal = -2
      } else if (iPval <200) {
        slopeVal = -1
      } else  {
        slopeVal = -0.5
      }
      model  =  try(nls(PLC90 ~ 100/(1+exp(slope/25*(P-SEW))), data=ResAnalysis, start=c(slope=slopeVal,P=iPval)), silent = TRUE )
      # model  =  try(nls(QPLCl9 ~ ((100-min(ResAnalysis[,2]))/(1+exp(slope/25*(P-SEW)))+min(ResAnalysis[,2])), data=ResAnalysis, start=c(slope=-2,P=80)), silent = TRUE )
      if ( class(model) != "try-error" ) modelOK = TRUE
    }
  }

  return(model)
}

.tryNLSmodel2 <- function(ResAnalysis) {
  Pval = NULL
  if ( any(ResAnalysis[-1,2]>= 40 & ResAnalysis[-1,2] <= 60) ) {
    Pval = ResAnalysis[-1,1][which(ResAnalysis[-1,2]>= 40 & ResAnalysis[-1,2] <= 60)[1]]
  } else if ( any(ResAnalysis[-1,2]>= 15 & ResAnalysis[-1,2] < 40) && any(ResAnalysis[-1,2]> 60 & ResAnalysis[-1,2] <= 85) ) {
    Pval = mean(max(ResAnalysis[-1,1][which(ResAnalysis[-1,2]>= 15 & ResAnalysis[-1,2] < 40)]),min(ResAnalysis[-1,1][which(ResAnalysis[-1,2]> 60 & ResAnalysis[-1,2] <= 85)]))
  }
  Pval = c(Pval, 80, -30, 160, 320, 640, 960, 1280, 40, 20)

  modelOK = FALSE
  for ( iPval in Pval ) {
    if ( !modelOK ) {
      if (iPval <0) {
        slopeVal = -0.75
        sigmoVal = -2
      } else if (iPval <100) {
        slopeVal = -0.6
        sigmoVal = 0.8
      } else if (iPval <200) {
        slopeVal = -0.4
        sigmoVal = 0.8
      } else  {
        slopeVal = -0.4
        sigmoVal = 1
      }
      model  =  try(nls(PLC90 ~ 100/(sigmo+exp(slope/25*(P-SEW))), data=ResAnalysis, start=c(slope=slopeVal,P=iPval,sigmo=sigmoVal)), silent = TRUE )
      if ( class(model) != "try-error" ) modelOK = TRUE
    }
  }

  return(model)
}

.tryNLSmodel3 <- function(ResAnalysis) {

  model = try(nls(PLC90 ~ slope/(SEW+P), data=ResAnalysis, start=c(slope = 50, P=0.5)), silent = TRUE )

  # if ( class(model) != "try-error" && nrow(ResAnalysis)<=3 && summary(model)$sigma>0.5 ) model = try(nls(QPLCl9 ~ slope/(SEW+P), data=ResAnalysis[1,], start=c(slope = 50, P=0.5)), silent = TRUE )
  # if ( class(model) != "try-error" && summary(model)$sigma>50 ) model = try(nls(QPLCl9 ~ slope/(SEW+P), data=ResAnalysis[1,], start=c(slope = 50, P=0.5)), silent = TRUE )

  return(model)
}

.tryNLSmodel4 <- function(ResAnalysis) {
  model4 = try(nls(PLC90 ~ slope/(SEW+P) + offS, data=ResAnalysis, start=c(slope = 50, P=0.5, offS = 0)), silent = TRUE ) # add goffS
  return(model4)
}

.RUfromModels <- function(ResAnalysis, PLC90_target, doModels = c(TRUE, TRUE, TRUE, TRUE), bavard = FALSE){
  if (!any(doModels)) stop("You have to chose at least one model !")

  if ( doModels[1] ) model1 = .tryNLSmodel(ResAnalysis)
  if ( doModels[2] ) model2 = .tryNLSmodel2(ResAnalysis)
  if ( doModels[3] ) model3 = .tryNLSmodel3(ResAnalysis)
  if ( doModels[4] ) model4 = .tryNLSmodel4(ResAnalysis)


  sigmaRes = rep(Inf,4)
  if ( doModels[1] && class(model1) != "try-error" ) sigmaRes[1] = summary(model1)$sigma
  if ( doModels[2] && class(model2) != "try-error" ) sigmaRes[2] = summary(model2)$sigma
  if ( doModels[3] && class(model3) != "try-error" ) sigmaRes[3] = summary(model3)$sigma
  if ( doModels[4] && class(model4) != "try-error" ) sigmaRes[4] = summary(model4)$sigma
  if ( nrow(ResAnalysis)<=3 && min(sigmaRes)%in%sigmaRes[3:4] && all(sigmaRes[3:4] > 0.5) ) sigmaRes = sigmaRes*Inf # Remove choice on the sigmoide at simu 2 if there is an another choice with 1/x.... but to be confirmed
  minSigmaModel = 0
  if ( any(sigmaRes!=Inf) ) minSigmaModel = which(sigmaRes==min(sigmaRes))

  RU_cible = NULL
  if ( 2 %in% minSigmaModel ) {
    if ( bavard ) cli::cli_li("The model sigmoid NEW is selected for SEW target. ")
    model_Pval = summary(model2)$parameters['P',1]
    model_Sval = summary(model2)$parameters['slope',1]
    model_Sigval = summary(model2)$parameters['sigmo',1]
    RU_cible = model_Pval - log(100/PLC90_target - model_Sigval) * 25/model_Sval
  } else if ( 1 %in% minSigmaModel ) {
    if ( bavard ) cli::cli_li("The model sigmoid original is selected for SEW target. ")
    model_Pval = summary(model1)$parameters['P',1]
    model_Sval = summary(model1)$parameters['slope',1]
    RU_cible = model_Pval - log(100/PLC90_target - 1) * 25/model_Sval
  } else if ( 3 %in% minSigmaModel ) {
    if ( bavard ) cli::cli_li("The model 1/x is selected for SEW target. ")
    model_Pval = summary(model3)$parameters['P',1]
    model_Sval = summary(model3)$parameters['slope',1]
    RU_cible = model_Sval / PLC90_target - model_Pval
  } else if ( 4 %in% minSigmaModel ) {
    if ( bavard ) cli::cli_li("The model 1/x+offs is selected for SEW target. ")
    model_Pval = summary(model4)$parameters['P',1]
    model_Sval = summary(model4)$parameters['slope',1]
    model_Oval = summary(model4)$parameters['offS',1]
    RU_cible = model_Sval / (PLC90_target-model_Oval) - model_Pval
  }
  return(RU_cible)
}

#' @rdname spwb_ldrOptimization
#' @param PLC90_target Leaf PLC target (quantile 90)
#' @param PLC90_tol Limit of the PLC target tolerance, only in some conditions
#' @param max_simu Maximum of simulation authorized before to stop
#' @param model_varLim Limit of the soil extractable water (SEW) variation of the model accepted
#' @param max_rocks Maximum content in coarse fragments
#'
spwb_rockOptimization<-function(x, meteo,
                                PLC90_target = 12, PLC90_tol = 0.5,
                                max_simu = 7, model_varLim = 10,
                                max_rocks = 99, verbose = FALSE, lastSimu = FALSE, ...){

  x$control$verbose = FALSE
  x$control$cavitationRefill = "rate"
  soil <- x$soil
  nlayers <- length(soil$dVec)
  LAI_max_coh <- x$above$LAI_live
  LAI_max <- sum(LAI_max_coh, na.rm = TRUE)

  # Select non variable parameters
  coarseFragOri <- sum(soil$rfc*soil$dVec)/sum(soil$dVec)
  listCoarseFragOri <- soil$rfc
  orderCF <- order(listCoarseFragOri)
  listDepth <- soil$dVec

  RU_vg_ori <- sum(medfate::soil_waterExtractable(soil, model = x$control$soilFunctions))

  soil_max <- soil
  soil_max$rfc <- rep(0, nlayers)
  soil_min <- soil
  soil_min$rfc <- rep(max_rocks, nlayers)
  RU_vg_max <- sum(medfate::soil_waterExtractable(soil_max, model = x$control$soilFunctions))
  RU_vg_min <- sum(medfate::soil_waterExtractable(soil_min, model = x$control$soilFunctions))

  f_ru_diff <- function(factor, ru_target) {
    soil_tmp <- soil
    soil_tmp$rfc <- pmax(pmin(soil_tmp$rfc*factor,max_rocks),0)
    ru <- sum(medfate::soil_waterExtractable(soil_tmp, model = x$control$soilFunctions))
    return(ru_target - ru)
  }

  RU_cible   <- NA
  fracFind   <- FALSE
  illBeBack  <- FALSE
  model_Pval <- NULL
  model_Sval <- NULL
  ResAnalysis <- data.frame( SEW = 0 , PLC90 = 100 )
  if (LAI_max ==0) fracFind <- TRUE else RU_cible   <- 200
  while( ( nrow(ResAnalysis) < (max_simu+1) && !fracFind ) || (( nrow(ResAnalysis) < (max_simu+2) && illBeBack) ) ) {

    # Compute soil coarse fragment corresponding to target SEW
    if ( RU_cible <= RU_vg_min ) { # limit of maximum coarse fragments
      listCoarseFragNew <- rep(max_rocks, nlayers)
    } else if ( RU_cible == RU_vg_ori ) { # When target is equal to original
      listCoarseFragNew <- listCoarseFragOri
    } else if ( RU_cible >= RU_vg_max ) { # Limit of 0 coarse fragments
      listCoarseFragNew <- rep(0, nlayers)
    } else { # Find the the coarse fragment values corresponding to RU_cible
      r <- uniroot(f_ru_diff, c(0,10), RU_cible)
      listCoarseFragNew <- pmax(pmin(listCoarseFragOri*r$root,max_rocks),0)
    }

    if(verbose) cli::cli_li(paste0("Simulation #", nrow(ResAnalysis)))

    # Update soil for simulation
    soil_new <- soil
    soil_new$rfc <- round(listCoarseFragNew,4)
    RU_vg_new <- sum(medfate::soil_waterExtractable(soil_new, model = x$control$soilFunctions))
    x_new <- x
    x_new$soil <- soil_new

    # Launch simulation
    S_new <- spwb(x = x_new, meteo = meteo, ...)

    # 90% quantile by species of annual maximum PLC
    PLC_new <- 100*apply(summary(S_new, output="StemPLC", FUN = max),2,quantile, prob = 0.9)
    PLC_av_new <- sum(PLC_new*LAI_max_coh)/LAI_max

    # Load result of simulation
    ResAnalysis = rbind.data.frame(ResAnalysis, c(SEW=round(RU_vg_new,2), PLC90=PLC_av_new))

    if ( nrow(ResAnalysis)==2 ) {
      if ( ResAnalysis[2,2] > 50 ) {
        if ( abs(ResAnalysis[2,2]-(PLC90_target)) < PLC90_tol && abs(ResAnalysis[2,1]-RU_vg_max) < model_varLim ) {
          fracFind = TRUE
          RU_cible = ResAnalysis[2,1]
        } else if ( abs(ResAnalysis[2,1]-RU_vg_max) < model_varLim ) {
          fracFind = TRUE
          RU_cible = NA
        } else {
          RU_cible = 350
        }
      } else if ( ResAnalysis[2,2] < 10 ) {
        RU_cible = 50
      } else {
        RU_cible = 100
      }
      RU_cible = min(max(RU_cible, RU_vg_min),RU_vg_max)
      if(verbose) cli::cli_li(paste0('After the first simulation, the second is function of simulated PLC90 value: ', round(ResAnalysis[2,2],4), ". New SEW target = ", round(RU_cible)))
    } else if ( (all(ResAnalysis[-1,2] > 80) && min(ResAnalysis[  ,2])>PLC90_target) ||
                (all(ResAnalysis[-1,2] < 5 ) && min(ResAnalysis[-1,2])<PLC90_target) ||
                (all((ResAnalysis[-1,2] > max(90, PLC90_target)) | (ResAnalysis[-1,2] < min(9, PLC90_target))) &&
                 ( sum(ResAnalysis[-1,2] < min(9, PLC90_target))<=1 || (max(ResAnalysis[-1,2][ResAnalysis[-1,2] < min(9, PLC90_target)])-min(ResAnalysis[-1,2][ResAnalysis[-1,2] < min(9, PLC90_target)])) ) ) )  {
      if(verbose) cli::cli_li("There is only extreme PLC90 values (close to 100 pr close to 0). Try to find intermediate values if it's possible.")
      diffTarget = min(abs(ResAnalysis[-1,2]-PLC90_target))
      if ( diffTarget < PLC90_tol  ) {
        fracFind = TRUE
        RU_cible = ResAnalysis[which(abs(ResAnalysis[-1,2]-PLC90_target) == diffTarget)+1,1]
      } else if ( all(ResAnalysis[-1,2] > 80) && min(ResAnalysis[,2])>PLC90_target ) {
        if ( abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) {
          fracFind = TRUE
          RU_cible = NA
        } else {
          RU_cible = RU_vg_max
        }
      } else if ( all(ResAnalysis[-1,2] < 5 ) && min(ResAnalysis[-1,2])<PLC90_target ) {
        if ( abs(min(ResAnalysis[-1,1])-RU_vg_min) < min(model_varLim,RU_vg_min/10) ) {
          fracFind = TRUE
          RU_cible = NA
        } else {
          RU_cible = RU_vg_min
        }
      } else { # case: all((ResAnalysis[-1,2] > max(90, PLC90_target)) | (ResAnalysis[-1,2] < min(10, PLC90_target))) )
        RU_justabove = which(ResAnalysis$PLC90[order(ResAnalysis$PLC90)] > PLC90_target)[1]
        RU_cible = mean(ResAnalysis$SEW[order(ResAnalysis$PLC90)[c(RU_justabove-1,RU_justabove)]])
      }
      if ( !is.na(RU_cible) && min(abs(ResAnalysis[-1,1] - RU_cible)) < model_varLim ) {
        fracFind = TRUE # TOTO I'LL BE BACK (relancer une dernier fois ?)
        if ( abs(min(ResAnalysis[-1,1])-RU_vg_min) < min(model_varLim,RU_vg_min/10) || abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) {
          RU_cible = NA
          stop('==> fail. Too low or high values. Stop here. \n')
        } else if ( lastSimu ) illBeBack = TRUE
      }
    } else {

      # Try to create the model
      # Chose the value of P (and slope?)
      # model = tryNLSmodel(ResAnalysis)
      RUmodel = .RUfromModels(ResAnalysis, PLC90_target, bavard = verbose)

      if ( illBeBack && !is.null(RUmodel) ) {
        if ( abs(min(max(RUmodel,RU_vg_min), RU_vg_max) - RU_cible) > (model_varLim*2) ) { #  || abs( ResAnalysis[-1,][which.min(abs(ResAnalysis[-1,1]-RUmodel)),2] - PLC90_target) > (PLC90_tol*2) ) {
          cat("########################################################################\n# WARNING: with ILLBEBACK THE NEW VALUE IS FAR FROM TARGET VALUE !!!!! #\n########################################################################\n")
          illBeBack = FALSE
        }
      }

      if ( !illBeBack ) {

        #if ( class(model) != "try-error" ) {
        if ( !is.null(RUmodel) ) {
          #if ( !is.null(model_Pval) && abs((model_Pval - summary(model)$parameters['P',1])/model_Pval)<model_varLim && abs((model_Sval - summary(model)$parameters['slope',1])/model_Sval)<(6*model_varLim) ) {
          # RU_target_old = RU_cible

          RU_cible      = min(max(RUmodel,RU_vg_min), RU_vg_max)
          # model_Pval = summary(model)$parameters['P',1]
          # model_Sval = summary(model)$parameters['slope',1]
          # RU_cible = model_Pval - log(100/PLC90_target - 1) * 25/model_Sval

          # Check if the model it's not wrong ==> we are on a wrong side of an existing value
          resOrder_above = ResAnalysis$PLC90[order(ResAnalysis$PLC90)] > PLC90_target
          res_justAbove = ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]],]
          if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below PLC90_target (which is above 10)
            res_justBelow = ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]-1],]
            if ( RU_cible > res_justBelow$SEW || RU_cible < res_justAbove$SEW ) {
              aa = (res_justBelow$PLC90 - res_justAbove$PLC90) / (res_justBelow$SEW- res_justAbove$SEW)
              bb = res_justAbove$PLC90 - res_justAbove$SEW * aa
              RU_cible = ( PLC90_target - bb ) / aa # mean pondéré (régression linéaire)
              cat( "NB: The model give a not logical value. Try manually....\n ")
            }
          } else if ( RU_cible < res_justAbove$SEW ) { # Only value above target and RU_cible below maw SEW compute
            RU_cible = min(max(ResAnalysis[,1]) + (RU_vg_max-RU_vg_min)/3,RU_vg_max)
          }

          pt_closest    = which.min(abs(ResAnalysis[-1,1]-RU_cible) )
          # PLC_closest   = ResAnalysis[-1,][pt_closest,2]
          RU_target_closest = ResAnalysis[-1,][pt_closest,1]

          if (verbose)  cli::cli_li(paste0("For simulation #", nrow(ResAnalysis)-1, " the SEW difference between target and model closest value is ",round(abs(RU_target_closest - RU_cible),3)))
          if ( abs(RU_target_closest - RU_cible) < (model_varLim*2) ) {
            fracFind = TRUE
            if ( RU_cible != RUmodel && !(abs(RU_target_closest - RUmodel) < (model_varLim*2)) && (RU_cible>RUmodel || PLC90_target<ResAnalysis[-1,][pt_closest,2]) ) { RU_cible = NA } else if ( lastSimu ) { illBeBack = TRUE } # else TOTO I'LL BE BACK (relancerune dernier fois)
          }
        } else {
          if (  min(abs(ResAnalysis[-1,2]-PLC90_target)) < PLC90_tol  ) {
            fracFind = TRUE
            RU_cible = ResAnalysis[which(abs(ResAnalysis[-1,2]-PLC90_target)== min(abs(ResAnalysis[-1,2]-PLC90_target)))+1,1]
          } else if ( min(ResAnalysis[,2]) > min(10,PLC90_target) ) {
            # if ( abs(min(ResAnalysis[,2])-(PLC90_target)) < PLC90_tol && abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) {
            #   fracFind = TRUE
            #   RU_cible = max(ResAnalysis[,1]) } else
            if ( min(ResAnalysis[,2])>PLC90_target && abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) { # Permet que si on est dans une simulation intermédiare, ça ne s'arrete pas
              fracFind = TRUE # Si on est dans le cas de PLC > PLC_cible alors que SEW ~= RU_max
              RU_cible = NA
            } else if ( abs(max(ResAnalysis[,1])-RU_vg_max) < model_varLim ) { # une PLC <= PLC_cible et SEW ~= RU_max (==> donc valeur 100 (au dessus) et au moins une valeur en dessous)
              resOrder_above = ResAnalysis$PLC90[order(ResAnalysis$PLC90)] > PLC90_target
              if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below PLC90_target (which is above 10)
                res_justAbove = ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]],]
                res_justBelow = ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]-1],]
                # RU_cible = mean(ResAnalysis$SEW[order(ResAnalysis$PLC90)[c(which(resOrder_above)[1]-1,which(resOrder_above)[1])]])  # simple mean
                aa = (res_justBelow$PLC90 - res_justAbove$PLC90) / (res_justBelow$SEW- res_justAbove$SEW)
                bb = res_justAbove$PLC90 - res_justAbove$SEW * aa
                RU_cible = ( PLC90_target - bb ) / aa # mean pondéré (régression linéaire)
              } else { # seulement des valeurs au dessus de PLC90_target
                RU_cible = min(max(ResAnalysis[,1]) + (RU_vg_max-RU_vg_min)/3,RU_vg_max)
              }
              if ( abs(min(ResAnalysis$SEW - RU_cible)) < model_varLim ) { # on vérifie que l'on a pas fait une simulation similaire....
                fracFind  = TRUE   # else TOTO I'LL BE BACK (relancerune dernier fois ?)
                if ( lastSimu ) illBeBack = TRUE
              }
            } else {
              RU_cible = min(max(ResAnalysis[,1]) + (RU_vg_max-RU_vg_min)/3,RU_vg_max)
            }
          } else if (  max(ResAnalysis[-1,2]) < max(60,PLC90_target) && min(ResAnalysis[-1,1])> (RU_vg_min + model_varLim) ) { # Cas où on veut un point avec plus faible PLC (<60 ou < target) mais SEW!=RU_min
            RU_cible = max(min(ResAnalysis[-1,1]) / 2, RU_vg_min)
            # } else if (  abs(max(ResAnalysis[-1,2])-(PLC90_target)) < PLC90_tol && min(ResAnalysis[-1,1])<20 ) {
            #   fracFind = TRUE
            #   RU_cible = min(ResAnalysis[-1,1])
          } else if (  max(ResAnalysis[-1,2]) < PLC90_target && abs(min(ResAnalysis[-1,1])-RU_vg_min) < model_varLim ) { # Cas où on a pas de point possible (< target + SEW = RU_min)
            fracFind = TRUE
            RU_cible = NA
          } else {
            # listeRU = c(200,150,250,100,300,50)
            # sort(listeRU)
            resOrder_above = ResAnalysis$PLC90[order(ResAnalysis$PLC90)] > PLC90_target
            if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below PLC90_target (which is above 10)
              res_justAbove = ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]],]
              res_justBelow = ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]-1],]
              # RU_cible = mean(ResAnalysis$SEW[order(ResAnalysis$PLC90)[c(which(resOrder_above)[1]-1,which(resOrder_above)[1])]])  # simple mean
              aa = (res_justBelow$PLC90 - res_justAbove$PLC90) / (res_justBelow$SEW- res_justAbove$SEW)
              bb = res_justAbove$PLC90 - res_justAbove$SEW * aa
              RU_cible = ( PLC90_target - bb ) / aa # mean pondéré (régression linéaire)
            } else { # seulement des valeurs au dessus de PLC90_target / a priori pas possible
              RU_cible = min(max(ResAnalysis[,1]) + (RU_vg_max-RU_vg_min)/3,RU_vg_max)
            }
            if ( abs(min(ResAnalysis$SEW - RU_cible)) < model_varLim ) { # on vérifie que l'on a pas fait une simulation similaire....
              fracFind  = TRUE   # else TOTO I'LL BE BACK (relancerune dernier fois ?)
              if ( lastSimu ) illBeBack = TRUE
            }
          }
        }

      } else {
        illBeBack = FALSE
      }
    }
    if ( illBeBack && min(abs(ResAnalysis[-1,1] - RU_cible)) <  (model_varLim/4) ) illBeBack = FALSE

  }


  r <- uniroot(f_ru_diff, c(0,10), RU_cible)
  listCoarseFragNew <- pmax(pmin(listCoarseFragOri*r$root,max_rocks),0)
  return(list(SEW = RU_cible, RFC = listCoarseFragNew, SimulationTable = ResAnalysis))
}
