
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

.SEWfromModels <- function(ResAnalysis, PLC90_target, doModels = c(TRUE, TRUE, TRUE, TRUE), bavard = FALSE){
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

  SEW_target = NULL
  if ( 2 %in% minSigmaModel ) {
    if ( bavard ) cli::cli_li("The model sigmoid NEW is selected for SEW target. ")
    model_Pval = summary(model2)$parameters['P',1]
    model_Sval = summary(model2)$parameters['slope',1]
    model_Sigval = summary(model2)$parameters['sigmo',1]
    SEW_target = model_Pval - log(100/PLC90_target - model_Sigval) * 25/model_Sval
  } else if ( 1 %in% minSigmaModel ) {
    if ( bavard ) cli::cli_li("The model sigmoid original is selected for SEW target. ")
    model_Pval = summary(model1)$parameters['P',1]
    model_Sval = summary(model1)$parameters['slope',1]
    SEW_target = model_Pval - log(100/PLC90_target - 1) * 25/model_Sval
  } else if ( 3 %in% minSigmaModel ) {
    if ( bavard ) cli::cli_li("The model 1/x is selected for SEW target. ")
    model_Pval = summary(model3)$parameters['P',1]
    model_Sval = summary(model3)$parameters['slope',1]
    SEW_target = model_Sval / PLC90_target - model_Pval
  } else if ( 4 %in% minSigmaModel ) {
    if ( bavard ) cli::cli_li("The model 1/x+offs is selected for SEW target. ")
    model_Pval = summary(model4)$parameters['P',1]
    model_Sval = summary(model4)$parameters['slope',1]
    model_Oval = summary(model4)$parameters['offS',1]
    SEW_target = model_Sval / (PLC90_target-model_Oval) - model_Pval
  }
  return(SEW_target)
}



#' Optimization of rock fragment content
#'
#' Function \code{spwb_rockOptimization} finds optimum rock fragment content in the soil
#' corresponding to given vegetation, weather and target percent loss
#' of conductance (PLC), following the method described in Druel et al. (2023).
#'
#' @param x An object of class \code{\link{spwbInput}}.
#' @param meteo A data frame with daily meteorological data series (see \code{\link{spwb}}).
#' @param PLC90_target Stem PLC target (quantile 90).
#' @param PLC90_tol Limit of the PLC target tolerance, only in some conditions.
#' @param max_simu Maximum of simulation authorized before stopping.
#' @param model_varLim Limit of the soil extractable water (SEW) variation of the model accepted.
#' @param max_rocks Maximum content in coarse fragments allowed for any soil layer.
#' @param verbose A logical value. Print the internal messages of the function?
#' @param ... Additional parameters to function \code{\link{spwb}}.
#'
#' @details
#' The function performs a model inversion based on an ecohydrological assumption,
#' consisting in that forest leaf area index is in equilibrium with a low embolism
#' rate under normal conditions. This is translated in that the 90\% interannual quantile of
#' the maximum annual percent loss of conductance (PLC), averaged over plant cohorts,
#' should be close to a target PLC value (by default 12\%).
#'
#' @return
#' Function \code{spwb_rockOptimization} returns a list containing the estimated rock fragment content,
#' the corresponding soil extractable water and a dataframe of simulation results needed for the estimation.
#'
#'
#' @references
#' Druel, A., Martins, N., Cochard, H., De Caceres, M., Delzon, S., Mencuccini, M., Torres-Ruiz, J., and Ruffault, J.: European forest vulnerability to hydraulic failure: an ecohydrological approach, EGU General Assembly 2023, Vienna, Austria, 24–28 Apr 2023, EGU23-17068, https://doi.org/10.5194/egusphere-egu23-17068, 2023.
#'
#' @author
#' \enc{Arsène}{Arsene} Druel, URFM-INRAE
#'
#' Nicolas Martin-StPaul, URFM-INRAE
#'
#' Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF
#'
#' @seealso \code{\link{spwb}}, \code{\link{soil}}, \code{\link{spwb_ldrOptimization}}
#'
#' @examples
#' \donttest{
#' #Load example daily meteorological data
#' data(examplemeteo)
#'
#' #Load example plot plant data
#' data(exampleforest)
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
#' x <- spwbInput(exampleforest,examplesoil, SpParamsMED, control)
#'
#' #Rock fragment content optimization
#' spwb_rockOptimization(x, meteo = examplemeteo,
#'                       elevation = 100, latitude = 41.82592)
#' }
#'
spwb_rockOptimization<-function(x, meteo,
                                PLC90_target = 12, PLC90_tol = 0.5,
                                max_simu = 7, model_varLim = 10,
                                max_rocks = 99, verbose = FALSE, ...){

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

  SEW_ori <- sum(medfate::soil_waterExtractable(soil, model = x$control$soilFunctions))

  soil_max <- soil
  soil_max$rfc <- rep(0, nlayers)
  soil_min <- soil
  soil_min$rfc <- rep(max_rocks, nlayers)
  SEW_max <- sum(medfate::soil_waterExtractable(soil_max, model = x$control$soilFunctions))
  SEW_min <- sum(medfate::soil_waterExtractable(soil_min, model = x$control$soilFunctions))

  # Function to be optimized for factor corresponding to sew_target
  f_sew_diff <- function(factor, sew_target) {
    soil_tmp <- soil
    soil_tmp$rfc <- pmax(pmin(soil_tmp$rfc*factor,max_rocks),0)
    sew <- sum(medfate::soil_waterExtractable(soil_tmp, model = x$control$soilFunctions))
    return(sew_target - sew)
  }

  SEW_target   <- NA
  fracFind   <- FALSE
  illBeBack  <- FALSE
  model_Pval <- NULL
  model_Sval <- NULL
  ResAnalysis <- data.frame( SEW = 0 , PLC90 = 100 )
  if (LAI_max ==0) fracFind <- TRUE else SEW_target   <- 200
  while( (nrow(ResAnalysis) < (max_simu+1) && !fracFind ) && (nrow(ResAnalysis) < (max_simu+2)) ) {
    # Compute soil coarse fragment corresponding to target SEW
    if ( SEW_target <= SEW_min ) { # limit of maximum coarse fragments
      listCoarseFragNew <- rep(max_rocks, nlayers)
    } else if ( SEW_target == SEW_ori ) { # When target is equal to original
      listCoarseFragNew <- listCoarseFragOri
    } else if ( SEW_target >= SEW_max ) { # Limit of 0 coarse fragments
      listCoarseFragNew <- rep(0, nlayers)
    } else { # Find the the coarse fragment values corresponding to SEW_target
      r <- uniroot(f_sew_diff, c(0,10), SEW_target)
      listCoarseFragNew <- pmax(pmin(listCoarseFragOri*r$root,max_rocks),0)
    }

    if(verbose) cli::cli_li(paste0("Simulation #", nrow(ResAnalysis)))

    # Update soil for simulation
    soil_new <- soil
    soil_new$rfc <- round(listCoarseFragNew,4)
    SEW_new <- sum(medfate::soil_waterExtractable(soil_new, model = x$control$soilFunctions))
    x_new <- x
    x_new$soil <- soil_new

    # Launch simulation
    S_new <- spwb(x = x_new, meteo = meteo, ...)

    # 90% quantile by species of annual maximum PLC
    PLC_new <- 100*apply(summary(S_new, output="StemPLC", FUN = max),2,quantile, prob = 0.9)
    PLC_av_new <- sum(PLC_new*LAI_max_coh)/LAI_max

    # Load result of simulation
    ResAnalysis = rbind.data.frame(ResAnalysis, c(SEW=round(SEW_new,2), PLC90=PLC_av_new))

    if ( nrow(ResAnalysis)==2 ) {
      if ( ResAnalysis[2,2] > 50 ) {
        if ( abs(ResAnalysis[2,2]-(PLC90_target)) < PLC90_tol && abs(ResAnalysis[2,1]-SEW_max) < model_varLim ) {
          fracFind <- TRUE
          SEW_target <- ResAnalysis[2,1]
        } else if ( abs(ResAnalysis[2,1]-SEW_max) < model_varLim ) {
          fracFind <- TRUE
          SEW_target <- NA
        } else {
          SEW_target <- 350
        }
      } else if ( ResAnalysis[2,2] < 10 ) {
        SEW_target <- 50
      } else {
        SEW_target <- 100
      }
      SEW_target <- min(max(SEW_target, SEW_min),SEW_max)
      if(verbose) cli::cli_li(paste0('After the first simulation, the second is function of simulated PLC90 value: ', round(ResAnalysis[2,2],4), ". New SEW target = ", round(SEW_target)))
    } else if ( (all(ResAnalysis[-1,2] > 80) && min(ResAnalysis[  ,2])>PLC90_target) ||
                (all(ResAnalysis[-1,2] < 5 ) && min(ResAnalysis[-1,2])<PLC90_target) ||
                (all((ResAnalysis[-1,2] > max(90, PLC90_target)) | (ResAnalysis[-1,2] < min(9, PLC90_target))) &&
                 ( sum(ResAnalysis[-1,2] < min(9, PLC90_target))<=1 || (max(ResAnalysis[-1,2][ResAnalysis[-1,2] < min(9, PLC90_target)])-min(ResAnalysis[-1,2][ResAnalysis[-1,2] < min(9, PLC90_target)])) ) ) )  {
      if(verbose) cli::cli_li("There is only extreme PLC90 values (close to 100 pr close to 0). Try to find intermediate values if it's possible.")
      diffTarget <- min(abs(ResAnalysis[-1,2]-PLC90_target))
      if ( diffTarget < PLC90_tol  ) {
        fracFind <- TRUE
        SEW_target <- ResAnalysis[which(abs(ResAnalysis[-1,2]-PLC90_target) == diffTarget)+1,1]
      } else if ( all(ResAnalysis[-1,2] > 80) && min(ResAnalysis[,2])>PLC90_target ) {
        if ( abs(max(ResAnalysis[,1])-SEW_max) < model_varLim ) {
          fracFind <- TRUE
          SEW_target <- NA
        } else {
          SEW_target <- SEW_max
        }
      } else if ( all(ResAnalysis[-1,2] < 5 ) && min(ResAnalysis[-1,2])<PLC90_target ) {
        if ( abs(min(ResAnalysis[-1,1])-SEW_min) < min(model_varLim,SEW_min/10) ) {
          fracFind <- TRUE
          SEW_target <- NA
        } else {
          SEW_target <- SEW_min
        }
      } else {
        SEW_justabove <- which(ResAnalysis$PLC90[order(ResAnalysis$PLC90)] > PLC90_target)[1]
        SEW_target <- mean(ResAnalysis$SEW[order(ResAnalysis$PLC90)[c(SEW_justabove-1,SEW_justabove)]])
      }
      if ( !is.na(SEW_target) && min(abs(ResAnalysis[-1,1] - SEW_target)) < model_varLim ) {
        fracFind <- TRUE
        if ( abs(min(ResAnalysis[-1,1])-SEW_min) < min(model_varLim,SEW_min/10) || abs(max(ResAnalysis[,1])-SEW_max) < model_varLim ) {
          SEW_target <- NA
          stop('==> fail. Too low or high values. Stop here. \n')
        }
      }
    } else {
      # Try to create the model
      SEW_model <- .SEWfromModels(ResAnalysis, PLC90_target, bavard = verbose)

      if ( !is.null(SEW_model) ) {
        SEW_target <- min(max(SEW_model,SEW_min), SEW_max)

        # Check if the model it's not wrong ==> we are on a wrong side of an existing value
        resOrder_above <- ResAnalysis$PLC90[order(ResAnalysis$PLC90)] > PLC90_target
        res_justAbove <- ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]],]
        if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below PLC90_target (which is above 10)
          res_justBelow <- ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]-1],]
          if ( SEW_target > res_justBelow$SEW || SEW_target < res_justAbove$SEW ) {
            aa <- (res_justBelow$PLC90 - res_justAbove$PLC90) / (res_justBelow$SEW- res_justAbove$SEW)
            bb <- res_justAbove$PLC90 - res_justAbove$SEW * aa
            SEW_target <- ( PLC90_target - bb ) / aa # mean pondéré (régression linéaire)
            if(verbose) cli::cli_li( "NB: The model give a not logical value. Try manually....\n ")
          }
        } else if ( SEW_target < res_justAbove$SEW ) { # Only value above target and SEW_target below maw SEW compute
          SEW_target <- min(max(ResAnalysis[,1]) + (SEW_max-SEW_min)/3,SEW_max)
        }

        pt_closest <- which.min(abs(ResAnalysis[-1,1]-SEW_target) )
        SEW_target_closest <- ResAnalysis[-1,][pt_closest,1]

        if (verbose)  cli::cli_li(paste0("The SEW difference between target and model closest value is ",round(abs(SEW_target_closest - SEW_target),3)))
        if ( abs(SEW_target_closest - SEW_target) < (model_varLim*2) ) {
          fracFind <- TRUE
          if ( SEW_target != SEW_model && !(abs(SEW_target_closest - SEW_model) < (model_varLim*2)) && (SEW_target>SEW_model || PLC90_target<ResAnalysis[-1,][pt_closest,2]) ) { SEW_target = NA }
        }
      } else {
        if (  min(abs(ResAnalysis[-1,2]-PLC90_target)) < PLC90_tol  ) {
          fracFind <- TRUE
          SEW_target <- ResAnalysis[which(abs(ResAnalysis[-1,2]-PLC90_target)== min(abs(ResAnalysis[-1,2]-PLC90_target)))+1,1]
        } else if ( min(ResAnalysis[,2]) > min(10,PLC90_target) ) {
          if ( min(ResAnalysis[,2])>PLC90_target && abs(max(ResAnalysis[,1])-SEW_max) < model_varLim ) { # Permet que si on est dans une simulation intermédiare, ça ne s'arrete pas
            fracFind <- TRUE # Si on est dans le cas de PLC > PLC_cible alors que SEW ~= RU_max
            SEW_target <- NA
          } else if ( abs(max(ResAnalysis[,1])-SEW_max) < model_varLim ) { # une PLC <= PLC_cible et SEW ~= RU_max (==> donc valeur 100 (au dessus) et au moins une valeur en dessous)
            resOrder_above <- ResAnalysis$PLC90[order(ResAnalysis$PLC90)] > PLC90_target
            if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below PLC90_target (which is above 10)
              res_justAbove <- ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]],]
              res_justBelow <- ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]-1],]
              aa <- (res_justBelow$PLC90 - res_justAbove$PLC90) / (res_justBelow$SEW- res_justAbove$SEW)
              bb <- res_justAbove$PLC90 - res_justAbove$SEW * aa
              SEW_target <- ( PLC90_target - bb ) / aa # mean pondéré (régression linéaire)
            } else { # seulement des valeurs au dessus de PLC90_target
              SEW_target <- min(max(ResAnalysis[,1]) + (SEW_max-SEW_min)/3,SEW_max)
            }
            if ( abs(min(ResAnalysis$SEW - SEW_target)) < model_varLim ) { # on vérifie que l'on a pas fait une simulation similaire....
              fracFind <- TRUE
            }
          } else {
            SEW_target <- min(max(ResAnalysis[,1]) + (SEW_max-SEW_min)/3,SEW_max)
          }
        } else if (  max(ResAnalysis[-1,2]) < max(60,PLC90_target) && min(ResAnalysis[-1,1])> (SEW_min + model_varLim) ) { # Cas où on veut un point avec plus faible PLC (<60 ou < target) mais SEW!=RU_min
          SEW_target <- max(min(ResAnalysis[-1,1]) / 2, SEW_min)
        } else if (  max(ResAnalysis[-1,2]) < PLC90_target && abs(min(ResAnalysis[-1,1])-SEW_min) < model_varLim ) { # Cas où on a pas de point possible (< target + SEW = RU_min)
          fracFind <- TRUE
          SEW_target <- NA
        } else {
          resOrder_above <- ResAnalysis$PLC90[order(ResAnalysis$PLC90)] > PLC90_target
          if ( resOrder_above[1] == FALSE ) { # case when there is a value above and a value below PLC90_target (which is above 10)
            res_justAbove <- ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]],]
            res_justBelow <- ResAnalysis[order(ResAnalysis$PLC90)[which(resOrder_above)[1]-1],]
            aa <- (res_justBelow$PLC90 - res_justAbove$PLC90) / (res_justBelow$SEW- res_justAbove$SEW)
            bb <- res_justAbove$PLC90 - res_justAbove$SEW * aa
            SEW_target <- ( PLC90_target - bb ) / aa # mean pondéré (régression linéaire)
          } else { # seulement des valeurs au dessus de PLC90_target / a priori pas possible
            SEW_target <- min(max(ResAnalysis[,1]) + (SEW_max-SEW_min)/3,SEW_max)
          }
          if ( abs(min(ResAnalysis$SEW - SEW_target)) < model_varLim ) { # on vérifie que l'on a pas fait une simulation similaire....
            fracFind <- TRUE
          }
        }
      }
    }
  }
  if(fracFind) {
    r <- uniroot(f_sew_diff, c(0,10), SEW_target)
    listCoarseFragNew <- pmax(pmin(listCoarseFragOri*r$root,max_rocks),0)
    res <- list(SEW = SEW_target, RFC = listCoarseFragNew, SimulationTable = ResAnalysis)
  } else {
    if (verbose)  cli::cli_warn(paste0("Valid SEW estimates could not be found given the input parameters"))
    res <- list(SEW = NA, RFC = NA, SimulationTable = ResAnalysis)
  }
  return(res)
}
