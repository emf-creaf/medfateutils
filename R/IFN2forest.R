.densityFactor<-function(d) {
  factor<-c(127.3239546, 31.83098865,14.14710607, 5.092958185)
  FACTOREXP = rep(NA, length(d))
  FACTOREXP[which(d<12.5)] = 1
  FACTOREXP[which(d>=12.5 & d<22.5)] = 2
  FACTOREXP[which(d>=22.5 & d<42.5)] = 3
  FACTOREXP[which(d>=42.5)] = 4
  return(factor[FACTOREXP])
}
.IFN2forest<-function(xid, yid, SpParams,
                      zid = NULL,
                      setDefaults=TRUE, filterWrongRecords = TRUE,
                      keepNumOrden = TRUE) {
  f <- emptyforest()
  f$treeData <- data.frame(Species = xid$Species,
                          N = rep(NA, nrow(xid)),
                          DBH = (xid$Dn1+xid$Dn2)/10, # From mm to cm
                          Height = xid$Ht*100, # From m to cm
                          Z50 = rep(NA, nrow(xid)),
                          Z95 = rep(NA, nrow(xid)))
  f$treeData$N <- .densityFactor(f$treeData$DBH)
  if(keepNumOrden) {
    if("OrdenIfn2" %in% names(xid)) f$treeData$OrdenIfn2 = xid$OrdenIfn2
    if("OrdenIfn3" %in% names(xid)) f$treeData$OrdenIfn3 = xid$OrdenIfn3
    if("OrdenIfn4" %in% names(xid)) f$treeData$OrdenIfn4 = xid$OrdenIfn4
  }
  f$shrubData <- data.frame(Species = yid$Species,
                           Cover = as.numeric(yid$Fcc),
                           Height = yid$Hm*10,# From dm to cm
                           Z50 = rep(NA, nrow(yid)),
                           Z95 = rep(NA, nrow(yid)))
  if(setDefaults) {
    f$treeData$Z95 <- species_parameter(f$treeData$Species, SpParams, "Z95")
    f$treeData$Z50 <- species_parameter(f$treeData$Species, SpParams, "Z50")
    f$shrubData$Z95 <- species_parameter(f$shrubData$Species, SpParams, "Z95")
    f$shrubData$Z50 <- species_parameter(f$shrubData$Species, SpParams, "Z50")
    f$treeData$Z95[is.na(f$treeData$Z95)] <- 1000
    f$shrubData$Z95[is.na(f$shrubData$Z95)] = 800
    f$treeData$Z50[is.na(f$treeData$Z50)] <- exp(log(f$treeData$Z95[is.na(f$treeData$Z50)])/1.3)
    f$shrubData$Z50[is.na(f$shrubData$Z50)] <- exp(log(f$shrubData$Z95[is.na(f$shrubData$Z50)])/1.3)
  }
  if(filterWrongRecords) {
    #Remove missing
    f$treeData <- f$treeData[!is.na(f$treeData$Height),, drop=FALSE]
    f$treeData <- f$treeData[!is.na(f$treeData$DBH),, drop=FALSE]
    f$treeData <- f$treeData[!is.na(f$treeData$N),, drop=FALSE]
    f$shrubData <- f$shrubData[!is.na(f$shrubData$Cover),, drop=FALSE]
    f$shrubData <- f$shrubData[!is.na(f$shrubData$Height),, drop=FALSE]
    #Remove zero values
    f$treeData <- f$treeData[f$treeData$Height>0,, drop=FALSE]
    f$treeData <- f$treeData[f$treeData$DBH>0,, drop=FALSE]
    f$treeData <- f$treeData[f$treeData$N>0,, drop=FALSE]
    f$shrubData <- f$shrubData[f$shrubData$Cover>0,, drop=FALSE]
    f$shrubData <- f$shrubData[f$shrubData$Height>0,, drop=FALSE]
    #Remove wrong growthform
    tgf <- species_characterParameter(f$treeData$Species, SpParams, "GrowthForm")
    f$treeData <- f$treeData[tgf!="Shrub",, drop=FALSE]
    sgf <- species_characterParameter(f$shrubData$Species, SpParams, "GrowthForm")
    f$shrubData <- f$shrubData[sgf!="Tree",, drop=FALSE]
  }

  if(!is.null(zid)) {
    f$herbCover <- zid$Cover
    f$herbHeight <- zid$Height
    if(length(f$herbCover)>1) f$herbCover <- mean(f$herbCover, na.rm=TRUE)
    if(length(f$herbHeight)>1) f$herbHeight <- mean(f$herbHeight, na.rm=TRUE)
  }
  return(f)
}

#' Extract forest from IFN data
#'
#' Creates a \code{\link{forest}} object from Spanish Forest Inventory (IFN) data (DGCN 2005).
#'
#' @param pies_mayores A data frame with measured tree data (PCPiesMayores).
#' @param matorral A data frame with measured shrub data (PCMatorral).
#' @param ID A string with the ID of the plot to be extracted.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param IFNherbData A data frame with cover and mean height of the herb layer.
#' @param setDefaults Initializes default values for missing fields in IFN data.
#' @param filterWrongRecords Filters wrong records (records with missing values, zero values or wrong growth forms)
#' @param keepNumOrden Keeps num orden as additional column (OrdenIfn2, OrdenIfn3, ...)
#' @param verbose A boolean flag to indicate console output.
#'
#' @details
#' IFN input data needs to be in a specific format.
#' \itemize{
#'   \item{
#'     For \code{pies_mayores}, the following columns are required:
#'     \itemize{
#'       \item{ID: Plot ID string}
#'       \item{Especie: String of tree IFN species code (will be mapped to medfate code).}
#'       \item{Dn1: Diameter at breast height (in mm), first measurement.}
#'       \item{Dn2: Diameter at breast height (in mm), second measurement.}
#'       \item{Ht: Total tree height (in m)}
#'     }
#'   }
#'   \item{
#'     For \code{matorral}, the following columns are required:
#'     \itemize{
#'        \item{ID: Plot ID string.}
#'        \item{Especie: String of shrub IFN species code (will be mapped to medfate code).}
#'        \item{Fcc: Numeric values of shrub cover (in \%).}
#'        \item{Hm: Average shrub height (in dm).}
#'     }
#'   }
#' }
#'
#' Functions \code{IFN2forest} and \code{IFN2forestlist} call \code{\link{translateIFNSpeciesCodes}} internally
#' to translate IFN codes into medfate codes.
#'
#' @return Function \code{IFN2forest} returns an object of class \code{\link{forest}}, whereas function \code{IFN2forestlist} returns a list of \code{\link{forest}} objects.
#'
#' @export
#'
#' @name IFN2forest
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @references DGCN (2005). Tercer Inventario Forestal Nacional (1997-2007): Catalunya. Dirección General de Conservación de la Naturaleza, Ministerio de Medio Ambiente, Madrid.
#' @seealso \code{\link{forest}}, \code{\link{translateIFNSpeciesCodes}}
#'
#' @examples
#' data(SpParamsMED)
#' data(piesMayoresIFN2)
#' data(matorralIFN2)
#'
#' # Builds an object 'forest' corresponding to one specific forest plot
#' f = IFN2forest(piesMayoresIFN2, matorralIFN2,
#'                ID = "081065", SpParams = SpParamsMED)
#' print(f)
#'
#' # Builds a list whose elements are 'forest' objects
#' l = IFN2forestlist(piesMayoresIFN2, matorralIFN2, SpParamsMED)
#'
#' # Plot codes are in list names
#' names(l)
#'
#' # First forest object
#' l[[1]]
IFN2forest<-function(pies_mayores, matorral, ID, SpParams,
                     IFNherbData = NULL,
                     setDefaults=TRUE,
                     filterWrongRecords = TRUE, keepNumOrden = TRUE, verbose = TRUE) {

  xid <- pies_mayores[pies_mayores$ID==ID,, drop = FALSE]
  yid <- matorral[matorral$ID==ID,, drop = FALSE]

  toRemX <- is.na(xid$Especie) | is.na(xid$Dn1) | is.na(xid$Ht)
  if(sum(toRemX)>0) {
    if(verbose) cat(paste0("Filtered records in tree data: ", sum(toRemX),"\n"))
    xid <- xid[!toRemX,]
  }
  toRemY <- is.na(yid$Especie) | is.na(yid$Fcc) | is.na(yid$Hm)
  if(sum(toRemY)>0) {
    if(verbose) cat(paste0("Filtered records in shrub data: ", sum(toRemY),"\n"))
    yid <- yid[!toRemY,]
  }

  xid$Species <- translateIFNSpeciesCodes(xid$Especie, SpParams$IFNcodes)
  yid$Species <- translateIFNSpeciesCodes(yid$Especie, SpParams$IFNcodes)
  #Remove NA species
  if(sum(is.na(xid$Species))>0) {
    if(verbose) {
      cat(paste0("Tree data records with unrecognized IFN species codes: ",
                 sum(is.na(xid$Species)),"/", length(xid$Species),
                 " (",round(100*sum(is.na(xid$Species))/length(xid$Species),1),"%)\n"))
      print(table(xid$Especie[is.na(xid$Species)], useNA = "ifany"))
    }
    xid <- xid[!is.na(xid$Species),]
  }
  if(sum(is.na(yid$Species))>0) {
    if(verbose) {
      cat(paste0("Shrub data records with unrecognized IFN species codes: ",
                 sum(is.na(yid$Species)),"/", length(yid$Species),
                 " (",round(100*sum(is.na(yid$Species))/length(yid$Species),1),"%)\n"))
      print(table(yid$Especie[is.na(yid$Species)], useNA = "ifany"))
    }
    yid <- yid[!is.na(yid$Species),]
  }

  if(!is.null(IFNherbData)) {
    zid <- IFNherbData[IFNherbData$ID==ID,, drop =FALSE]
  } else {
    zid <- NULL
  }
  return(.IFN2forest(xid,yid,SpParams,
                     zid, setDefaults, filterWrongRecords, keepNumOrden))
}

#' @rdname IFN2forest
#' @export
IFN2forestlist<-function(pies_mayores, matorral, SpParams,
                         IFNherbData=NULL,
                         setDefaults=TRUE,
                         filterWrongRecords = TRUE, keepNumOrden = TRUE, verbose = TRUE) {

  if(sum(c("Ht","Dn1", "Dn2","Especie","ID") %in% names(pies_mayores))<4) stop("Columns in 'pies_mayores' must include 'ID','Especie','Dn1', 'Dn2' and 'Ht'")
  if(sum(c("Hm","Fcc","Especie","ID") %in% names(matorral))<4) stop("Columns in 'matorral' must include 'ID','Especie','Fcc' and 'Hm'")
  IDs <- as.character(sort(unique(c(pies_mayores$ID, matorral$ID))))

  if(verbose) cat(paste0("Number of plots: ", length(IDs),"\n"))
  x <- pies_mayores[pies_mayores$ID %in% IDs, , drop = FALSE]
  y <- matorral[matorral$ID %in% IDs, , drop = FALSE]

  toRemX <- is.na(x$Especie) | is.na(x$Dn1) | is.na(x$Dn2) | is.na(x$Ht)
  if(sum(toRemX)>0) {
    if(verbose) cat(paste0("Filtered records in tree data: ", sum(toRemX),"\n"))
    x <- x[!toRemX,]
  }
  toRemY <- is.na(y$Especie) | is.na(y$Fcc) | is.na(y$Hm)
  if(sum(toRemY)>0) {
    if(verbose) cat(paste0("Filtered records in shrub data: ", sum(toRemY),"\n"))
    y <- y[!toRemY,]
  }

  if(verbose) cat("Translating species codes...\n")

  x$Species <- translateIFNSpeciesCodes(x$Especie, SpParams$IFNcodes)
  y$Species <- translateIFNSpeciesCodes(y$Especie, SpParams$IFNcodes)

  #Remove NA species
  if(sum(is.na(x$Species))>0) {
    if(verbose) {
      cat(paste0("Tree data records with unrecognized IFN species codes: ",
                 sum(is.na(x$Species)),"/", length(x$Species),
                 " (",round(100*sum(is.na(x$Species))/length(x$Species),1),"%)\n"))
      print(table(x$Especie[is.na(x$Species)], useNA = "ifany"))
    }
    x <- x[!is.na(x$Species),]
  }
  if(sum(is.na(y$Species))>0) {
    if(verbose) {
      cat(paste0("Shrub data records with unrecognized IFN species codes: ",
                 sum(is.na(y$Species)),"/", length(y$Species),
                 " (",round(100*sum(is.na(y$Species))/length(y$Species),1),"%)\n"))
      print(table(y$Especie[is.na(y$Species)], useNA = "ifany"))
    }
    y <- y[!is.na(y$Species),]
  }

  if(verbose) cat("Extracting IFN data...\n")
  if(is.null(IFNherbData)) {
    lx <- split(x, factor(x$ID, levels=IDs))
    ly <- split(y, factor(y$ID, levels=IDs))

    forestlist <- Map(function(x,y, id) {
      .IFN2forest(x,y, SpParams=SpParams, setDefaults = setDefaults, filterWrongRecords = filterWrongRecords, keepNumOrden = keepNumOrden)
    }, lx, ly, IDs)
  } else {
    z <- IFNherbData[IFNherbData$ID %in% IDs, ]
    lx <- split(x, factor(x$ID, levels=IDs))
    ly <- split(y, factor(y$ID, levels=IDs))
    lz <- split(z, factor(z$ID, levels=IDs))
    forestlist <- Map(function(x,y, z, id) {
      .IFN2forest(x,y, z, SpParams=SpParams,setDefaults = setDefaults, filterWrongRecords = filterWrongRecords, keepNumOrden = keepNumOrden)
    }, lx, ly, lz, IDs)
  }
  if(verbose) cat("done.\n")
  return(forestlist)
}
