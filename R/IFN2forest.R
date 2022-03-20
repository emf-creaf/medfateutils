
.IFN2forest<-function(ID, xid, yid, SpParams,
                      zid = NULL,
                      patchsize = 10000, setDefaults=TRUE, filterWrongRecords = TRUE,
                      keepNumOrden = TRUE) {
  f = list()
  f$ID = ID
  f$patchsize = patchsize
  f$treeData = data.frame(Species = xid$Species, N = as.numeric(xid$N), DBH = xid$DBH, Height = xid$H*100)
  f$treeData$Z50 = rep(NA, nrow(f$treeData))
  f$treeData$Z95 = rep(NA, nrow(f$treeData))
  if(keepNumOrden) {
    if("OIF2" %in% names(xid)) f$treeData$OIF2 = xid$OIF2
    if("OIF3" %in% names(xid)) f$treeData$OIF3 = xid$OIF3
    if("OIF4" %in% names(xid)) f$treeData$OIF4 = xid$OIF4
  }
  f$shrubData = data.frame(Species = yid$Species, Cover = as.numeric(yid$FCC), Height = yid$H*100)
  f$shrubData$Z50 =rep(NA, nrow(f$shrubData))
  f$shrubData$Z95 =rep(NA, nrow(f$shrubData))
  if(setDefaults) {
    f$treeData$Z95 = species_parameter(f$treeData$Species, SpParams, "Z95")
    f$treeData$Z50 = species_parameter(f$treeData$Species, SpParams, "Z50")
    f$shrubData$Z95 = species_parameter(f$shrubData$Species, SpParams, "Z95")
    f$shrubData$Z50 = species_parameter(f$shrubData$Species, SpParams, "Z50")
    f$treeData$Z95[is.na(f$treeData$Z95)] = 1000
    f$shrubData$Z95[is.na(f$shrubData$Z95)] = 800
    f$treeData$Z50[is.na(f$treeData$Z50)] = exp(log(f$treeData$Z95[is.na(f$treeData$Z50)])/1.3)
    f$shrubData$Z50[is.na(f$shrubData$Z50)] = exp(log(f$shrubData$Z95[is.na(f$shrubData$Z50)])/1.3)
  }
  if(filterWrongRecords) {
    #Remove missing
    f$treeData = f$treeData[!is.na(f$treeData$Height),, drop=FALSE]
    f$treeData = f$treeData[!is.na(f$treeData$DBH),, drop=FALSE]
    f$treeData = f$treeData[!is.na(f$treeData$N),, drop=FALSE]
    f$shrubData = f$shrubData[!is.na(f$shrubData$Cover),, drop=FALSE]
    f$shrubData = f$shrubData[!is.na(f$shrubData$Height),, drop=FALSE]
    #Remove zero values
    f$treeData = f$treeData[f$treeData$Height>0,, drop=FALSE]
    f$treeData = f$treeData[f$treeData$DBH>0,, drop=FALSE]
    f$treeData = f$treeData[f$treeData$N>0,, drop=FALSE]
    f$shrubData = f$shrubData[f$shrubData$Cover>0,, drop=FALSE]
    f$shrubData = f$shrubData[f$shrubData$Height>0,, drop=FALSE]
    #Remove wrong growthform
    tgf = species_characterParameter(f$treeData$Species, SpParams, "GrowthForm")
    f$treeData = f$treeData[tgf!="Shrub",, drop=FALSE]
    sgf = species_characterParameter(f$shrubData$Species, SpParams, "GrowthForm")
    f$shrubData = f$shrubData[sgf!="Tree",, drop=FALSE]
  }

  f$herbCover = NA
  f$herbHeight = NA
  if(!is.null(zid)) {
    f$herbCover = zid$Cover
    f$herbHeight = zid$Height
    if(length(f$herbCover)>1) f$herbCover = mean(f$herbCover, na.rm=TRUE)
    if(length(f$herbHeight)>1) f$herbHeight = mean(f$herbHeight, na.rm=TRUE)
  }
  class(f)<-c("forest","list")
  return(f)
}

#' Extract forest from IFN data
#'
#' Creates a \code{\link{forest}} object from Spanish Forest Inventory (IFN) data (DGCN 2005).
#'
#' @param IFNtreeData A data frame with measured tree data.
#' @param IFNshrubData A data frame with measured shrub data.
#' @param ID A string with the ID of the plot to be extracted.
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param IFNherbData A data frame with cover and mean height of the herb layer.
#' @param patchsize The area of the forest stand, in square meters.
#' @param setDefaults Initializes default values for missing fields in IFN data.
#' @param filterWrongRecords Filters wrong records (records with missing values, zero values or wrong growth forms)
#' @param keepNumOrden Keeps num orden as additional column (OIF2, OIF3, ...)
#' @param verbose A boolean flag to indicate console output.
#'
#' @details IFN input data needs to be in a specific format, see package \code{IFNread}. Functions \code{IFN2forest} and \code{IFN2forestlist} call \code{\link{translateIFNSpeciesCodes}} internally.
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
#' data(exampletreedata)
#' data(exampleshrubdata)
#'
#' # Builds an object 'forest' corresponding to one specific forest plot
#' f = IFN2forest(exampletreedata, exampleshrubdata,
#'                ID = "81065", SpParams = SpParamsMED)
#' print(f)
#'
#' # Builds a list whose elements are 'forest' objects
#' l = IFN2forestlist(exampletreedata, exampleshrubdata, SpParamsMED)
#' names(l)
#' l[[1]]
IFN2forest<-function(IFNtreeData, IFNshrubData, ID, SpParams,
                     IFNherbData = NULL,
                     patchsize = 10000, setDefaults=TRUE,
                     filterWrongRecords = TRUE, keepNumOrden = TRUE, verbose = TRUE) {

  xid = IFNtreeData[IFNtreeData$ID==ID,]
  yid = IFNshrubData[IFNshrubData$ID==ID,]

  toRemX = is.na(xid$Species) | is.na(xid$DBH) | is.na(xid$H)
  if(sum(toRemX)>0) {
    if(verbose) cat(paste0("Filtered records in tree data: ", sum(toRemX),"\n"))
    xid = xid[!toRemX,]
  }
  toRemY = is.na(yid$Species) | is.na(yid$FCC) | is.na(yid$H)
  if(sum(toRemY)>0) {
    if(verbose) cat(paste0("Filtered records in shrub data: ", sum(toRemY),"\n"))
    yid = yid[!toRemY,]
  }

  IFNcodes = SpParams$IFNcodes
  xid$SpeciesMF = translateIFNSpeciesCodes(xid$Species, IFNcodes)
  yid$SpeciesMF = translateIFNSpeciesCodes(yid$Species, IFNcodes)
  #Remove NA species
  if(sum(is.na(xid$SpeciesMF))>0) {
    if(verbose) {
      cat(paste0("Tree data records with unrecognized IFN species codes: ",
                 sum(is.na(xid$SpeciesMF)),"/", length(xid$SpeciesMF),
                 " (",round(100*sum(is.na(xid$SpeciesMF))/length(xid$SpeciesMF),1),"%)\n"))
      print(table(xid$Species[is.na(xid$SpeciesMF)], useNA = "ifany"))
    }
    xid = xid[!is.na(xid$SpeciesMF),]
  }
  if(sum(is.na(yid$SpeciesMF))>0) {
    if(verbose) {
      cat(paste0("Shrub data records with unrecognized IFN species codes: ",
                 sum(is.na(yid$SpeciesMF)),"/", length(yid$SpeciesMF),
                 " (",round(100*sum(is.na(yid$SpeciesMF))/length(yid$SpeciesMF),1),"%)\n"))
      print(table(yid$Species[is.na(yid$SpeciesMF)], useNA = "ifany"))
    }
    yid = yid[!is.na(yid$SpeciesMF),]
  }
  xid$Species =xid$SpeciesMF
  yid$Species =yid$SpeciesMF

  if(!is.null(IFNherbData)) {
    zid = IFNherbData[IFNherbData$ID==ID,]
  } else {
    zid = NULL
  }
  return(.IFN2forest(ID, xid,yid,SpParams,
                     zid, patchsize, setDefaults, filterWrongRecords, keepNumOrden))
}

#' @rdname IFN2forest
#' @export
IFN2forestlist<-function(IFNtreeData, IFNshrubData, SpParams,
                         IFNherbData=NULL,
                         setDefaults=TRUE,
                         filterWrongRecords = TRUE, keepNumOrden = TRUE, verbose = TRUE) {

  if(sum(c("H","DBH","Species","ID") %in% names(IFNtreeData))<4) stop("Columns in IFNtreeData must include 'ID','Species','DBH' and 'H'")
  if(sum(c("H","FCC","Species","ID") %in% names(IFNshrubData))<4) stop("Columns in IFNshrubData must include 'ID','Species','FCC' and 'H'")
  IDs = as.character(sort(unique(c(IFNtreeData$ID, IFNshrubData$ID))))

  if(verbose) cat(paste0("Number of plots: ", length(IDs),"\n"))
  x = IFNtreeData[IFNtreeData$ID %in% IDs, ]
  y = IFNshrubData[IFNshrubData$ID %in% IDs, ]

  toRemX = is.na(x$Species) | is.na(x$DBH) | is.na(x$H)
  if(sum(toRemX)>0) {
    if(verbose) cat(paste0("Filtered records in tree data: ", sum(toRemX),"\n"))
    x = x[!toRemX,]
  }
  toRemY = is.na(y$Species) | is.na(y$FCC) | is.na(y$H)
  if(sum(toRemY)>0) {
    if(verbose) cat(paste0("Filtered records in shrub data: ", sum(toRemY),"\n"))
    y = y[!toRemY,]
  }

  if(verbose) cat("Translating species codes...\n")


  IFNcodes = SpParams$IFNcodes
  x$SpeciesMF = translateIFNSpeciesCodes(x$Species, IFNcodes)
  y$SpeciesMF = translateIFNSpeciesCodes(y$Species, IFNcodes)

  #Remove NA species
  if(sum(is.na(x$SpeciesMF))>0) {
    if(verbose) {
      cat(paste0("Tree data records with unrecognized IFN species codes: ",
                 sum(is.na(x$SpeciesMF)),"/", length(x$SpeciesMF),
                 " (",round(100*sum(is.na(x$SpeciesMF))/length(x$SpeciesMF),1),"%)\n"))
      print(table(x$Species[is.na(x$SpeciesMF)], useNA = "ifany"))
    }
    x = x[!is.na(x$SpeciesMF),]
  }
  if(sum(is.na(y$SpeciesMF))>0) {
    if(verbose) {
      cat(paste0("Shrub data records with unrecognized IFN species codes: ",
                 sum(is.na(y$SpeciesMF)),"/", length(y$SpeciesMF),
                 " (",round(100*sum(is.na(y$SpeciesMF))/length(y$SpeciesMF),1),"%)\n"))
      print(table(y$Species[is.na(y$SpeciesMF)], useNA = "ifany"))
    }
    y = y[!is.na(y$SpeciesMF),]
  }
  x$Species =x$SpeciesMF
  y$Species =y$SpeciesMF

  if(verbose) cat("Extracting IFN data...\n")
  if(is.null(IFNherbData)) {
    lx = split(x, factor(x$ID, levels=IDs))
    ly = split(y, factor(y$ID, levels=IDs))

    forestlist = Map(function(x,y, id) {
      .IFN2forest(id, x,y, SpParams=SpParams,setDefaults = setDefaults, filterWrongRecords = filterWrongRecords, keepNumOrden = keepNumOrden)
    }, lx, ly, IDs)
  } else {
    z = IFNherbData[IFNherbData$ID %in% IDs, ]
    lx = split(x, factor(x$ID, levels=IDs))
    ly = split(y, factor(y$ID, levels=IDs))
    lz = split(z, factor(z$ID, levels=IDs))
    forestlist = Map(function(x,y, z, id) {
      .IFN2forest(id, x,y, z, SpParams=SpParams,setDefaults = setDefaults, filterWrongRecords = filterWrongRecords, keepNumOrden = keepNumOrden)
    }, lx, ly, lz, IDs)
  }
  if(verbose) cat("done.\n")
  return(forestlist)
}
