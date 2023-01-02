.densityFactor<-function(d) {
  factor<-c(127.3239546, 31.83098865,14.14710607, 5.092958185)
  FACTOREXP = rep(NA, length(d))
  FACTOREXP[which(d<12.5)] = 1
  FACTOREXP[which(d>=12.5 & d<22.5)] = 2
  FACTOREXP[which(d>=22.5 & d<42.5)] = 3
  FACTOREXP[which(d>=42.5)] = 4
  return(factor[FACTOREXP])
}

#' Extract forest from IFN data
#'
#' Creates a list of \code{\link{forest}} objects from Spanish Forest Inventory (IFN) data in its
#' second (IFN2), third (IFN3; DGCN 2005) and fourth (IFN4) editions.
#'
#' @param pies_mayores A data frame with measured tree data (PCPiesMayores).
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}).
#' @param pies_menores A data frame with measured regeneration tree data (PCPiesMenores in IFN2).
#' @param regenera A data frame with measured regeneration tree data (PCRegenera in IFN3 and IFN4).
#' @param matorral A data frame with measured shrub data (PCMatorral).
#' @param herb_data A data frame with cover and mean height of the herb layer for each forest plot.
#' @param setDefaults Initializes default values for missing fields in IFN data.
#' @param filterWrongRecords Filters wrong records (records with missing values, zero values or wrong growth forms).
#'                          This should normally result in the removal of dead/cut trees.
#' @param keepNumOrden Keeps num orden as additional column (OrdenIf2, OrdenIf3 or OrdenIf4) to identify trees.
#' @param verbose A boolean flag to indicate console output.
#'
#' @details
#' IFN input data needs to be in a specific format, following IFN specifications.
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
#'     For \code{pies_menores}, the following columns are required:
#'     \itemize{
#'       \item{ID: Plot ID string}
#'       \item{Especie: String of tree IFN species code (will be mapped to medfate code).}
#'       \item{Numero: Number of stems for 2.5-7.5 cm DBH class.}
#'       \item{Hm: Average height (in dm).}
#'       \item{Regena: Regeneration code ('0', 1', '2', '3').}
#'     }
#'   }
#'   \item{
#'     For \code{regenera}, the following columns are required:
#'     \itemize{
#'       \item{ID: Plot ID string}
#'       \item{Especie: String of tree IFN species code (will be mapped to medfate code).}
#'       \item{CatDes: Regeneration category ( 1', '2', '3' or '4').}
#'       \item{Densidad: Density category ( 1', '2', or '3').}
#'       \item{NumPies: Number of stems for 2.5-7.5 cm DBH class.}
#'       \item{Hm: Average height (in dm).}
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
#' Function \code{IFN2forest} call \code{\link{translateIFNSpeciesCodes}} internally
#' to translate IFN codes into medfate codes.
#'
#' @return A list of \code{\link{forest}} objects.
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
#'
#' # Medfate species parameters
#' data(SpParamsMED)
#'
#' # Builds from IFN2 data a list whose elements are 'forest' objects
#' data(piesMenoresIFN2)
#' data(piesMayoresIFN2)
#' data(matorralIFN2)
#' l <- IFN2forest(piesMayoresIFN2, SpParamsMED,
#'                matorral = matorralIFN2, pies_menores = piesMenoresIFN2)
#'
#' # Plot codes are in list names
#' names(l)
#'
#'
#' # Builds from IFN3 data a list whose elements are 'forest' objects
#' data(piesMayoresIFN3)
#' data(regeneraIFN3)
#' data(matorralIFN3)
#' l <- IFN2forest(piesMayoresIFN3, SpParamsMED,
#'                matorral = matorralIFN3, regenera = regeneraIFN3)
#'
#' @export
IFN2forest<-function(pies_mayores, SpParams,
                     pies_menores = NULL, regenera = NULL, matorral = NULL,
                     herb_data=NULL,
                     setDefaults=TRUE,
                     filterWrongRecords = TRUE, keepNumOrden = TRUE, verbose = TRUE) {

  if(sum(c("Ht","Dn1", "Dn2","Especie","ID") %in% names(pies_mayores))<4) stop("Columns in 'pies_mayores' must include 'ID','Especie','Dn1', 'Dn2' and 'Ht'")
  IDs <- pies_mayores$ID
  pies_mayores$Dn1 <- as.numeric(pies_mayores$Dn1)
  pies_mayores$Dn2 <- as.numeric(pies_mayores$Dn2)
  pies_mayores$Ht <- as.numeric(pies_mayores$Ht)

  if(!is.null(pies_menores)) {
    if(sum(c("Hm","Numero","Especie","ID", "Regena") %in% names(pies_menores))<5) stop("Columns in 'pies_menores' must include 'ID','Especie','Numero', 'Regena' and 'Hm'")
    IDs <- c(IDs, pies_menores$ID)
    pies_menores$Numero <- as.numeric(pies_menores$Numero)
    pies_menores$Regena <- as.numeric(pies_menores$Regena)
    pies_menores$Hm <- as.numeric(pies_menores$Hm)
  } else {
    pies_menores <- data.frame(ID = character(0), Especie = character(0), Numero = numeric(0), Hm = numeric(0), Regena = numeric(0))
  }

  if(!is.null(regenera)) {
    if(sum(c("Hm","CatDes","Especie","ID", "Densidad", "NumPies") %in% names(regenera))<6)
      stop("Columns in 'regenera' must include 'ID','Especie','CatDes', 'Densidad', 'NumPies' and 'Hm'")
    IDs <- c(IDs, regenera$ID)
    regenera$NumPies <- as.numeric(regenera$NumPies)
    regenera$Hm <- as.numeric(regenera$Hm)
  } else {
    regenera <- data.frame(ID = character(0), Especie = character(0), CatDes = character(0), Densidad = character(0), NumPies = numeric(0), Hm = numeric(0))
  }

  if(!is.null(matorral)) {
    if(sum(c("Hm","Fcc","Especie","ID") %in% names(matorral))<4) stop("Columns in 'matorral' must include 'ID','Especie','Fcc' and 'Hm'")
    IDs <- c(IDs, matorral$ID)
    matorral$Fcc <- as.numeric(matorral$Fcc)
    matorral$Hm <- as.numeric(matorral$Hm)
  } else {
    matorral <- data.frame(ID = character(0), Especie = character(0), Fcc = numeric(0), Hm = numeric(0))
  }

  IDs <- as.character(sort(unique(IDs)))

  if(verbose) cat(paste0("Number of plots: ", length(IDs),"\n"))
  x_mayores <- pies_mayores[pies_mayores$ID %in% IDs, , drop = FALSE]
  x_mayores$DBH <- (as.numeric(x_mayores$Dn1) + as.numeric(x_mayores$Dn2))/10 # From mm to cm
  x_mayores$Height <- as.numeric(x_mayores$Ht)*100 # From m to cm
  x_mayores$N <- .densityFactor(x_mayores$DBH)

  ## Pies Menores from IFN2
  x_menores2 <- pies_menores[pies_menores$ID %in% IDs, , drop = FALSE]
  x_menores2$DBH <- rep(5, nrow(x_menores2))
  x_menores2$Height <- as.numeric(x_menores2$Hm)*10 # From dm to cm
  x_menores2$N <- .densityFactor(x_menores2$DBH) * as.numeric(x_menores2$Numero)
  x_menores2 <- x_menores2[!is.na(x_menores2$Height),,drop=FALSE]
  x_menores2$Regena <- as.numeric(x_menores2$Regena)

  x_regenera2 <- pies_menores[pies_menores$ID %in% IDs, , drop = FALSE]
  x_regenera2 <- x_regenera2[!is.na(x_regenera2$Regena),,drop=FALSE]
  x_regenera2$Regena <- as.numeric(x_regenera2$Regena)
  x_regenera2 <- x_regenera2[x_regenera2$Regena>0 & x_regenera2$Regena<4,,drop=FALSE]
  x_regenera2$DBH <- rep(1, nrow(x_regenera2)) # Default
  x_regenera2$Height <- rep(100, nrow(x_regenera2)) # Default
  x_regenera2$N <- c(2.5,10,20)[x_regenera2$Regena]*.densityFactor(x_regenera2$DBH) # Density from Regena


  # Pies menores/regenera from IFN3/IFN4
  x_regenera34 <- regenera[regenera$ID %in% IDs, , drop = FALSE]
  x_menores34<-x_regenera34[x_regenera34$CatDes=="4",, drop = FALSE]
  x_menores34$DBH <- rep(5, nrow(x_menores34))
  x_menores34$N <- .densityFactor(x_menores34$DBH) * as.numeric(x_menores34$NumPies)
  x_menores34$Height <- as.numeric(x_menores34$Hm)*10 # From dm to cm

  x_regenera34 <- x_regenera34[x_regenera34$CatDes!="4",, drop = FALSE]
  x_regenera34$DBH <- c(0.1,0.5,1.5)[as.numeric(x_regenera34$CatDes)]
  x_regenera34$Height <- c(10,80,100)[as.numeric(x_regenera34$CatDes)]
  x_regenera34$N <- c(2.5,10,20)[as.numeric(x_regenera34$Densidad)]*.densityFactor(x_regenera34$DBH) # Density from CatDes

  # Merge all tree data sources
  x <- dplyr::bind_rows(x_mayores,
                        x_menores2, x_menores34,
                        x_regenera2, x_regenera34)

  y <- matorral[matorral$ID %in% IDs, , drop = FALSE]
  y$Cover <- as.numeric(y$Fcc)
  y$Height <- as.numeric(y$Hm)*10 # From dm to cm

  if(filterWrongRecords) {
    if(verbose) cat("Filtering missing and zero values...\n")

    #Remove missing
    x <- x[!is.na(x$Height),, drop=FALSE]
    x <- x[!is.na(x$DBH),, drop=FALSE]
    x <- x[!is.na(x$N),, drop=FALSE]
    x <- x[!is.na(x$Height),, drop=FALSE]
    y <- y[!is.na(y$Cover),, drop=FALSE]
    y <- y[!is.na(y$Height),, drop=FALSE]
    #Remove zero values
    x <- x[x$Height>0,, drop=FALSE]
    x <- x[x$DBH>0,, drop=FALSE]
    x <- x[x$N>0,, drop=FALSE]
    y <- y[y$Cover>0,, drop=FALSE]
    y <- y[y$Height>0,, drop=FALSE]

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
    x <- x[!is.na(x$Species),, drop = FALSE]
  }
  if(sum(is.na(y$Species))>0) {
    if(verbose) {
      cat(paste0("Shrub data records with unrecognized IFN species codes: ",
                 sum(is.na(y$Species)),"/", length(y$Species),
                 " (",round(100*sum(is.na(y$Species))/length(y$Species),1),"%)\n"))
      print(table(y$Especie[is.na(y$Species)], useNA = "ifany"))
    }
    y <- y[!is.na(y$Species),, drop = FALSE]
  }

  # Translate to species names
  x$Species[!is.na(x$Species)] <- medfate:::.speciesCharacterParameterFromSpIndex(x$Species[!is.na(x$Species)], SpParams, "Name")
  y$Species[!is.na(y$Species)] <- medfate:::.speciesCharacterParameterFromSpIndex(y$Species[!is.na(y$Species)], SpParams, "Name")

  if(filterWrongRecords) {
    if(verbose) cat("Filtering wrong growth forms ...\n")
    tgf <- species_characterParameter(x$Species, SpParams, "GrowthForm")
    x <- x[tgf!="Shrub",, drop=FALSE]
    sgf <- species_characterParameter(y$Species, SpParams, "GrowthForm")
    y <- y[sgf!="Tree",, drop=FALSE]
  }

  x$Z50 <- rep(NA, nrow(x))
  x$Z95 <- rep(NA, nrow(x))
  y$Z50 <- rep(NA, nrow(y))
  y$Z95 <- rep(NA, nrow(y))
  if(setDefaults) {
    if(verbose) cat("Setting default root distribution ...\n")
    x$Z95 <- species_parameter(x$Species, SpParams, "Z95")
    x$Z50 <- species_parameter(x$Species, SpParams, "Z50")
    y$Z95 <- species_parameter(y$Species, SpParams, "Z95")
    y$Z50 <- species_parameter(y$Species, SpParams, "Z50")
    x$Z95[is.na(x$Z95)] <- 1000
    y$Z95[is.na(y$Z95)] <- 800
    x$Z50[is.na(x$Z50)] <- exp(log(x$Z95[is.na(x$Z50)])/1.3)
    y$Z50[is.na(y$Z50)] <- exp(log(y$Z95[is.na(y$Z50)])/1.3)
  }

  if(verbose) cat("Building forest objects...\n")
  xf <- x[,c("ID", "Species","DBH", "Height", "N", "Z50", "Z95")]
  if(keepNumOrden) {
    if("OrdenIf2" %in% names(x)) xf$OrdenIf2 <- x$OrdenIf2
    if("OrdenIf3" %in% names(x)) xf$OrdenIf3 <- x$OrdenIf3
    if("OrdenIf4" %in% names(x)) xf$OrdenIf4 <- x$OrdenIf4
  }
  yf <- y[,c("ID", "Species", "Height", "Cover", "Z50", "Z95")]

  lx <- split(xf, factor(x$ID, levels=IDs), drop = FALSE)
  ly <- split(yf, factor(y$ID, levels=IDs), drop = FALSE)

  if(!is.null(herb_data)) {
    z <- herb_data[herb_data$ID %in% IDs, , drop = FALSE]
  } else {
    z <- data.frame(ID=character(0), Cover = numeric(0), Height = numeric(0))
  }
  lz <- split(z, factor(z$ID, levels=IDs), drop = FALSE)

  forestlist <- vector("list", length(IDs))
  names(forestlist) <- IDs
  for(i in 1:length(IDs)) {
    f <- list()
    f$treeData <- data.frame(lx[[i]][,-1], row.names=NULL)
    f$shrubData <- data.frame(ly[[i]][,-1], row.names=NULL)
    if(nrow(lz[[i]])>0) {
      zi <- lz[[i]]
      f$herbCover <- mean(zi$Cover, na.rm=TRUE)
      f$herbHeight <- mean(zi$Height, na.rm=TRUE)
    } else {
      f$herbCover <- NA
      f$herbHeight <- NA
    }
    class(f) <- c("forest", "list")
    forestlist[[i]] <- f
  }
  if(verbose) cat("done.\n")
  return(forestlist)
}
