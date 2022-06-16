#' Map forest plot data
#'
#' Mapping functions to facilitate building forest objects from forest plot data
#'
#' @param x A data frame with tree records in rows and attributes in columns
#' @param y A data frame with shrub records in rows and attributes in columns
#' @param mapping A named character vector to specify mappings of columns in \code{x} or \code{y} into attributes of \code{treeData} and \code{shrubData} data frames. Accepted names (and the corresponding specifications for the columns in \code{x} and \code{y}) are:
#' \itemize{
#' \item{"Species": Species code (should follow codes in \code{SpParams}).}
#' \item{"Species.name": Species name. In this case, the species code will be drawn by matching names with species names in \code{SpParams}.}
#' \item{"N": Tree density (in ind./ha).}
#' \item{"Cover": Shrub cover (in \%).}
#' \item{"D1": Shrub largest crown diameter (in cm).}
#' \item{"D2": Shrub crown diameter orthogonal to the largest one (in cm).}
#' \item{"plot.size": Plot size (in m2) to which each plot record refers to.}
#' \item{"DBH": Diameter at breast height (in cm).}
#' \item{"Height": Tree or shrub height (in cm).}
#' \item{"Z50": Depth (in mm) corresponding to 50\% of fine roots.}
#' \item{"Z95": Depth (in mm) corresponding to 95\% of fine roots.}
#' }
#' @param SpParams A data frame with species parameters (see \code{\link{SpParamsMED}}) from which valid species names are drawn.
#' @param plot.size The size of plot sampled area (in m2). Alternatively, 'plot.size'
#' can be a column in \code{x} or \code{y} and specified in \code{mapping} to indicate that trees/shrubs
#' have been measured in different subplots and, therefore, they represent different
#' densities per hectare or cover values.
#'
#' @return Functions \code{forest_mapTreeTable} and \code{forest_mapShrubTable} return a data frame with the structure of \code{treeData} and \code{shrubData} from \code{\link{forest}} objects. Function \code{forest_mapWoodyTable} returns directly a \code{\link{forest}} object.
#'
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @seealso \code{\link[medfate]{forest}}
#'
#' @name forest_mapWoodyTables
#' @export
#'
#' @examples
#'
#' # Load species parameters
#' data(SpParamsMED)
#'
#' # (1) Mapping tree data
#' # Load Poblet tree data
#' data(poblet_trees)
#'
#' # Subset control plot
#' x <- subset(poblet_trees, Plot.Code=="POBL_CTL")
#'
#' # Estimate sampled area (15-m radius plot)
#' sampled_area <- pi*15^2
#'
#' # Define mapping
#' mapping_x <- c("Species.name" = "Species", "DBH" = "Diameter.cm")
#'
#' # Map tree data for plot 'POBL_CTL'
#' forest_mapTreeTable(x,
#'                     mapping = mapping_x, SpParams = SpParamsMED,
#'                     plot.size = sampled_area)
#'
#' # (2) Mapping shrub individual data
#' #
#' # Create the individual shrub data frame
#' species <- c("Erica arborea","Cistus albidus", "Erica arborea", "Cistus albidus", "Cistus albidus")
#' H <- c(200,50,100,40,30)
#' D1 <- c(140,40,100, 35,30)
#' D2 <- D1
#' y <- data.frame(species, H, D1, D2)
#'
#' # Define mapping (D1 and D2 map to variables with the same name)
#' mapping_y <- c("Species.name"= "species", "Height" ="H", "D1", "D2")
#'
#' # Map individual shrub data to cover data (here each individual becomes a cohort)
#' # assuming that the sampled area was 4 m2
#' forest_mapShrubTable(y,
#'                      mapping = mapping_y, SpParams = SpParamsMED,
#'                      plot.size = 4)
forest_mapTreeTable<-function(x, mapping, SpParams, plot.size = NULL) {
  n = nrow(x)
  treeData = data.frame(
    Species = rep(NA, n),
    N = rep(NA, n),
    Height = rep(NA, n),
    DBH = rep(NA, n),
    Z50 = rep(NA, n),
    Z95 = rep(NA, n))

  # Fill any empty mapping names with correspoinding mapping values
  names(mapping)[names(mapping)==""] = mapping[names(mapping)==""]

  if("Height" %in% names(mapping)) {
    treeData$Height = x[[mapping[["Height"]]]]
  }
  if("DBH" %in% names(mapping)) {
    treeData$DBH = x[[mapping[["DBH"]]]]
  }
  if("N" %in% names(mapping)) {
    treeData$N = x[[mapping[["N"]]]]
  } else {
    treeData$N = 1
  }
  if("Z50" %in% names(mapping)) {
    treeData$Z50 = x[[mapping[["Z50"]]]]
  }
  if("Z95" %in% names(mapping)) {
    treeData$Z95 = x[[mapping[["Z95"]]]]
  }
  if("Species" %in% names(mapping)) {
    treeData$Species = x[[mapping[["Species"]]]]
  }
  if("plot.size" %in% names(mapping)) {
    plot.size = x[[mapping[["plot.size"]]]]
  }
  if(!is.null(plot.size)) {
    treeData$N = treeData$N*(10000/plot.size)
  }
  if("Species.name" %in% names(mapping)) {
    Species.name = x[[mapping[["Species.name"]]]]
    for(i in 1:n) {
      indices = which(SpParams$Name==Species.name[i])
      if(length(indices)>0) {
        treeData$Species[i] = SpParams$SpIndex[indices]
      }
    }
  }
  return(treeData)
}

#' @rdname forest_mapWoodyTables
forest_mapShrubTable<-function(y, mapping, SpParams, plot.size = NULL) {
  n = nrow(y)
  shrubData = data.frame(
    Species = rep(NA, n),
    Height = rep(NA, n),
    Cover = rep(NA, n),
    Z50 = rep(NA, n),
    Z95 = rep(NA, n))

  # Fill any empty mapping names with correspoinding mapping values
  names(mapping)[names(mapping)==""] = mapping[names(mapping)==""]

  if("Height" %in% names(mapping)) {
    shrubData$Height = y[[mapping[["Height"]]]]
  }
  if("plot.size" %in% names(mapping)) {
    plot.size = y[[mapping[["plot.size"]]]]
  }
  if("Cover" %in% names(mapping)) {
    shrubData$Cover = y[[mapping[["Cover"]]]]
  } else if("D1" %in% names(mapping)) {
    if(is.null(plot.size)) stop("You must supply plot size when mapping crown diameter data.")
    D1 = y[[mapping[["D1"]]]] # D1 in cm
    D2 = D1
    if("D2" %in% names(mapping)) {
      D2 = y[[mapping[["D2"]]]] # D2 in cm
    }
    area = pi*((D1/200)*(D2/200)) #area in m2
    shrubData$Cover = pmin(100,100*area/plot.size)
  }
  if("Z50" %in% names(mapping)) {
    shrubData$Z50 = y[[mapping[["Z50"]]]]
  }
  if("Z95" %in% names(mapping)) {
    shrubData$Z95 = y[[mapping[["Z95"]]]]
  }
  if("Species" %in% names(mapping)) {
    shrubData$Species = y[[mapping[["Species"]]]]
  }
  if("Species.name" %in% names(mapping)) {
    Species.name = y[[mapping[["Species.name"]]]]
    for(i in 1:n) {
      indices = which(SpParams$Name==Species.name[i])
      if(length(indices)>0) {
        shrubData$Species[i] = SpParams$SpIndex[indices]
      }
    }
  }
  return(shrubData)
}

#' @rdname forest_mapWoodyTables
forest_mapWoodyTables<-function(x, y, mapping, SpParams, plot.size=NULL) {
  f = emptyforest()
  f$treeData = forest_mapTreeTable(x, mapping = mapping,  SpParams=SpParams, plot.size = plot.size)
  f$shrubData = forest_mapShrubTable(y, mapping = mapping,  SpParams=SpParams, plot.size = plot.size)
  return(f)
}
