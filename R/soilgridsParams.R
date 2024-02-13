#' SoilGrids soil description fetcher
#'
#' \code{soilgridsParams} takes a vector of depths and returns a list of soil characteristics ready to use with \code{\link{soil}} function.
#'
#' @param x An object of class (or subclass) \code{sf} or \code{sfc} with a valid CRS definition.
#' @param widths A numeric vector indicating the desired layer widths, in \emph{mm}. If \code{NULL} the default soil grids layer definition is returned.
#' @param verbose A logical flag to include a progress bar while processing the output of the query to the SoilGrids REST API.
#'
#' @details This function connects with the SoilGrids REST API (https://rest.isric.org)
#' to retrieve the soil physical and chemical characteristics for a site (Hengl \emph{et al}. 2007), selected
#' by its coordinates. Also, in case the depths are not the default ones in the SoilGrids API, the function uses
#' averages the values of soil grid layers depending on the overlap between soil layer definitions.  Input coordinates
#' are transformed to longitude/latitude within the function.
#'
#' @return If only one point is supplied, a data frame containing the soil characteristics ready to use with the
#' \code{\link{soil}} function. If more than one point is supplied, the function returns a list with as many elements
#' as points, each one containing the mentioned list.
#'
#' @author \enc{Víctor}{Victor} Granda, EMF-CREAF
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @encoding UTF-8
#' @export
#'
#' @references
#' Hengl T, Mendes de Jesus J, Heuvelink GBM, Ruiperez Gonzalez M, Kilibarda M, \enc{Blagotić}{Blagotic} A, et al. (2017) SoilGrids250m: Global gridded soil information based on machine learning. PLoS ONE 12(2): e0169748. doi:10.1371/journal.pone.0169748.
#'
#' @seealso  \code{\link[medfate]{soil}}, \code{\link[medfate]{defaultSoilParams}}
#'
#' @examples
#'  \dontrun{
#'   library(sf)
#'   coords_sf <- st_sfc(st_point(c(-5.6333, 42.6667)), crs = 4326)
#'   foo <- soilgridsParams(coords_sf, widths = c(300, 700, 1000))
#'   foo_soil <- soil(foo)
#'   foo_soil
#'  }
#'
soilgridsParams <- function(x, widths = c(300, 700, 1000, 2000), verbose = FALSE) {
  if((!inherits(x, "sfc")) && (!inherits(x, "sf")))  stop("Object 'x' has to be of class 'sf' or 'sfc'")
  x_lonlat <- sf::st_transform(sf::st_geometry(x), 4326)
  coords <- sf::st_coordinates(x_lonlat)

  npoints = nrow(coords)

  url.base = "https://rest.isric.org/soilgrids/v2.0/properties/query?"


  props_str = "property=bdod&property=cfvo&property=clay&property=ocd&property=ocs&property=sand&property=silt&property=soc"
  depths_str = "depth=0-5cm&depth=0-30cm&depth=5-15cm&depth=15-30cm&depth=30-60cm&depth=60-100cm&depth=100-200cm"

  if(verbose) {
    cat(paste0("Querying ", npoints," points to rest.isric.org:\n"))
    cli::cli_progress_bar(name = "Points", total = npoints)
  }
  reslist = vector("list", npoints)
  for(i in 1:npoints) {
    if(verbose) cli::cli_progress_update()
    tryCatch( {
      resSG = data.frame(matrix(nrow = 6, ncol = 6))
      names(resSG) = c("widths", "clay", "sand", "om", "bd", "rfc")
      resSG$widths = c(50,100,150,300,400,1000)
      coord_str = paste0("lon=",coords[i,1],"&lat=", coords[i,2])
      dest = paste(coord_str, props_str, depths_str,"value=mean",sep="&")
      url1 = paste0(url.base, dest)
      path1 <- httr::GET(url1, httr::add_headers("accept"= "application/json"))
      ans.text <- httr::content(path1, as = "text", encoding = "utf-8")
      ans <- jsonlite::fromJSON(ans.text)
      propNames = ans$properties$layers$name
      d_factors = ans$properties$layers$unit_measure$d_factor
      for(j in 1:length(propNames)) {
        if(propNames[j]=="clay") {
          resSG$clay = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
        } else if(propNames[j]=="sand") {
          resSG$sand = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
        } else if(propNames[j]=="bdod") {
          resSG$bd = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
        } else if(propNames[j]=="soc") {
          resSG$om = ans$properties$layers$depths[[j]]$values$mean/(d_factors[j]*10)
        } else if(propNames[j]=="cfvo") {
          resSG$rfc = ans$properties$layers$depths[[j]]$values$mean/d_factors[j]
        }
      }
      if(!is.null(widths)) {
        reslist[[i]] = redefineSoilLayers(resSG, widths)
      } else {
        reslist[[i]] = resSG
      }
    }, error  = function(cond) {
      message(paste("Problems retrieving point",i,": ", cond,"\n"))
    })
  }
  if(verbose) cli::cli_progress_done()
  if(length(reslist)==1) reslist = reslist[[1]]
  return(reslist)
}
