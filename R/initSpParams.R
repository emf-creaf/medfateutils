#' Initializes Species Parameter Table
#'
#'  Creates an empty plant parameter table for medfate, populating taxonomic information if desired.
#'
#' @param sp_names Vector of plant names, either taxon names or arbitrary species group names
#' @param accepted_names Vector of accepted taxon names of the same length of `sp_names`
#' @param SpParamsDefinition Data frame of species parameter definition from package medfate
#' @param fill_taxonomic Boolean flag to indicate that taxonomic information should be filled (retrieved from GBIF using package 'taxize')
#' @param sort Boolean flag to force sorting in ascending order by `Name`
#' @param verbose A boolean flag to indicate extra console output
#'
#' @details Taxonomic information is retrieved using functions in package taxize and GBIF as data source.
#'
#' @return A data frame with empty species parameter values suitable for medfate simulations
#'
#' @export
#'
#' @encoding UTF-8
#' @author  Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#'
#' @seealso \code{\link[medfate]{SpParamsMED}}
#'
#' @examples
#' \dontrun{
#' # Load species parameter definition from medfate
#' data(SpParamsDefinition)
#'
#' # Simple example with two species
#' sp_names = c("Salvia rosmarinifolia", "Pinus contorta")
#' initSpParams(sp_names, SpParamsDefinition)
#'
#' # Simple example with three species using synonyms and subspecies
#' sp_names = c("Rosmarinus officinalis", "Pinus contorta", "Quercus ilex subsp. ilex")
#' accepted_names = c("Salvia rosmarinifolia", "Pinus contorta", "Quercus ilex subsp. ilex")
#' initSpParams(sp_names, SpParamsDefinition, accepted_names)
#'
#' # Initialisation with IFN species mapping
#' data(IFN_species_mapping)
#' initSpParams(IFN_species_mapping$Name, SpParamsDefinition, verbose = TRUE)
#'
#' }
initSpParams<-function(sp_names, SpParamsDefinition,
                       accepted_names = NULL,
                       fill_taxonomic = TRUE,
                       sort = TRUE,
                       verbose = FALSE) {

  if(length(sp_names)==0) stop("Please, supply at least one species name")
  SpParams <- data.frame(Name = as.character(sp_names))
  if(length(SpParams$Name) !=length(unique(SpParams$Name))) warning("Final species/group names should be unique!")
  for(cn in SpParamsDefinition$ParameterName) {
    if(!(cn %in% names(SpParams))) {
      SpParams[[cn]] = NA
    }
  }
  if(!is.null(accepted_names)) {
    if(length(accepted_names) != length(sp_names)) stop("The vector of accepted names has to be of the same length as the vector of species names")
    SpParams$AcceptedName <- accepted_names
  } else {
    SpParams$AcceptedName <- SpParams$Name
  }
  if(fill_taxonomic) {
    if(verbose) cat(paste0("Retrieving taxonomic data: \n"))
    if(verbose) pb = txtProgressBar(0, nrow(SpParams), style=3)
    for(i in 1:nrow(SpParams)) {
      if(verbose) setTxtProgressBar(pb, i)
      s = strsplit(SpParams$AcceptedName[i]," ")[[1]]
      SpParams$Genus[i] = s[1] # Genus always first word
      if(length(s)>1) SpParams$Species[i] = paste0(s[1], " ", s[2])
      id_df<-taxize::get_gbifid_(s[1], messages = FALSE)[[1]]
      gbif_id<-numeric(0)
      if(nrow(id_df)>0) {
        sel1 = id_df$kingdom=="Plantae" & id_df$matchtype=="EXACT"
        if(sum(sel1)>0) {
          gbif_id<-id_df$usagekey[sel1]
        }
      }
      if(length(gbif_id)>0) {
        cdf<-taxize::classification(gbif_id[1], db="gbif")[[1]]
        if(inherits(cdf,"data.frame")) {
          if("family" %in% cdf$rank) SpParams$Family[i] = cdf$name[cdf$rank=="family"]
          if("order" %in% cdf$rank) {
            SpParams$Order[i] = cdf$name[cdf$rank=="order"]
            if(SpParams$Order[i] %in% c("Ginkgoales", "Pinales", "Welwitschiales", "Ephedrales")) {
              SpParams$Group[i] = "Gymnosperm"
            } else {
              SpParams$Group[i] = "Angiosperm"
            }
          }
        }
      }
    }
  }

  if(sort) SpParams<- SpParams[order(SpParams$Name),, drop = FALSE]
  row.names(SpParams) <- NULL
  SpParams$SpIndex <- 0:(nrow(SpParams)-1)
  return(SpParams)
}
