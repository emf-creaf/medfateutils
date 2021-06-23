#' Initializes Species Parameter Table
#'
#'  Creates an empty species parameter table for medfate, populating taxonomic information if desired.
#'
#' @param sp_codes Vector of IFN species codes
#' @param sp_names Vector of IFN species names
#' @param group_codes List of species group codes (strings where species of the same group are separated by "/", e.g. "code1/code2/code3")
#' @param group_names Vector of group names of the same length as \code{group_codes}
#' @param fill_taxonomic Boolean flag to indicate that taxonomic information should be filled (retrieved from GBIF using package 'taxize')
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
#'   sp_codes = c("76","84")
#'   sp_names = c("Salvia rosmarinifolia", "Pinus contorta")
#'   initSpParams(sp_codes, sp_names)
#' }
initSpParams<-function(sp_codes, sp_names,
                       group_codes = NULL, group_names = NULL,
                       fill_taxonomic = TRUE,
                       verbose = FALSE) {

  sp_codes = as.character(sp_codes)
  sp_names = as.character(sp_names)

  if(length(sp_codes) !=length(unique(sp_codes))) stop("Species codes must be unique!")

  if(!is.null(group_codes)) {
    in_groups = unlist(strsplit(group_codes, split="/"))
    sel = !(sp_codes %in% in_groups)
    if(verbose) cat(paste0(sum(!sel), " codes found in groups.\n"))
    sp_codes = sp_codes[sel]
    sp_names = sp_names[sel]
  }
  SpParams <- data.frame(Name = c(sp_names, group_names),
                         IFNcodes = c(sp_codes, group_codes),
                         Genus = NA,
                         Family = NA,
                         Order = NA,
                         Group = NA)
  SpParams<- SpParams[order(SpParams$Name),]
  row.names(SpParams)<-NULL
  SpParams$SpIndex = 0:(nrow(SpParams)-1)
  data("SpParamsMED",package = "medfate", envir = environment())
  cols = names(SpParamsMED)
  for(cn in cols) {
    if(!(cn %in% names(SpParams))) {
      SpParams[[cn]] = NA
    }
  }
  if(fill_taxonomic) {
    if(verbose) cat(paste0("Retrieving taxonomic data: \n"))
    if(verbose) pb = txtProgressBar(0, nrow(SpParams), style=3)
    for(i in 1:nrow(SpParams)) {
      if(verbose) setTxtProgressBar(pb, i)
      s = strsplit(SpParams$Name[i]," ")[[1]]
      SpParams$Genus[i] = s[1] # Genus always first word
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
        if(class(cdf)=="data.frame") {
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

  if(length(SpParams$Name) !=length(unique(SpParams$Name))) warning("Final species/group names should be unique!")
  return(SpParams)
}
