#' Populate species parameters from trait data
#'
#' Generic function to fill medfate species trait parameters from a data table of species traits
#'
#' @param SpParams A data frame of medfate species parameters to be populated.
#' @param trait_table A data frame with species traits in columns and row.names corresponding to taxonomic entities.
#' @param trait_mapping A named string vector specifying which trait data column should used to populate each medfate param. Elements are data base columns and names are medfate params.
#' @param taxon_column A string identifying the column in \code{trait_table} that identifies taxa (normally species). If \code{taxon_column = NULL} then taxon names are taken from row.names.
#' @param scalar_functions A named list of scalar functions for traits needing transformation of units or scaling. Names are medfate params.
#' @param erase_previous A boolean flag to indicate that values should be set to NA before populating with data
#'
#' @return A modified data frame of medfate species parameters
#' @export
#'
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' @seealso \code{\link{initSpParams}}
populateTraits<-function(SpParams,
                         trait_table, trait_mapping,
                         taxon_column = NULL,
                         scalar_functions = NULL,
                         erase_previous = FALSE) {

  trait_params <- names(trait_mapping)
  if(sum(names(trait_table) %in% trait_mapping)!=length(trait_params)) stop("Trait data table should contain all variables to be mapped!")
  if(is.null(taxon_column)) {
    trait_taxa <- row.names(trait_table)
  } else {
    if(!(taxon_column %in% names(trait_table))) stop(paste0("Column '",taxon_column, "' not found among columns in trait data table!"))
    trait_taxa <- trait_table[[taxon_column]]
  }
  df <- data.frame(rn = trait_taxa) %>%
    tidyr::separate("rn", c("Genus", "Species"), sep=" ")


  if(erase_previous) SpParams[,trait_params] = NA

  for(j in 1:length(trait_mapping)) {
    trait <- trait_mapping[[j]]
    param <- trait_params[j]
    nsp <- 0
    ngen <- 0
    ncon <- 0
    nfam <- 0
    norder <- 0
    ngroup <- 0
    for(i in 1:nrow(SpParams)) {
      nm = SpParams$Name[i]
      genus = SpParams$Genus[i]
      family = SpParams$Family[i]
      order = SpParams$Order[i]
      group = SpParams$Group[i]
      ## Find species
      trait_row <- NA
      found <- FALSE
      if(nm %in% trait_taxa) { # Species level
        trait_row <- which(trait_taxa==nm)
        if(sum(!is.na(trait_table[trait_row, trait]))>0) {
          found <- TRUE
          nsp <- nsp + 1
        }
      }
      if((!found) && (genus %in% trait_taxa)) { #Genus level
        trait_row <- which(trait_taxa==genus)
        if(sum(!is.na(trait_table[trait_row, trait]))>0) {
          found <- TRUE
          ngen <- ngen + 1
        }
      }
      if((!found) && (genus %in% df$Genus)) { # Try conspecifics
        sel = (df$Genus==genus)
        sel[is.na(sel)] = FALSE
        trait_row <- which(sel)
        if(sum(!is.na(trait_table[trait_row, trait]))>0) {
          found <- TRUE
          ncon <- ncon + 1
        }
      }

      if((!found) && (family %in% trait_taxa)) { #Family level
        trait_row <- which(trait_taxa==family)
        if(sum(!is.na(trait_table[trait_row, trait]))>0) {
          found <- TRUE
          nfam <- nfam + 1
        }
      }
      if((!found) && (order %in% trait_taxa)) { #Order level
        trait_row <- which(trait_taxa==order)
        if(sum(!is.na(trait_table[trait_row, trait]))>0) {
          found <- TRUE
          norder <- norder + 1
        }
      }
      if((!found) && (group %in% trait_taxa)) { #Group level
        trait_row <- which(trait_taxa==group)
        if(sum(!is.na(trait_table[trait_row, trait]))>0) {
          found <- TRUE
          ngroup <- ngroup + 1
        }
      }
      if(sum(!is.na(trait_row))>0) {
        val <- mean(trait_table[trait_row, trait], na.rm=TRUE)
        if(!is.na(val)) {
          if(param %in% names(scalar_functions)) {
            SpParams[i, param] <- do.call(scalar_functions[[param]], list(val))
          } else {
            SpParams[i, param] <- val
          }
        }
      }
    }
    message(paste0("Mapping [",trait_mapping[j]," -> ",trait_params[j],
                   "] species: ", nsp, " genus: ", ngen, " conspecific: ", ncon,
                   " family: ",nfam, " order:", norder," group: ", ngroup))
    nmis <- sum(is.na(SpParams[[param]]))
    if(nmis>0) message(paste0(nmis,
                              " missing trait values (",
                              round(100*nmis/nrow(SpParams),1),
                              " %) after populating with input data.\n"))
  }
  return(SpParams)
}
