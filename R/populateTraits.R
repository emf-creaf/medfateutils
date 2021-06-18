#' Populate species parameters from trait data
#'
#' Generic function to fill medfate species trait parameters from a data table of species traits
#'
#' @param SpParams A data frame of medfate species parameters to be populated.
#' @param trait_table A data frame with species traits in columns and row.names corresponding to taxonomic entities.
#' @param trait_mapping A named string vector specifying which trait data column should used to populate each medfate param. Elements are data base columns and names are medfate params.
#' @param transformation_functions A named list of scalar functions for traits needing transformation of units or scaling. Names are medfate params.
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
                         transformation_functions = NULL,
                         erase_previous = FALSE) {

  trait_params <- names(trait_mapping)
  if(sum(names(trait_table) %in% trait_mapping)!=length(trait_params)) stop("Trait data table should contain all variables to be mapped!")
  trait_rownames <- row.names(trait_table)

  if(erase_previous) SpParams[,trait_params] = NA

  for(j in 1:length(trait_mapping)) {
    trait <- trait_mapping[[j]]
    param <- trait_params[j]
    nmis <- 0
    nsp <- 0
    ngen <- 0
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
      if(nm %in% trait_rownames) { # Species level
        trait_row <- which(trait_rownames==nm)
        nsp <- nsp + 1
      } else if(genus %in% trait_rownames) { #Genus level
        trait_row <- which(trait_rownames==genus)
        ngen <- ngen + 1
      } else if(family %in% trait_rownames) { #Family level
        trait_row <- which(trait_rownames==family)
        nfam <- nfam + 1
      } else if(order %in% trait_rownames) { #Order level
        trait_row <- which(trait_rownames==order)
        norder <- norder + 1
      } else if(group %in% trait_rownames) { #Group level
        trait_row <- which(trait_rownames==group)
        ngroup <- ngroup + 1
      }
      if(!is.na(trait_row)) {
        SpParams[i, param] <- trait_table[trait_row, trait]
        if(param %in% names(transformation_functions)) {
          f <- transformation_functions[[param]]
          p <- list(SpParams[i,param])
          SpParams[i, param] <- do.call(f, p)
        }
      } else {
        nmis <- nmis +1
      }
    }
    message(paste0("Mapping [",trait_mapping[j]," -> ",trait_params[j], "] species: ", nsp, " genus: ", ngen," family: ",nfam, " order:", norder," group: ", ngroup))
    if(nmis>0) message(paste0(nmis,
                              " missing trait values (out of ",
                              nrow(SpParams),
                              " tree species) after populating with input data.\n"))
  }
  return(SpParams)
}
