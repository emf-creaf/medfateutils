#' Populate species parameters from trait data
#'
#' Generic function to fill medfate species trait parameters from a data table of species traits
#'
#' @param SpParams A data frame of medfate species parameters to be populated.
#' @param trait_table A data frame with functional traits in columns and plants in rows.
#' @param trait_mapping A named string vector specifying which trait data column should be used
#'                      to populate each medfate param. Elements are data base columns and names are medfate params.
#' @param taxon_column A string identifying the column in \code{trait_table} that identifies taxon names).
#' @param genus_column A string identifying the column in \code{trait_table} that identifies genus. If \code{NULL} then genus are taken from first word of taxon names.
#' @param character_traits Boolean flag to treat traits as character-valued
#' @param summary_function A function to summarize multiple values for the same taxonomic entity. By default,
#'                         median values are taken for quantitative traits and the most frequent value is taken for qualitative traits.
#' @param summary_params A list of summary function params (by default \code{na.rm=TRUE}).
#' @param scalar_functions A named list of scalar functions for traits needing transformation of units or scaling. Names are medfate params.
#' @param replace_previous A boolean flag to indicate that non-missing previous values should be replaced with new data
#' @param erase_previous A boolean flag to indicate that all previous values should be set to NA before populating with new data
#' @param verbose A boolean flag to indicate extra console output
#'
#' @return A modified data frame of medfate species parameters
#' @export
#'
#' @details
#' If the target taxon is a species, values are taken from those rows in trait_table where species names match.
#' If the target taxon is a genus, then values are taken from those rows where genus is the same.
#'
#'
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' @seealso \code{\link{initSpParams}}
populateTraits<-function(SpParams,
                         trait_table,
                         trait_mapping,
                         taxon_column,
                         genus_column = NULL,
                         character_traits = FALSE,
                         summary_function = "median",
                         summary_params = list(na.rm=TRUE),
                         scalar_functions = NULL,
                         replace_previous = FALSE,
                         erase_previous = FALSE,
                         verbose = FALSE) {

  trait_table <- as.data.frame(trait_table)
  trait_params <- names(trait_mapping)
  if(sum(names(trait_table) %in% trait_mapping)!=length(trait_params)) stop("Trait data table should contain all variables to be mapped!")
  if(!(taxon_column %in% names(trait_table))) stop(paste0("Column '",taxon_column, "' not found among columns in trait data table!"))
  trait_taxa <- trait_table[[taxon_column]]
  if(!is.null(genus_column)) {
    trait_genus <- trait_table[[genus_column]]
  } else {
    trait_genus <- unlist(lapply(strsplit(trait_taxa," "), function(x) x[1]))
  }

  if(erase_previous) SpParams[,trait_params] = NA

  for(j in 1:length(trait_mapping)) {
    trait <- trait_mapping[[j]]
    param <- trait_params[j]

    nsp <- 0
    ngen <- 0

    for(i in 1:nrow(SpParams)) {
      #Should we replace current value (only if is NA or we are to replace values)
      can_replace <- (is.na(SpParams[i,param]) || replace_previous)
      if(can_replace) {
        nm <-  SpParams$Name[i]
        species <-  SpParams$Species[i]
        genus <-  SpParams$Genus[i]
        family <-  SpParams$Family[i]
        order <-  SpParams$Order[i]
        group <-  SpParams$Group[i]
        is_species <- !is.na(species)
        is_genus <- (!is_species) && (!is.na(genus))

        ## Find taxon name
        trait_row <- NA
        if(is_species) {
          trait_row <- which(trait_taxa==nm)
          if(sum(!is.na(trait_table[trait_row, trait]))>0) {
            nsp <- nsp + 1
          }
        } else if(is_genus) {
          trait_row <- which(trait_genus==genus)
          if(sum(!is.na(trait_table[trait_row, trait]))>0) {
            ngen <- ngen + 1
          }
        }
        if(sum(!is.na(trait_row))>0) {
          if(verbose) message(paste0("Parameter: ",param ," Taxon: ", nm , " Rows: ",paste0(trait_row, collapse=","),"\n"))
          if(!character_traits) {
            l = c(list("x"=as.numeric(trait_table[trait_row, trait])), summary_params)
            val <- do.call(summary_function, l)
            if(!is.na(val)) {
              if(param %in% names(scalar_functions)) {
                SpParams[i, param] <- do.call(scalar_functions[[param]], list(val))
              } else {
                SpParams[i, param] <- val
              }
            }
          } else { # Keep most frequent
            vals <- trait_table[trait_row, trait]
            vals <- vals[!is.na(vals)]
            tfr<-table(vals)
            if(length(tfr)>0) {
              tfr<-tfr[order(tfr, decreasing=TRUE)]
              SpParams[i, param] <- names(tfr)[1]
            }
          }
        }
      }
    }
    if(verbose) {
      message(paste0("Mapping [",trait_mapping[j]," -> ",trait_params[j],
                     "] species: ", nsp, " genus: ", ngen))
      nmis <- sum(is.na(SpParams[[param]]))
      if(nmis>0) message(paste0(nmis,
                                " missing trait values (",
                                round(100*nmis/nrow(SpParams),1),
                                " %) after populating with input data.\n"))
    }
  }
  return(SpParams)
}
