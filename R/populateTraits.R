#' Populate species parameters from trait data
#'
#' Generic function to fill medfate species trait parameters from a data table of species traits
#'
#' @param SpParams A data frame of medfate species parameters to be populated.
#' @param trait_table A data frame with species traits in columns and row.names corresponding to taxonomic entities.
#' @param trait_mapping A named string vector specifying which trait data column should used to populate each medfate param. Elements are data base columns and names are medfate params.
#' @param character_traits Boolean flag to treat traits as character-valued
#' @param taxon_column A string identifying the column in \code{trait_table} that identifies taxa (normally species). If \code{taxon_column = NULL} then taxon names are taken from row.names.
#' @param summary_function A function to summarize multiple values for the same taxonomic entity. By default, arithmetic averages are used, excluding missing values.
#' @param summary_params A list of summary function params (by default \code{na.rm=TRUE}).
#' @param scalar_functions A named list of scalar functions for traits needing transformation of units or scaling. Names are medfate params.
#' @param replace_previous A boolean flag to indicate that non-missing previous values should be replaced with new data
#' @param erase_previous A boolean flag to indicate that all previous values should be set to NA before populating with new data
#' @param verbose A boolean flag to indicate extra console output
#'
#' @return A modified data frame of medfate species parameters
#' @export
#'
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' @seealso \code{\link{initSpParams}}
populateTraits<-function(SpParams,
                         trait_table, trait_mapping,
                         character_traits = FALSE,
                         taxon_column = NULL,
                         summary_function = "mean",
                         summary_params = list(na.rm=TRUE),
                         scalar_functions = NULL,
                         replace_previous = FALSE,
                         erase_previous = FALSE,
                         verbose = FALSE) {

  trait_params <- names(trait_mapping)
  if(sum(names(trait_table) %in% trait_mapping)!=length(trait_params)) stop("Trait data table should contain all variables to be mapped!")
  if(is.null(taxon_column)) {
    trait_taxa <- row.names(trait_table)
  } else {
    if(!(taxon_column %in% names(trait_table))) stop(paste0("Column '",taxon_column, "' not found among columns in trait data table!"))
    trait_taxa <- trait_table[[taxon_column]]
  }

  s_taxa <- strsplit(trait_taxa," ")
  df<-data.frame(Genus = unlist(lapply(s_taxa, function(x) x[1])),
                 Species = unlist(lapply(s_taxa, function(x) {
                   if(length(x)>1) x[2]
                   else ""
                  }))
  )
  trait_taxa <-paste(df$Genus, df$Species)

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
      #Should we replace current value (only if is NA or we are to replace values)
      can_replace <- (is.na(SpParams[i,param]) || replace_previous)
      if(can_replace) {
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
          if(verbose) message(paste0("Parameter: ",param ," Taxon:", nm , " Rows: ",paste0(trait_row, collapse=","),"\n"))
          if(!character_traits) {
            l = c(list("x"=as.numeric(trait_table[trait_row, trait])), summary_params)
            val <- do.call(summary_function, l)
              # val <- mean(trait_table[trait_row, trait], na.rm=TRUE)
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
    message(paste0("Mapping [",trait_mapping[j]," -> ",trait_params[j],
                   "] species: ", nsp, " conspecific: ", ncon,
                   " genus: ", ngen, " family: ",nfam, " order: ", norder," group: ", ngroup))
    nmis <- sum(is.na(SpParams[[param]]))
    if(nmis>0) message(paste0(nmis,
                              " missing trait values (",
                              round(100*nmis/nrow(SpParams),1),
                              " %) after populating with input data.\n"))
  }
  return(SpParams)
}
