#' Populate species parameters from inventory data
#'
#' Functions to populate species parameter values from forest inventory source data
#'
#' @name populateSpParamsFromInventory
#'
#' @param SpParams A data frame of medfate species parameters to be populated
#' @param tree_names String vector of translated tree species names
#' @param shrub_names String vector of translated shrub species names
#' @param erase_previous A boolean flag to indicate that values should be set to NA before populating with data
#' @param fill_fromGenus A boolean flag to indicate that genus adscription of species should be used to fill missing values
#'
#' @return A modified data frame of medfate species parameters
#' @export
#'
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' @seealso \code{\link{initSpParams}}
#'
populateGrowthForm<-function(SpParams,
                             tree_names, shrub_names = character(0),
                             erase_previous = FALSE,
                             fill_fromGenus = FALSE) {
  tree_names <- as.character(unique(tree_names[!is.na(tree_names)]))
  shrub_names <- as.character(unique(shrub_names[!is.na(shrub_names)]))

  tree_inSpParams <- tree_names %in% SpParams$Name
  shrub_inSpParams <- shrub_names %in% SpParams$Name

  if(erase_previous) SpParams$GrowthForm<-NA

  is_tree <- SpParams$Name %in% tree_names
  is_shrub <- SpParams$Name %in% shrub_names
  SpParams$GrowthForm[is_tree && !is_shrub] <- "Tree"
  SpParams$GrowthForm[!is_tree && is_shrub] <- "Shrub"
  SpParams$GrowthForm[is_tree && is_shrub] <- "Tree/Shrub"

  ntrees <- sum(is_tree && !is_shrub)
  nshrubs <- sum(!is_tree && is_shrub)
  ntreeshrubs <- sum(is_tree && is_shrub)
  notfound <- sum(!is_tree && !is_shrub)

  message(paste0(" Tree: ", ntrees, " shrub: ", nshrubs, " tree/shrub: ", ntreeshrubs, " not found: ", notfound))
  if(sum(!tree_inSpParams)>0) message(paste0(" Tree input names not in SpParams: ", paste0(tree_names[!tree_inSpParams], collapse=",")))
  if(sum(!shrub_inSpParams)>0) message(paste0(" Shrub input names not in SpParams: ", paste0(shrub_names[!shrub_inSpParams], collapse=",")))
  if(fill_fromGenus) {
    for(i in 1:nrow(SpParams)) {
      if(is.na(SpParams$GrowthForm[i])) { # Finds species within the same genus and copy growth form if all equal
        genGF <- SpParams$GrowthForm[SpParams$Genus==SpParams$Genus[i]]
        genGF <- genGF[!is.na(genGF)]
        if(length(unique(genGF))==1) {
          SpParams$GrowthForm[i] <- genGF[1]
          message(paste0("Species '", SpParams$Name[i],"' assigned growth form '",
                         SpParams$GrowthForm[i],"'"))
        }
      }
    }
  }
  return(SpParams)
}

#' @rdname populateSpParamsFromInventory
#'
#' @param species_names A string vector of translated species names (see \code{\link{translateSpeciesCodes}})
#' @param height_values A numeric vector of plant heights (in cm)
#' @param quantile_Hmed Quantile for Hmed
#' @param quantile_Hmax Quantile for Hmax
#'
#' @export
populateHeightParams<-function(SpParams,
                               species_names,
                               height_values,
                               quantile_Hmed = 0.5,
                               quantile_Hmax = 0.99,
                               erase_previous = FALSE) {
  if(length(species_codes)!=length(height_values)) stop("Vectors for codes and values should have the same length!")

  height_values = as.numeric(height_values)
  species_codes = as.character(species_codes)

  toRemove = is.na(height_values) | is.na(species_codes)
  if(sum(toRemove)>0) {
    height_values = height_values[!toRemove]
    species_codes = species_codes[!toRemove]
    message(paste0(sum(toRemove), " species/height missing values removed from input."))
  }
  toRemove = (height_values==0)
  if(sum(toRemove)>0) {
    height_values = height_values[!toRemove]
    species_codes = species_codes[!toRemove]
    message(paste0(sum(toRemove), " zero height values removed from input."))
  }

  if(erase_previous) {
    SpParams$Hmed <- NA
    SpParams$Hmax <- NA
  }
  for(i in 1:nrow(SpParams)) {
    heights <- height_values[species_names == SpParams$Name[i]]
    heights <- heights[!is.na(heights)]
    if(length(heights)>0) {
      SpParams$Hmed[i] <- round(as.numeric(quantile(heights, probs=quantile_Hmed, na.rm=FALSE)))
      SpParams$Hmax[i] <- round(as.numeric(quantile(heights, probs=quantile_Hmax, na.rm=FALSE)))
    }
  }
  if(sum(is.na(SpParams$Hmed))>0) message(paste0(sum(is.na(SpParams$Hmed))," missing Hmed/Hmax values (out of ", nrow(SpParams),") after populating with input data.\n"))
  return(SpParams)
}


#' @rdname populateSpParamsFromInventory
#'
#' @param diameter_values A numeric vector of tree diameter at breast height (in cm)
#' @param quantile_fHDmin Quantile for fHDmin
#' @param quantile_fHDmax Quantile for fHDmax
#'
#' @export
populateTreeDiameterHeightParams<-function(SpParams,
                                           species_names,
                                           height_values,
                                           diameter_values,
                                           quantile_fHDmin = 0.05,
                                           quantile_fHDmax = 0.95,
                                           erase_previous = FALSE) {
  if((length(species_codes)!=length(height_values)) || (length(diameter_values)!=length(height_values))) {
    stop("Vectors for codes and values should have the same length!")
  }

  height_values = as.numeric(height_values)
  diameter_values = as.numeric(diameter_values)
  species_codes = as.character(species_codes)


  toRemove = is.na(height_values) |  is.na(diameter_values) | is.na(species_codes)
  if(sum(toRemove)>0) {
    diameter_values = diameter_values[!toRemove]
    height_values = height_values[!toRemove]
    species_codes = species_codes[!toRemove]
    message(paste0(sum(toRemove), " species/height/diameter missing values removed from input."))
  }
  toRemove = (height_values==0) | (diameter_values==0)
  if(sum(toRemove)>0) {
    diameter_values = diameter_values[!toRemove]
    height_values = height_values[!toRemove]
    species_codes = species_codes[!toRemove]
    message(paste0(sum(toRemove), " zero height/diameter values removed from input."))
  }

  if(erase_previous) {
    SpParams$fHDmin <- NA
    SpParams$fHDmax <- NA
  }

  ntree <- 0
  nmis <- 0
  for(i in 1:nrow(SpParams)) {
    growth_form = SpParams$GrowthForm[i]
    if(growth_form %in% c("Tree", "Tree/Shrub")) {
      ntree <- ntree + 1
      sel = (species_names == SpParams$Name[i])
      sel[is.na(sel)] = FALSE
      if(sum(sel)>0) {
        heights <- height_values[sel]
        heights <- heights[!is.na(heights)]
        diameters <- diameter_values[sel]
        diameters <- diameters[!is.na(diameters)]
        HD_values <- heights/diameters
        SpParams$fHDmin[i] <- round(as.numeric(quantile(HD_values, probs=quantile_fHDmin, na.rm=FALSE)))
        SpParams$fHDmax[i] <- round(as.numeric(quantile(HD_values, probs=quantile_fHDmax, na.rm=FALSE)))
      } else {
        nmis <- nmis +1
      }
    }
  }
  if(nmis>0) message(paste0(nmis,
                            " missing fHDmin/fHDmax values (out of ",
                            ntree,
                            " tree species) after populating with input data.\n"))
  return(SpParams)

}
