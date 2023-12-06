#' Translate species codes
#'
#' Maps a vector of species codes from forest inventory data to medfate species names
#'
#' @param x A vector of forest inventory species codes to be mapped.
#' @param species_codes A string vector of the forest inventory species codes that correspond to the model species codification (\code{species_names}).
#' Each string may contain different coma-separated codes in order to map different forest inventory species into a single model species.
#' @param species_names A string vector of medfate species names with the same length as \code{codes}.
#'
#' @return A string vector with the length of \code{x} and translated species names
#'
#' @export
#'
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' @seealso \code{\link{IFN2forest}}
#'
#' @examples
#' data(NFI_SP_mapping)
#' data(piesMayoresIFN2)
#'
#' piesMayoresIFN2$Especie
#'
#' translateSpeciesCodes(x = piesMayoresIFN2$Especie,
#'                       species_names = NFI_SP_mapping$NFIName,
#'                       species_codes = NFI_SP_mapping$NFICode)
#'
translateSpeciesCodes<-function(x, species_names, species_codes) {
  if(length(species_codes)!=length(species_names)) stop("Vectors 'species_codes' and 'species_names' must have the same length")
  lsfi = strsplit(species_codes,"[,./]")
  sfiNumCod = unique(as.numeric(unlist(lsfi)))
  repVect = rep(NA,max(sfiNumCod))
  for(i in 1:length(lsfi)) {
    cv = as.numeric(lsfi[[i]])
    for(ch in cv) {
      repVect[ch] = i #Species indices
    }
  }
  # Translate codes by transforming input to numeric values.
  # Non-numeric strings will result in NA codes
  x_numeric = as.numeric(x)
  x_numeric[x_numeric <= 0] = NA
  result = rep(NA, length(x_numeric))
  result[!is.na(x_numeric)] <- repVect[x_numeric[!is.na(x_numeric)]]
  return(species_names[result])
}
