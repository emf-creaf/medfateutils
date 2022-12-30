#' Translate species codes
#'
#' Translate IFN species codes to medfate SpIndex codes
#'
#' @param x A vector of IFN species codes.
#' @param IFNcodes A string vector (of length equal to the number of rows in \code{SpParams} of the IFN species codes that correspond to the model species codification. Each string may contain different coma-separated codes in order to merge IFN species into a single model species.
#'
#' @return A string vector with the length of \code{x} and translated codes.
#'
#' @export
#'
#' @encoding UTF-8
#' @author Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
#' @seealso \code{\link{IFN2forest}}
#'
translateIFNSpeciesCodes<-function(x, IFNcodes) {
  lsfi = strsplit(IFNcodes,"[,./]")
  sfiNumCod = unique(as.numeric(unlist(lsfi)))
  repVect = rep(NA,max(sfiNumCod))
  for(i in 1:length(lsfi)) {
    cv = as.numeric(lsfi[[i]])
    for(ch in cv) {
      repVect[ch] = (i-1) #Species indices start from 0 in medfate
    }
  }
  # Translate codes by transforming input to numeric values.
  # Non-numeric strings will result in NA codes
  x_numeric = as.numeric(x)
  x_numeric[x_numeric <= 0] = NA
  result = rep(NA, length(x_numeric))
  result[!is.na(x_numeric)] <- repVect[x_numeric[!is.na(x_numeric)]]
  return(result)
}
