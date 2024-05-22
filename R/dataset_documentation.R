#' Example forest inventory data
#'
#' Example data to illustrate the creation of forest objects from inventory data
#'
#' Six example data tables were extracted from the second and third surveys of the Spanish
#' National Forest Inventory (IFN2 and IFN3), to illustrate function \code{\link{IFN2forest}}:
#'
#' \itemize{
#'  \item \code{piesMayoresIFN2} - Data frame with tree data (79 observations and 9 variables) from IFN2.
#'    \itemize{
#'      \item Provincia - Spanish province code (character)
#'      \item Estadillo - Plot code within province (character)
#'      \item ID - Plot ID, resulting from the concatenation of 'Provincia' and 'Estadillo' (character)
#'      \item OrdenIf2 - Tree code in the plot (character)
#'      \item Especie - Species code in IFN2 (character)
#'      \item Dn1: Diameter at breast height (in mm), first measurement.
#'      \item Dn2: Diameter at breast height (in mm), second measurement.
#'      \item Ht: Total tree height (in m)
#'    }
#'  \item \code{piesMenoresIFN2} - Data frame with tree regeneration data (27 observations and 7 variables) from IFN2.
#'    \itemize{
#'      \item Provincia - Spanish province code (character)
#'      \item Estadillo - Plot code within province (character)
#'      \item ID - Plot ID, resulting from the concatenation of 'Provincia' and 'Estadillo' (character)
#'      \item Especie - Species code in IFN2 (character)
#'      \item Numero - Number of stems for 2.5-7.5 cm DBH class (integer)
#'      \item Hm - Average tree height (dm)
#'      \item Regena - Regeneration code ('0', 1', '2', '3') (integer)
#'    }
#'  \item \code{matorralIFN2} - Data frame with shrub data (20 observations and 6 variables) from IFN2.
#'    \itemize{
#'      \item Provincia - Spanish province code (character)
#'      \item Estadillo - Plot code within province (character)
#'      \item ID - Plot ID, resulting from the concatenation of 'Provincia' and 'Estadillo' (character)
#'      \item Especie - Species code in IFN2 (character)
#'      \item Fcc - Shrub cover value (\%)
#'      \item Hm - Average shrub height (dm)
#'    }
#'    \item \code{piesMayoresIFN3} - Data frame with tree data (118 observations and 13 variables) from IFN3.
#'    \itemize{
#'      \item Provincia - Spanish province code (character)
#'      \item Estadillo - Plot code within province (character)
#'      \item Clase - Plot type (character)
#'      \item Subclase - Plot subtype (character)
#'      \item IDPARCELA - Plot ID, resulting from the concatenation of 'Provincia' and 'Estadillo' (character)
#'      \item IDCLASE - Plot type ID, resulting from the concatenation of 'Clase' and 'SubClase' (character)
#'      \item ID - Unique plot ID, resulting from the concatenation of 'IDPARCELA' and 'IDCLASE' (character)
#'      \item OrdenIf2 - Tree code in the plot during IFN2 (character)
#'      \item OrdenIf3 - Tree code in the plot during IFN3 (character)
#'      \item Especie - Species code in IFN3 (character)
#'      \item Dn1: Diameter at breast height (in mm), first measurement.
#'      \item Dn2: Diameter at breast height (in mm), second measurement.
#'      \item Ht: Total tree height (in m)
#'    }
#'    \item \code{regeneraIFN3} - Data frame with tree regeneration data (86 observations and 13 variables) from IFN3.
#'    \itemize{
#'      \item Provincia - Spanish province code (character)
#'      \item Estadillo - Plot code within province (character)
#'      \item Clase - Plot type (character)
#'      \item Subclase - Plot subtype (character)
#'      \item IDPARCELA - Plot ID, resulting from the concatenation of 'Provincia' and 'Estadillo' (character)
#'      \item IDCLASE - Plot type ID, resulting from the concatenation of 'Clase' and 'SubClase' (character)
#'      \item ID - Unique plot ID, resulting from the concatenation of 'IDPARCELA' and 'IDCLASE' (character)
#'      \item Especie - Species code in IFN3 (character)
#'      \item CatDes - Regeneration category, either '1', '2', '3' or '4' (character)
#'      \item Tipo - Kind of regeneration (character)
#'      \item Densidad - Density category ( 1', '2', or '3') (character)
#'      \item NumPies - Number of stems (for CatDes == '1') (numeric)
#'    }
#'  \item \code{matorralIFN3} - Data frame with shrub data (43 observations and 10 variables) from IFN3.
#'    \itemize{
#'      \item Provincia - Spanish province code (character)
#'      \item Estadillo - Plot code within province (character)
#'      \item Clase - Plot type (character)
#'      \item Subclase - Plot subtype (character)
#'      \item IDPARCELA - Plot ID, resulting from the concatenation of 'Provincia' and 'Estadillo' (character)
#'      \item IDCLASE - Plot type ID, resulting from the concatenation of 'Clase' and 'SubClase' (character)
#'      \item ID - Unique plot ID, resulting from the concatenation of 'IDPARCELA' and 'IDCLASE' (character)
#'      \item Especie - Species code in IFN3 (character)
#'      \item Fcc - Shrub cover value (\%)
#'      \item Hm - Average shrub height (dm)
#'    }
#'  }
#'
#' @name example_datasets
#' @aliases piesMayoresIFN2 piesMenoresIFN2 matorralIFN2 piesMayoresIFN3 regeneraIFN3 matorralIFN3
#' @docType data
#' @source
#' \itemize{
#'   \item{Data tables \code{piesMayoresIFN2}, \code{piesMenoresIFN2} and \code{matorralIFN2} were extracted from the Second Spanish National Forest Inventory.}
#'   \item{Data tables \code{piesMayoresIFN3}, \code{regeneraIFN3} and \code{matorralIFN3} were extracted from the Third Spanish National Forest Inventory.}
#'  }
#' @keywords data
NULL


#' Species mapping for the Spanish NFI
#'
#' Data frame with the mapping of species codes from Spanish National Forest Inventory into medfate' species names
#'
#' See vignette 'Species mapping for the Spanish NFI' in the package website.
#'
#' @name IFN_species_mapping
#' @aliases IFN_species_mapping
#' @docType data
#'
#' @keywords data
NULL


#' Data tables with species parameter definition and values for different countries
#'
#' A data sets of species parameter tables resulting from existing databases to be used in conjunction
#' with national forest inventory data.
#'
#' @name SpParamsES
#' @aliases SpParamsES SpParamsFR SpParamsUS SpParamsAU
#'
#' @docType data
#'
#' @format
#' \itemize{
#'   \item{Data frames \code{SpParamsES} (for Spain), \code{SpParamsFR} (for France), \code{SpParamsAU} (for Australia) and \code{SpParamsUS} (for US) have species or genus as rows and column names equal to parameter names
#'   in \code{SpParamsDefinition} (the latter from package \code{medfate}).}
#' }
#' @details
#' \code{SpParamsES}, \code{SpParamsFR},  \code{SpParamsAU} and \code{SpParamsUS} are species parameter data frames designed to be used with National Forest Inventories
#' of Spain, France, Australia and USA, respectively.
#'
#' Details of the procedures used to obtain the species parameter tables can be found in an article at https://emf-creaf.github.io/medfate/.
#' @examples
#' data(SpParamsES)
#' data(SpParamsFR)
#' data(SpParamsUS)
#' data(SpParamsAU)
#' @keywords data
NULL
