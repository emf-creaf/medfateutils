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
#'  An additional data table comes from an independent forest inventory survey, used to illustrate the general function \code{\link{forest_mapTreeTable}}:
#' \itemize{
#'  \item \code{poblet_trees} - Data frame with example tree plot data from Poblet, Catalonia (717 observations and 4 variables).
#'    \itemize{
#'      \item Plot.Code - Plot ID (character)
#'      \item Indv.Ref - Tree individual (integer)
#'      \item Species - Species name (character)
#'      \item Diameter.cm - Tree diameter at breast height (cm)
#'    }
#' }
#' @name example_datasets
#' @aliases piesMayoresIFN2 piesMenoresIFN2 matorralIFN2 piesMayoresIFN3 regeneraIFN3 matorralIFN3 poblet_trees
#' @docType data
#' @source
#' \itemize{
#'   \item{Data tables \code{piesMayoresIFN2}, \code{piesMenoresIFN2} and \code{matorralIFN2} were extracted from the Second Spanish National Forest Inventory.}
#'   \item{Data tables \code{piesMayoresIFN3}, \code{regeneraIFN3} and \code{matorralIFN3} were extracted from the Third Spanish National Forest Inventory.}
#'   \item{Data table \code{poblet_trees} corresponds to field data sampled by the Catalan Forest Ownership Center (Centre de la Propietat Forestal; CPF).}
#'  }
#' @keywords data
NULL
