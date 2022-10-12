#' Example datasets
#'
#' Example data to illustrate the creation of forest objects
#'
#' Three example datasets are included:
#'
#' \itemize{
#'  \item example_shrubdata_ifn - Data frame with shrub plot data (43 observations and 4 variables).
#'    \itemize{
#'      \item ID - Plot ID
#'      \item Species - Species code
#'      \item FCC - Shrub cove (\%)
#'      \item H - Shrub height (m)
#'    }
#'  \item example_treedata_ifn - Data frame with tree plot data (118 observations and 11 variables).
#'    \itemize{
#'      \item ID - Plot ID
#'      \item Type - Plot type
#'      \item Species - Species code
#'      \item N - Tree density (ind·m-2)
#'      \item DBH - Tree diameter at breast height (cm)
#'      \item H - Tree height (m)
#'      \item Angle - Angle (degrees from north) measured from the center of the plot
#'      \item Dist - Distance (m) from the center of the plot
#'      \item OIF2 - Tree code at the Second Spanish Forest Inventory
#'      \item OIF3 - Tree code at the Third Spanish Forest Inventory
#'      \item FC - Code for the equation describing volume
#'    }
#'  \item poblet_trees - Data frame with example tree plot data from Poblet, Catalonia (717 observations and 4 variables).
#'    \itemize{
#'      \item Plot.Code - Plot ID
#'      \item Indv.Ref - Tree individual (integer)
#'      \item Species - Species name
#'      \item Diameter.cm - Tree diameter at breast height (cm)
#'    }
#' }
#' @name example_datasets
#' @aliases example_shrubdata_ifn example_treedata_ifn poblet_trees
#' @docType data
#' @source
#' \itemize{
#'   \item{Datasets \code{example_shrubdata_ifn} and \code{example_treedata_ifn} were extracted from the Third Spanish National Forest Inventory.}
#'   \item{Dataset \code{poblet_trees} corresponds to field data sampled by the Catalan Forest Ownership Center (Centre de la Propietat Forestal; CPF).}
#'  }
#' @keywords data
NULL