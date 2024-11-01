#' medfateutils: Utility Functions for Package `medfate`
#'
#' Utility functions to facilitate defining inputs (soil, species or vegetation parameters) for simulations with
#' package medfate.
#'
#' @name medfateutils-package
#' @aliases medfateutils medfateutils-package
#' @docType package
#' @author
#' Maintainer: Miquel De Cáceres
#' \email{miquelcaceres@@gmail.com}
#' [ORCID](https://orcid.org/0000-0001-7132-2080)
#'
#' Author: Víctor Granda
#' [ORCID](https://orcid.org/0000-0002-0469-1991)
#'
#' Author: Adriana Tovar
#'
#' Author: Antoine Cabon
#' [ORCID](https://orcid.org/0000-0001-6426-1726)
#'
#' Author: Nicolas Martin-StPaul
#' [ORCID](https://orcid.org/0000-0001-7574-0108)
#'
#' Author: Arsène Druel
#' [ORCID](https://orcid.org/0000-0002-3938-0085)
#'
#' @seealso Useful links: \itemize{ \item{
#' \url{https://emf-creaf.github.io/medfateutils/index.html}} }
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import medfate
#' @importFrom assertthat assert_that
#' @importFrom methods as
#' @importFrom cli cli_li cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom stats predict quantile aggregate filter nls uniroot complete.cases
#' @importFrom utils data
## usethis namespace: end
NULL
