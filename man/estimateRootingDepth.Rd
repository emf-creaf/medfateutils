% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimateRootingDepth.R
\encoding{UTF-8}
\name{estimateRootingDepth}
\alias{estimateRootingDepth}
\alias{estimateRootingDepthList}
\title{Estimate Rooting Depth Parameters}
\usage{
estimateRootingDepth(
  forest,
  soil,
  PET_summer,
  P_summer,
  SpParams,
  fillMissing = FALSE
)

estimateRootingDepthList(
  forestlist,
  soillist,
  PET_summer,
  P_summer,
  SpParams,
  fillMissing = FALSE
)
}
\arguments{
\item{forest}{An object of class \code{\link{forest}}.}

\item{soil}{An object of class \code{\link{soil}} or a data frame with soil parameters.}

\item{PET_summer}{Average (climatic) potential evapotranspiration (mm) corresponding to summer months.}

\item{P_summer}{Average (climatic) precipitation (mm) corresponding to summer months.}

\item{SpParams}{A data frame with species parameters (see \code{\link{SpParamsMED}}).}

\item{fillMissing}{A boolean flag to indicate that missing estimates should be filled with mean values.}

\item{forestlist}{A list of objects of class \code{\link{forest}}.}

\item{soillist}{A list with objects of class \code{\link{soil}} or data frames with soil parameters.}
}
\value{
Function \code{estimateRootingDepth} returns object of class \code{\link{forest}} with modified values for \code{Z50} and \code{Z95}. Function \code{estimateRootingDepthList} returns a list of forest objects with modified root distribution parameters.
}
\description{
Estimates rooting depth parameters for forest objects following Cabon et al. (2018).
}
\details{
If \code{PET_summer} or \code{P_summer} are of length one, their elements are repeated for all elements in \code{forestlist}. Similarly, if \code{soillist} is an object of class \code{\link{soil}} or a data frame with soil parameters, the same soil parameters are repeated for all elements in \code{forestlist}.
}
\examples{
# Builds from IFN3 data a list whose elements are 'forest' objects
data(IFN_species_mapping)
data(piesMayoresIFN3)
data(regeneraIFN3)
data(matorralIFN3)
l1 <- IFN2forest(piesMayoresIFN3, IFN_species_mapping, SpParamsMED,
               matorral = matorralIFN3, regenera = regeneraIFN3)

# Estimate rooting depth
soilpar <- defaultSoilParams()
l2 <- estimateRootingDepthList(l1, soilpar, 300, 100, SpParamsMED)

# Compare tree rooting depths
print(l1[[1]]$treeData)
print(l2[[1]]$treeData)

}
\references{
Cabon, A., Martinez-Vilalta, J., de Aragon, J.M., Poyatos, R., De Caceres, M., 2018. Applying the eco-hydrological equilibrium hypothesis to model root distribution in water-limited forests. Ecohydrology 11, e2015. https://doi.org/10.1002/eco.2015
}
\seealso{
\code{\link[medfate]{forest}}, \code{\link{IFN2forest}}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, EMF-CREAF
}
