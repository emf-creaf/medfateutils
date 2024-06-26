% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modifySoilParams.R
\name{modifySoilParams}
\alias{modifySoilParams}
\alias{modifySoilDepth}
\alias{modifySoilRockContent}
\alias{modifySoilRockContentFromSoilDepth}
\title{Modifies soil properties estimated from SoilGrids}
\usage{
modifySoilDepth(soildata, soildepth)

modifySoilRockContent(soildata, surfacerock)

modifySoilRockContentFromSoilDepth(soildata, soildepth, soildepthrock = 70)
}
\arguments{
\item{soildata}{A data frame of soil physical parameters or a list of data frames}

\item{soildepth}{An estimate of soil depth (in mm) or a vector of soil depths if `soildata` is a list}

\item{surfacerock}{An estimate of surface rock content (in percent) or a vector of such values if `soildata` is a list}

\item{soildepthrock}{An estimate of rock content (in percent) at soil depth, or a vector of such values if `soildata` is a list}
}
\value{
A data frame of modified physical parameters or a list of modified data frames
}
\description{
Modifies soil properties estimated from SoilGrids
}
\details{
These functions are intended to correct the amount of water that can be extracted from a soil. Each of them is based on different assumptions:
\itemize{
  \item{Function \code{modifySoilDepth} trims the set of input layers to soil depth, while keeping a final rocky layer.}
  \item{Function \code{modifySoilRockContent} Modifies the rock content of soil layers from an estimate of surface rock percent, assuming that rock content increases with depth.}
  \item{Function \code{modifySoilRockContentFromSoilDepth} Modifies the rock content of soil layers from an estimate of soil depth, assuming that rock content increases with depth beyond soil depth.}
}
}
\examples{

\dontrun{
library(sf)
coords_sf <- st_sfc(st_point(c(-5.6333, 42.6667)), crs = 4326)
foo_1 <- soilgridsParams(coords_sf, widths = c(300, 700, 1000, 2000))
foo_1

# trimming to 90 cm depth
foo_2 <- modifySoilDepth(foo_1, 900)
foo_2

# 20 \% rocks in the surface
foo_3 <- modifySoilRockContent(foo_1, 20)
foo_3

# soil depth has been estimated as 50 cm
foo_4 <- modifySoilRockContentFromSoilDepth(foo_1, 500)
foo_4
}
}
