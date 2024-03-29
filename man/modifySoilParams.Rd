% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modifySoilParams.R
\name{modifySoilParams}
\alias{modifySoilParams}
\alias{modifySoilDepth}
\alias{modifySoilRockContent}
\title{Modifies soil properties estimated from SoilGrids}
\usage{
modifySoilDepth(soildata, soildepth)

modifySoilRockContent(soildata, surfacerock)
}
\arguments{
\item{soildata}{A data frame of soil physical parameters or a list of data frames}

\item{soildepth}{An estimate of soil depth (in mm) or a vector of soil depths if `soildata` is a list}

\item{surfacerock}{An estimate of surface rock content (in percent) or a vector of such values if `soildata` is a list}
}
\value{
A data frame of modified physical parameters or a list of modified data frames
}
\description{
Modifies soil properties estimated from SoilGrids
}
\examples{

\dontrun{
library(sf)
coords_sf <- st_sfc(st_point(c(-5.6333, 42.6667)), crs = 4326)
foo_1 <- soilgridsParams(coords_sf, widths = c(300, 700, 1000, 2000))
foo_1
foo_2 <- modifySoilDepth(foo_1, 900) # 90 cm depth
foo_2
foo_3 <- modifySoilRockContent(foo_2, 20) # 20 \% rocks in the surface
foo_3
}
}
