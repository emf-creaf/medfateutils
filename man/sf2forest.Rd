% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sf2forest.R
\name{sf2forest}
\alias{sf2forest}
\title{Forest inventory processing}
\usage{
sf2forest(
  input_df,
  filterNA = FALSE,
  filterDead = TRUE,
  minDBH = NA,
  setDefaults = TRUE,
  .verbose = FALSE
)
}
\arguments{
\item{input_df}{Standard data frame generated using package \code{esus}.}

\item{filterNA}{Boolean flag. If TRUE records with missing values for DBH or Height are eliminated.}

\item{filterDead}{Boolean flag. If TRUE only records of live tree are kept.}

\item{minDBH}{Min DBH value to keep. If non-missing, records are eliminated below minDBH}

\item{setDefaults}{set defaults param for roots}

\item{.verbose}{A boolean flag to indicate console output}
}
\value{
A list of \code{\link{forest}} objects.
}
\description{
This function takes a standard data frame as input and creates a list of \code{\link{forest}} objects
}
\examples{
# Load example 'sf' for the french forest inventory
data(example_ffi)

# Create a list of forest objects
forest_list <- sf2forest(example_ffi)

# Examine first forest
forest_list[[1]]
}
