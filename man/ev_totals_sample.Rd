% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ev_totals_sample}
\alias{ev_totals_sample}
\title{Example output from \code{assign_evs()}}
\format{
A data frame with 100 rows and 3 variables: \describe{
\item{GEOID}{FIPS code for each home block group of these vehicles}
\item{bevs_20k}{Total number of BEVs in this block group under a 20,000 BEV scenario}
\item{phevs_10k}{Total number of PHEVs in this block group under a 10,000 PHEV scenario}}
}
\source{
Produced from this data using the \code{assign_evs()} function`
}
\usage{
ev_totals_sample
}
\description{
A 100 block group random sample of an output from \code{assign_evs()} with 20,000 BEVs and 10,000 PHEVs assigned regionwide.
}
\keyword{datasets}
