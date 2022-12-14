% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_penalties.R
\name{get_penalties}
\alias{get_penalties}
\title{Helper function to help set up penalties}
\usage{
get_penalties(
  varnames,
  poly,
  poly_prefix = "poly_",
  int_sep = "\\\\:",
  pool = FALSE,
  gamma = 0.5,
  cumulative_k = FALSE,
  cumulative_poly = TRUE
)
}
\arguments{
\item{varnames}{names of the covariates in the model matrix}

\item{poly}{max polynomial considered}

\item{poly_prefix}{what comes before the polynomial specification in these
varnames?}

\item{int_sep}{What denotes the multiplication for interactions?}

\item{pool}{Should polynomials and interactions be pooled?}

\item{gamma}{How much should the penalty increase with group size (0.5
assumes equal contribution of prior information)}

\item{cumulative_k}{Should penalties be increased cumulatively as order
interaction increases? (only used if !pool)}

\item{cumulative_poly}{Should penalties be increased cumulatively as order
polynomial increases? (only used if !pool)}
}
\value{
a list of relevant information for the variables, including:

\item{penalties}{the numeric value of the penalties}
\item{vartype}{Variable type (main effect, order k interaction, etc)}
\item{varname}{names of variables}
}
\description{
Helper function to help set up penalties
}
\details{
This is primarily a helper function for sparseR, but it may be
  useful if doing the model matrix set up by hand.
}
