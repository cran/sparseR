% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparseRBIC_step.R
\name{sparseRBIC_step}
\alias{sparseRBIC_step}
\title{Fit a ranked-sparsity model with forward stepwise RBIC (experimental)}
\usage{
sparseRBIC_step(
  formula,
  data,
  family = c("gaussian", "binomial", "poisson"),
  k = 1,
  poly = 1,
  ic = c("RBIC", "RAIC", "BIC", "AIC", "EBIC"),
  hier = c("strong", "weak", "none"),
  sequential = (hier[1] != "none"),
  cumulative_k = FALSE,
  cumulative_poly = TRUE,
  pool = FALSE,
  ia_formula = NULL,
  pre_process = TRUE,
  model_matrix = NULL,
  y = NULL,
  poly_prefix = "_poly_",
  int_sep = "\\\\:",
  pre_proc_opts = c("knnImpute", "scale", "center", "otherbin", "none"),
  filter = c("nzv", "zv"),
  extra_opts = list(),
  trace = 0,
  message = TRUE,
  ...
)
}
\arguments{
\item{formula}{Names of the terms}

\item{data}{Data}

\item{family}{The family of the model}

\item{k}{The maximum order of interactions to consider}

\item{poly}{The maximum order of polynomials to consider}

\item{ic}{The information criterion to use}

\item{hier}{Should hierarchy be enforced (weak or strong)? Must be set
with sequential == TRUE (see details)}

\item{sequential}{Should the main effects be considered first, orders
sequentially added/considered?}

\item{cumulative_k}{Should penalties be increased cumulatively as order
interaction increases?}

\item{cumulative_poly}{Should penalties be increased cumulatively as order
polynomial increases?}

\item{pool}{Should interactions of order k and polynomials of order k+1 be
pooled together for calculating the penalty?}

\item{ia_formula}{formula to be passed to step_interact via terms argument}

\item{pre_process}{Should the data be preprocessed (if FALSE, must provide
model_matrix)}

\item{model_matrix}{A data frame or matrix specifying the full model matrix
(used if !pre_process)}

\item{y}{A vector of responses (used if !pre_process)}

\item{poly_prefix}{If model_matrix is specified, what is the prefix for
polynomial terms?}

\item{int_sep}{If model_matrix is specified, what is the separator for
interaction terms?}

\item{pre_proc_opts}{List of preprocessing steps (see details)}

\item{filter}{The type of filter applied to main effects + interactions}

\item{extra_opts}{A list of options for all preprocess steps (see details)}

\item{trace}{Should intermediate results of model selection process be output}

\item{message}{should experimental message be suppressed}

\item{...}{additional arguments for running stepwise selection}
}
\value{
an object of class \code{sparseRBIC} containing the following:

\item{fit}{the final fit object}
\item{srprep}{a \code{recipes} object used to prep the data}
\item{pen_info}{coefficient-level variable counts, types + names}
\item{data}{the (unprocessed) data}
\item{family}{the family argument (for non-normal, eg. poisson)}
\item{info}{a list containing meta-info about the procedure}
\item{stats}{the IC for each fit and respective terms included}
}
\description{
Fit a ranked-sparsity model with forward stepwise RBIC (experimental)
}
\details{
This function mirrors \code{sparseR} but uses stepwise selection guided by RBIC.

Additionally, setting \code{cumulative_poly} or \code{cumulative_k} to \code{TRUE} increases
the penalty cumulatively based on the order of either polynomial or
interaction.

The \code{hier} hierarchy enforcement will only work if \code{sequential == TRUE}, and
notably will only consider the "first gen" hierarchy, that is, that all
main effects which make up an interaction are already in the model. It
is therefore possible for a third order interaction (x1:x2:x3) to
enter a model without x1:x2 or x2:x3, so long as x1, x2, and x3 are all
in the model.

The options that can be passed to \code{pre_proc_opts} are:
\itemize{
\item knnImpute (should
missing data be imputed?)
\item scale (should data be standardized)?
\item center
(should data be centered to the mean or another value?)
\item otherbin (should
factors with low prevalence be combined?)
\item none (should no preprocessing be
done? can also specify a null object)
}

The options that can be passed to \code{extra_opts} are:
\itemize{
\item centers (named numeric
vector which denotes where each covariate should be centered)
\item center_fn
(alternatively, a function can be specified to calculate center such as \code{min}
or \code{median})
\item freq_cut, unique_cut (see ?step_nzv - these get used by the
filtering steps)
\item neighbors (the number of neighbors for knnImpute)
\item one_hot (see ?step_dummy), this defaults to cell-means coding which can be
done in regularized regression (change at your own risk)
\item raw (should polynomials not be orthogonal? defaults to true because variables are
centered and scaled already by this point by default)
}
}
