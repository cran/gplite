% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gp_loo.R
\name{gp_loo}
\alias{gp_loo}
\alias{gp_compare}
\title{Model assessment and comparison}
\usage{
gp_loo(
  gp,
  x,
  y,
  quadrature = TRUE,
  quad_order = 11,
  draws = 4000,
  jitter = NULL,
  seed = NULL,
  ...
)

gp_compare(..., ref = NULL)
}
\arguments{
\item{gp}{The gp model object to be fitted.}

\item{x}{n-by-d matrix of input values (n is the number of observations and d the input dimension).
Can also be a vector of length n if the model has only a single input.}

\item{y}{Vector of n output (target) values.}

\item{quadrature}{Whether to use deterministic Gauss-Hermite quadrature to estimate the
required integrals. If FALSE, then Monte Carlo estimate is used.}

\item{quad_order}{Order of the numerical quadrature
(only applicable if \code{quadrature=TRUE}).}

\item{draws}{Number of posterior draws to estimate the required integrals (only applicable
if \code{quadrature=FALSE}).}

\item{jitter}{Magnitude of diagonal jitter for covariance matrices for numerical stability.
Default is 1e-6.}

\item{seed}{Random seed.}

\item{...}{For \code{gp_compare}, LOO statistics for the models to compare. For
\code{gp_loo}, possible additional data that is required for LOO predictions (for example,
argument \code{trials} in case of binomial likelihood).}

\item{ref}{Index of the model against which to compare the other models (pairwise
comparison for LOO difference). If not given, then the model with the best LOO is
used as the reference for comparisons.}
}
\value{
\code{gp_loo} returns a list with LOO statistics.
\code{gp_compare} returns a matrix with comparison statistics (LOO differences 
and stardard errors in the estimates).
}
\description{
Function \code{gp_loo} computes the approximate leave-one-out (LOO)
cross-validation statistics for the given GP model with the current
hyperparameters.
Function \code{gp_compare} estimates the difference in the expected
predictive accuracy of two or more GP models given their LOO statistics.
}
\section{References}{


Vehtari A., Mononen T., Tolvanen V., Sivula T. and Winther O. (2016).
Bayesian Leave-One-Out Cross-Validation Approximations for Gaussian Latent
Variable Models. Journal of Machine Learning Research 17(103):1-38.
}

\examples{

# Generate some toy data
set.seed(32004)
n <- 50
sigma <- 0.1
x <- rnorm(n)
ycont <- sin(3 * x) * exp(-abs(x)) + rnorm(n) * sigma
y <- rep(0, n)
y[ycont > 0] <- 1
trials <- rep(1, n)

# Set up two models
gp1 <- gp_init(cf_sexp(), lik_binomial())
gp2 <- gp_init(cf_matern32(), lik_binomial())

# Optimize
gp1 <- gp_optim(gp1, x, y, trials = trials)
gp2 <- gp_optim(gp2, x, y, trials = trials)

# Compare
loo1 <- gp_loo(gp1, x, y, trials = trials)
loo2 <- gp_loo(gp2, x, y, trials = trials)
gp_compare(loo1, loo2)


}
