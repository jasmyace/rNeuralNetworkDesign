% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newton.R
\name{newtons_method}
\alias{newtons_method}
\title{Evaluate steepest descent on a quadratic function, assuming a
positive-definite symmetric matrix.}
\usage{
newtons_method(x_0, fn, verbose = FALSE, tol = 1e-06)
}
\arguments{
\item{x_0}{Initial values for the parameters to be optimized over.}

\item{fn}{A function to be minimized (or maximized), with first argument the
vector of parameters over which minimization is to take place. It should
return a scalar result.  Currently unused.}

\item{verbose}{A logical indicating of the estimated location returned from
steepest descent should be reported to the Console for each iteration.
Default is \code{FALSE}.}

\item{tol}{A tolerance to which the algorithm should calculate and surpass.}

\item{A}{A symmetric numeric matrix.}

\item{d}{A numeric vector of the same size as \code{A}.}

\item{c}{A numeric real number.}

\item{alpha}{A numeric step size.}
}
\value{
A list with two elements.  The first, numeric vector \code{xStar},
with the steepest-descent-found minimum of the provided quadratic function.
The second, tibble \code{conv}, containing the sequential steps evaluated
via steepest descent to find \code{xStar}.
}
\description{
Evaluate steepest descent on a quadratic function, assuming a
positive-definite symmetric matrix.
}
\details{
The arguments \code{A}, \code{d}, and \code{c} together form the
quadratic function \eqn{F(\mathbf{x}) =
  \frac{1}{2}\mathbf{x}^T\mathbf{A}\mathbf{x} + \mathbf{d}^T\mathbf{x} + c},
where \eqn{\mathbf{A}} is of size \eqn{2\times 2}, \eqn{\mathbf{x}} and
\eqn{\mathbf{d}} are of size \eqn{2 \times 1}, and \eqn{c \in \mathbb{R}}.

The steepest descent algorithm calculates, for each iteration \eqn{k},
values \eqn{\mathbf{x}_{k+1} = \mathbf{x}_k + \alpha_k \mathbf{g}_k}, where
\eqn{\alpha_k > 0} is the learning rate, and \eqn{\mathbf{g}_k} is the
gradient of the provided quadratic function \eqn{F(\mathbf{x})}.
Typically, \eqn{\alpha_k} is a small positive number.  The steepest descent
algorithm continues iterating until the distance between
\eqn{\mathbf{x}_{k+1} - \mathbf{x}_k}, calculated here via the
\eqn{2}-norm, is less than the tolerance \code{tol}.
}
\author{
Jason Mitchell
}
