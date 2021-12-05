#' @export
#'
#' @title Evaluate steepest descent on a quadratic function, assuming a
#'   positive-definite symmetric matrix.
#'
#' @description Evaluate steepest descent on a quadratic function, assuming a
#'   positive-definite symmetric matrix.
#'
#' @param x_0	Initial values for the parameters to be optimized over.
#'
#' @param fn A function to be minimized (or maximized), with first argument the
#'   vector of parameters over which minimization is to take place. It should
#'   return a scalar result.  Currently unused.
#'
#' @param A A symmetric numeric matrix.
#'
#' @param d A numeric vector of the same size as \code{A}.
#'
#' @param c A numeric real number.
#'
#' @param verbose A logical indicating of the estimated location returned from
#'   steepest descent should be reported to the Console for each iteration.
#'   Default is \code{FALSE}.
#'
#' @param alpha A numeric step size.
#'
#' @param tol A tolerance to which the algorithm should calculate and surpass.
#'
#' @details The arguments \code{A}, \code{d}, and \code{c} together form the
#'   quadratic function \eqn{F(\mathbf{x}) =
#'   \frac{1}{2}\mathbf{x}^T\mathbf{A}\mathbf{x} + \mathbf{d}^T\mathbf{x} + c,
#'   where \eqn{\mathbf{A}} is of size \eqn{2\times 2}, \eqn{\mathbf{x}} and
#'   \eqn{\mathbf{d}} are of size \eqn{2 \times 1}, and \eqn{c \in \mathbb{R}}.
#'
#'   The steepest descent algorithm calculates, for each iteration \eqn{k},
#'   values \eqn{\mathbf{x}_{k+1} = \mathbf{x}_k + \alpha_k \mathbf{g}_k}, where
#'   \eqn{\alpha_k > 0} is the learning rate, and \eqn{\mathbf{g}_k} is the
#'   gradient of the provided quadratic function \eqn{F(\mathbf{x})}.
#'   Typically, \eqn{\alpha_k} is a small positive number.  The steepest descent
#'   algorithm continues iterating until the distance between
#'   \eqn{\mathbf{x}_{k+1} - \mathbf{x}_k}, calculated here via the
#'   \eqn{2}-norm, is less than the tolerance \code{tol}.
#'   
#' @return A list with two elements.  The first, numeric vector \code{xStar},
#'   with the steepest-descent-found minimum of the provided quadratic function.
#'   The second, tibble \code{conv}, containing the sequential steps evaluated
#'   via steepest descent to find \code{xStar}.
#'   
#' @author Jason Mitchell 
#' 
steepest_descent <- function(x_0, fn, A, d, c, verbose=FALSE, alpha=0.01, tol=1e-6){ 
  
  # par <- c(4, 5)
  # fn <- function(x1, x2, A, d, c){
  #   q <- c(x1, x2) %*% A %*% c(x1, x2) + d %*% c(x1, x2) + c
  #   return(q)
  # }
  # A <- matrix(c(10, -6, -6, 10), byrow=TRUE, ncol=2, nrow=2)
  # d <- c(4, 4)
  # c <- c(0, 0)
  # alpha <- 0.01
  # tol <- 1e-6
  
  # Ensure A is positive definite. 
  stopifnot("Not all eigenvalues greater than zero.\n" = (eigen(A)$values > rep(0, nrow(A))))
  par_k <- x_0
  k <- 1
  ans <- tibble::as_tibble(t(par_k))
  repeat{
    
    # Calculate gradient of quadratic function directly. 
    g <- A %*% par_k + d
    par_k1 <- par_k - alpha * g
    epsilon <- norm(par_k1 - par_k, type = "F")
    if(epsilon < tol){
      break 
    } else {
      par_k <- par_k1
      k <- k + 1
    }
    ans <- dplyr::bind_rows(ans, tibble::as_tibble(t(par_k)))
    
    if(verbose == TRUE) cat(paste0(k, ": (", paste(round(par_k1, 4), collapse = ", "), ")\n"))
  }
  
  # Clean up results for output. 
  ans <- ans %>% 
    dplyr::mutate(iter = seq(1, nrow(ans))) %>% 
    stats::setNames(., c("x", "y", "iter"))
  
  return(list(xStar = par_k1, 
              conv = ans))
}