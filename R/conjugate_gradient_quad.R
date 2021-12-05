#' @export
#'
#' @title Utilize the method of conjugate gradient descent on a quadratic
#'   function \eqn{F(**x**)}, assuming a positive-definite symmetric matrix, to
#'   find its minimum.
#'
#' @description Utilize the method of conjugate gradient descent on a quadratic
#'   function \eqn{F(**x**)}, assuming a positive-definite symmetric matrix, to
#'   find its minimum.
#'   
#' @param x	Initial guess for the location of a minimum.
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
conjuage_gradient_quad <- function(x, fn, A, d, c, beta="hs", verbose=FALSE, alpha=0.01, tol=1e-6){ 
  
  # x <- c(0.8, -0.25)
  # fn <- function(x1, x2, A, d, c){
  #   q <- c(x1, x2) %*% A %*% c(x1, x2) + d %*% c(x1, x2) + c
  #   return(q)
  # }
  # A <- matrix(c(2, 1, 1, 2), byrow=TRUE, ncol=2, nrow=2)
  # d <- c(0, 0)
  # c <- c(0, 0)
  # alpha <- 0.01
  # tol <- 1e-6
  
  # Ensure A is positive definite. 
  stopifnot("Not all eigenvalues greater than zero.\n" = (eigen(A)$values > rep(0, nrow(A))))
  k <- 0
  x_km1 <- x
  ans <- tibble::as_tibble(t(x))
  
  repeat{
    
    # Calculate gradient of quadratic function directly. 
    g_k <- as.numeric(A %*% x_km1 + d)
    
    # Establish the search direction. 
    if(k == 0){
      p_k <- -1 * g_k
    } else {
      p_k <- -g_k + as.vector(beta_fn(g_k, g_km1, p_km1, beta)) * p_km1
    }
    
    # Calculate learning rate, and take step. 
    alpha_k <- -1 * (g_k %*% p_k) / (p_k %*% A %*% p_k)
    x_k <- x_km1 + as.vector(alpha_k) * p_k
    
    epsilon <- norm(as.matrix(x_km1 - x_k, nrow=1), type = "F")
    if(epsilon < tol){
      break 
    } else {
      g_km1 <- g_k
      p_km1 <- p_k
      x_km1 <- x_k
      k <- k + 1
    }
    ans <- dplyr::bind_rows(ans, tibble::as_tibble(t(x_k)))
    
    if(verbose == TRUE) cat(paste0(k, ": (", paste(round(x_k, 4), collapse = ", "), ")\n"))
  }
  
  # Clean up results for output. 
  ans <- ans %>% 
    dplyr::mutate(iter = seq(1, nrow(ans)) - 1) %>% 
    stats::setNames(., c("x", "y", "iter"))
  
  return(list(xStar = x_k, 
              conv = ans))
}

