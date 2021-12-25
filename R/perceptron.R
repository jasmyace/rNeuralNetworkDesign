#' @export
#'
#' @title Display perceptron results for two-dimensional targets.
#'
#' @description Display perceptron results in two-dimensional space via
#'   two-dimensional targets.  Decision boundaries, valid regions, and training
#'   data are included.
#'
#' @param p A list of input vectors of uniform size.
#'
#' @param t A list of target vectors of uniform size.
#'
#' @param verbose A logical indicating if the function should report progress
#'   after each iteration.  Default is \code{FALSE}.
#'
#' @param W_0 A numeric matrix with initial values.  Default is \code{NULL}, in
#'   which case the zero matrix initializes the algorithm.
#'
#' @param b_0 A numeric matrix with initial values.  Default is \code{NULL}, in
#'   which case the zero vector initializes the algorithm.
#'   
#' @details Functon \code{perceptron} tries to identify a set of linear decision
#'   boundaries that correctly classify a set of inputs.  In the case no such
#'   set of linear decision boundaries exists, nothing will happen without
#'   intervention from the user; i.e., the function will continue to iterate
#'   indefintely.
#' 
#' The function works to identify an optimum weight matrix \eqn{\mathbf{W}} and
#' bias vector \eqn{\mathbf{b}} such that the \eqn{i^{th}} neuron correctly
#' classifies that dimension of an input vector with regards to its supervised
#' target.  The \eqn{i^{th}} row of \eqn{\mathbf{W}} and \eqn{\mathbf{b}}
#' corresponds to \eqn{\mathbf{w}_i^T} and bias vector \eqn{b_i}, respectively,
#' meaning that \eqn{n_i = \mathbf{w}_i^T\mathbf{p} + b_i \geq 0} are classified
#' as \eqn{+1}, while \eqn{\mathbf{w}_i^T\mathbf{p} + b_i < 0} classify as
#' \eqn{0}.  Thus, the \code{perceptron} function uses a \code{hardlim}
#' activation function \eqn{f(n_i) = a_i}, where \eqn{a_i} equals \eqn{0} or
#' \eqn{1}.
#' 
#' Identification of a valid \eqn{\mathbf{W}} and \eqn{\mathbf{b}} proceeds
#' algorithmically via stochastic descent, meaning that input vectors \code{p}
#' evaluate one at a time, in order.  If necessary, vectors evaluate multiple
#' times, in order, until all points correctly match their targets.
#' 
#' Use of the \code{verbse = TRUE} option may be useful to help identify
#' progress in slow-to-converge or seemingly unsolvable (read: possibly not
#' linearly separable) problems.  Note that weight matrices report as a long
#' vector, so care must be taken to ensure appropriate interpretation.
#' 
#' @seealso \code{perceptron_plot}
#' 
#' @author Jason Mitchell
#' 
#' @references  Martin T. Hagan, Howard B. Demuth, Mark H. Beale and Orlando De
#'   Jesús. 2014. Neural Network Design (2nd. ed.). Martin Hagan, Stillwater,
#'   OK, USA.
#'   
#' @examples 
#' \dontrun{
#' p <- list(matrix(c(2, 2), ncol=1), matrix(c(1, -2), ncol=1), 
#' matrix(c(-2, 2), ncol=1), matrix(c(-1, 1), ncol=1))
#' t <- list(0, 1, 0, 1)
#' verbose <- TRUE
#' perceptron(p, t, verbose)
#' 
#' p <- list(matrix(c(0, 2), ncol=1), matrix(c(1, 0), ncol=1), 
#' matrix(c(0, -2), ncol=1), matrix(c(2, 0), ncol=1))
#' t <- list(1, 1, 0, 0)
#' verbose <- TRUE
#' perceptron(p, t, verbose)
#' 
#' p <- list(c(1, 1), c(1, 2), c(2, -1), c(2, 0), c(-1, 2), c(-2, 1), c(-1, -1), c(-2 ,-2))
#' t <- list(c(0, 0), c(0, 0), c(0, 1), c(0, 1), c(1, 0), c(1, 0), c(1, 1), c(1, 1))
#' verbose <- TRUE
#' W_0 <- matrix(c(1, 0, 0, 1), ncol = 2)
#' b_0 <- c(1, 1)
#' ans <- perceptron(p, t, verbose, W_0, b_0)}
#' 
perceptron <- function(p, t, verbose=FALSE, W_0=NULL, b_0=NULL){
    
  # p <- list(c(1, 1), c(1, 2), c(2, -1), c(2, 0), c(-1, 2), c(-2, 1), c(-1, -1), c(-2 ,-2))
  # t <- list(c(0, 0), c(0, 0), c(0, 1), c(0, 1), c(1, 0), c(1, 0), c(1, 1), c(1, 1))
  # verbose <- TRUE
  # W_0 <- matrix(c(1, 0, 0, 1), ncol = 2)
  # b_0 <- c(1, 1)
  
  # Set up neurons and iterator. 
  k <- 1
  d <- length(t[[1]])     # number of neurons

  # Initialize with zeros. 
  if(is.null(W_0)){
    W_k <- matrix(rep(0, d * length(p[[1]])), nrow = d)
  } else {
    W_k <- W_0
  }
  
  if(is.null(b_0)){
    b_k <- b_k <- rep(0, d)
  } else {
    b_k <- b_0
  }

  # Assume that to start, no points are classified correct. 
  P <- length(p)
  samples <- seq(1, P)
  
  repeat{
    
    # Stochastic descent.  Assuming the p are entered randomly, the next is 'random.'  
    j <- samples[ifelse( (k %% P) == 0, P, k %% P)]
    
    p_k <- p[[j]]
    t_k <- t[[j]]
    
    # Calculate error and take a step. 
    e_k <- t_k - hardlim(W_k, b_k, p_k)$a
    W_kp1 = W_k + e_k %*% t(p_k)
    b_kp1 = b_k + e_k 
  
    # Now check which inputs classify correctly. 
    correct <- rep(0, P)
    for(i in 1:P){
      if(all(hardlim(W_kp1, b_kp1, p[[i]])$a == t[[i]])) {correct[i] <- 1}
    }
    
    if(verbose == TRUE) cat(paste0(k, ": via sample ", j, ": ", sum(correct), " of ", P, " classified correctly.  W = [", paste(W_kp1, collapse = ", "), "], b = [", paste(b_kp1, collapse = ", "), "].\n"))
    if(sum(correct) == P){
      break 
    } else {
      W_k <- W_kp1
      b_k <- b_kp1
      k <- k + 1
    }
  }
  return(list(W = W_kp1, 
              b = b_kp1, 
              p = p, 
              t = t))
}


p <- list(c(2, 2), c(1, -2), c(-2, 2), c(-1, 1))
t <- list(0, 1, 0, 1)
verbose <- TRUE
W_0 <- matrix(c(0, 0), ncol = 2)
b_0 <- 0
ans <- perceptron(p, t, verbose, W_0, b_0)

plot_perceptron(ans, -3, 3, -3, 3, 
                title = "Linearly Separable\nTwo-Class Perceptron")
