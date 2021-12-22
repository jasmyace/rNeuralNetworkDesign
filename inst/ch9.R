
#' Chapter 9
#' Solved Problems. 
#'
#' P9.1.  We want to find the minimum of the following function: 
#' 
#' F(x) = 5x_1^2 - 6x_1x_2 + 5x_2^2 + 4x_1 + 4x_2
#' 
#' i. Sketch a contour plot of this function.
#'
#' ii. Ketch the trajecotry of the steepest descent algorithm on the contour
#' plot of part (i) if the initial guess is x_0 = (-1, -2.5).  Assume a very
#' small learning rate is used.
#'
#' iii. What is the maximum stable learning rate?

A <- matrix(c(10, -6, -6, 10), byrow=TRUE, ncol=2, nrow=2)
d <- c(4, 4)
c <- 0
x_0 <- c(-1, -2.5)

ans <- steepest_descent(x_0, A, d, c)
quad_contour(A, d, c, x = seq(-4, 2, 0.05), y = seq(-4, 2, 0.05), traj=ans)



#' P9.3.  Recall Problem P8.6, in which we derived a performance index for a
#' linear neural network.  The network, which is displayed again Figure P9.4,
#' was to be trained for the following input/output pairs:
#'
#' {(p_1 = 2), (t_1 = 0.5), (p_2 = -1), (t_2 = 0)}
#'
#' The performance index for the network was defined to be
#'
#' F(x) = (t_1 - a_1(x))^2 + (t_2 - a_2(x))^2,
#'
#' which was displayed in Figure P8.8.
#'
#' i. Use the steepest descent algorithm to locate the optimal paramteters for
#' this network (recall that x = [w b]^T), starting from the initial guess x_0 =
#' (1, 1). Use a learning rate of \alpha = 0.05.
#' 
#' ii. What is the maximum stable learning rate? 




#' P9.4 Consider the function
#'
#' F(x) = exp(x1^2 - x1 + 2*x2^2 + 4)
#'
#' Take one iteration of Newton's method from the initial guess x_0 = (1, -2).
#' How close is this result to the minimum point of F(x)?  Explain.

x_0 <- c(1, -2)
fn <- "exp(x1^2 - x1 + 2*x2^2 + 4)"
verbose <- FALSE
tol <- 1e-6
nm_P94 <- newtons_method(x_0, fn, verbose, tol)



#' P9.5 Compare the performance of Newton's method and steepest descent on the
#' following function: 
#' 
#' F(x) = 0.5 * \vec{x}^T [[1 -1] [-1 1]] \vec{x}
#' 
#' Start from the initial guess x_0 = (1, 0). 

x_0 <- c(1, 0)
fn <- "0.5 * (x1^2 - 2*x1*x2 + x2^2)"
verbose <- FALSE
tol <- 1e-6
nm_P95 <- newtons_method(x_0, fn, verbose, tol)

A <- matrix(c(1, -1, -1, 1), byrow=TRUE, ncol=2, nrow=2)
d <- c(0, 0)
c <- 0
sd_P95 <- steepest_descent(x_0, A, d, c)
quad_contour(A, d, c, x = seq(-4, 4, 0.05), y = seq(-4, 4, 0.05), traj=sd_P95)



