#' @export
#'
#' @title Display perceptron results for two-dimensional targets.
#'
#' @description Display perceptron results in two-dimensional space via
#'   two-dimensional targets.  Decision boundaries, valid regions, and training
#'   data are included.
#'
#' @param ans A result originating from a call to package function
#'   \code{perceptron}.
#'
#' @param xlo The minimum value to display with respect to the \eqn{x}-axis.
#'
#' @param xhi The maximum value to display with respect to the \eqn{x}-axis.
#'
#' @param ylo The minimum value to display with respect to the \eqn{y}-axis.
#'
#' @param yhi The maximum value to display with respect to the \eqn{y}-axis.
#'
#' @param repper An integer giving the number of points to use to display linear
#'   decision boundaries.  Resulting piecewise segments numbers \code{repper -
#'   1}.
#'
#' @details Function \code{plot_perceptron} assumes target vectors are
#'   two-dimensional, i.e., \eqn{\mathbf{t} \in \mathbb{R}^2}, so as to induce
#'   ease in depicting graphical results.  Currently, the function assumes that
#'   no more than 26 distinct classes are to be included.
#'
#'   Output from function \code{perceptron} includes a final weights matrix
#'   \eqn{\mathbf{W}} and bias vector \eqn{\mathbf{b}}, each of which
#'   contributes to plotting final linear decision boundaries.  Note that the
#'   \eqn{i^{th}} neuron corresponds to the \eqn{i^{th}} row of
#'   \eqn{\mathbf{W}}, say \eqn{\mathbf{w}_i^T} and bias vector
#'   \eqn{\mathbf{b}}, or \eqn{b_i}.  From these, a linear decision boundary can
#'   be found.
#'
#'   To see this, let \eqn{\mathbf{w}_i^T = [A_i \,\, B_i]} and \eqn{b_i = c_i},
#'   so that the \eqn{i^{th}} decision boundary has equation \eqn{\mathbf{w}_i^T
#'   \begin{bmatrix} 1 \\ 1 \end{bmatrix} + b_i = 0}, or \eqn{A_i x_i + B_i y_i
#'   + c_i = 0}.  Simple rearrangement into the traditional line form of \eqn{y
#'   = mx + b} leads to \eqn{y_i = -\frac{A_i}{B_i}x - \frac{c_i}{B}}.  Thus,
#'   the decision boundary of the \eqn{i^{th}} neuron has slope
#'   \eqn{-\frac{A_i}{B_i}} and y-intercept \eqn{- \frac{c_i}{B}}.
#'
#'   Function \code{perceptron_plot} uses these derived values of the slope and
#'   y-intercept to then draw linear decision boundaries.  Colored regions
#'   always correspond to areas greater than the given line.  In the case of a
#'   vertical line, the colored region subsumes \eqn{+\infty} if the weight row
#'   vector \eqn{\mathbf{w}_i^T} points towards \eqn{-\infty} along the
#'   \eqn{y}-axis, with the opposite true when \eqn{\mathbf{w}_i^T} points
#'   positively along the \eqn{y}-axis.
#'   
#' @seealso \code{perceptron}
#' 
#' @author Jason Mitchell
#' 
#' @references  Martin T. Hagan, Howard B. Demuth, Mark H. Beale and Orlando De
#'   Jesús. 2014. Neural Network Design (2nd. ed.). Martin Hagan, Stillwater,
#'   OK, USA.
#'   
#' @examples 
#' \dontrun{
#' p <- list(c(1, 1), c(1, 2), c(2, -1), c(2, 0), c(-1, 2), c(-2, 1), c(-1, -1), c(-2 ,-2))
#' t <- list(c(0, 0), c(0, 0), c(0, 1), c(0, 1), c(1, 0), c(1, 0), c(1, 1), c(1, 1))
#' verbose <- TRUE
#' W_0 <- matrix(c(1, 0, 0, 1), ncol = 2)
#' b_0 <- c(1, 1)
#' ans <- perceptron(p, t, verbose, W_0, b_0)
#' 
#' plot_perceptron(ans, -5, 5, -5, 5)}

plot_perceptron <- function(ans, xlo, xhi, ylo, yhi, repper=100){
  
  # ans <- ans
  # xlo <- -5
  # xhi <- 5
  # ylo <- -5
  # yhi <- 5
  # repper <- 100
  
  # Pull out important pieces.  
  W <- ans$W
  b <- ans$b 
  
  # Define x-axis range and helpers. 
  WW <- nrow(W)
  l <- over_l <- vector("list", WW)
  for(w in 1:WW){
    A <- ans$W[w, 1]  # First column in weight vector, w^th neuron. 
    B <- ans$W[w, 2]  # Second column in weight vector, w^th neuron.
    c <- ans$b[w]     # Bias of w^th neuron. 
    m <- -1 * A / B   # Slope of decision boundary. 
    b <- -1 * (c / B) # y-intercept of decision boundary. 
    x <- seq(xlo, xhi, length.out = repper)
    
    # Build lines via their type.  A and B cannot both be zero.  
    # Horizonal line. 
    if(A == 0){
      y <- c / B
      
    # Vertical line. 
    } else if(B == 0){
      x <- -1 * c / A
      y <- seq(ylo, yhi, length.out = repper)
      
    # Oblique line. 
    } else {
      y <- m * x + b
    }
    
    # Store line. 
    l[[w]] <- tibble::tibble(x = x, y = y)
    
    # Positive infinite slope.  Weight vector points to +x. 
    if((is.infinite(m)) & (m > 0)){
      over_l[[w]] <- l[[w]] %>% 
        tibble::add_row(y = c(Inf, -Inf), x = c(Inf, Inf))
      
    # Negative infinite slope.  Weight vector points to -x. 
    } else if((is.infinite(m)) & (m < 0)){ 
      over_l[[w]] <- l[[w]] %>% 
        tibble::add_row(y = c(Inf, -Inf), x = c(-Inf, -Inf))
      
    # Regular case. 
    } else {
      over_l[[w]] <- l[[w]] %>% 
        tibble::add_row(x = c(xhi, xlo), y = c(Inf, Inf))
    }
  }
  
  # Build the p into a tibble. 
  tib_p <- tibble::enframe(ans$p) %>% 
    tidyr::unnest(cols = c(.data$value)) %>% 
    dplyr::mutate(coord = rep(c("x", "y"), length(ans$t))) %>% 
    pivot_wider(names_from = c("coord"), values_from = c("value"))
  
  # Build the t into a tibble.  
  tib_t <- tibble::enframe(ans$t) %>% 
    tidyr::unnest(cols = c(.data$value)) %>% 
    dplyr::mutate(coord = rep(c("tx", "ty"), length(ans$t))) %>% 
    pivot_wider(names_from = c("coord"), values_from = c("value")) 
  
  # Assumes p and t has same entries N and in same order.  
  tib <- tib_p %>% 
    dplyr::left_join(tib_t, by = c("name")) %>% 
    dplyr::group_by(.data$tx, .data$ty) %>% 
    dplyr::mutate(class = dplyr::cur_group_id()) %>% 
    dplyr::select(-.data$name) %>% 
    dplyr::ungroup()
    
  # Identify number of distinct classes.  
  classN <- tib %>% 
    dplyr::select(.data$class) %>% 
    dplyr::distinct() %>% 
    dplyr::pull() %>% 
    length()
  
  # Identify pch symbols for plot.  Can only use up to 26 distinct. 
  pch <- c(seq(15, 25), seq(0, 14))[c(rep(TRUE, classN), rep(FALSE, 26 - classN))]
  cols <- gg_color_hue(WW)
  
  # Build the plot.  
  p1 <- ggplot2::ggplot(tib) + 
    ggplot2::scale_shape_manual(name = "class", values = pch) 
  
  # Add each decision boundary and valid region. 
  for(w in 1:WW){
    if(w == 1){
      p1 <- p1 + 
        ggplot2::geom_line(data = l[[w]], ggplot2::aes(x = x, y = y)) + 
        ggplot2::geom_polygon(data = over_l[[w]], ggplot2::aes(x = x, y = y), fill = cols[w], alpha = 0.20)
    }
    p1 <- p1 + 
      ggplot2::geom_line(data = l[[w]], ggplot2::aes(x = x, y = y)) + 
      ggplot2::geom_polygon(data = over_l[[w]], ggplot2::aes(x = x, y = y), fill = cols[w], alpha = 0.20, inherit.aes = FALSE)
  }

  # Build remainder of plot. 
  p1 <- p1 + 
    ggplot2::geom_point(ggplot2::aes(x = x, y = y, shape = factor(class)), alpha = 1.00, size = 8) +  
    ggplot2::coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3)) + 
    ggplot2::labs(title = 'Linearly Separable\nFour-Class Two-Neuron Perceptron', 
                  subtitle = "Lines indicate decision boundaries of convenience.", 
                  caption = "",  
                  shape = "Class")
  print(p1)
}
