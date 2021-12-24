#' @export
#' 
#' @title 
#' 
#' @description 
#' 
#' @param ans 
#' 
#' @param xlo 
#' 
#' @param xhi 
#' 
#' @param ylo 
#' 
#' @param yhi 
#' 
#' @param repper 
#' 
#' @details 
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
    tidyr::unnest(cols = c(value)) %>% 
    dplyr::mutate(coord = rep(c("x", "y"), length(ans$t))) %>% 
    pivot_wider(names_from = c("coord"), values_from = c("value"))
  
  # Build the t into a tibble.  
  tib_t <- tibble::enframe(ans$t) %>% 
    tidyr::unnest(cols = c(value)) %>% 
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
