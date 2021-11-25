quad_contour_with_trajectory <- function(traj, x, y, A, d, c){
  
  # Orthogonally decompose A.  Assumes A is symmetric. 
  stopifnot("A is not symmetric. Go now and die in what way seems best for you.\n" = isSymmetric(A))
  eA <- eigen(A)
  lambda <- eA$values
  P <- eA$vectors
  
  # Adapted from emulator::quad.form.  Evaluate quadratic 
  # function over several points. 
  eg <- expand.grid(x=x, y=y) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(z = 0.5* crossprod(crossprod(A, c(x, y)), c(x, y)) + d %*% c(x, y) + c)
  
  # Prepare trajectory from steepest descent for 
  # graphical depiction. 
  trajectory <- traj[["conv"]] %>% 
    dplyr::select(-iter) %>% 
    dplyr::mutate(z = rep(0, nrow(traj[["conv"]])))
  
  # Plot the contour and trajectory. 
  ggplot2::ggplot(eg, ggplot2::aes(x, y, z = z)) + 
    ggplot2::geom_contour() + #breaks = seq(min(eg$z), max(eg$z), length=10)) + 
    ggplot2::geom_point(data = trajectory, ggplot2::aes(x, y)) +
    ggplot2::geom_segment(data = trajectory, ggplot2::aes(
      xend=c(tail(x, n=-1), NA), 
      yend=c(tail(y, n=-1), NA))) + 
    metR::geom_text_contour(ggplot2::aes(z = z)) + 
    ggplot2::geom_segment(ggplot2::aes(x = 0 + traj[["xStar"]][1], y = 0 + traj[["xStar"]][2], xend = P[1, 1] + traj[["xStar"]][1], yend = P[2, 1]+ traj[["xStar"]][2])) + 
    ggplot2::coord_fixed() + 
    ggplot2::labs(title = expression(paste("Contour plot of quadratic function centered ")), 
                  subtitle = "Cool something here.", 
                  caption = "Something here.")
}