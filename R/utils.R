beta_hs <- function(g_k, g_km1, p_km1){
  ((g_k - g_km1) %*% g_k) / ((g_k - g_km1) %*% p_km1)
}

beta_fr <- function(g_k, g_km1){
  (g_k %*% g_k) / (g_km1 %*% g_km1)
}

beta_pr <- function(g_k, g_km1){
  ((g_k - g_km1) %*% g_k) / (g_km1 %*% g_km1)
}


beta_fn <- function(g_k, g_km1, p_km1, beta) {
  switch(beta,
         hs = beta_hs(g_k, g_km1, p_km1),
         fr = beta_fr(g_k, g_km1),
         pr = beta_pr(g_k, g_km1),
         stop("Unknown beta function!")
  )
}


#' @importFrom dplyr bind_rows cur_group_id distinct group_by left_join mutate pull rowwise select ungroup 
#' @importFrom ggplot2 aes coord_cartesian coord_fixed element_blank geom_contour geom_line geom_point geom_polygon geom_segment ggplot labs scale_shape_manual theme 
#' @importFrom magrittr %>%
#' @importFrom metR geom_text_contour
#' @importFrom purrr rdunif
#' @importFrom rlang .data
#' @importFrom stats as.formula deriv runif setNames
#' @importFrom tibble add_row as_tibble enframe tibble 
#' @importFrom tidyr pivot_wider unnest 
#' @importFrom utils globalVariables tail 




utils::globalVariables(c(".", ".data"))



gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



logsig <- function(W, b, p){
  n <- W %*% p + b
  a <- 1 / (1 + exp(-n))
  deriv <- a * (1 - a)
  return(list(a = a, deriv = deriv))
}

purelin <- function(W, b, p){
  n <- W %*% p + b
  a <- n
  deriv <- 1
  return(list(a = a, deriv = deriv))
}

hardlim <- function(W, b, p){
  n <- W %*% p + b
  a <- ifelse(n >= 0, 1, 0)   # these can vary?
  deriv <- -99
  return(list(a = a, deriv = deriv))
}

hardlims <- function(W, b, p){
  n <- W %*% p + b
  a <- ifelse(n >= 0, 1, -1)
  deriv <- -99
  return(list(a = a, deriv = deriv))
}