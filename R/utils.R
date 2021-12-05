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




