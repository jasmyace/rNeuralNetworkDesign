
perceptron <- function(p, t, verbose, W_0=NULL, b_0=NULL){
    
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
              b = b_kp1))
}

# p <- list(matrix(c(2, 2), ncol=1), matrix(c(1, -2), ncol=1), matrix(c(-2, 2), ncol=1), matrix(c(-1, 1), ncol=1))
# t <- list(0, 1, 0, 1)
# verbose <- TRUE
# perceptron(p, t, verbose)
# 
# p <- list(matrix(c(0, 2), ncol=1), matrix(c(1, 0), ncol=1), matrix(c(0, -2), ncol=1), matrix(c(2, 0), ncol=1))
# t <- list(1, 1, 0, 0)
# verbose <- TRUE
# perceptron(p, t, verbose)
# 
# 
# 
# p <- list(c(1, 1), c(1, 2), c(2, -1), c(2, 0), c(-1, 2), c(-2, 1), c(-1, -1), c(-2 ,-2))
# t <- list(c(0, 0), c(0, 0), c(0, 1), c(0, 1), c(1, 0), c(1, 0), c(1, 1), c(1, 1))
# verbose <- TRUE
# W_0 <- matrix(c(1, 0, 0, 1), ncol = 2)
# b_0 <- c(1, 1)
# perceptron(p, t, verbose, W_0, b_0)
