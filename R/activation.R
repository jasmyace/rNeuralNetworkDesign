

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