# Binom calculations
analyse_function <- function(p, P, method, pool_size = NULL, het = NULL) {
  
  n_binom <- function(p, P) {
    ceiling(log(1 - P) / log(1 - (p*0.983))) # PCR sensitivity = 98.3% compared to dissection method
  }
  
  n_pooled_binom <- function(p, P, pool_size) {
    ceiling(log(1 - P) / (pool_size * log(1 - (p*0.983)))) # PCR sensitivity = 98.3% compared to dissection method
  }
  
  n_pooled_beta_binom <- function(p, P, pool_size, het) {
    ceiling(-het * log(1 - P) / ((p*0.983) * log(1 + pool_size * het))) # PCR sensitivity = 98.3% compared to dissection method
  }
  
  # Validate inputs
  if (missing(p) || missing(P) || missing(method)) {
    stop("Error: p, P, and method are required parameters.")
  }
  
  if (method == "n_pooled_binom" && is.null(pool_size)) {
    stop("Error: For n_pooled_binom, pool_size must be specified.")
  }
  
  if (method == "n_pooled_beta_binom" && (is.null(pool_size) || is.null(het))) {
    stop("Error: For pooled_beta_binomial, both pool_size and heterogeneity (het) must be specified.")
  }
  
  # Main switch to handle the method toggle
  result <- switch(method,
                   binomial = n_binom(p, P),
                   pooled_binomial = n_pooled_binom(p, P, pool_size),
                   pooled_beta_binomial = n_pooled_beta_binom(p, P, pool_size, het),
                   stop("Error: Invalid method. Choose 'binomial', 'pooled_binomial', or 'pooled_beta_binomial'.")
  )
  
  return(result)
}