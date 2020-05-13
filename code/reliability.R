"
Reliability/internal validity assessment for P-MEDM.
"

sae <- function(res){
  
  "
  Standard allocation error for P-MEDM fit (Rose and Nagle 2017). 
  "
  
  Y.start = prod(dim(res$Y[[1]])) + 1 # starting index for SF constraints
  Y.end = prod(dim(res$Y[[1]])) + prod(dim(res$Y[[2]])) # final index
  
  Yhat = res$t$pred[Y.start : Y.end] * res$N # convert probabilities to counts
  Yhat = matrix(Yhat, nrow = dim(res$Y[[2]])[1], ncol = dim(res$Y[[2]])[2]) # reshape to matrix
  colnames(Yhat) <- colnames(res$Y[[1]])
  
  p.sae <- function(p, a){
    
    sum(p-a) / sum(a)
    
  }
  
  sapply(colnames(Yhat), function(p){
    
    p.sae(Yhat[,p], res$Y[[2]][,p])
    
  })
  
}

cov.lambda <- function(t, X, sV, N){
  
  "
  Computes the covariance matrix of the P-MEDM
  parameters lambda. 
  
      t: P-MEDM object
  
      X: P-MEDM constraints (individuals (x) areas)
    
      sV: Variances in area-level P-MEDM constraints 
      
  "
  
  a <- (t(X) %*% t$p) %*% (t$p %*% X)
  a <- -1 * a
  
  dp <- Diagonal(x = t$p)
  b <- t(X) %*% dp %*% X
  
  H <- a + b + sV
  
  solve(H)
  
}

compute_allocation <- function(q, X, lambda){
  
  "
  Computes P-MEDM allocation probabilities. 
  
    q: prior allocation probabilities
    
    X: P-MEDM constraints (individuals (x) areas)
    
    lambda: P-MEDM parameters
  "
  
  i <- (q * exp(-X %*% lambda))
  j <- (t(q) %*% exp(-X %*% lambda))
  
  as.numeric(i / as.numeric(j))
  
}

reshape_probabilities <- function(p, n, A){
  
  "
  Reshapes P-MEDM allocation probabilities `p`
  into a matrix of n individuals x m geographies.
  
    t: P-MEDM object
  
    n: Sample size
    
    A: Geographic crosswalk
  "
  
  matrix(p, n, dim(A[[1]])[2], byrow = TRUE)
  
}

pmedm_probabilities <- function(p = NULL, q = NULL, X = NULL, lambda = NULL, n, A){
  
  "
  Reshapes P-MEDM allocation probabilities into a matrix of `n` respondents
  by  `m` units. 
  
    p: A vector of P-MEDM allocation probabilities. If none given, computes 
    them using `compute_allocation`. 
  "
  
  if(is.null(p)){
    p <- compute_allocation(q, X, lambda)    
  }
  
  reshape_probabilities(p, n, A)

}

pmedm_replicate_probabilities <- function(res, rep_lambda){
  
  "
  Wrapper function to compute replicated P-MEDM probability
  matrices from 1:R replicate coefficients (lambda).
  
    res: P-MEDM result 
    
    rep_lambda: replicate P-MEDM coefficients (lambda)
  "
  
  
  with(res, lapply(1:nrow(rep_lambda), function(i){
    
    pmedm_probabilities(p = NULL,
                        q = q,
                        X = X,
                        lambda = rep_lambda[i,],
                        n = n,
                        A = A)
    
  }))
  
  
}