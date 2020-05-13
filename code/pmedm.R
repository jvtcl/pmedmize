library(PMEDMrcpp)
library(methods)

modpath = dirname(sys.frame(1)$ofile)

source(paste(modpath, 'build_constraints.R', sep = '/'))

pmedm <- function(pums, pums_in, geo_lookup, datch, datpt, type='person', output_minimal = TRUE){

  "
  Wrapper for `PMEDMrcpp::pmedm_solve`. Adapted from various examples by Nagle (2013, 2015). 
  See `PMEDMrcpp::??pmedm_solve`.
  
  Additional info at https://www.rpubs.com/nnnagle/PMEDM_Example.
  
  ARGUMENTS
 
    datch: child summary level constraints, estimates AND se's

    datpt: parent summary level constraints, estimates AND se's

    type: the design weights to use, one of 'person' or 'household'
        
    output_minimal : if TRUE, excludes intermediate variables from the output. If fALSE, outputs all P-MEDM
        inputs/outputs.
    

  VALUES

    Returns a list containing the P-MEDM object and model variables.
  
  "
  
  ## Microdata inputs
  if(type=='person'){
    wt <- pums$PERWT
    serial <- assign_person_ids(pums)
  }else{
    wt <- pums$HHWT
    serial <- pums$SERIAL
  }
  pX <- list(drop0(pums_in), drop0(pums_in))

  ## Geographies
  A1 <- do.call('rbind',lapply(unique(geo_lookup[,2]),function(g){
    nbt <- geo_lookup[geo_lookup[,2]==g,][,1]
    ifelse(geo_lookup[,1] %in% nbt,1,0)
  }))

  rownames(A1) <- unique(geo_lookup[,2])
  colnames(A1) <- geo_lookup[,1]
  A1 <- as(A1,'dgCMatrix')

  A2 <- do.call('rbind',lapply(geo_lookup[,1],function(g){
    ifelse(geo_lookup[,1] %in% g,1,0)
  }))
  rownames(A2) <- geo_lookup[,1]
  colnames(A2) <- geo_lookup[,1]
  A2=as(A2,'dtCMatrix')

  A <- list(A1,A2)

  ## Prep Summary-Level Constraints
  datch[,1] <- as.character(datch[,1])
  datch <- datch[datch[,1] %in% geo_lookup[,1],]
  datch <- datch[match(row.names(A2),datch[,1]),]
  
  # subset by pums input names
  colnames(datch)[-1]=gsub('_','',colnames(datch[-1]))
  
  # ensure geoid, standard error values included
  tempch <- data.frame(GEOID = datch$GEOID, datch[,na.omit(match(colnames(pums_in), colnames(datch)))]) 
  datch <- data.frame(tempch, datch[,paste0(names(tempch)[-1], 's')])
  
  datpt <- datpt[datpt[,1] %in% geo_lookup[,2],]
  datpt <- datpt[match(row.names(A1),datpt[,1]),]
  
  # subset by pums input name
  colnames(datpt)[-1] <- gsub('_','',colnames(datpt[-1]))
  
  # ensure geoid, standard error values included
  temppt <- data.frame(GEOID = datpt$GEOID, datpt[,na.omit(match(colnames(pums_in), colnames(datpt)))])
  datpt <- data.frame(temppt, datpt[,paste0(names(temppt)[-1], 's')])
  
  ## Generate summary-level inputs
  sumpt <- datpt[,-1]
  sumpt <- sumpt[!endsWith(names(sumpt), 's')]
  rownames(sumpt) <- datpt[,1]

  sumch <- datch[,names(sumpt)]
  rownames(sumch) <- as.character(datch[,1])

  Y <- list(as.matrix(sumpt), as.matrix(sumch))

  ## Assemble the variances
  vpt <- datpt[,paste0(names(sumpt),'s')]
  rownames(vpt) <- rownames(sumpt)
  vch <- datch[,paste0(names(sumch),'s')]
  rownames(vch) <- rownames(sumch)
  V <- list(as.matrix(vpt^2),as.matrix(vch^2))

  ## Check column match across constraints
  check_nb_cols <- ncol(pums_in) == ncol(sumpt)
  check_trt_cols <- ncol(pums_in) == ncol(sumch)
  
  ## Check dimensions of constraints
  sumpt.check <- sapply(colnames(sumpt), function(x) paste(unlist(strsplit(x, '_')), collapse=''))
  sumch.check <- sapply(colnames(sumch), function(x) paste(unlist(strsplit(x, '_')), collapse=''))
  
  check_nb_cols_len <- sum(colnames(pums_in) == sumpt.check) == ncol(sumpt)
  check_trt_cols_len <- sum(colnames(pums_in) == sumch.check) == ncol(sumch)
  checks <- c(check_nb_cols, check_nb_cols_len, check_trt_cols, check_trt_cols_len)
  
  if(sum(checks)!=length(checks)){
    stop('PUMS and Summary Level columns do not match.')
  }

  ## Generate PUMS Solver Inputs
  N <- sum(Y[[1]][,1]) # Population Size
  n <- NROW(pX[[1]]) # Sample Size

  # Since we are optimizing probabilities p, rather than weights w
  # Normalize Y (tract/bg data) by N (pop size) and V (tract/bg variances) by n/N^2
  Y_vec <- do.call('c', lapply(Y, function(x) as.vector(as.matrix(x)))) / N
  V_vec <- do.call('c', lapply(V, function(x) as.vector(as.matrix(x)))) * n / N^2

  # Will need a matrix V, not a vector V (variance-covariance matrix as sparse diagonal mat)
  sV <- .sparseDiagonal(n = length(V_vec), x = V_vec)

  # All possible PUMS Allocations
  X <- t(rbind(kronecker(t(pX[[1]]), A[[1]]), kronecker(t(pX[[2]]), A[[2]])))
  
  # Create design weights and normalize
  q <- matrix(wt, n, dim(A[[1]])[2])
  q <- q / sum(as.numeric(q))
  q <- as.vector(t(q))

  X <- as(X, 'dgCMatrix')
  sV <- as(sV, 'dgCMatrix')

  ## Solve PMEDM Problem
  t <- PMEDM_solve(X, Y_vec, sV, q)

  ## Allocation matrix
  wt_matrix <- matrix(t$p, nrow(pums_in), dim(A[[1]])[2], byrow = TRUE) * N
  dimnames(wt_matrix) <- list(serial, rownames(Y[[2]]))

  ## Return inputs/outputs
  if(output_minimal){
    out <- list(datpt,datch,pums,pums_in,pX,Y,V,A,wt,t,wt_matrix)
    names(out)=c('parent.data','child.data','pums','pums_in','pX','Y','V','A','wt','t','wt_matrix')    
  }else{
    out <- list(n,N,datpt,datch,pums,pums_in,pX,X,Y,V,sV,A,wt,t,q,wt_matrix)
    names(out)=c('n','N','datpt','datch','pums','pums_in','pX','X','Y','V','sV','A','wt','t','q','wt_matrix')
  }
  
  out

}
