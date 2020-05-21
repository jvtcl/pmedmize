"
Helper functions for preparing P-MEDM constraints.
"

library(matrixStats)

prepare_constraints_ind <- function(schema){
  
  "
  Prepares P-MEDM constraints for individuals/PUMS.
  "
  
  cid <- unique(substr(schema$code, 1, 6))
  
  ctable <- lapply(cid, function(v){
    
    do.call(v, args = list(pums = ipums))
    
  })
  ctable <- do.call(cbind, ctable)
  
  sc <- unique(schema$constraint)
  cind <- lapply(sc, function(x){
    
    xc <- schema$code[schema$constraint == x]
    
    cout <- ctable[,xc]
    
    if(length(xc) > 1){
      
      cout <- ifelse(rowSums(cout) >= 1, 1, 0)
      
    }
    
    cout
    
  })
  names(cind) <- sc
  do.call(cbind, cind)
  
}

prepare_constraints_geo <- function(dat, schema){
  
  "
  Prepares P-MEDM constraints for areas/Summary File.
  "
  
  sc <- unique(schema$constraint)
  
  pmedm_constraints_geo <- lapply(sc, function(x){
    
    xc <- schema$code[schema$constraint == x]
    
    cout <- dat[,xc]
    
    se.cout <- dat[,paste0(xc, 's')]
    
    if(length(xc) > 1){
      
      cout <- rowSums(cout)
      
      se.cout <- sqrt(rowSums(se.cout^2))
      
    }
    
    cout <- cbind(cout, se.cout)
    
    colnames(cout) <- c(x, paste0(x, 's'))
    
    cout
    
  })
  data.frame(GEOID = dat[,1], do.call(cbind, pmedm_constraints_geo))
  
}