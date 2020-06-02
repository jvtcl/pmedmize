"
Utilities for aggregating and collapsing P-MEDM estimates, i.e.,
by unique cases or across multiple PUMAs.
"

library(WeightedCluster)

build_unique_ids <- function(pums){
  
  "
  Generates unique IDs for PUMS cases.
  
  pums: PUMS variables of interest, in binary format.
  "
  
  key = wcAggregateCases(pums)$disaggIndex
  
  list(key = key,
       link = unique(key))
  
}


build_synthetic_pops <- function(pums, alc, uid = NULL, normalize = T){
  
  "
  Builds synthetic population estimates at
  P-MEDM target geography based on unique
  PUMS cases. 
  
  pums: PUMS variables of interest, in binary format.
  
  alc: P-MEDM allocation matrix
  
  normalize: whether or not to normalize the synthetic
    population estimates by each area's total population
    (default TRUE).
    
  uid: response IDs. Default NULL (just uses the response 
    IDs for PUMS records belonging to the PUMA).
    If given, builds the synthetic pops relative
    to a custom set of unique IDs instead
    (i.e., unique IDs across multiple PUMAs).
  
  "
  
  
  if(missing(uid)){
    uid <- build_unique_ids(pums)
  }
  
  key <- with(uid, factor(as.character(key), levels = link))
  
  dwt <- alc
  
  dwt <- aggregate(dwt, by = list(key), FUN = sum, drop = F)
  
  dwt <- dwt[match(uid$link, dwt[,1]),]
  dwt <- dwt[,-1]
  
  dwt <- t(dwt)
  
  dwt[is.na(dwt)] <- 0
  
  if(normalize){
    dwt <- dwt / rowSums(dwt)    
  }
  
  dwt
  
}

aggregate_by_geo <- function(alc, geo_lookup, N = NULL, normalize = T){
  
  "
  Aggregates P-MEDM allocation estimates (probabilities OR counts) based
  on a geographic lookup table.
  
    alc: P-MEDM allocation matrix
    
    geo_lookup: Geographic lookup table. Column 1: P-MEDM allocation units,
    Column 2: Aggregation units.
    
    N: Population size. Provide this only if `alc` consists of counts.
    
    normalize: whether or not to normalize the synthetic
      population estimates by each area's total population. Set TRUE
      only if `alc` consists of counts.
  "
  
  if(!missing(N)){
    alc <- alc / N
  }
  
  alc <- t(aggregate(t(alc) ~ geo_lookup[,2], FUN = sum)[,-1])
  colnames(alc) <- unique(geo_lookup[,2])
  
  if(normalize){
    alc <- alc / rowSums(alc)
  }
  
  alc
  
}


