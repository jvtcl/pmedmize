source('code/pmedm.R')

#### PUMS constraints ####
tp <- '00813'
ipums <- read.csv('data/co_pums_acs5_2016.csv.gz')
ipums <- ipums[ipums$PUMA == as.numeric(tp),]

## premade individual-level constraints
## we'll select from these to run the P-MEDM solver
constraints_pums <- build_data_person(ipums, 'trt', 'bg')

schema <- read.csv('data/example_constraints.csv', stringsAsFactors = F)
schema <- schema[!schema$constraint %in% c('LEP', 'POV.AGE18U'),] # oops, these aren't available for block groups

## build constraints according to schema
library(matrixStats)
sc <- unique(schema$constraint)
pmedm_constraints_ind <- lapply(sc, function(x){
  
  xc <- schema$code[schema$constraint == x]
  cout <- constraints_pums$pums_in[,xc]
  if(length(xc) > 1){
    cout <- ifelse(rowSums(cout) >= 1, 1, 0)
  }
  cout
  
})
names(pmedm_constraints_ind) <- sc
pmedm_constraints_ind <- do.call(cbind, pmedm_constraints_ind)

# check
apply(pmedm_constraints_ind, 2, function(x){
  sum(x * ipums$PERWT) / sum(ipums$PERWT)
})

#### summary level constraints ####

## temp, will replace with census API 
constraints_bg <- read.csv('data/boulder_sum_est_2016_person_bg.csv')
constraints_trt <- read.csv('data/boulder_sum_est_2016_person_trt.csv')

## not needed here
## but if we had multiple PUMAs we could use a lookup table
## to subset them...
# source('code/build_puma_lookup.R')
# puma_lookup <- build_puma_lookup(state = '08')
# puma_lookup <- puma_lookup[puma_lookup$PUMA5CE %in% tp,]
# 
# constraints_trt <- constraints_trt[paste0('0', constraints_trt$GEOID) %in% puma_lookup$trt_id,]
# constraints_bg <- constraints_bg[substr(constraints_bg$GEOID, 1, 10) %in% constraints_trt$GEOID,]

pmedm_constraints_geo <- function(dat, schema){
  
  sc <- unique(schema$constraint)
  
  pmedm_constraints_geo <- lapply(sc, function(x){
    # print(x)  
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

pmedm_constraints_bg <- pmedm_constraints_geo(constraints_bg, schema)
pmedm_constraints_trt <- pmedm_constraints_geo(constraints_trt, schema)

# exclude areas with zero pop (i.e., open water)
pmedm_constraints_bg <- pmedm_constraints_bg[pmedm_constraints_bg[,2] > 0,]
pmedm_constraints_trt <- pmedm_constraints_trt[pmedm_constraints_trt[,2] > 0,]

# crosswalk
geo_lookup <- data.frame(bg = pmedm_constraints_bg$GEOID, trt = substr(pmedm_constraints_bg$GEOID, 1, 10))
 
#### run P-MEDM solver ####
library(tictoc) # time it
tic()
res <- pmedm(pums = ipums,
            pums_in = pmedm_constraints_ind,
            datch = pmedm_constraints_bg,
            datpt = pmedm_constraints_trt,
            geo_lookup = geo_lookup,
            output_minimal = FALSE)
toc()