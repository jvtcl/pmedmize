# setwd('../')
source('code/pmedm.R')

#### PUMS constraints ####
tp <- '00803'
ipums <- read.csv('data/co_pums_acs5_2016.csv.gz')
ipums <- ipums[ipums$PUMA == as.numeric(tp),]

## Generate tables for building constraints
schema <- read.csv('data/example_constraints_household.csv', stringsAsFactors = F)
schema <- schema[!startsWith(schema$constraint, 'BUILT.'),] # test

cid <- unique(substr(schema$code, 1, 6)) # constraint table IDs

source('code/intermediates.R')
source('code/build_constraints_ind.R')
ctable <- lapply(cid, function(v){
  
  do.call(v, args = list(pums = ipums))
  
})
ctable <- do.call(cbind, ctable)

# apply(ctable, 2, table) # check

## build constraints according to schema
library(matrixStats)
sc <- unique(schema$constraint)
pmedm_constraints_ind <- lapply(sc, function(x){
  # print(x)
  xc <- schema$code[schema$constraint == x]
  cout <- ctable[,xc]
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
### Build data
## functions for parsing tables from Census API
source('code/build_pmedm_constraints.R')

# ## set a system environment variable for the API key
#' ```
#' Sys.setenv(censusapikey = 'your_key_here')
#' ````

v = listCensusMetadata(name = 'acs/acs5', vintage = 2016, type = 'variables')

# acs_tables = c('B01001', 'B03002', 'B09019', 'B17021')

## need to use sub-tables for B25003
cid2 <- cid[cid != 'B25003']
cid2 <- c(cid2, paste0('B25003', toupper(letters[1:9])))

constraints_bg = build_constraints(v = v,
                                   tables = cid2,
                                   key = Sys.getenv('censusapikey'),
                                   name = 'acs/acs5',
                                   year = 2016,
                                   level = 'block group:*',
                                   geo = "state:08+county:013",
                                   verbose = F)

constraints_trt = build_constraints(v = v,
                                    tables = cid2,
                                    key = Sys.getenv('censusapikey'),
                                    name = 'acs/acs5',
                                    year = 2016,
                                    level = 'tract:*',
                                    geo = "state:08+county:013",
                                    verbose = F)


## subset summary-level constraints to Boulder City PUMA
source('code/build_puma_lookup.R')
puma_lookup <- build_puma_lookup(state = '08')
puma_lookup <- puma_lookup[puma_lookup$PUMA5CE %in% tp,]

constraints_trt <- constraints_trt[constraints_trt$GEOID %in% puma_lookup$trt_id,]
constraints_bg <- constraints_bg[substr(constraints_bg$GEOID, 1, 11) %in% constraints_trt$GEOID,]

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
geo_lookup <- data.frame(bg = pmedm_constraints_bg$GEOID, trt = substr(pmedm_constraints_bg$GEOID, 1, 11))

#### run P-MEDM solver ####
library(tictoc) # time it
tic()
res <- pmedm(pums = ipums,
             type = 'household',
            pums_in = pmedm_constraints_ind,
            datch = pmedm_constraints_bg,
            datpt = pmedm_constraints_trt,
            geo_lookup = geo_lookup,
            output_minimal = FALSE)
toc()

#### Reliability Assessment ####
source('code/reliability.R')

# quick diagnostic - standard allocation error of constraints
round(sort(abs(sae(res)), decreasing = T), 3)
mean(abs(sae(res)))

### Dual Hessian method
## estimate covariances of model parameters (lambda)
clam <- with(res, cov.lambda(t, X, sV, N))

## simulate lambdas
nrep <- 100
tic()
rep_lambda <- with(res, MASS::mvrnorm(n = nrep, mu = res$t$lambda, Sigma = clam / res$N))
toc()

## estimated P-MEDM allocation probabilities
wm <- with(res, wt_matrix / N)

## replicate allocation probabilities
rep_wm <- pmedm_replicate_probabilities(res, rep_lambda)

## example segment
s <- rowProds(res$pums_in[,c('HH.MINR', 'HH.INCOME.L50K', 'TENR.RENT')])
# s <- rowProds(res$pums_in[,c('AGE.GE65', 'LIVING.ALONE', 'UNITS.GE10.RENT')])

## P-MEDM estimates of segment
est <- colSums(s * wm * res$N) / colSums(wm * res$N)

## replicate estimates of segment
rep_est <- lapply(rep_wm, function(r){

  colSums(s * r * res$N) / colSums(r * res$N)

})
rep_est <- do.call(cbind, rep_est)

## monte carlo error
mce <- rowSds(rep_est)

## monte carlo coefficient of variation
mcv <- mce / est

## Monte carlo coefficient of variation
summary(mcv)
plot(mcv ~ est, pch = 16, xlab = 'Prevalence', ylab = 'Monte Carlo CV')

## map results
library(sf)
locs <- colnames(res$wt_matrix)
# locs <- locs[!locs == '080130123001'] # exclude CU block group (very low non-GQ pop)

if(!dir.exists('temp')){
  dir.create('temp')
}

download.file('https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_08_bg_500k.zip',
              destfile = 'temp/co_bg.zip')
unzip(zipfile = 'temp/co_bg.zip', exdir = 'temp')
file.remove('temp/co_bg.zip')

bg <- read_sf('temp', 'cb_2016_08_bg_500k')
bg <- bg[match(locs, bg$GEOID),]

bg['est'] <- est
bg['mcv'] <- mcv

plot(bg['est'], lwd = 0.25, main = 'Proportional Estimate')
plot(bg['mcv'], lwd = 0.25, main = 'Monte Carlo Coefficient of Variation') 

plot(bg[c('est', 'mcv')], lwd = 0.25)
