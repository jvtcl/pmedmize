# setwd('../')
source('code/pmedm.R')
source('code/constraints.R')

#### PUMS constraints ####
tp <- '00803'
ipums <- read.csv('data/co_pums_acs5_2016.csv.gz')
ipums <- ipums[ipums$PUMA == as.numeric(tp),]

## Generate tables for building constraints
schema <- read.csv('data/example_constraints.csv', stringsAsFactors = F)
schema <- schema[!schema$constraint %in% c('LEP', 'POV.AGE18U'),] # oops, these aren't available for block groups

cid <- unique(substr(schema$code, 1, 6)) # constraint table IDs

source('code/intermediates.R')
source('code/build_constraints_ind.R')

pmedm_constraints_ind <- prepare_constraints_ind(schema)

# check
apply(pmedm_constraints_ind, 2, function(x){
  sum(x * ipums$PERWT) / sum(ipums$PERWT)
})

#### summary level constraints ####
### Build data
## functions for parsing tables from Census API
source('code/build_constraints_geo.R')

# ## set a system environment variable for the API key
#' ```
#' Sys.setenv(censusapikey = 'your_key_here')
#' ````

v = listCensusMetadata(name = 'acs/acs5', vintage = 2016, type = 'variables')

# acs_tables = c('B01001', 'B03002', 'B09019', 'B17021')

constraints_bg = build_constraints(v = v,
                                   tables = cid,
                                   key = Sys.getenv('censusapikey'),
                                   name = 'acs/acs5',
                                   year = 2016,
                                   level = 'block group:*',
                                   geo = "state:08+county:013",
                                   verbose = F)

constraints_trt = build_constraints(v = v,
                                    tables = cid,
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

pmedm_constraints_bg <- prepare_constraints_geo(constraints_bg, schema)
pmedm_constraints_trt <- prepare_constraints_geo(constraints_trt, schema)

# exclude areas with zero pop (i.e., open water)
pmedm_constraints_bg <- pmedm_constraints_bg[pmedm_constraints_bg[,2] > 0,]
pmedm_constraints_trt <- pmedm_constraints_trt[pmedm_constraints_trt[,2] > 0,]

# crosswalk
geo_lookup <- data.frame(bg = pmedm_constraints_bg$GEOID, trt = substr(pmedm_constraints_bg$GEOID, 1, 11))

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

#### Reliability Assessment ####
source('code/reliability.R')

# quick diagnostic - standard allocation error of constraints
sort(abs(sae(res)), decreasing = T)
mean(abs(sae(res)))

### Dual Hessian method
## estimate covariances of model parameters (lambda)
clam <- with(res, cov.lambda(t, X, sV, N))

## simulate lambdas
nrep <- 100
tic()
rep_lambda <- with(res, MASS::mvrnorm(n = nrep, mu = res$t$lambda, Sigma = clam / res$N))
toc()

## synthetic population estimate
syp <- with(res, wt_matrix / N)

## replicate synthetic pop estimates
rep_syp <- pmedm_replicate_probabilities(res, rep_lambda)

## example segment
s <- rowProds(res$pums_in[,c('AGE18U', 'POV')])

## P-MEDM estimates of segment
est <- colSums(s * syp * res$N) / colSums(syp * res$N)

## replicate estimates of segment
rep_est <- lapply(rep_syp, function(r){

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

if(!dir.exists('temp')){
  dir.create('temp')
}

if(!file.exists('temp/cb_2016_08_bg_500k.shp')){
  download.file('https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_08_bg_500k.zip',
                destfile = 'temp/co_bg.zip')
  unzip(zipfile = 'temp/co_bg.zip', exdir = 'temp')
  file.remove('temp/co_bg.zip')
}

bg <- read_sf('temp', 'cb_2016_08_bg_500k')
bg <- bg[match(locs, bg$GEOID),]

bg['est'] <- est
bg['mcv'] <- mcv

bg_cent <- st_centroid(bg)

bg$mcv[bg$est < 0.001] <- NA # omit ests less than 0.1% of pop

library(ggplot2)
ggplot() +
  geom_sf(data = bg, aes(fill = mcv, size = est)) +
  geom_sf(data = bg_cent, aes(size = est), alpha = 0.75) +
  scale_fill_gradient2(low = 'springgreen4', mid = 'lightyellow', high = 'red3',
                       midpoint = 0.3, limits = c(0, 1), na.value = 'snow') +
  scale_size_continuous(range = c(0, 1.5), limits = c(0, max(bg$est))) +
  theme_void() + 
  labs(size = 'Proportional \nEstimate', fill = 'Monte Carlo\nCV')
