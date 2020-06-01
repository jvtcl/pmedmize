# setwd('../')
source('code/pmedm.R')
source('code/constraints.R')

#### PUMS constraints ####
# tp <- '00803'
tp <- paste0('008', 12:16)
ipums <- read.csv('data/co_pums_acs5_2016.csv.gz')
ipums <- ipums[ipums$PUMA %in% as.numeric(tp),]

## Generate tables for building constraints
schema <- read.csv('data/example_constraints.csv', stringsAsFactors = F)
schema <- schema[!schema$constraint %in% c('LEP', 'POV.AGE18U'),] # oops, these aren't available for block groups

cid <- unique(substr(schema$code, 1, 6)) # constraint table IDs

source('code/intermediates.R')
source('code/build_constraints_ind.R')

pmedm_constraints_ind <- prepare_constraints_ind(schema)

pmedm_constraints_ind <- split(data.frame(pmedm_constraints_ind), ipums$PUMA)
ipums <- split(ipums, ipums$PUMA)
names(pmedm_constraints_ind) <- tp
names(ipums) <- tp

# check
lapply(tp, function(p){
  colSums(pmedm_constraints_ind[[p]] * ipums[[p]]$PERWT) / sum(ipums[[p]]$PERWT)
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
                                   geo = "state:08+county:031",
                                   verbose = F)

constraints_trt = build_constraints(v = v,
                                    tables = cid,
                                    key = Sys.getenv('censusapikey'),
                                    name = 'acs/acs5',
                                    year = 2016,
                                    level = 'tract:*',
                                    geo = "state:08+county:031",
                                    verbose = F)


## subset summary-level constraints to Denver
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
res <- lapply(tp, function(p){
  
  print(p)
  
  G <- puma_lookup$trt_id[puma_lookup$PUMA5CE == p]
  g <- geo_lookup$bg[geo_lookup$trt %in% G]
  
  pmedm(pums = ipums[[p]],
        pums_in = pmedm_constraints_ind[[p]],
        datch = pmedm_constraints_bg[pmedm_constraints_bg$GEOID %in% g,],
        datpt = pmedm_constraints_trt[pmedm_constraints_trt$GEOID %in% G,],
        geo_lookup = geo_lookup[geo_lookup$bg %in% g,],
        output_minimal = FALSE)
})
toc()
names(res) <- tp

#### Build Synthetic Populations for a single PUMA ####
source('code/collapse.R')

p <- '00812'

# do not specify uid - will compute internally
syp <- build_synthetic_pops(
                       pums = pmedm_constraints_ind[[p]],
                       alc = res[[p]]$wt_matrix
                     )

#### Build Synthetic Populations for all PUMAs ###
source('code/collapse.R')

pid <- unlist(sapply(tp, function(p) rep(p, nrow(ipums[[p]]))))

uid <- build_unique_ids(do.call(rbind, pmedm_constraints_ind))

syp <- lapply(tp, function(p){
  
  build_synthetic_pops(pums = pmedm_constraints_ind[[p]], 
                       alc = res[[p]]$wt_matrix,
                       uid = list(key = uid$key[pid == p],
                                  link = uid$link))
})
syp <- do.call(rbind, syp)

head(syp)
