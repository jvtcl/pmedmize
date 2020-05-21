source('code/build_constraints_ind.R')
source('code/intermediates.R')

pums <- read.csv('data/co_pums_acs5_2016.csv.gz')

pums <- pums[pums$PUMA == 803,]

# constraints <- c('B01001', 'B03002', 'B17001', 'B17021', 'B16008', 'B09019') # person
constraints <- c('B11005', 'B11006', 'B11012', 'B11016', 'B19037', 'B25003',  # household
                  'B25007', 'B25032', 'B25036')

pmedm_constraints_ind <- lapply(constraints, function(v){

  do.call(v, args = list(pums = pums))

})
pmedm_constraints_ind <- do.call(cbind, pmedm_constraints_ind)

apply(pmedm_constraints_ind, 2, table)
