source('code/build_constraints_ind.R')
source('code/intermediates.R')

pums <- read.csv('data/co_pums_acs5_2016.csv.gz')

pums <- pums[pums$PUMA == 803,]

constraints <- c('B01001', 'B03002', 'B17001', 'B17021', 'B16008', 'B09019')

pmedm_constraints_ind <- lapply(constraints, function(v){

  do.call(v, args = list(pums = pums))

})
pmedm_constraints_ind <- do.call(cbind, pmedm_constraints_ind)

apply(pmedm_constraints_ind, 2, table)
