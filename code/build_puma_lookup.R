## prep the lookup

data_path = paste(dirname(dirname(sys.frame(1)$ofile)), 'data/', sep = '/')

build_puma_lookup = function(state){
  
  puma_lookup = read.table(file.path(data_path, '2010_Census_Tract_to_2010_PUMA.txt'), sep = ',', header = TRUE)
  
  puma_lookup$STATEFP = sprintf('%02d', puma_lookup$STATEFP)
  puma_lookup$COUNTYFP = sprintf('%03d', puma_lookup$COUNTYFP)
  puma_lookup$TRACTCE = sprintf('%06d', puma_lookup$TRACTCE)
  puma_lookup$PUMA5CE = sprintf('%05d', puma_lookup$PUMA5CE)
  
  ## subset puma lookup table rows containing target bg's
  puma_lookup$trt_id = with(puma_lookup, paste0(STATEFP, COUNTYFP, TRACTCE))
  
  ## subset to state
  puma_lookup = puma_lookup[puma_lookup$STATEFP %in% state,]

}
