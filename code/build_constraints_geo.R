"
Builds P-MEDM constraints from the ACS Summary File using the Census API.
"

library(censusapi)

get_table = function(v, table, key, name, year, level, geo){

  "
  Gets/processes table from censusapi for estimate, margins of error, SE,
  to match SocialExplorer tables
  "

  table_vars = v$name[v$group == table]
  moe_vars = gsub('E$', 'M', table_vars)

  a = getCensus(name = name,
                vintage = year,
                key = key,
                vars = table_vars,
                region = level,
                regionin = geo)

  ## create GEOIDs
  endGeo = which(startsWith(names(a), table))[1] - 1 # last column describing the geography
  geoid = apply(a[,1:endGeo], 1, function(x) paste(x, collapse = ''))

  a = a[,-c(1:endGeo)] # remove geographic vars

  b = getCensus(name = name,
                vintage = year,
                key = key,
                vars = moe_vars,
                region = level,
                regionin = geo)
  b = b[,-c(1:endGeo)] # remove geographic vars

  ## rename variables
  a_rename = gsub('_', '', table_vars)
  a_rename = gsub('E$', '', a_rename)
  b_rename = gsub('_', '', moe_vars)
  b_rename = gsub('M$', '', b_rename)

  est = a[,order(a_rename)]
  names(est) = sort(a_rename)
  moe = b[,order(b_rename)]
  names(moe) = sort(b_rename)

  ## MOE to SE
  se = do.call(cbind.data.frame, lapply(1:ncol(moe), function(i) moe[,i] / 1.645))
  names(se) = paste0(names(moe), 's')

  list(geoid = geoid, est = est, moe = moe, se = se)

}

build_constraints = function(v, tables, key, name, year, level, geo, verbose = T){

  "
  Builds table to match P-MEDM constraints.
  "

  table_ests = lapply(tables, function(t){

    if(verbose){
      cat('Table', t, '\n')
    }

    get_table(v = v,
              table = t,
              key = key,
              name = name,
              year = year,
              level = level,
              geo = geo)

  })

  ests_combined = do.call(cbind.data.frame, lapply(table_ests, function(x)x$est))
  ses_combined = do.call(cbind.data.frame, lapply(table_ests, function(x)x$se))

  out = data.frame(GEOID = table_ests[[1]]$geoid, ests_combined, ses_combined)

  return(out)

}
