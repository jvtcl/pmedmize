"
Definitions for ACS Summary File constraints at the individual (PUMS) level.
"

build_intermediates <- function(pums, intermediates){

  "
  Helper function, builds a list of intermediate PUMS variables to construct
  P-MEDM constraints based on the definitions in `intermediates.R`
  "

  its <- sapply(intermediates, function(i){

    do.call(i, args = list(pums = pums))

  })
  names(its) <- intermediates

  its

}

B01001 <- function(pums){

  "
  B01001: Sex by Age
  "

  its <- build_intermediates(pums, c('ager', 'sexr'))

  pums.B01001 <- with(its,
                       cbind(
                         rowSums(sexr),
                         sexr[,1],
                         ager * sexr[,1],
                         sexr[,2],
                         ager * sexr[,2]
                        )
                      )

  colnames(pums.B01001) <- paste0('B01001',
                                  sprintf('%03d', 1:ncol(pums.B01001))
                                )

  pums.B01001

}

B03002 <- function(pums){

  "
  B03002: Race by Hispanic/Latino Ethnicity
  "

  its <- build_intermediates(pums, c('racer', 'hispanr'))

  pums.B03002=with(its,
                    cbind(
                      rowSums(racer),
                      hispanr[,2],
                      hispanr[,2] * racer,
                      hispanr[,1],
                      hispanr[,1] * racer
                    )
                  )

  # colnames(pums.B03002)=rip.metadata.from.census.reporter('B03002')$codes
  colnames(pums.B03002) = paste0('B03002', sprintf('%03d', 1:ncol(pums.B03002)))

  # consolidate 'two or more races' levels
  pums.B03002[,which(colnames(pums.B03002) == 'B03002009')] =
    rowSums(pums.B03002[,which(colnames(pums.B03002) == 'B03002009')
                        :which(colnames(pums.B03002) == 'B03002011')])
  pums.B03002[,which(colnames(pums.B03002) == 'B03002019')] =
    rowSums(pums.B03002[,which(colnames(pums.B03002) == 'B03002019')
                        :which(colnames(pums.B03002) == 'B03002021')])

  pums.B03002

}
