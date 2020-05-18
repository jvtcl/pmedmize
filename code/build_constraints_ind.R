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

B17001 <- function(pums){

  "
  B17001: Poverty Status by Sex by Age
  "

  its <- build_intermediates(pums, c('povr', 'sexr', 'agepovr'))

  B17001 <- with(its,
                cbind(
                    rowSums(povr[,1:2]),
                    povr[,2],
                    povr[,2]*sexr[,1],
                    povr[,2]*sexr[,1]*agepovr,
                    povr[,2]*sexr[,2],
                    povr[,2]*sexr[,2]*agepovr,
                    povr[,1],
                    povr[,1]*sexr[,1],
                    povr[,1]*sexr[,1]*agepovr,
                    povr[,1]*sexr[,2],
                    povr[,1]*sexr[,2]*agepovr
                  )
                )

    colnames(B17001)=paste0('B17001',sprintf('%03d',1:ncol(B17001)))

    B17001

}

B17021 <- function(pums){

  "
  B17021: Poverty Status of Individuals by Living Arrangement
  "

  its <- build_intermediates(pums, c('famr', 'marstr', 'relater',
                                      'hheadsexr', 'povr'))

  ## familiy households by marital status
  # col1: married vs. other family household
  # col2: other family household by sex of household head by relation to household head
  fam.hh=its$famr[,1]
  fam.marst=(fam.hh*its$marstr)[,c(2,1)] # flip `marstr` since married households listed first
  fam.married=fam.marst[,1] # married couple families
  fam.married.relate=fam.married*its$relater[,c(2,1)]
  fam.other=fam.marst[,2] # other families
  fam.other.sexr=fam.other*its$hheadsexr # other family households by sex
  fam.other.male=fam.other.sexr[,1] # other family households, male householder
  fam.other.relate.male=fam.other.male*its$relater[,c(2,1)] # other households ,male head, relatives/nonrelatives
  fam.other.female=fam.other.sexr[,2] # other family households, female householder
  fam.other.relate.female=fam.other.female*its$relater[,c(2,1)] # other households female head, relatives/nonrelatives

  ## nonfamily households by living arrangement
  nfam.other.living.arrangement=rowSums(its$famr[,-1])
  nfam.hh=rowSums(its$famr[,2:3]) # this isn't included in data, but used to compute sublevels
  nfam.householder=nfam.hh # nonfamily householder
  nfam.alone=nfam.householder*its$famr[,3] # householder living alone
  nfam.not.alone=nfam.householder*its$famr[,2] # householder not living alone
  other.living.arrangement=nfam.other.living.arrangement-nfam.hh # other living arrangement

  # assemble household type without poverty status
  B17021.sub=cbind(fam.hh,
                   fam.married,
                   fam.married.relate,
                   fam.other,
                   fam.other.male,
                   fam.other.relate.male,
                   fam.other.female,
                   fam.other.relate.female,
                   nfam.other.living.arrangement,
                   nfam.householder,
                   nfam.alone,
                   nfam.not.alone,
                   other.living.arrangement)

  # add poverty status
  # flip pov columns since 'below pov' listed first
  B17021 <- with(its, cbind(rowSums(povr[,1:2]),
                    povr[,2],
                    povr[,2] * B17021.sub,
                    povr[,1],
                    povr[,1] * B17021.sub))

  colnames(B17021)=paste0('B17021',sprintf('%03d',1:ncol(B17021)))

  B17021

}

B16008 <- function(pums){

  "
  B16008: Citizenship Status by Age by Language Spoken at Home
          and Ability to Speak English
  "

  its <- build_intermediates(pums, c('agecitizenr', 'citizenr', 'speakengr', 'langr'))

  pop.5yo=rowSums(its$agecitizenr[,2:3]) # Total, population 5 years and older

  # native-born population #
  nat.pop=its$citizenr[,1]
  nat.pop.5y_17y=nat.pop*its$agecitizenr[,2]
  nat.pop.5y_17y_only_english=nat.pop.5y_17y*its$speakengr[,2]
  nat.pop.5y_17y_spanish=nat.pop.5y_17y*its$langr[,3]
  nat.pop.5y_17y_spanish_speak_english_vwell=nat.pop.5y_17y_spanish*its$speakengr[,3]
  nat.pop.5y_17y_spanish_speak_english_less_vwell=nat.pop.5y_17y_spanish*its$speakengr[,4]
  nat.pop.5y_17y_other_langs=nat.pop.5y_17y*its$langr[,4]
  nat.pop.5y_17y_other_langs_speak_english_vwell=nat.pop.5y_17y_other_langs*its$speakengr[,3]
  nat.pop.5y_17y_other_langs_speak_english_less_vwell=nat.pop.5y_17y_other_langs*its$speakengr[,4]
  nat.pop.18yo=nat.pop*its$agecitizenr[,3]
  nat.pop.18yo_only_english=nat.pop.18yo*its$speakengr[,2]
  nat.pop.18yo_spanish=nat.pop.18yo*its$langr[,3]
  nat.pop.18yo_spanish_speak_english_vwell=nat.pop.18yo_spanish*its$speakengr[,3]
  nat.pop.18yo_spanish_speak_english_less_vwell=nat.pop.18yo_spanish*its$speakengr[,4]
  nat.pop.18yo_other_langs=nat.pop.18yo*its$langr[,4]
  nat.pop.18yo_other_langs_speak_english_vwell=nat.pop.18yo_other_langs*its$speakengr[,3]
  nat.pop.18yo_other_langs_speak_english_less_vwell=nat.pop.18yo_other_langs*its$speakengr[,4]

  # foreign-born population #
  foreign.pop=rowSums(its$citizenr[,2:3])
  foreign.pop.naturalized=its$citizenr[,2]
  foreign.pop.naturalized.5y_17y=foreign.pop.naturalized*its$agecitizenr[,2]
  foreign.pop.naturalized.5y_17y_only_english=foreign.pop.naturalized.5y_17y*its$speakengr[,2]
  foreign.pop.naturalized.5y_17y_spanish=foreign.pop.naturalized.5y_17y*its$langr[,3]
  foreign.pop.naturalized.5y_17y_spanish_speak_english_vwell=foreign.pop.naturalized.5y_17y_spanish*its$speakengr[,3]
  foreign.pop.naturalized.5y_17y_spanish_speak_english_less_vwell=foreign.pop.naturalized.5y_17y_spanish*its$speakengr[,4]
  foreign.pop.naturalized.5y_17y_other_langs=foreign.pop.naturalized.5y_17y*its$langr[,4]
  foreign.pop.naturalized.5y_17y_other_langs_speak_english_vwell=foreign.pop.naturalized.5y_17y_other_langs*its$speakengr[,3]
  foreign.pop.naturalized.5y_17y_other_langs_speak_english_less_vwell=foreign.pop.naturalized.5y_17y_other_langs*its$speakengr[,4]
  foreign.pop.naturalized.18yo=foreign.pop.naturalized*its$agecitizenr[,3]
  foreign.pop.naturalized.18yo_only_english=foreign.pop.naturalized.18yo*its$speakengr[,2]
  foreign.pop.naturalized.18yo_spanish=foreign.pop.naturalized.18yo*its$langr[,3]
  foreign.pop.naturalized.18yo_spanish_speak_english_vwell=foreign.pop.naturalized.18yo_spanish*its$speakengr[,3]
  foreign.pop.naturalized.18yo_spanish_speak_english_less_vwell=foreign.pop.naturalized.18yo_spanish*its$speakengr[,4]
  foreign.pop.naturalized.18yo_other_langs=foreign.pop.naturalized.18yo*its$langr[,4]
  foreign.pop.naturalized.18yo_other_langs_speak_english_vwell=foreign.pop.naturalized.18yo_other_langs*its$speakengr[,3]
  foreign.pop.naturalized.18yo_other_langs_speak_english_less_vwell=foreign.pop.naturalized.18yo_other_langs*its$speakengr[,4]
  foreign.pop.non.citizen=its$citizenr[,3]
  foreign.pop.non.citizen.5y_17y=foreign.pop.non.citizen*its$agecitizenr[,2]
  foreign.pop.non.citizen.5y_17y_only_english=foreign.pop.non.citizen.5y_17y*its$speakengr[,2]
  foreign.pop.non.citizen.5y_17y_spanish=foreign.pop.non.citizen.5y_17y*its$langr[,3]
  foreign.pop.non.citizen.5y_17y_spanish_speak_english_vwell=foreign.pop.non.citizen.5y_17y_spanish*its$speakengr[,3]
  foreign.pop.non.citizen.5y_17y_spanish_speak_english_less_vwell=foreign.pop.non.citizen.5y_17y_spanish*its$speakengr[,4]
  foreign.pop.non.citizen.5y_17y_other_langs=foreign.pop.non.citizen.5y_17y*its$langr[,4]
  foreign.pop.non.citizen.5y_17y_other_langs_speak_english_vwell=foreign.pop.non.citizen.5y_17y_other_langs*its$speakengr[,3]
  foreign.pop.non.citizen.5y_17y_other_langs_speak_english_less_vwell=foreign.pop.non.citizen.5y_17y_other_langs*its$speakengr[,4]
  foreign.pop.non.citizen.18yo=foreign.pop.non.citizen*its$agecitizenr[,3]
  foreign.pop.non.citizen.18yo_only_english=foreign.pop.non.citizen.18yo*its$speakengr[,2]
  foreign.pop.non.citizen.18yo_spanish=foreign.pop.non.citizen.18yo*its$langr[,3]
  foreign.pop.non.citizen.18yo_spanish_speak_english_vwell=foreign.pop.non.citizen.18yo_spanish*its$speakengr[,3]
  foreign.pop.non.citizen.18yo_spanish_speak_english_less_vwell=foreign.pop.non.citizen.18yo_spanish*its$speakengr[,4]
  foreign.pop.non.citizen.18yo_other_langs=foreign.pop.non.citizen.18yo*its$langr[,4]
  foreign.pop.non.citizen.18yo_other_langs_speak_english_vwell=foreign.pop.non.citizen.18yo_other_langs*its$speakengr[,3]
  foreign.pop.non.citizen.18yo_other_langs_speak_english_less_vwell=foreign.pop.non.citizen.18yo_other_langs*its$speakengr[,4]

  B16008=cbind(pop.5yo,
                    nat.pop,
                    nat.pop.5y_17y,
                    nat.pop.5y_17y_only_english,
                    nat.pop.5y_17y_spanish,
                    nat.pop.5y_17y_spanish_speak_english_vwell,
                    nat.pop.5y_17y_spanish_speak_english_less_vwell,
                    nat.pop.5y_17y_other_langs,
                    nat.pop.5y_17y_other_langs_speak_english_vwell,
                    nat.pop.5y_17y_other_langs_speak_english_less_vwell,
                    nat.pop.18yo,
                    nat.pop.18yo_only_english,
                    nat.pop.18yo_spanish,
                    nat.pop.18yo_spanish_speak_english_vwell,
                    nat.pop.18yo_spanish_speak_english_less_vwell,
                    nat.pop.18yo_other_langs,
                    nat.pop.18yo_other_langs_speak_english_vwell,
                    nat.pop.18yo_other_langs_speak_english_less_vwell,
                    foreign.pop,
                    foreign.pop.naturalized,
                    foreign.pop.naturalized.5y_17y,
                    foreign.pop.naturalized.5y_17y_only_english,
                    foreign.pop.naturalized.5y_17y_spanish,
                    foreign.pop.naturalized.5y_17y_spanish_speak_english_vwell,
                    foreign.pop.naturalized.5y_17y_spanish_speak_english_less_vwell,
                    foreign.pop.naturalized.5y_17y_other_langs,
                    foreign.pop.naturalized.5y_17y_other_langs_speak_english_vwell,
                    foreign.pop.naturalized.5y_17y_other_langs_speak_english_less_vwell,
                    foreign.pop.naturalized.18yo,
                    foreign.pop.naturalized.18yo_only_english,
                    foreign.pop.naturalized.18yo_spanish,
                    foreign.pop.naturalized.18yo_spanish_speak_english_vwell,
                    foreign.pop.naturalized.18yo_spanish_speak_english_less_vwell,
                    foreign.pop.naturalized.18yo_other_langs,
                    foreign.pop.naturalized.18yo_other_langs_speak_english_vwell,
                    foreign.pop.naturalized.18yo_other_langs_speak_english_less_vwell,
                    foreign.pop.non.citizen,
                    foreign.pop.non.citizen.5y_17y,
                    foreign.pop.non.citizen.5y_17y_only_english,
                    foreign.pop.non.citizen.5y_17y_spanish,
                    foreign.pop.non.citizen.5y_17y_spanish_speak_english_vwell,
                    foreign.pop.non.citizen.5y_17y_spanish_speak_english_less_vwell,
                    foreign.pop.non.citizen.5y_17y_other_langs,
                    foreign.pop.non.citizen.5y_17y_other_langs_speak_english_vwell,
                    foreign.pop.non.citizen.5y_17y_other_langs_speak_english_less_vwell,
                    foreign.pop.non.citizen.18yo,
                    foreign.pop.non.citizen.18yo_only_english,
                    foreign.pop.non.citizen.18yo_spanish,
                    foreign.pop.non.citizen.18yo_spanish_speak_english_vwell,
                    foreign.pop.non.citizen.18yo_spanish_speak_english_less_vwell,
                    foreign.pop.non.citizen.18yo_other_langs,
                    foreign.pop.non.citizen.18yo_other_langs_speak_english_vwell,
                    foreign.pop.non.citizen.18yo_other_langs_speak_english_less_vwell)

  colnames(B16008)=paste0('B16008',sprintf('%03d',1:ncol(B16008)))

  B16008

}

B09019 <- function(pums){

  "
  B09019: Household Type (Including Living Alone) by Relationship
  "

  its <- build_intermediates(pums, c('famr', 'marstr', 'relater', 'relatedr',
                                      'householdr', 'hheadr', 'hheadsexr', 'sexr'))

  ## familiy households by marital status
  # col1: married vs. other family household
  # col2: other family household by sex of household head by relation to household head
  fam.hh=its$famr[,1]
  fam.marst=(fam.hh*its$marstr)[,c(2,1)] # flip `marstr` since married households listed first
  fam.married=fam.marst[,1] # married couple families
  fam.married.relate=fam.married*its$relater[,c(2,1)]
  fam.other=fam.marst[,2] # other families
  fam.other.sexr=fam.other*its$hheadsexr # other family households by sex
  fam.other.male=fam.other.sexr[,1] # other family households, male householder
  fam.other.relate.male=fam.other.male*its$relater[,c(2,1)] # other households ,male head, relatives/nonrelatives
  fam.other.female=fam.other.sexr[,2] # other family households, female householder
  fam.other.relate.female=fam.other.female*its$relater[,c(2,1)] # other households female head, relatives/nonrelatives

  ## nonfamily households by living arrangement
  nfam.other.living.arrangement=rowSums(its$famr[,-1])
  nfam.hh=rowSums(its$famr[,2:3]) # this isn't included in data, but used to compute sublevels
  nfam.householder=nfam.hh # nonfamily householder
  nfam.alone=nfam.householder*its$famr[,3] # householder living alone
  nfam.not.alone=nfam.householder*its$famr[,2] # householder not living alone
  other.living.arrangement=nfam.other.living.arrangement-nfam.hh # other living arrangement

  in.household=its$householdr[,2] # in households

  ## Family Households
  fam.hh.householder=fam.hh*its$hheadr[,2]
  fam.hh.householder.male=fam.hh.householder*its$sexr[,1]
  fam.hh.householder.female=fam.hh.householder*its$sexr[,2]
  fam.hh.spouse=fam.hh*its$relatedr[,2]
  fam.hh.biological.child=fam.hh*its$relatedr[,4]
  fam.hh.adopted.child=fam.hh*its$relatedr[,5]
  fam.hh.stepchild=fam.hh*its$relatedr[,6]

  # column header for "child"
  fam.hh.child.all=rowSums(cbind(fam.hh.biological.child,
                                 fam.hh.adopted.child,
                                 fam.hh.stepchild))
  fam.hh.grandchild=fam.hh*its$relatedr[,18]
  fam.hh.sibling=fam.hh*its$relatedr[,14]
  fam.hh.parent=fam.hh*its$relatedr[,10]
  fam.hh.parent.in.law=fam.hh*its$relatedr[,12]
  fam.hh.child.in.law=fam.hh*its$relatedr[,8]
  fam.hh.other.relatives=fam.hh*its$relatedr[,22]
  fam.hh.nonrelative.roomer.boarder=fam.hh*its$relatedr[,49]
  fam.hh.nonrelative.housemate.roommate=fam.hh*its$relatedr[,43]
  fam.hh.nonrelative.unmarried.partner=fam.hh*its$relatedr[,42]
  fam.hh.nonrelative.foster.child=fam.hh*its$relatedr[,50]
  fam.hh.nonrelative.other.nonrelatives=fam.hh*its$relatedr[,55]

  # column header for 'nonrelatives in fam households'
  fam.hh.nonrelative.all=rowSums(cbind(fam.hh.nonrelative.roomer.boarder,
                                       fam.hh.nonrelative.housemate.roommate,
                                       fam.hh.nonrelative.unmarried.partner,
                                       fam.hh.nonrelative.foster.child,
                                       fam.hh.nonrelative.other.nonrelatives))

  ## Nonfamily households
  nfam.hh=rowSums(its$famr[,2:3]) # nonfamlily households, placeholder
  nfam.hh.householder=nfam.hh*its$hheadr[,2] # nonfamily householder, placeholder

  nfam.hh.householder.male=nfam.hh.householder*its$sexr[,1]
  nfam.hh.householder.male.alone=nfam.hh.householder.male*its$famr[,3] # nfam.alone from b17021
  nfam.hh.householder.male.not.alone=nfam.hh.householder.male*its$famr[,2] # nfam.not.alone from b17021

  nfam.hh.householder.female=nfam.hh.householder*its$sexr[,2]
  nfam.hh.householder.female.alone=nfam.hh.householder.female*its$famr[,3] # nfam.alone from b17021
  nfam.hh.householder.female.not.alone=nfam.hh.householder.female*its$famr[,2] # nfam.not.alone from b17021

  nfam.hh.nonrelative.roomer.boarder=nfam.hh*its$relatedr[,49]
  nfam.hh.nonrelative.housemate.roommate=nfam.hh*its$relatedr[,43]
  nfam.hh.nonrelative.unmarried.partner=nfam.hh*its$relatedr[,42]
  nfam.hh.nonrelative.foster.child=nfam.hh*its$relatedr[,50]
  nfam.hh.nonrelative.other.nonrelatives=nfam.hh*its$relatedr[,55]

  # column header for 'nonrelatives in nfam households'
  nfam.hh.nonrelative.all=rowSums(cbind(nfam.hh.nonrelative.roomer.boarder,
                                        nfam.hh.nonrelative.housemate.roommate,
                                        nfam.hh.nonrelative.unmarried.partner,
                                        nfam.hh.nonrelative.foster.child,
                                        nfam.hh.nonrelative.other.nonrelatives))
  in.group.quarters=its$famr[,4]

  B09019 <- cbind(rowSums(its$householdr),
                    its$householdr[,2],
                    fam.hh,
                    fam.hh.householder,
                    fam.hh.householder.male,
                    fam.hh.householder.female,
                    fam.hh.spouse,
                    fam.hh.child.all,
                    fam.hh.biological.child,
                    fam.hh.adopted.child,
                    fam.hh.stepchild,
                    fam.hh.grandchild,
                    fam.hh.sibling,
                    fam.hh.parent,
                    fam.hh.parent.in.law,
                    fam.hh.child.in.law,
                    fam.hh.other.relatives,
                    fam.hh.nonrelative.all,
                    fam.hh.nonrelative.roomer.boarder,
                    fam.hh.nonrelative.housemate.roommate,
                    fam.hh.nonrelative.unmarried.partner,
                    fam.hh.nonrelative.foster.child,
                    fam.hh.nonrelative.other.nonrelatives,
                    nfam.hh,
                    nfam.hh.householder,
                    nfam.hh.householder.male,
                    nfam.hh.householder.male.alone,
                    nfam.hh.householder.male.not.alone,
                    nfam.hh.householder.female,
                    nfam.hh.householder.female.alone,
                    nfam.hh.householder.female.not.alone,
                    nfam.hh.nonrelative.all,
                    nfam.hh.nonrelative.roomer.boarder,
                    nfam.hh.nonrelative.housemate.roommate,
                    nfam.hh.nonrelative.unmarried.partner,
                    nfam.hh.nonrelative.foster.child,
                    nfam.hh.nonrelative.other.nonrelatives,
                    in.group.quarters)

  colnames(B09019) <- paste0('B09019',sprintf('%03d',1:ncol(B09019)))

  B09019

}
