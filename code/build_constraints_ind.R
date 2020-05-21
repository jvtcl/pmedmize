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
  B01001: Sex by Age (Person)
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
  B03002: Race by Hispanic/Latino Ethnicity (Person)
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

B09019 <- function(pums){

  "
  B09019: Household Type (Including Living Alone) by Relationship) (Person)
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

  pums.B09019 <- cbind(rowSums(its$householdr),
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

  colnames(pums.B09019) <- paste0('B09019',sprintf('%03d',1:ncol(pums.B09019)))

  pums.B09019

}

B16008 <- function(pums){

  "
  B16008: Citizenship Status by Age by Language Spoken at Home
          and Ability to Speak English (Person)
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

  pums.B16008=cbind(pop.5yo,
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

  colnames(pums.B16008)=paste0('B16008',sprintf('%03d',1:ncol(pums.B16008)))

  pums.B16008

}

B17001 <- function(pums){

  "
  B17001: Poverty Status by Sex by Age (Person)
  "

  its <- build_intermediates(pums, c('povr', 'sexr', 'agepovr'))

  pums.B17001 <- with(its,
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

    colnames(pums.B17001)=paste0('B17001',sprintf('%03d',1:ncol(pums.B17001)))

    pums.B17001

}

B17021 <- function(pums){

  "
  B17021: Poverty Status of Individuals by Living Arrangement (Person)
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
  pums.B17021.sub <- cbind(fam.hh,
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
  pums.B17021 <- with(its, cbind(rowSums(povr[,1:2]),
                    povr[,2],
                    povr[,2] * pums.B17021.sub,
                    povr[,1],
                    povr[,1] * pums.B17021.sub))

  colnames(pums.B17021)=paste0('B17021',sprintf('%03d',1:ncol(pums.B17021)))

  pums.B17021

}

B11005 <- function(pums){

  "
  B11005: Presence of People Under 18 Years By Household Type (Household)
  "

  its <- build_intermediates(pums, c('minr', 'famr', 'marstr','hheadsexr'))

  # hh = rep(1, nrow(pums)) # Total 001 (old)
  hh = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  hh.w.under.18 = its$minr[,2] # households with one or more person under 18 002
  hh.w.under.18.fam = hh.w.under.18 * its$famr[,1]
  hh.w.under.18.fam.married = hh.w.under.18.fam * its$marstr[,2]
  hh.w.under.18.fam.other = hh.w.under.18.fam * its$marstr[,1]
  hh.w.under.18.fam.other.male.no.wife.present = hh.w.under.18.fam.other * its$hheadsexr[,1]
  hh.w.under.18.fam.other.female.no.husband.present = hh.w.under.18.fam.other * its$hheadsexr[,2]
  hh.w.under.18.nfam = hh.w.under.18 * rowSums(its$famr[,2:3])
  hh.w.under.18.nfam.male = hh.w.under.18.nfam * its$hheadsexr[,1]
  hh.w.under.18.nfam.female = hh.w.under.18.nfam * its$hheadsexr[,2]
  hh.no.under.18 = its$minr[,1] # households with no people under 18 011
  hh.no.under.18.fam = hh.no.under.18 * its$famr[,1]
  hh.no.under.18.fam.married = hh.no.under.18.fam * its$marstr[,2]
  hh.no.under.18.fam.other = hh.no.under.18.fam * its$marstr[,1]
  hh.no.under.18.fam.other.male.no.wife.present = hh.no.under.18.fam.other * its$hheadsexr[,1]
  hh.no.under.18.fam.other.female.no.husband.present = hh.no.under.18.fam.other * its$hheadsexr[,2]
  hh.no.under.18.nfam = hh.no.under.18 * rowSums(its$famr[,2:3])
  hh.no.under.18.nfam.male = hh.no.under.18.nfam * its$hheadsexr[,1]
  hh.no.under.18.nfam.female = hh.no.under.18.nfam * its$hheadsexr[,2]

  pums.B11005 <- cbind(hh,
                hh.w.under.18,
                hh.w.under.18.fam,
                hh.w.under.18.fam.married,
                hh.w.under.18.fam.other,
                hh.w.under.18.fam.other.male.no.wife.present,
                hh.w.under.18.fam.other.female.no.husband.present,
                hh.w.under.18.nfam,
                hh.w.under.18.nfam.male,
                hh.w.under.18.nfam.female,
                hh.no.under.18,
                hh.no.under.18.fam,
                hh.no.under.18.fam.married,
                hh.no.under.18.fam.other,
                hh.no.under.18.fam.other.male.no.wife.present,
                hh.no.under.18.fam.other.female.no.husband.present,
                hh.no.under.18.nfam,
                hh.no.under.18.nfam.male,
                hh.no.under.18.nfam.female)

  colnames(pums.B11005) <- paste0('B11005',sprintf('%03d',1:ncol(pums.B11005)))

  pums.B11005

}

B11006 <- function(pums){

  "
  B11006: Households by Presence of People 60 Years or Older By Household Type
  "

  its <- build_intermediates(pums, c('eldr', 'famr', 'marstr', 'hheadsexr'))

  # hh = rep(1, nrow(pums)) # Total 001 (old)
  hh = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  hh.w.60.over = its$eldr[,2] # households with one or more person 60 or over 002
  hh.w.60.over.fam = hh.w.60.over * its$famr[,1]
  hh.w.60.over.fam.married = hh.w.60.over.fam * its$marstr[,2]
  hh.w.60.over.fam.other = hh.w.60.over.fam * its$marstr[,1]
  hh.w.60.over.fam.other.male.no.wife.present = hh.w.60.over.fam.other * its$hheadsexr[,1]
  hh.w.60.over.fam.other.female.no.husband.present = hh.w.60.over.fam.other * its$hheadsexr[,2]
  hh.w.60.over.nfam = hh.w.60.over * rowSums(its$famr[,2:3])
  hh.no.60.over = its$eldr[,1] # households with no people 60 or over 009
  hh.no.60.over.fam = hh.no.60.over * its$famr[,1]
  hh.no.60.over.fam.married = hh.no.60.over.fam * its$marstr[,2]
  hh.no.60.over.fam.other = hh.no.60.over.fam * its$marstr[,1]
  hh.no.60.over.fam.other.male.no.wife.present = hh.no.60.over.fam.other * its$hheadsexr[,1]
  hh.no.60.over.fam.other.female.no.husband.present = hh.no.60.over.fam.other * its$hheadsexr[,2]
  hh.no.60.over.nfam = hh.no.60.over * rowSums(its$famr[,2:3])

  pums.B11006 = cbind(hh,
                      hh.w.60.over,
                      hh.w.60.over.fam,
                      hh.w.60.over.fam.married,
                      hh.w.60.over.fam.other,
                      hh.w.60.over.fam.other.male.no.wife.present,
                      hh.w.60.over.fam.other.female.no.husband.present,
                      hh.w.60.over.nfam,
                      hh.no.60.over,
                      hh.no.60.over.fam,
                      hh.no.60.over.fam.married,
                      hh.no.60.over.fam.other,
                      hh.no.60.over.fam.other.male.no.wife.present,
                      hh.no.60.over.fam.other.female.no.husband.present,
                      hh.no.60.over.nfam)

  colnames(pums.B11006) <- paste0('B11006',sprintf('%03d',1:ncol(pums.B11006)))

  pums.B11006

}

B11012 <- function(pums){

  "
  B11012: Household Type by Tenure (Households)
  "

  its <- build_intermediates(pums, c('famr', 'marstr', 'tenr', 'hheadsexr'))

  # hh = rep(1, nrow(pums)) # Total 001 (old)
  hh = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  hh.fam = its$famr[,1] # family households 002
  hh.fam.married = hh.fam * its$marstr[,2] # married couple households 003
  hh.fam.married.own = hh.fam.married * its$tenr[,1]
  hh.fam.married.rent = hh.fam.married * its$tenr[,2]
  hh.fam.other = hh.fam * its$marstr[,1] # other family households 006
  hh.fam.other.male.no.wife.present = hh.fam.other * its$hheadsexr[,1] # male head, no wife present 007
  hh.fam.other.male.no.wife.present.own = hh.fam.other.male.no.wife.present * its$tenr[,1]
  hh.fam.other.male.no.wife.present.rent = hh.fam.other.male.no.wife.present * its$tenr[,2]
  hh.fam.other.female.no.husband.present = hh.fam.other * its$hheadsexr[,2] # female head, no husband present 010
  hh.fam.other.female.no.husband.present.own = hh.fam.other.female.no.husband.present * its$tenr[,1]
  hh.fam.other.female.no.husband.present.rent = hh.fam.other.female.no.husband.present * its$tenr[,2]
  hh.nfam = rowSums(its$famr[,-1]) # nonfamily households 013
  hh.nfam.own = hh.nfam * its$tenr[,1]
  hh.nfam.rent = hh.nfam * its$tenr[,2]

  pums.B11012 = cbind(hh,
                      hh.fam,
                      hh.fam.married,
                      hh.fam.married.own,
                      hh.fam.married.rent,
                      hh.fam.other,
                      hh.fam.other.male.no.wife.present,
                      hh.fam.other.male.no.wife.present.own,
                      hh.fam.other.male.no.wife.present.rent,
                      hh.fam.other.female.no.husband.present,
                      hh.fam.other.female.no.husband.present.own,
                      hh.fam.other.female.no.husband.present.rent,
                      hh.nfam,
                      hh.nfam.own,
                      hh.nfam.rent)

  colnames(pums.B11012) <- paste0('B11012',sprintf('%03d',1:ncol(pums.B11012)))

  pums.B11012

}

B11016 <- function(pums){

  "
  B11016: Household Type by Household Size (Households)
  "

  its <- build_intermediates(pums, c('famr', 'hhsizr'))

  # hh = rep(1, nrow(pums)) # Total 001 (old)
  hh = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  hh.fam = its$famr[,1] # family households 002
  hh.fam.p2 = hh.fam * its$hhsizr[,2]
  hh.fam.p3 = hh.fam * its$hhsizr[,3]
  hh.fam.p4 = hh.fam * its$hhsizr[,4]
  hh.fam.p5 = hh.fam * its$hhsizr[,5]
  hh.fam.p6 = hh.fam * its$hhsizr[,6]
  hh.fam.p7.or.more = hh.fam * its$hhsizr[,7]
  hh.nfam = rowSums(its$famr[,-1]) # nonfamily households 009
  hh.nfam.p1 = hh.nfam * its$hhsizr[,1]
  hh.nfam.p2 = hh.nfam * its$hhsizr[,2]
  hh.nfam.p3 = hh.nfam * its$hhsizr[,3]
  hh.nfam.p4 = hh.nfam * its$hhsizr[,4]
  hh.nfam.p5 = hh.nfam * its$hhsizr[,5]
  hh.nfam.p6 = hh.nfam * its$hhsizr[,6]
  hh.nfam.p7.or.more = hh.nfam * its$hhsizr[,7]

  pums.B11016 <- cbind(hh,
                      hh.fam,
                      hh.fam.p2,
                      hh.fam.p3,
                      hh.fam.p4,
                      hh.fam.p5,
                      hh.fam.p6,
                      hh.fam.p7.or.more,
                      hh.nfam,
                      hh.nfam.p1,
                      hh.nfam.p2,
                      hh.nfam.p3,
                      hh.nfam.p4,
                      hh.nfam.p5,
                      hh.nfam.p6,
                      hh.nfam.p7.or.more)

  colnames(pums.B11016) <- paste0('B11016',sprintf('%03d',1:ncol(pums.B11016)))

  pums.B11016

}

B19037 <- function(pums){

  "
  B19037: Age of Householder By Household Income in the Past 12 Months (Household)
  "

  its <- build_intermediates(pums, c('agehhincr', 'hhincr'))

  # hh = rep(1, nrow(pums)) # Total 001 (old)
  hh = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  hh.under.25 = its$agehhincr[,1] # Householder under 25 years 002
  hh.under.25.less.than.10k = hh.under.25 * its$hhincr[,1]
  hh.under.25.10k.14k = hh.under.25 * its$hhincr[,2]
  hh.under.25.15k.19k = hh.under.25 * its$hhincr[,3]
  hh.under.25.20k.24k = hh.under.25 * its$hhincr[,4]
  hh.under.25.25k.29k = hh.under.25 * its$hhincr[,5]
  hh.under.25.30k.34k = hh.under.25 * its$hhincr[,6]
  hh.under.25.35k.39k = hh.under.25 * its$hhincr[,7]
  hh.under.25.40k.44k = hh.under.25 * its$hhincr[,8]
  hh.under.25.45k.49k = hh.under.25 * its$hhincr[,9]
  hh.under.25.50k.59k = hh.under.25 * its$hhincr[,10]
  hh.under.25.60k.74k = hh.under.25 * its$hhincr[,11]
  hh.under.25.75k.99k = hh.under.25 * its$hhincr[,12]
  hh.under.25.100k.124k = hh.under.25 * its$hhincr[,13]
  hh.under.25.125k.149k = hh.under.25 * its$hhincr[,14]
  hh.under.25.150k.199k = hh.under.25 * its$hhincr[,15]
  hh.under.25.200k.more = hh.under.25 * its$hhincr[,16]
  hh.25.44 = its$agehhincr[,2] # Householder 25-44 years 019
  hh.25.44.less.than.10k = hh.25.44 * its$hhincr[,1]
  hh.25.44.10k.14k = hh.25.44 * its$hhincr[,2]
  hh.25.44.15k.19k = hh.25.44 * its$hhincr[,3]
  hh.25.44.20k.24k = hh.25.44 * its$hhincr[,4]
  hh.25.44.25k.29k = hh.25.44 * its$hhincr[,5]
  hh.25.44.30k.34k = hh.25.44 * its$hhincr[,6]
  hh.25.44.35k.39k = hh.25.44 * its$hhincr[,7]
  hh.25.44.40k.44k = hh.25.44 * its$hhincr[,8]
  hh.25.44.45k.49k = hh.25.44 * its$hhincr[,9]
  hh.25.44.50k.59k = hh.25.44 * its$hhincr[,10]
  hh.25.44.60k.74k = hh.25.44 * its$hhincr[,11]
  hh.25.44.75k.99k = hh.25.44 * its$hhincr[,12]
  hh.25.44.100k.124k = hh.25.44 * its$hhincr[,13]
  hh.25.44.125k.149k = hh.25.44 * its$hhincr[,14]
  hh.25.44.150k.199k = hh.25.44 * its$hhincr[,15]
  hh.25.44.200k.more = hh.25.44 * its$hhincr[,16]
  hh.45.64 = its$agehhincr[,3] # Householder 44-64 years 036
  hh.45.64.less.than.10k = hh.45.64 * its$hhincr[,1]
  hh.45.64.10k.14k = hh.45.64 * its$hhincr[,2]
  hh.45.64.15k.19k = hh.45.64 * its$hhincr[,3]
  hh.45.64.20k.24k = hh.45.64 * its$hhincr[,4]
  hh.45.64.25k.29k = hh.45.64 * its$hhincr[,5]
  hh.45.64.30k.34k = hh.45.64 * its$hhincr[,6]
  hh.45.64.35k.39k = hh.45.64 * its$hhincr[,7]
  hh.45.64.40k.44k = hh.45.64 * its$hhincr[,8]
  hh.45.64.45k.49k = hh.45.64 * its$hhincr[,9]
  hh.45.64.50k.59k = hh.45.64 * its$hhincr[,10]
  hh.45.64.60k.74k = hh.45.64 * its$hhincr[,11]
  hh.45.64.75k.99k = hh.45.64 * its$hhincr[,12]
  hh.45.64.100k.124k = hh.45.64 * its$hhincr[,13]
  hh.45.64.125k.149k = hh.45.64 * its$hhincr[,14]
  hh.45.64.150k.199k = hh.45.64 * its$hhincr[,15]
  hh.45.64.200k.more = hh.45.64 * its$hhincr[,16]
  hh.65.over = its$agehhincr[,4] # Householder 65 years or over 053
  hh.65.over.less.than.10k = hh.65.over * its$hhincr[,1]
  hh.65.over.10k.14k = hh.65.over * its$hhincr[,2]
  hh.65.over.15k.19k = hh.65.over * its$hhincr[,3]
  hh.65.over.20k.24k = hh.65.over * its$hhincr[,4]
  hh.65.over.25k.29k = hh.65.over * its$hhincr[,5]
  hh.65.over.30k.34k = hh.65.over * its$hhincr[,6]
  hh.65.over.35k.39k = hh.65.over * its$hhincr[,7]
  hh.65.over.40k.44k = hh.65.over * its$hhincr[,8]
  hh.65.over.45k.49k = hh.65.over * its$hhincr[,9]
  hh.65.over.50k.59k = hh.65.over * its$hhincr[,10]
  hh.65.over.60k.74k = hh.65.over * its$hhincr[,11]
  hh.65.over.75k.99k = hh.65.over * its$hhincr[,12]
  hh.65.over.100k.124k = hh.65.over * its$hhincr[,13]
  hh.65.over.125k.149k = hh.65.over * its$hhincr[,14]
  hh.65.over.150k.199k = hh.65.over * its$hhincr[,15]
  hh.65.over.200k.more = hh.65.over * its$hhincr[,16]

  pums.B19037 = cbind(hh,
                      hh.under.25,
                      hh.under.25.less.than.10k,
                      hh.under.25.10k.14k,
                      hh.under.25.15k.19k,
                      hh.under.25.20k.24k,
                      hh.under.25.25k.29k,
                      hh.under.25.30k.34k,
                      hh.under.25.35k.39k,
                      hh.under.25.40k.44k,
                      hh.under.25.45k.49k,
                      hh.under.25.50k.59k,
                      hh.under.25.60k.74k,
                      hh.under.25.75k.99k,
                      hh.under.25.100k.124k,
                      hh.under.25.125k.149k,
                      hh.under.25.150k.199k,
                      hh.under.25.200k.more,
                      hh.25.44,
                      hh.25.44.less.than.10k,
                      hh.25.44.10k.14k,
                      hh.25.44.15k.19k,
                      hh.25.44.20k.24k,
                      hh.25.44.25k.29k,
                      hh.25.44.30k.34k,
                      hh.25.44.35k.39k,
                      hh.25.44.40k.44k,
                      hh.25.44.45k.49k,
                      hh.25.44.50k.59k,
                      hh.25.44.60k.74k,
                      hh.25.44.75k.99k,
                      hh.25.44.100k.124k,
                      hh.25.44.125k.149k,
                      hh.25.44.150k.199k,
                      hh.25.44.200k.more,
                      hh.45.64,
                      hh.45.64.less.than.10k,
                      hh.45.64.10k.14k,
                      hh.45.64.15k.19k,
                      hh.45.64.20k.24k,
                      hh.45.64.25k.29k,
                      hh.45.64.30k.34k,
                      hh.45.64.35k.39k,
                      hh.45.64.40k.44k,
                      hh.45.64.45k.49k,
                      hh.45.64.50k.59k,
                      hh.45.64.60k.74k,
                      hh.45.64.75k.99k,
                      hh.45.64.100k.124k,
                      hh.45.64.125k.149k,
                      hh.45.64.150k.199k,
                      hh.45.64.200k.more,
                      hh.65.over,
                      hh.65.over.less.than.10k,
                      hh.65.over.10k.14k,
                      hh.65.over.15k.19k,
                      hh.65.over.20k.24k,
                      hh.65.over.25k.29k,
                      hh.65.over.30k.34k,
                      hh.65.over.35k.39k,
                      hh.65.over.40k.44k,
                      hh.65.over.45k.49k,
                      hh.65.over.50k.59k,
                      hh.65.over.60k.74k,
                      hh.65.over.75k.99k,
                      hh.65.over.100k.124k,
                      hh.65.over.125k.149k,
                      hh.65.over.150k.199k,
                      hh.65.over.200k.more)

  colnames(pums.B19037) <- paste0('B19037',sprintf('%03d',1:ncol(pums.B19037)))

  pums.B19037

}

B25003 <- function(pums){

  "
  B25003: Tenure by Race/Ethnicity of Householder (Households)
  "

  its <- build_intermediates(pums, c('hhracer', 'hhhispanr', 'tenr'))

  # occ.hu = rep(1, nrow(pums)) # Total 001 (old)
  occ.hu = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  white.occ.hu = its$hhracer[,1] # householder who is White alone A001
  white.occ.hu.own = white.occ.hu * its$tenr[,1]
  white.occ.hu.rent = white.occ.hu * its$tenr[,2]
  black.occ.hu = its$hhracer[,2] # householder who is Black alone B001
  black.occ.hu.own = black.occ.hu * its$tenr[,1]
  black.occ.hu.rent = black.occ.hu * its$tenr[,2]
  aiana.occ.hu = its$hhracer[,3] # householder who is American Indian/Alaska Native alone C001
  aiana.occ.hu.own = aiana.occ.hu * its$tenr[,1]
  aiana.occ.hu.rent = aiana.occ.hu * its$tenr[,2]
  asian.occ.hu = its$hhracer[,4] # householder who is Asian alone D001
  asian.occ.hu.own = asian.occ.hu * its$tenr[,1]
  asian.occ.hu.rent = asian.occ.hu * its$tenr[,2]
  nhopi.occ.hu = its$hhracer[,5] # householder who is Native Hawaiian/Other Pacific Islander alone E001
  nhopi.occ.hu.own = nhopi.occ.hu * its$tenr[,1]
  nhopi.occ.hu.rent = nhopi.occ.hu * its$tenr[,2]
  other.occ.hu = its$hhracer[,6] # householder who is Some Other Race alone F001
  other.occ.hu.own = other.occ.hu * its$tenr[,1]
  other.occ.hu.rent = other.occ.hu * its$tenr[,2]
  twomore.occ.hu = rowSums(its$hhracer[,7:9]) # householder who is Two or More Races G001
  twomore.occ.hu.own = twomore.occ.hu * its$tenr[,1]
  twomore.occ.hu.rent = twomore.occ.hu * its$tenr[,2]
  white.non.hispanic.occ.hu = its$hhracer[,1] * its$hhhispanr[,2] # householder who is White Non-Hispanic alone H001
  white.non.hispanic.occ.hu.own = white.non.hispanic.occ.hu * its$tenr[,1]
  white.non.hispanic.occ.hu.rent = white.non.hispanic.occ.hu * its$tenr[,2]
  hisp.occ.hu = its$hhhispanr[,1] # householder who is Hispanic/Latino I001
  hisp.occ.hu.own = hisp.occ.hu * its$tenr[,1]
  hisp.occ.hu.rent = hisp.occ.hu * its$tenr[,2]

  pums.B25003 <- cbind(white.occ.hu,
                      white.occ.hu.own,
                      white.occ.hu.rent,
                      black.occ.hu,
                      black.occ.hu.own,
                      black.occ.hu.rent,
                      aiana.occ.hu,
                      aiana.occ.hu.own,
                      aiana.occ.hu.rent,
                      asian.occ.hu,
                      asian.occ.hu.own,
                      asian.occ.hu.rent,
                      nhopi.occ.hu,
                      nhopi.occ.hu.own,
                      nhopi.occ.hu.rent,
                      other.occ.hu,
                      other.occ.hu.own,
                      other.occ.hu.rent,
                      twomore.occ.hu,
                      twomore.occ.hu.own,
                      twomore.occ.hu.rent,
                      white.non.hispanic.occ.hu,
                      white.non.hispanic.occ.hu.own,
                      white.non.hispanic.occ.hu.rent,
                      hisp.occ.hu,
                      hisp.occ.hu.own,
                      hisp.occ.hu.rent)

    pums.B25003 <- occ.hu * pums.B25003 # limit to occupied (non-GQ) units

    # generate column names
    names.B25003 <- as.vector(sapply(toupper(letters[1:9]),function(i){

      sub_name = unname(paste0('B25003',i))

      sapply(1:3, function(j){
        paste0(sub_name,sprintf('%03d',j))
      })

    }))

    colnames(pums.B25003) <- names.B25003

    pums.B25003

}

B25007 <- function(pums){

  "
  B25007: Tenure by Age of Householder (Housheholds)
  "

  its <- build_intermediates(pums, c('tenr', 'agetenr'))

  # occ.hu = rep(1, nrow(pums)) # Total 001 (old)
  occ.hu = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  occ.hu.own = its$tenr[,1] # Owner occupied 002
  occ.hu.own.15.24 = occ.hu.own * its$agetenr[,2]
  occ.hu.own.25.34 = occ.hu.own * its$agetenr[,3]
  occ.hu.own.35.44 = occ.hu.own * its$agetenr[,4]
  occ.hu.own.45.54 = occ.hu.own * its$agetenr[,5]
  occ.hu.own.55.59 = occ.hu.own * its$agetenr[,6]
  occ.hu.own.60.64 = occ.hu.own * its$agetenr[,7]
  occ.hu.own.65.74 = occ.hu.own * its$agetenr[,8]
  occ.hu.own.75.84 = occ.hu.own * its$agetenr[,9]
  occ.hu.own.85o = occ.hu.own * its$agetenr[,10]
  occ.hu.rent = its$tenr[,2] # Renter occupied 012
  occ.hu.rent.15.24 = occ.hu.rent * its$agetenr[,2]
  occ.hu.rent.25.34 = occ.hu.rent * its$agetenr[,3]
  occ.hu.rent.35.44 = occ.hu.rent * its$agetenr[,4]
  occ.hu.rent.45.54 = occ.hu.rent * its$agetenr[,5]
  occ.hu.rent.55.59 = occ.hu.rent * its$agetenr[,6]
  occ.hu.rent.60.64 = occ.hu.rent * its$agetenr[,7]
  occ.hu.rent.65.74 = occ.hu.rent * its$agetenr[,8]
  occ.hu.rent.75.84 = occ.hu.rent * its$agetenr[,9]
  occ.hu.rent.85o = occ.hu.rent * its$agetenr[,10]

  pums.B25007 <- cbind(occ.hu,
                      occ.hu.own,
                      occ.hu.own.15.24,
                      occ.hu.own.25.34,
                      occ.hu.own.35.44,
                      occ.hu.own.45.54,
                      occ.hu.own.55.59,
                      occ.hu.own.60.64,
                      occ.hu.own.65.74,
                      occ.hu.own.75.84,
                      occ.hu.own.85o,
                      occ.hu.rent,
                      occ.hu.rent.15.24,
                      occ.hu.rent.25.34,
                      occ.hu.rent.35.44,
                      occ.hu.rent.45.54,
                      occ.hu.rent.55.59,
                      occ.hu.rent.60.64,
                      occ.hu.rent.65.74,
                      occ.hu.rent.75.84,
                      occ.hu.rent.85o)

  pums.B25007 <- occ.hu * pums.B25007 # limit to occupied (non-GQ) units

  colnames(pums.B25007) <- paste0('B25007',sprintf('%03d',1:ncol(pums.B25007)))

  pums.B25007

}

B25032 <- function(pums){

  "
  B25032: Tenure by Units in Structure (Household)
  "

  its <- build_intermediates(pums, c('unitsr', 'tenr'))

  # occ.hu = rep(1, nrow(pums)) # Total 001 (old)
  occ.hu = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  occ.hu.own = its$tenr[,1] # Owner occupied 002
  occ.hu.own.u1.detached = occ.hu.own * its$unitsr[,3]
  occ.hu.own.u1.attached = occ.hu.own * its$unitsr[,4]
  occ.hu.own.u2 = occ.hu.own * its$unitsr[,5]
  occ.hu.own.u3.4 = occ.hu.own * its$unitsr[,6]
  occ.hu.own.u5.9 = occ.hu.own * its$unitsr[,7]
  occ.hu.own.u10.19 = occ.hu.own * its$unitsr[,8]
  occ.hu.own.u20.49 = occ.hu.own * its$unitsr[,9]
  occ.hu.own.u50.more = occ.hu.own * its$unitsr[,10]
  occ.hu.own.mobile.home = occ.hu.own * its$unitsr[,1]
  occ.hu.own.boat.rv.van.etc = occ.hu.own * its$unitsr[,2]
  occ.hu.rent = its$tenr[,2] # Renter occupied 013
  occ.hu.rent.u1.detached = occ.hu.rent * its$unitsr[,3]
  occ.hu.rent.u1.attached = occ.hu.rent * its$unitsr[,4]
  occ.hu.rent.u2 = occ.hu.rent * its$unitsr[,5]
  occ.hu.rent.u3.4 = occ.hu.rent * its$unitsr[,6]
  occ.hu.rent.u5.9 = occ.hu.rent * its$unitsr[,7]
  occ.hu.rent.u10.19 = occ.hu.rent * its$unitsr[,8]
  occ.hu.rent.u20.49 = occ.hu.rent * its$unitsr[,9]
  occ.hu.rent.u50.more = occ.hu.rent * its$unitsr[,10]
  occ.hu.rent.mobile.home = occ.hu.rent * its$unitsr[,1]
  occ.hu.rent.boat.rv.van.etc = occ.hu.rent * its$unitsr[,2]

  pums.B25032 <- cbind(occ.hu,
                      occ.hu.own,
                      occ.hu.own.u1.detached,
                      occ.hu.own.u1.attached,
                      occ.hu.own.u2,
                      occ.hu.own.u3.4,
                      occ.hu.own.u5.9,
                      occ.hu.own.u10.19,
                      occ.hu.own.u20.49,
                      occ.hu.own.u50.more,
                      occ.hu.own.mobile.home,
                      occ.hu.own.boat.rv.van.etc,
                      occ.hu.rent,
                      occ.hu.rent.u1.detached,
                      occ.hu.rent.u1.attached,
                      occ.hu.rent.u2,
                      occ.hu.rent.u3.4,
                      occ.hu.rent.u5.9,
                      occ.hu.rent.u10.19,
                      occ.hu.rent.u20.49,
                      occ.hu.rent.u50.more,
                      occ.hu.rent.mobile.home,
                      occ.hu.rent.boat.rv.van.etc)

  pums.B25032 <- occ.hu * pums.B25032 # limit to occupied (non-GQ) units

  colnames(pums.B25032) <- paste0('B25032',sprintf('%03d',1:ncol(pums.B25032)))

  pums.B25032

}

B25036 <- function(pums){

  "
  B25036: Tenure by Year Structure Built (Household)
  "

  its <- build_intermediates(pums, c('builtr', 'tenr'))

  # occ.hu = rep(1, nrow(pums)) # Total 001 (old)
  occ.hu = ifelse(pums$GQ %in% c(1, 2), 1, 0) # Total 001
  occ.hu.own = its$tenr[,1] # Owner occupied 002
  occ.hu.own.built.2010.later = occ.hu.own * its$builtr[,10]
  occ.hu.own.built.2000.2009 = occ.hu.own * its$builtr[,9]
  occ.hu.own.built.1990.1999 = occ.hu.own * its$builtr[,7]
  occ.hu.own.built.1980.1989 = occ.hu.own * its$builtr[,6]
  occ.hu.own.built.1970.1979 = occ.hu.own * its$builtr[,5]
  occ.hu.own.built.1960.1969 = occ.hu.own * its$builtr[,4]
  occ.hu.own.built.1950.1959 = occ.hu.own * its$builtr[,3]
  occ.hu.own.built.1940.1949 = occ.hu.own * its$builtr[,2]
  occ.hu.own.built.1939.earlier = occ.hu.own * its$builtr[,1]
  occ.hu.rent = its$tenr[,2] # Renter occupied 012
  occ.hu.rent.built.2010.later = occ.hu.rent * its$builtr[,10]
  occ.hu.rent.built.2000.2009 = occ.hu.rent * its$builtr[,9]
  occ.hu.rent.built.1990.1999 = occ.hu.rent * its$builtr[,7]
  occ.hu.rent.built.1980.1989 = occ.hu.rent * its$builtr[,6]
  occ.hu.rent.built.1970.1979 = occ.hu.rent * its$builtr[,5]
  occ.hu.rent.built.1960.1969 = occ.hu.rent * its$builtr[,4]
  occ.hu.rent.built.1950.1959 = occ.hu.rent * its$builtr[,3]
  occ.hu.rent.built.1940.1949 = occ.hu.rent * its$builtr[,2]
  occ.hu.rent.built.1939.earlier = occ.hu.rent * its$builtr[,1]

  pums.B25036 <- cbind(occ.hu,
                      occ.hu.own,
                      occ.hu.own.built.2010.later,
                      occ.hu.own.built.2000.2009,
                      occ.hu.own.built.1990.1999,
                      occ.hu.own.built.1980.1989,
                      occ.hu.own.built.1970.1979,
                      occ.hu.own.built.1960.1969,
                      occ.hu.own.built.1950.1959,
                      occ.hu.own.built.1940.1949,
                      occ.hu.own.built.1939.earlier,
                      occ.hu.rent,
                      occ.hu.rent.built.2010.later,
                      occ.hu.rent.built.2000.2009,
                      occ.hu.rent.built.1990.1999,
                      occ.hu.rent.built.1980.1989,
                      occ.hu.rent.built.1970.1979,
                      occ.hu.rent.built.1960.1969,
                      occ.hu.rent.built.1950.1959,
                      occ.hu.rent.built.1940.1949,
                      occ.hu.rent.built.1939.earlier)

  pums.B25036 <- occ.hu * pums.B25036 # limit to occupied (non-GQ) units

  colnames(pums.B25036) <- paste0('B25036',sprintf('%03d',1:ncol(pums.B25036)))

  pums.B25036

}
