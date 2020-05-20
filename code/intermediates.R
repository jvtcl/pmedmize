"
Definitions for intermediate variables to build individual-level
P-MEDM constraints.
"

assign_householder_item_level <- function(pums, v){

  "
  Helper function. Assigns the item level of the
  householder to all members of the household
  (for building household-level intermediates).
  "

  v <- lapply(split(v, pums$SERIAL), function(s){

    rep(s[1], length(s))

  })

  factor(unlist(v))

}

ager <- function(pums){

  "
  Person: Age
  "

  age.brks <- c(0,5,10,15,18,20,21,22,25,30,35,40,45,50,55,60,62,65,67,
     70,75,80,85,Inf)

  age.labels<-c(paste(age.brks [1:22], c(age.brks[2:23]) - 1, sep=' - '), '85+')

  v <- cut(
    as.numeric(pums$AGE),
    breaks=age.brks,
     include.lowest=TRUE,
     right=FALSE,
     labels=age.labels
   )

   model.matrix(~v - 1)

}

agecitizenr <- function(pums){

  "
  Person: Age (Citizenship Status definitions)
  "

  age.brks.citizen <- c(-Inf,5,18,Inf)

  age.citizen.labels <- c('5under', '5-17', '18+')

  v <- cut(
          as.numeric(pums$AGE),
          breaks = age.brks.citizen,
          include.lowest = TRUE,
          right = FALSE,
          labels = age.citizen.labels)

  model.matrix(~v - 1)

}

agehhincr <- function(pums){

  "
  Household: Householder Age (Income Definitions)
  "

  age.brks.hhinc=c(0,25,45,65,Inf)
  age.hhinc.labels=c(paste(age.brks.hhinc[1:3],c(age.brks.hhinc[2:4])-1,sep='-'),'65+')

  v <- cut(as.numeric(pums$AGE),
           breaks = age.brks.hhinc,
           include.lowest = TRUE,
           right = FALSE,
           labels = age.hhinc.labels)

  v <- assign_householder_item_level(pums, v)

  model.matrix(~v - 1)

}

agepovr <- function(pums){

  "
  Person: Age (Poverty Status definitions)
  "

  age.brks.pov <- c(0,5,6,12,15,16,18,25,35,45,55,65,75,Inf)

  age.pov.labels <- c(paste(age.brks.pov[1:12],
                 c(age.brks.pov[2:13])-1, sep='-'), '75+')

  v <- cut(
          as.numeric(pums$AGE),
          breaks = age.brks.pov,
          include.lowest = TRUE,
          right = FALSE,
          labels = age.pov.labels)

  model.matrix(~v - 1)

}

agetenr <- function(pums){

  "
  Household: Householder Age (Tenure Definitions)
  "

  age.brks.tenure <- c(0,15,25,35,45,55,60,65,75,85,Inf)
  age.tenure.labels<-c(paste(age.brks.tenure[1:9],c(age.brks.tenure[2:10])-1,sep='-'),'85+')

  v <- cut(as.numeric(pums$AGE),
           breaks = age.brks.tenure,
           include.lowest = TRUE,
           right = FALSE,
           labels = age.tenure.labels)

  v <- assign_householder_item_level(pums, v)

  model.matrix(~v - 1)

}

builtr <- function(pums){

  "
  Household: Dwelling Built Year
  "

  built_key <- read.csv('data/PUMS_BUILTYR2.csv')
  v <- factor(built_key$label[match(pums$BUILTYR2,built_key$code)],
                 levels = unique(built_key$label))

  model.matrix(~v - 1)[,-1] # omit `gq` column (placeholder)

}

citizenr <- function(pums){

  "
  Person: Citizen Status
  1: US citizen, 2: naturalized, 3: non citizen
  "

  v <- factor(ifelse(pums$CITIZEN<2,1,
                              ifelse(pums$CITIZEN==2,2,3)))

  model.matrix(~v - 1)

}

eldr <- function(pums){

  "
  Household: Presence of People Age 60 and Over
  "

  v <- factor(unlist(sapply(unique(pums$SERIAL), function(s){
    as = pums$AGE[pums$SERIAL == s]
    elders = ifelse(any(as >= 60),1,0)
    rep(elders,length(as))
  })))

  model.matrix(~v - 1)

}

famr <- function(pums){

  "
  Person: In Family Household
  "

  v <- unlist(sapply(unique(pums$SERIAL),function(s){

    fs=pums$FAMSIZE[pums$SERIAL == s][1]

    # If multiple members of the household, discard the head (coded 1)
    # If only one member of the household, recode the head as 'living alone'
    # (1 -> 99)
    fr=pums$RELATE[pums$SERIAL==s]
    if(length(fr)>1){
      fr=fr[-1]
    }else{
      fr=99
    }

    # If some or all other household members are related to head, family
    # household (1)
    # If no other household members are related to head, nonfamily household,
    # not living alone (2)
    # If head is only household member (coded 99), lives alone (3)
    famtype=ifelse(min(fr)<11,1,
                   ifelse(min(fr)>=11 & max(fr)<99,2,3))
    if(max(fr)<99){
      rep(famtype,length(fr)+1)
    }else{
      famtype
    }
  }))

  v[pums$GQ > 1] <- 4 # code group quarters pop as 4
  v <- factor(v)

  model.matrix(~v - 1)

}

hheadr <- function(pums){

  "
  Person: Head of Household (yes/no)
  "

  v <- factor(ifelse(pums$RELATED == 101 & pums$GQ < 2, 1, 0))

  model.matrix(~v - 1)

}

hhincr <- function(pums){

  "
  Household: Income
  "

  hhinc.brks=c(0,10,15,20,25,30,35,40,45,50,60,75,100,125,150,200,Inf)
  hhinc.labels=c(paste0(paste(hhinc.brks[1:15],c(hhinc.brks[2:16])-0.1,sep='-'),'k'),'200k+')

  # safeguard - treat negative income as 0 income
  HHINCOME.safe = pums$HHINCOME
  HHINCOME.safe[HHINCOME.safe < 0] = 0

  v <- cut(as.numeric(HHINCOME.safe),
      breaks=(1000*hhinc.brks),
      include.lowest=TRUE,
      right=FALSE,
      labels=hhinc.labels)

  model.matrix(~v - 1)

}

hheadsexr <- function(pums){

  "
  Person: Sex of Household Head
  "

  v <- factor(sapply(pums$SERIAL,function(s){
    pums$SEX[pums$SERIAL==s][1]
  }),labels=c('Male', 'Female'))

  model.matrix(~v - 1)

}

hhracer <- function(pums){

  "
  Household: Race of Householder
  "

  race.brks <- c(-Inf,2,3,4,5,6,7,8,9,Inf)

  race.labels <-c('White alone',
                 'Black or African American alone',
                 'American Indian and Alaska Native alone',
                 'Asian alone',
                 'Native Hawaiian and Other Pacific Islander alone',
                 'Some other race alone',
                 'Two or more races',
                 'Two races including Some other race',
                 'Two races excluding Some other race, and three or more races')

  v <- cut(pums$RACE,
           breaks=race.brks,
           labels=race.labels,
           include.lowest=TRUE,
           right=FALSE)

  v <- assign_householder_item_level(pums, v)

  model.matrix(~v - 1)

}

hhhispanr <- function(pums){

  "
  Household: Hispanic/Latino Ethnicity of Householder
  "

  v <- factor(ifelse(pums$HISPAN>0,'Hispanic/Latino','Not Hispanic/Latino'))

  v <- assign_householder_item_level(pums, v)

  model.matrix(~v - 1)

}

hhsizr <- function(pums){

  hhsize <- lapply(split(pums, pums$SERIAL), function(s){

    rep(nrow(s), nrow(s))

  })

  hhsize <- unlist(hhsize)

  v <- factor(ifelse(hhsize >= 7, '7+', hhsize),
              levels=c(as.character(seq(1, 6, by = 1)), '7+'))

  model.matrix(~v - 1)


}

hispanr <- function(pums){

  "
  Person: Hispanic/Latino Ethnicity
  "

  v <- ifelse(pums$HISPAN > 0, 'Hispanic/Latino', 'Not Hispanic/Latino')

  model.matrix(~v - 1)

}

householdr <- function(pums){

  "
  Person: In Household (vs. Group Quarters)
  "

  v <- factor(ifelse(pums$GQ<3,1,0))

  model.matrix(~v - 1)

}

langr <- function(pums){

  "
  Person: Language Spoken at Home

  1: English, 2: Spanish, 3: Other, 0: NA (age under 5)
  "

  v <- factor(with(pums,
               ifelse(LANGUAGE == 0, 0, 1) *
                 ifelse(LANGUAGE == 01, 1,
                        ifelse(LANGUAGE == 12, 2, 3))))

  model.matrix(~v - 1)

}

marstr <- function(pums){

  "
  Person: Marital Status of Household Head
  "

  v <- factor(unlist(sapply(unique(pums$SERIAL),function(s){
    sm=pums$MARST[pums$SERIAL==s]
    head_spouse_present = sm[1]
    mar_hh=ifelse(head_spouse_present==1,1,0)
    rep(mar_hh,length(sm))
  })))

  model.matrix(~v - 1)

}

minr <- function(pums){

  "
  Household: Presence of People Under Age 18
  "

  v <- factor(unlist(sapply(unique(pums$SERIAL), function(s){
    as = pums$AGE[pums$SERIAL == s]
    minors = ifelse(any(as < 18), 1, 0)
    rep(minors, length(as))
  })))

  model.matrix(~v - 1)

}

povr <- function(pums){

  "
  Person: Poverty Status
  "

  v <- factor(
          ifelse(pums$POVERTY < 100 & pums$POVERTY > 0,'Below_Pov',
            ifelse(pums$POVERTY >= 100, 'Above_Pov', 'Undetermined')
          )
        )

  model.matrix(~v - 1)


}

racer <- function(pums){

  "
  Person: Race
  "

  race.brks <- c(-Inf,2,3,4,5,6,7,8,9,Inf)

  race.labels <-c('White alone',
                 'Black or African American alone',
                 'American Indian and Alaska Native alone',
                 'Asian alone',
                 'Native Hawaiian and Other Pacific Islander alone',
                 'Some other race alone',
                 'Two or more races',
                 'Two races including Some other race',
                 'Two races excluding Some other race, and three or more races')

  v <- cut(pums$RACE,
           breaks=race.brks,
           labels=race.labels,
           include.lowest=TRUE,
           right=FALSE)

   model.matrix(~v - 1)

}

relater <- function(pums){

  "
  Person: Relation to head of household
  "

  v <- factor(ifelse(pums$RELATE < 11, 1, 0))

  model.matrix(~v - 1)

}

relatedr <- function(pums){

  "
  Person: Detailed relation to head of household
  "

  # metadata
  related <- read.csv('data/PUMS_RELATED.csv', stringsAsFactors = F)[,c('code','label')]

  v <- factor(related$label[match(pums$RELATED,related$code)],
              levels=related$label[!is.na(related$code)])

  model.matrix(~v - 1)

}

sexr <- function(pums){

  "
  Person: Sex
  "

  v <- ifelse(pums$SEX == 1, 'Male', 'Female')

  model.matrix(~v - 1)

}

speakengr <- function(pums){

  "
  Person: English Ability

  1: speaks only English, 2: speaks English 'very well',
  3: less than 'very well', 0: NA or blank (age under 5)
  "

  v <- factor(with(pums,
                   ifelse(SPEAKENG == 0, 0, 1) *
                     ifelse(SPEAKENG == 3, 1,
                            ifelse(SPEAKENG == 4, 2, 3))))

  model.matrix(~v - 1)

}

tenr <- function(pums){

  "
  Household: Tenure
  "

  v <- factor(with(pums, ifelse(OWNERSHP==1, 'Own', 'Rent')))

  model.matrix(~v - 1)

}

unitsr <- function(pums){

  "
  Household: Units in Structure
  "

  units_key <- read.csv('data/PUMS_UNITSSTR.csv')
  v <- factor(units_key$label[match(pums$UNITSSTR, units_key$code)],
              levels = units_key$label)

  model.matrix(~v - 1)[,-1] # omit `gq` column (placeholder)

}


#####

temp <- function(pums){

  "
  "

  v <- NA

  model.matrix(~v - 1)

}
