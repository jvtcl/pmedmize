"
Definitions for intermediate variables to build individual-level
P-MEDM constraints.
"

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

sexr <- function(pums){

  "
  Person: Sex
  "

  v <- ifelse(pums$SEX == 1, 'Male', 'Female')

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

hispanr <- function(pums){

  "
  Person: Hispanic/Latino Ethnicity
  "

  v <- ifelse(pums$HISPAN > 0, 'Hispanic/Latino', 'Not Hispanic/Latino')

  model.matrix(~v - 1)

}
