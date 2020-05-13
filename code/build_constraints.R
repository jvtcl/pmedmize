library(plyr)

data_path = paste(dirname(dirname(sys.frame(1)$ofile)), 'data/', sep = '/')

assign_person_ids=function(pums){
  
  # assign serial numbers to rownames
  unlist(sapply(unique(pums$SERIAL),function(s){
    ts=pums$SERIAL[pums$SERIAL==s]
    if(length(ts)>1){
      paste0(s,letters[1:length(ts)])
    }else{
      s
    }
  }))

}

build_data_person=function(pums, parent_unit, child_unit){
  
  #### Setup: Define Breaks ####
  cat('Setup: Define Breaks','\n')
  age.brks <- c(0,5,10,15,18,20,21,22,25,30,35,40,45,50,55,60,62,65,67, 70, 75,80,85,Inf)
  age.brks.pov=c(0,5,6,12,15,16,18,25,35,45,55,65,75,Inf)
  age.brks.citizen=c(-Inf,5,18,Inf)
  race.brks=c(-Inf,2,3,4,5,6,7,8,9,Inf)
  
  #### Setup: Define Labels ####
  age.labels<-c(paste(age.brks[1:22],c(age.brks[2:23])-1,sep='-'),'85+')
  age.pov.labels=c(paste(age.brks.pov[1:12],c(age.brks.pov[2:13])-1,sep='-'),'75+')
  age.citizen.labels=c(c('5under','5-17','18+'))
  race.labels<-c('White alone', 'Black or African American alone','American Indian and Alaska Native alone',
                 'Asian alone','Native Hawaiian and Other Pacific Islander alone','Some other race alone',
                 'Two or more races','Two races including Some other race',
                 'Two races excluding Some other race, and three or more races')
  # race.labels=rip.metadata.from.census.reporter('B02001')$colNames[-1]
  sex.labels<-c("Male","Female")
  
  #### Setup: Reclassify Variables ####
  cat('Setup: Reclassify Variables','\n')
  pums$ager <-cut(as.numeric(pums$AGE),breaks=age.brks, include.lowest=TRUE, right=FALSE,labels=age.labels)
  pums$agepovr <-cut(as.numeric(pums$AGE),breaks=age.brks.pov, include.lowest=TRUE, right=FALSE,labels=age.pov.labels)
  pums$agecitizenr <-cut(as.numeric(pums$AGE),breaks=age.brks.citizen, include.lowest=TRUE, right=FALSE,labels=age.citizen.labels)
  pums$racer <- cut(pums$RACE, breaks=race.brks, labels=race.labels,inclue.lowest=TRUE,right=FALSE)
  pums$sexr <- factor(pums[['SEX']], 1:2, labels=sex.labels)
  pums$hispanr=factor(ifelse(pums$HISPAN>0,'Hispanic/Latino','Not Hispanic/Latino'))
  pums$citizenr=factor(ifelse(pums$CITIZEN<2,1,
                              ifelse(pums$CITIZEN==2,2,3))) # citizen status: US citizen, naturalized, non citizen
  
  # Poverty Status #
  cat('...Poverty Status','\n')
  # pums$povr=factor(ifelse(as.numeric(pums$POVERTY)<100,'Below_Pov','Above_Pov'))
  pums$povr=factor(ifelse(pums$POVERTY<100 & pums$POVERTY>0,'Below_Pov',
                          ifelse(pums$POVERTY>=100,'Above_Pov','Undetermined')))
  
  # Language #
  cat('...Language','\n')
  # 1: English, 2: Spanish, 3: Other, 0: NA or blank
  pums$langr=factor(with(pums,
                         ifelse(LANGUAGE==0,0,1)*
                           ifelse(LANGUAGE==01,1,
                                  ifelse(LANGUAGE==12,2,3))))
  
  # English proficiency #
  cat('...English proficiency','\n')
  # 1: speaks only English, 2: speaks English "very well",
  # 3: less than 'very well', 0: NA or blank
  pums$speakengr=factor(with(pums,
                             ifelse(SPEAKENG==0,0,1)*
                               ifelse(SPEAKENG==3,1,
                                      ifelse(SPEAKENG==4,2,3))))
  
  pums$householdr=factor(ifelse(pums$GQ<3,1,0)) # households (vs. group quarters)
  pums$nfamr=with(pums,ifelse(FAMSIZE==1 | (FAMSIZE>=2 & NCOUPLES>=1 & MARST>=3 & NCHILD==0),1,0))
  # pums$aloner=factor(ifelse(pums$FAMSIZE==1,1,0)) # lives alone
  # pums$kidsr=factor(ifelse(pums$NCHILD>=1,1,0)) # has children
  
  # household marital status #
  cat('...Household marital status','\n')
  # ### ORIGINAL ###
  # pums$marstr=factor(unlist(sapply(unique(pums$SERIAL),function(s){
  #   # cat(which(pums$SERIAL==s))
  #   sm=pums$MARST[pums$SERIAL==s]
  #   # mar_hh=ifelse(sm[1]<3,1,0)
  #   # mar_hh=ifelse(sm[1]<2,1,0)
  #   mar_hh=ifelse(any(sm<3),1,0)
  #   rep(mar_hh,length(sm))
  # })))
  ### REVISED ###
  # household marital status #
  cat('...Household marital status','\n')
  pums$marstr=factor(unlist(sapply(unique(pums$SERIAL),function(s){
    sm=pums$MARST[pums$SERIAL==s]
    head_spouse_present = sm[1]
    mar_hh=ifelse(head_spouse_present==1,1,0)
    rep(mar_hh,length(sm))
  })))
  
  
  
  # sex of household head #
  cat('...Sex of household head','\n')
  pums$hheadsexr=factor(sapply(pums$SERIAL,function(s){ # by first entry serial no
    pums$SEX[pums$SERIAL==s][1]
  }),labels=sex.labels)
  # pums$hheadsexr=factor(unlist(sapply(unique(pums$SERIAL),function(s){ # by first entry serial no:famunit
  #   ssex=pums$SEX[pums$SERIAL==s]
  #   if(length(ssex)>1){
  #     sfamunit=pums$FAMUNIT[pums$SERIAL==s]
  #     if(length(unique(sfamunit))>1){
  #       unlist(sapply(unique(sfamunit),function(sf){
  #         rep(ssex[sfamunit==sf][1],length(sfamunit[sfamunit==sf]))
  #       }))
  #     }else{
  #       rep(ssex[1],length(ssex))
  #     }
  #   }else{
  #     ssex
  #   }
  # })),labels=sex.labels)
  
  
  
  # relation to head of household
  cat('...relation to head of household','\n')
  pums$relater=factor(ifelse(pums$RELATE<11,1,0))
  # pums$relater=factor(ifelse(pums$RELATE<11 | pums$RELATED==1114,1,0)) # include unmarried partners
  
  # detailed relation to head of household
  cat('...detailed relation to head of household','\n')
  # related=read.csv('data/PUMS_RELATED.csv',stringsAsFactors = F)[,c('code','label')]
  related=read.csv(paste0(data_path, 'PUMS_RELATED.csv'),stringsAsFactors = F)[,c('code','label')]
  # pums$relatedr=factor(related$label[match(pums$RELATED,related$code)])
  pums$relatedr=factor(related$label[match(pums$RELATED,related$code)],levels=related$label[!is.na(related$code)])
  
  # household head #
  cat('...ID head of household','\n')
  # hheadr=factor(ifelse(pums$RELATED==101 & pums$GQ<3,1,0))
  hheadr=factor(ifelse(pums$RELATED==101 & pums$GQ<2,1,0))
  
  
  ## family households
  cat('...id family households','\n')
  # pums$famr=factor(unlist(sapply(unique(pums$SERIAL),function(s){
  #   fs=pums$FAMSIZE[pums$SERIAL==s]
  #   blah=ifelse(fs[1]>1,1,0)
  #   rep(blah,length(fs))
  # })))
  
  fam=unlist(sapply(unique(pums$SERIAL),function(s){
    fs=pums$FAMSIZE[pums$SERIAL==s][1]
    # If multiple members of the household, discard the head (coded 1)
    # If only one member of the household, recode the head as 'living alone' (1 -> 99)
    fr=pums$RELATE[pums$SERIAL==s]
    if(length(fr)>1){
      fr=fr[-1]
    }else{
      fr=99
    }
    
    # If some or all other household members are related to head, family household (1)
    # If no other household members are related to head, nonfamily household, not living alone (2)
    # If head is only household member (coded 99), lives alone (3)
    famtype=ifelse(min(fr)<11,1,
                   ifelse(min(fr)>=11 & max(fr)<99,2,3))
    if(max(fr)<99){
      rep(famtype,length(fr)+1)
    }else{
      famtype
    }
  }))
  
  # fam=unlist(sapply(unique(pums$SERIAL),function(s){ # alt1, by famunit size
  #   fs=pums$FAMSIZE[pums$SERIAL==s]
  #   if(length(fs)>1){
  #     ifelse(fs>1,1,2)
  #   }else{
  #     3
  #   }
  # }))
  
  # # # alt2, all household members
  # # this seems to line up best with published Denver living arrangement ests
  # fam=unlist(sapply(unique(pums$SERIAL),function(s){
  #   fs=pums$FAMSIZE[pums$SERIAL==s]
  #   if(length(fs)>1){
  #     blah=ifelse(min(fs)>1,1,2)
  #     rep(blah,length(fs))
  #   }else{
  #     3
  #   }
  # }))
  
  fam[pums$GQ>1]=4 # code group quarters pop as 4
  pums$famr=factor(fam)
  
  #### Setup: Build Dummy Variables ####
  cat('Setup: Build Dummy Variables','\n')
  pums.sexr=with(pums,model.matrix(~sexr-1)) # sex
  pums.ager=with(pums,model.matrix(~ager-1)) # age
  pums.agepovr=with(pums,model.matrix(~agepovr-1)) # age, poverty breaks
  pums.agecitizenr=with(pums,model.matrix(~agecitizenr-1)) # age, citizen/language breaks
  pums.racer=with(pums,model.matrix(~racer-1)) # race
  pums.hispanr=with(pums,model.matrix(~hispanr-1)) # hispanic/latino
  pums.povr=with(pums,model.matrix(~povr-1)) # poverty
  pums.speakengr=with(pums,model.matrix(~speakengr-1)) # english proficiency
  pums.langr=with(pums,model.matrix(~langr-1)) # language
  pums.citizenr=with(pums,model.matrix(~citizenr-1)) # citizen status
  pums.famr=with(pums,model.matrix(~famr-1)) # in family household
  pums.marstr=with(pums,model.matrix(~marstr-1)) # in married household
  pums.householdr=with(pums,model.matrix(~householdr-1)) # in household
  pums.hheadr=with(pums,model.matrix(~hheadr-1)) # head of household
  pums.hheadsexr=with(pums,model.matrix(~hheadsexr-1)) # sex, head of household
  pums.relater=with(pums,model.matrix(~relater-1)) # related to head of household
  pums.relatedr=with(pums,model.matrix(~relatedr-1)) # detailed role in household
  
  ## build variables to match summary levels
  
  #### B01001: Sex by Age ####
  cat('B01001: Sex by Age','\n')
  pums.b01001 <- cbind(rowSums(pums.sexr),
                       pums.sexr[,1],
                       pums.ager*pums.sexr[,1],
                       pums.sexr[,2],
                       pums.ager*pums.sexr[,2])
  # colnames(pums.b01001)=rip.metadata.from.census.reporter('B01001')$codes
  colnames(pums.b01001)=paste0('B01001',sprintf('%03d',1:ncol(pums.b01001)))
  
  # #### B02001: Race ####
  # cat('B02001: Race','\n')
  # pums.b02001=cbind(rowSums(pums.racer),pums.racer)
  # # colnames(pums.b02001)=rip.metadata.from.census.reporter('B02001')$codes
  # colnames(pums.b02001)=paste0('B02001',sprintf('%03d',1:ncol(pums.b02001)))
  # 
  # # consolidate 'two or more races' levels
  # pums.b02001[,which(colnames(pums.b02001)=='B02001008')]=
  #   rowSums(pums.b02001[,which(colnames(pums.b02001)=='B02001009')
  #                       :which(colnames(pums.b02001)=='B02001010')])
  
  
  #### B03002: Race by Hispanic/Latino Ethnicity ####
  cat('B03002: Race by Hispanic/Latino Ethnicity','\n')
  pums.b03002=cbind(rowSums(pums.racer),
                    pums.hispanr[,2],
                    pums.hispanr[,2]*pums.racer,
                    pums.hispanr[,1],
                    pums.hispanr[,1]*pums.racer)
  # colnames(pums.b03002)=rip.metadata.from.census.reporter('B03002')$codes
  colnames(pums.b03002)=paste0('B03002',sprintf('%03d',1:ncol(pums.b03002)))
  
  # consolidate 'two or more races' levels
  pums.b03002[,which(colnames(pums.b03002)=='B03002009')]=
    rowSums(pums.b03002[,which(colnames(pums.b03002)=='B03002009')
                        :which(colnames(pums.b03002)=='B03002011')])
  pums.b03002[,which(colnames(pums.b03002)=='B03002019')]=
    rowSums(pums.b03002[,which(colnames(pums.b03002)=='B03002019')
                        :which(colnames(pums.b03002)=='B03002021')])
  
  #### B17001: Poverty Status by Sex by Age ####
  if(!'bg' %in% c(parent_unit, child_unit)){ # not available at the block group level
    cat('B17001: Poverty Status by Sex by Age','\n')
    pums.b17001=cbind(rowSums(pums.povr[,1:2]),
                      pums.povr[,2],
                      pums.povr[,2]*pums.sexr[,1],
                      pums.povr[,2]*pums.sexr[,1]*pums.agepovr,
                      pums.povr[,2]*pums.sexr[,2],
                      pums.povr[,2]*pums.sexr[,2]*pums.agepovr,
                      pums.povr[,1],
                      pums.povr[,1]*pums.sexr[,1],
                      pums.povr[,1]*pums.sexr[,1]*pums.agepovr,
                      pums.povr[,1]*pums.sexr[,2],
                      pums.povr[,1]*pums.sexr[,2]*pums.agepovr)
    # colnames(pums.b17001)=rip.metadata.from.census.reporter('B17001')$codes
    colnames(pums.b17001)=paste0('B17001',sprintf('%03d',1:ncol(pums.b17001)))
  }
  
  #### B17021: Poverty Status of Individuals by Living Arrangement ####
  
  ## familiy households by marital status
  # col1: married vs. other family household
  # col2: other family household by sex of household head by relation to household head
  fam.hh=pums.famr[,1]
  fam.marst=(fam.hh*pums.marstr)[,c(2,1)] # flip `marstr` since married households listed first
  fam.married=fam.marst[,1] # married couple families
  fam.married.relate=fam.married*pums.relater[,c(2,1)]
  fam.other=fam.marst[,2] # other families
  fam.other.sexr=fam.other*pums.hheadsexr # other family households by sex
  fam.other.male=fam.other.sexr[,1] # other family households, male householder
  fam.other.relate.male=fam.other.male*pums.relater[,c(2,1)] # other households ,male head, relatives/nonrelatives
  fam.other.female=fam.other.sexr[,2] # other family households, female householder
  fam.other.relate.female=fam.other.female*pums.relater[,c(2,1)] # other households female head, relatives/nonrelatives
  
  ## nonfamily households by living arrangement
  nfam.other.living.arrangement=rowSums(pums.famr[,-1])
  nfam.hh=rowSums(pums.famr[,2:3]) # this isn't included in data, but used to compute sublevels
  # nfam.householder=nfam.hh*pums.hheadr[,2] # nonfamily householder
  # nfam.householder=nfam.hh*pums.relatedr[,6] # nonfamily householder
  nfam.householder=nfam.hh # nonfamily householder
  nfam.alone=nfam.householder*pums.famr[,3] # householder living alone
  nfam.not.alone=nfam.householder*pums.famr[,2] # householder not living alone
  # other.living.arrangement=nfam.other.living.arrangement*pums.householdr[,1] # other living arrangement
  # other.living.arrangement=nfam.other.living.arrangement*pums.famr[,4] # other living arrangement
  other.living.arrangement=nfam.other.living.arrangement-nfam.hh # other living arrangement
  
  # assemble household type without poverty status
  pums.b17021.sub=cbind(fam.hh,
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
  pums.b17021=cbind(rowSums(pums.povr[,1:2]),
                    pums.povr[,2],
                    pums.povr[,2]*pums.b17021.sub,
                    pums.povr[,1],
                    pums.povr[,1]*pums.b17021.sub)
  # colnames(pums.b17021)=rip.metadata.from.census.reporter('B17021')$codes
  colnames(pums.b17021)=paste0('B17021',sprintf('%03d',1:ncol(pums.b17021)))
  
  #### B17021: Poverty Status of Individuals by Living Arrangement - CONDENSED ####
  # # Ignore 'all relatives/non-relatives',
  # # subcategories of nfam/other living arrangement
  # pums.b17021.excl=c('B17021005','B17021006','B17021009','B17021010','B17021012',
  #                    'B17021013','B17021015','B17021016','B17021017','B17021018',
  #                    'B17021022','B17021023','B17021026','B17021027','B17021029',
  #                    'B17021030','B17021032','B17021033','B17021034','B17021035')
  #
  # pums.b17021=pums.b17021[,!colnames(pums.b17021) %in% pums.b17021.excl]
  
  #### B09019: Household Type (Including Living Alone) by Relationship ####
  # we need to borrow some of the b17021 vars for this one
  in.household=pums.householdr[,2] # in households
  
  ## Family Households
  fam.hh=fam.hh # in family households, placeholder
  # fam.hh.householder=fam.hh*pums.relatedr[,6]
  fam.hh.householder=fam.hh*pums.hheadr[,2]
  fam.hh.householder.male=fam.hh.householder*pums.sexr[,1]
  fam.hh.householder.female=fam.hh.householder*pums.sexr[,2]
  fam.hh.spouse=fam.hh*pums.relatedr[,2]
  fam.hh.biological.child=fam.hh*pums.relatedr[,4]
  fam.hh.adopted.child=fam.hh*pums.relatedr[,5]
  fam.hh.stepchild=fam.hh*pums.relatedr[,6]
  # column header for "child"
  fam.hh.child.all=rowSums(cbind(fam.hh.biological.child,
                                 fam.hh.adopted.child,
                                 fam.hh.stepchild))
  fam.hh.grandchild=fam.hh*pums.relatedr[,18]
  fam.hh.sibling=fam.hh*pums.relatedr[,14]
  fam.hh.parent=fam.hh*pums.relatedr[,10]
  fam.hh.parent.in.law=fam.hh*pums.relatedr[,12]
  fam.hh.child.in.law=fam.hh*pums.relatedr[,8]
  fam.hh.other.relatives=fam.hh*pums.relatedr[,22]
  fam.hh.nonrelative.roomer.boarder=fam.hh*pums.relatedr[,49]
  fam.hh.nonrelative.housemate.roommate=fam.hh*pums.relatedr[,43]
  fam.hh.nonrelative.unmarried.partner=fam.hh*pums.relatedr[,42]
  fam.hh.nonrelative.foster.child=fam.hh*pums.relatedr[,50]
  fam.hh.nonrelative.other.nonrelatives=fam.hh*pums.relatedr[,55]
  # column header for 'nonrelatives in fam households'
  fam.hh.nonrelative.all=rowSums(cbind(fam.hh.nonrelative.roomer.boarder,
                                       fam.hh.nonrelative.housemate.roommate,
                                       fam.hh.nonrelative.unmarried.partner,
                                       fam.hh.nonrelative.foster.child,
                                       fam.hh.nonrelative.other.nonrelatives))
  
  ## Nonfamily households
  # nfam.hh=rowSums(pums.famr[,2:3])*pums.householdr[,2] # nonfamlily households, placeholder
  nfam.hh=rowSums(pums.famr[,2:3]) # nonfamlily households, placeholder
  # nfam.hh=nfam.hh # nonfamlily households, placeholder
  # nfam.hh.householder=nfam.householder # nonfamily householder, placeholder
  nfam.hh.householder=nfam.hh*pums.hheadr[,2] # nonfamily householder, placeholder
  
  nfam.hh.householder.male=nfam.hh.householder*pums.sexr[,1]
  nfam.hh.householder.male.alone=nfam.hh.householder.male*pums.famr[,3] # nfam.alone from b17021
  nfam.hh.householder.male.not.alone=nfam.hh.householder.male*pums.famr[,2] # nfam.not.alone from b17021
  
  nfam.hh.householder.female=nfam.hh.householder*pums.sexr[,2]
  nfam.hh.householder.female.alone=nfam.hh.householder.female*pums.famr[,3] # nfam.alone from b17021
  nfam.hh.householder.female.not.alone=nfam.hh.householder.female*pums.famr[,2] # nfam.not.alone from b17021
  
  nfam.hh.nonrelative.roomer.boarder=nfam.hh*pums.relatedr[,49]
  nfam.hh.nonrelative.housemate.roommate=nfam.hh*pums.relatedr[,43]
  nfam.hh.nonrelative.unmarried.partner=nfam.hh*pums.relatedr[,42]
  nfam.hh.nonrelative.foster.child=nfam.hh*pums.relatedr[,50]
  nfam.hh.nonrelative.other.nonrelatives=nfam.hh*pums.relatedr[,55]
  # column header for 'nonrelatives in nfam households'
  nfam.hh.nonrelative.all=rowSums(cbind(nfam.hh.nonrelative.roomer.boarder,
                                        nfam.hh.nonrelative.housemate.roommate,
                                        nfam.hh.nonrelative.unmarried.partner,
                                        nfam.hh.nonrelative.foster.child,
                                        nfam.hh.nonrelative.other.nonrelatives))
  # in.group.quarters=pums.householdr[,1]
  in.group.quarters=pums.famr[,4]
  
  pums.b09019=cbind(rowSums(pums.householdr),
                    pums.householdr[,2],
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
  # colnames(pums.b09019)=rip.metadata.from.census.reporter('B09019')$codes
  colnames(pums.b09019)=paste0('B09019',sprintf('%03d',1:ncol(pums.b09019)))
  
  #### B16008: Citizenship Status by Age by Language Spoken at Home and Ability to Speak English ####
  if(!'bg' %in% c(parent_unit, child_unit)){ # not available at the block group level
    cat('B16008: Citizenship Status by Age by Language Spoken at Home and Ability to Speak English','\n\n')
    pop.5yo=rowSums(pums.agecitizenr[,2:3]) # Total, population 5 years and older
    
    # native-born population #
    nat.pop=pums.citizenr[,1]
    nat.pop.5y_17y=nat.pop*pums.agecitizenr[,2]
    nat.pop.5y_17y_only_english=nat.pop.5y_17y*pums.speakengr[,2]
    nat.pop.5y_17y_spanish=nat.pop.5y_17y*pums.langr[,3]
    nat.pop.5y_17y_spanish_speak_english_vwell=nat.pop.5y_17y_spanish*pums.speakengr[,3]
    nat.pop.5y_17y_spanish_speak_english_less_vwell=nat.pop.5y_17y_spanish*pums.speakengr[,4]
    nat.pop.5y_17y_other_langs=nat.pop.5y_17y*pums.langr[,4]
    nat.pop.5y_17y_other_langs_speak_english_vwell=nat.pop.5y_17y_other_langs*pums.speakengr[,3]
    nat.pop.5y_17y_other_langs_speak_english_less_vwell=nat.pop.5y_17y_other_langs*pums.speakengr[,4]
    nat.pop.18yo=nat.pop*pums.agecitizenr[,3]
    nat.pop.18yo_only_english=nat.pop.18yo*pums.speakengr[,2]
    nat.pop.18yo_spanish=nat.pop.18yo*pums.langr[,3]
    nat.pop.18yo_spanish_speak_english_vwell=nat.pop.18yo_spanish*pums.speakengr[,3]
    nat.pop.18yo_spanish_speak_english_less_vwell=nat.pop.18yo_spanish*pums.speakengr[,4]
    nat.pop.18yo_other_langs=nat.pop.18yo*pums.langr[,4]
    nat.pop.18yo_other_langs_speak_english_vwell=nat.pop.18yo_other_langs*pums.speakengr[,3]
    nat.pop.18yo_other_langs_speak_english_less_vwell=nat.pop.18yo_other_langs*pums.speakengr[,4]
    
    # foreign-born population #
    foreign.pop=rowSums(pums.citizenr[,2:3])
    foreign.pop.naturalized=pums.citizenr[,2]
    foreign.pop.naturalized.5y_17y=foreign.pop.naturalized*pums.agecitizenr[,2]
    foreign.pop.naturalized.5y_17y_only_english=foreign.pop.naturalized.5y_17y*pums.speakengr[,2]
    foreign.pop.naturalized.5y_17y_spanish=foreign.pop.naturalized.5y_17y*pums.langr[,3]
    foreign.pop.naturalized.5y_17y_spanish_speak_english_vwell=foreign.pop.naturalized.5y_17y_spanish*pums.speakengr[,3]
    foreign.pop.naturalized.5y_17y_spanish_speak_english_less_vwell=foreign.pop.naturalized.5y_17y_spanish*pums.speakengr[,4]
    foreign.pop.naturalized.5y_17y_other_langs=foreign.pop.naturalized.5y_17y*pums.langr[,4]
    foreign.pop.naturalized.5y_17y_other_langs_speak_english_vwell=foreign.pop.naturalized.5y_17y_other_langs*pums.speakengr[,3]
    foreign.pop.naturalized.5y_17y_other_langs_speak_english_less_vwell=foreign.pop.naturalized.5y_17y_other_langs*pums.speakengr[,4]
    foreign.pop.naturalized.18yo=foreign.pop.naturalized*pums.agecitizenr[,3]
    foreign.pop.naturalized.18yo_only_english=foreign.pop.naturalized.18yo*pums.speakengr[,2]
    foreign.pop.naturalized.18yo_spanish=foreign.pop.naturalized.18yo*pums.langr[,3]
    foreign.pop.naturalized.18yo_spanish_speak_english_vwell=foreign.pop.naturalized.18yo_spanish*pums.speakengr[,3]
    foreign.pop.naturalized.18yo_spanish_speak_english_less_vwell=foreign.pop.naturalized.18yo_spanish*pums.speakengr[,4]
    foreign.pop.naturalized.18yo_other_langs=foreign.pop.naturalized.18yo*pums.langr[,4]
    foreign.pop.naturalized.18yo_other_langs_speak_english_vwell=foreign.pop.naturalized.18yo_other_langs*pums.speakengr[,3]
    foreign.pop.naturalized.18yo_other_langs_speak_english_less_vwell=foreign.pop.naturalized.18yo_other_langs*pums.speakengr[,4]
    foreign.pop.non.citizen=pums.citizenr[,3]
    foreign.pop.non.citizen.5y_17y=foreign.pop.non.citizen*pums.agecitizenr[,2]
    foreign.pop.non.citizen.5y_17y_only_english=foreign.pop.non.citizen.5y_17y*pums.speakengr[,2]
    foreign.pop.non.citizen.5y_17y_spanish=foreign.pop.non.citizen.5y_17y*pums.langr[,3]
    foreign.pop.non.citizen.5y_17y_spanish_speak_english_vwell=foreign.pop.non.citizen.5y_17y_spanish*pums.speakengr[,3]
    foreign.pop.non.citizen.5y_17y_spanish_speak_english_less_vwell=foreign.pop.non.citizen.5y_17y_spanish*pums.speakengr[,4]
    foreign.pop.non.citizen.5y_17y_other_langs=foreign.pop.non.citizen.5y_17y*pums.langr[,4]
    foreign.pop.non.citizen.5y_17y_other_langs_speak_english_vwell=foreign.pop.non.citizen.5y_17y_other_langs*pums.speakengr[,3]
    foreign.pop.non.citizen.5y_17y_other_langs_speak_english_less_vwell=foreign.pop.non.citizen.5y_17y_other_langs*pums.speakengr[,4]
    foreign.pop.non.citizen.18yo=foreign.pop.non.citizen*pums.agecitizenr[,3]
    foreign.pop.non.citizen.18yo_only_english=foreign.pop.non.citizen.18yo*pums.speakengr[,2]
    foreign.pop.non.citizen.18yo_spanish=foreign.pop.non.citizen.18yo*pums.langr[,3]
    foreign.pop.non.citizen.18yo_spanish_speak_english_vwell=foreign.pop.non.citizen.18yo_spanish*pums.speakengr[,3]
    foreign.pop.non.citizen.18yo_spanish_speak_english_less_vwell=foreign.pop.non.citizen.18yo_spanish*pums.speakengr[,4]
    foreign.pop.non.citizen.18yo_other_langs=foreign.pop.non.citizen.18yo*pums.langr[,4]
    foreign.pop.non.citizen.18yo_other_langs_speak_english_vwell=foreign.pop.non.citizen.18yo_other_langs*pums.speakengr[,3]
    foreign.pop.non.citizen.18yo_other_langs_speak_english_less_vwell=foreign.pop.non.citizen.18yo_other_langs*pums.speakengr[,4]
    
    # assemble data #
    pums.b16008=cbind(pop.5yo,
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
    # colnames(pums.b16008)=rip.metadata.from.census.reporter('B16008')$codes
    colnames(pums.b16008)=paste0('B16008',sprintf('%03d',1:ncol(pums.b16008)))
  }
    
  # cat('saving outputs','\n')
  # save(list=paste0('pums.',c('b01001','b02001','b03002','b09019','b16008','b17001','b17021')),
  #      file=paste0('data/pums_solver_inputs_',loc,'_',p,'.RData'))
  
  # cbind(pums.b01001,pums.b02001,pums.b03002,pums.b09019,pums.b16008,pums.b17001,pums.b17021)

  if(!'bg' %in% c(parent_unit, child_unit)){ # not available at the block group level
    list(pums=pums,
         # pums_in=cbind(pums.b01001,pums.b02001,pums.b03002,pums.b09019,pums.b16008,pums.b17001,pums.b17021))
    pums_in=cbind(pums.b01001,pums.b03002,pums.b09019,pums.b16008,pums.b17001,pums.b17021))
  }else{
    # list(pums=pums,
    #      pums_in=cbind(pums.b01001,pums.b02001,pums.b03002,pums.b09019,pums.b17021))
    list(pums=pums,
         pums_in=cbind(pums.b01001,pums.b03002,pums.b09019,pums.b17021))
  }
}

build_data_household=function(pums, year){
  
  # options(na.action = 'na.pass') # do not omit na items in model.matrix
  
  #### Setup: Define Breaks ####
  cat('Setup: Define Breaks','\n')
  age.brks.tenure <- c(0,15,25,35,45,55,60,65,75,85,Inf)
  age.brks.hhinc=c(0,25,45,65,Inf)

  race.brks=c(-Inf,2,3,4,5,6,7,8,9,Inf)
  
  hhinc.brks=c(0,10,15,20,25,30,35,40,45,50,60,75,100,125,150,200,Inf)
  
  #### Setup: Define Labels ####
  age.tenure.labels<-c(paste(age.brks.tenure[1:9],c(age.brks.tenure[2:10])-1,sep='-'),'85+')
  
  age.hhinc.labels=c(paste(age.brks.hhinc[1:3],c(age.brks.hhinc[2:4])-1,sep='-'),'65+')
  
  hhinc.labels=c(paste0(paste(hhinc.brks[1:15],c(hhinc.brks[2:16])-0.1,sep='-'),'k'),'200k+')

  race.labels<-c('White', 'Black or African American','American Indian and Alaska Native',
                 'Asian','Native Hawaiian and Other Pacific Islander','Some other race',
                 'Two or more races','Two races including Some other race',
                 'Two races excluding Some other race, and three or more races')
  
  sex.labels<-c("Male","Female")
  
  #### Setup: Reclassify Variables ####
  cat('Setup: Reclassify Variables','\n')
  
  ## age levels for household income
  pums$agehhincr <-cut(as.numeric(pums$AGE),breaks=age.brks.hhinc, include.lowest=TRUE, right=FALSE,labels=age.hhinc.labels)
  
  ## household income
  # safeguard - treat negative income as 0 income
  HHINCOME.safe = pums$HHINCOME
  HHINCOME.safe[HHINCOME.safe < 0] = 0
  
  # pums$hhincr<-cut(as.numeric(pums$HHINCOME),breaks=(1000*hhinc.brks), include.lowest=TRUE, right=FALSE,labels=hhinc.labels)
  pums$hhincr<-cut(as.numeric(HHINCOME.safe),breaks=(1000*hhinc.brks), include.lowest=TRUE, right=FALSE,labels=hhinc.labels)
  
  ## age levels for tenure 
  pums$agetenr <-cut(as.numeric(pums$AGE),breaks=age.brks.tenure, include.lowest=TRUE, right=FALSE,labels=age.tenure.labels)
  
  ## race/ethnicity
  pums$racer <- cut(pums$RACE, breaks=race.brks, labels=race.labels,inclue.lowest=TRUE,right=FALSE)
  pums$hispanr=factor(ifelse(pums$HISPAN>0,'Hispanic/Latino','Not Hispanic/Latino'))
  
  ## sex
  pums$sexr <- factor(pums[['SEX']], 1:2, labels=sex.labels)
  
  ## tenure
  pums$tenr = factor(with(pums, ifelse(OWNERSHP==1, 'Own', 'Rent')))
  
  ## living arrangement 
  pums$famr=unlist(sapply(unique(pums$SERIAL),function(s){
    fs=pums$FAMSIZE[pums$SERIAL==s][1]
    # If multiple members of the household, discard the head (coded 1)
    # If only one member of the household, recode the head as 'living alone' (1 -> 99)
    fr=pums$RELATE[pums$SERIAL==s]
    if(length(fr)>1){
      fr=fr[-1]
    }else{
      fr=99
    }
    
    # If some or all other household members are related to head, family household (1)
    # If no other household members are related to head, nonfamily household, not living alone (2)
    # If head is only household member (coded 99), lives alone (3)
    famtype=ifelse(min(fr)<11,1,
                   ifelse(min(fr)>=11 & max(fr)<99,2,3))
    if(max(fr)<99){
      rep(famtype,length(fr)+1)
    }else{
      famtype
    }
  }))
  
  pums$famr = factor(with(pums,ifelse(famr==1,'fam',
                                     ifelse(famr==2,'nfam_not_alone','nfam_alone'))))
  
  # household marital status #
  cat('...Household marital status','\n')
  pums$marstr=factor(unlist(sapply(unique(pums$SERIAL),function(s){
    sm=pums$MARST[pums$SERIAL==s]
    head_spouse_present = sm[1]
    mar_hh=ifelse(head_spouse_present==1,1,0)
    rep(mar_hh,length(sm))
  })))
  
  ## presence of household members under 18
  pums$minr = factor(unlist(sapply(unique(pums$SERIAL), function(s){
    as = pums$AGE[pums$SERIAL == s]
    minors = ifelse(any(as<18),1,0)
    rep(minors,length(as))
  })))

  ## presence of household members over 60
  pums$eldr = factor(unlist(sapply(unique(pums$SERIAL), function(s){
    as = pums$AGE[pums$SERIAL == s]
    elders = ifelse(any(as>=60),1,0)
    rep(elders,length(as))
  })))
  
  ## household size
  hhsize_by_serial = plyr::count(pums, 'SERIAL')
  pums$hhsizr = with(hhsize_by_serial, freq[match(pums$SERIAL, SERIAL)])
  pums$hhsizr = factor(ifelse(pums$hhsizr >= 7, '7+', pums$hhsizr),
                       levels=c(as.character(seq(1, 6, by = 1)), '7+'))  
  
  ## units in structure
  units_key = read.csv(paste0(data_path, 'PUMS_UNITSSTR.csv'))
  pums$unitsr = factor(units_key$label[match(pums$UNITSSTR,units_key$code)], levels = units_key$label)
      
  ## year built
  built_key = read.csv(paste0(data_path, 'PUMS_BUILTYR2.csv'))
  pums$builtr = factor(built_key$label[match(pums$BUILTYR2,built_key$code)], levels = unique(built_key$label))
  
  #### Subset Householders ####
  cat('Subset by Household Head','\n')
  
  ## id head of household 
  hheadr=ifelse(pums$RELATED==101,1,0)
  pums = pums[hheadr == 1,]
  
  #### Setup: Build Dummy Variables ####
  cat('Setup: Build Dummy Variables','\n')
  
  pums.sexr=with(pums,model.matrix(~sexr-1)) # sex (head of household)
  pums.famr=with(pums,model.matrix(~famr-1)) # family type/living arrangement 
  pums.marstr=with(pums,model.matrix(~marstr-1)) # spouse present
  pums.minr=with(pums,model.matrix(~minr-1)) # household members <18
  pums.eldr=with(pums,model.matrix(~eldr-1)) # household members 60+
  
  pums.hhsizr = with(pums, model.matrix(~hhsizr - 1)) # household size 
  
  pums.agehhincr = with(pums, model.matrix(~agehhincr - 1)) # age (income categories)
  pums.hhincr = with(pums, model.matrix(~hhincr - 1)) # household income

  
  pums.agetenr=with(pums,model.matrix(~agetenr-1)) # age (tenure categories)
  pums.tenr=with(pums,model.matrix(~tenr-1)) # tenure 
  pums.unitsr=with(pums,model.matrix(~unitsr-1)) # units in structure
  pums.builtr=with(pums,model.matrix(~builtr-1)) # year built
  
  pums.racer=with(pums,model.matrix(~racer-1)) # race
  pums.hispanr=with(pums,model.matrix(~hispanr-1)) # hispanic/latino ethnicity
  
  # options(na.action = 'na.omit') # reset the global na action
  
  #### B11005: Households by Presence of People Under 18 Years By Household Type ####
  cat('B11005: Households by Presence of People Under 18 Years By Household Type','\n')
  
  hh = rep(1, nrow(pums)) # Total 001
  hh.w.under.18 = pums.minr[,2] # households with one or more person under 18 002
  hh.w.under.18.fam = hh.w.under.18 * pums.famr[,1] 
  hh.w.under.18.fam.married = hh.w.under.18.fam * pums.marstr[,2]
  hh.w.under.18.fam.other = hh.w.under.18.fam * pums.marstr[,1]
  hh.w.under.18.fam.other.male.no.wife.present = hh.w.under.18.fam.other * pums.sexr[,1]
  hh.w.under.18.fam.other.female.no.husband.present = hh.w.under.18.fam.other * pums.sexr[,2]
  hh.w.under.18.nfam = hh.w.under.18 * rowSums(pums.famr[,2:3])
  hh.w.under.18.nfam.male = hh.w.under.18.nfam * pums.sexr[,1]
  hh.w.under.18.nfam.female = hh.w.under.18.nfam * pums.sexr[,2]
  hh.no.under.18 = pums.minr[,1] # households with no people under 18 011
  hh.no.under.18.fam = hh.no.under.18 * pums.famr[,1] 
  hh.no.under.18.fam.married = hh.no.under.18.fam * pums.marstr[,2]
  hh.no.under.18.fam.other = hh.no.under.18.fam * pums.marstr[,1]
  hh.no.under.18.fam.other.male.no.wife.present = hh.no.under.18.fam.other * pums.sexr[,1]
  hh.no.under.18.fam.other.female.no.husband.present = hh.no.under.18.fam.other * pums.sexr[,2]
  hh.no.under.18.nfam = hh.no.under.18 * rowSums(pums.famr[,2:3])
  hh.no.under.18.nfam.male = hh.no.under.18.nfam * pums.sexr[,1]
  hh.no.under.18.nfam.female = hh.no.under.18.nfam * pums.sexr[,2]
  
  pums.b11005 = cbind(hh,
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
  
  colnames(pums.b11005)=paste0('B11005',sprintf('%03d',1:ncol(pums.b11005)))
  
  #### B11006: Households by Presence of People 60 Years or Older By Household Type ####
  cat('B11006: Households by Presence of People 60 Years or Older By Household Type','\n')
  
  hh = rep(1, nrow(pums)) # Total 001
  hh.w.60.over = pums.eldr[,2] # households with one or more person 60 or over 002
  hh.w.60.over.fam = hh.w.60.over * pums.famr[,1] 
  hh.w.60.over.fam.married = hh.w.60.over.fam * pums.marstr[,2]
  hh.w.60.over.fam.other = hh.w.60.over.fam * pums.marstr[,1]
  hh.w.60.over.fam.other.male.no.wife.present = hh.w.60.over.fam.other * pums.sexr[,1]
  hh.w.60.over.fam.other.female.no.husband.present = hh.w.60.over.fam.other * pums.sexr[,2]
  hh.w.60.over.nfam = hh.w.60.over * rowSums(pums.famr[,2:3])
  hh.no.60.over = pums.eldr[,1] # households with no people 60 or over 009
  hh.no.60.over.fam = hh.no.60.over * pums.famr[,1] 
  hh.no.60.over.fam.married = hh.no.60.over.fam * pums.marstr[,2]
  hh.no.60.over.fam.other = hh.no.60.over.fam * pums.marstr[,1]
  hh.no.60.over.fam.other.male.no.wife.present = hh.no.60.over.fam.other * pums.sexr[,1]
  hh.no.60.over.fam.other.female.no.husband.present = hh.no.60.over.fam.other * pums.sexr[,2]
  hh.no.60.over.nfam = hh.no.60.over * rowSums(pums.famr[,2:3])
  
  pums.b11006 = cbind(hh,
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
  
  colnames(pums.b11006)=paste0('B11006',sprintf('%03d',1:ncol(pums.b11006)))
  
  #### B11012: Household Type by Tenure ####
  cat('B11012: Household Type by Tenure','\n')
  
  hh = rep(1, nrow(pums)) # Total 001
  hh.fam = pums.famr[,1] # family households 002
  hh.fam.married = hh.fam * pums.marstr[,2] # married couple households 003
  hh.fam.married.own = hh.fam.married * pums.tenr[,1]
  hh.fam.married.rent = hh.fam.married * pums.tenr[,2]
  hh.fam.other = hh.fam * pums.marstr[,1] # other family households 006
  hh.fam.other.male.no.wife.present = hh.fam.other * pums.sexr[,1] # male head, no wife present 007 
  hh.fam.other.male.no.wife.present.own = hh.fam.other.male.no.wife.present * pums.tenr[,1]
  hh.fam.other.male.no.wife.present.rent = hh.fam.other.male.no.wife.present * pums.tenr[,2]
  hh.fam.other.female.no.husband.present = hh.fam.other * pums.sexr[,2] # female head, no husband present 010
  hh.fam.other.female.no.husband.present.own = hh.fam.other.female.no.husband.present * pums.tenr[,1]
  hh.fam.other.female.no.husband.present.rent = hh.fam.other.female.no.husband.present * pums.tenr[,2]
  hh.nfam = rowSums(pums.famr[,-1]) # nonfamily households 013
  hh.nfam.own = hh.nfam * pums.tenr[,1]
  hh.nfam.rent = hh.nfam * pums.tenr[,2]
  
  pums.b11012 = cbind(hh,
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
  
  colnames(pums.b11012)=paste0('B11012',sprintf('%03d',1:ncol(pums.b11012)))
  
  #### B11016: Household Type by Household Size ####
  cat('B11016: Household Type by Household Size','\n')
  
  hh = rep(1, nrow(pums)) # Total 001
  hh.fam = pums.famr[,1] # family households 002
  hh.fam.p2 = hh.fam * pums.hhsizr[,2]
  hh.fam.p3 = hh.fam * pums.hhsizr[,3]
  hh.fam.p4 = hh.fam * pums.hhsizr[,4]
  hh.fam.p5 = hh.fam * pums.hhsizr[,5]
  hh.fam.p6 = hh.fam * pums.hhsizr[,6]
  hh.fam.p7.or.more = hh.fam * pums.hhsizr[,7]
  hh.nfam = rowSums(pums.famr[,-1]) # nonfamily households 009
  hh.nfam.p1 = hh.nfam * pums.hhsizr[,1]
  hh.nfam.p2 = hh.nfam * pums.hhsizr[,2]
  hh.nfam.p3 = hh.nfam * pums.hhsizr[,3]
  hh.nfam.p4 = hh.nfam * pums.hhsizr[,4]
  hh.nfam.p5 = hh.nfam * pums.hhsizr[,5]
  hh.nfam.p6 = hh.nfam * pums.hhsizr[,6]
  hh.nfam.p7.or.more = hh.nfam * pums.hhsizr[,7]
  
  pums.b11016 = cbind(hh, 
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
  
  colnames(pums.b11016)=paste0('B11016',sprintf('%03d',1:ncol(pums.b11016)))
  
  #### B19037: Age of Householder By Household Income in the Past 12 Months ####
  cat('B19037: Age of Householder By Household Income in the Past 12 Months','\n')
  
  hh = rep(1, nrow(pums)) # Total 001
  hh.under.25 = pums.agehhincr[,1] # Householder under 25 years 002
  hh.under.25.less.than.10k = hh.under.25 * pums.hhincr[,1]
  hh.under.25.10k.14k = hh.under.25 * pums.hhincr[,2]
  hh.under.25.15k.19k = hh.under.25 * pums.hhincr[,3]
  hh.under.25.20k.24k = hh.under.25 * pums.hhincr[,4]
  hh.under.25.25k.29k = hh.under.25 * pums.hhincr[,5]
  hh.under.25.30k.34k = hh.under.25 * pums.hhincr[,6]
  hh.under.25.35k.39k = hh.under.25 * pums.hhincr[,7]
  hh.under.25.40k.44k = hh.under.25 * pums.hhincr[,8]
  hh.under.25.45k.49k = hh.under.25 * pums.hhincr[,9]
  hh.under.25.50k.59k = hh.under.25 * pums.hhincr[,10]
  hh.under.25.60k.74k = hh.under.25 * pums.hhincr[,11]
  hh.under.25.75k.99k = hh.under.25 * pums.hhincr[,12]
  hh.under.25.100k.124k = hh.under.25 * pums.hhincr[,13]
  hh.under.25.125k.149k = hh.under.25 * pums.hhincr[,14]
  hh.under.25.150k.199k = hh.under.25 * pums.hhincr[,15]
  hh.under.25.200k.more = hh.under.25 * pums.hhincr[,16]
  hh.25.44 = pums.agehhincr[,2] # Householder 25-44 years 019
  hh.25.44.less.than.10k = hh.25.44 * pums.hhincr[,1]
  hh.25.44.10k.14k = hh.25.44 * pums.hhincr[,2]
  hh.25.44.15k.19k = hh.25.44 * pums.hhincr[,3]
  hh.25.44.20k.24k = hh.25.44 * pums.hhincr[,4]
  hh.25.44.25k.29k = hh.25.44 * pums.hhincr[,5]
  hh.25.44.30k.34k = hh.25.44 * pums.hhincr[,6]
  hh.25.44.35k.39k = hh.25.44 * pums.hhincr[,7]
  hh.25.44.40k.44k = hh.25.44 * pums.hhincr[,8]
  hh.25.44.45k.49k = hh.25.44 * pums.hhincr[,9]
  hh.25.44.50k.59k = hh.25.44 * pums.hhincr[,10]
  hh.25.44.60k.74k = hh.25.44 * pums.hhincr[,11]
  hh.25.44.75k.99k = hh.25.44 * pums.hhincr[,12]
  hh.25.44.100k.124k = hh.25.44 * pums.hhincr[,13]
  hh.25.44.125k.149k = hh.25.44 * pums.hhincr[,14]
  hh.25.44.150k.199k = hh.25.44 * pums.hhincr[,15]
  hh.25.44.200k.more = hh.25.44 * pums.hhincr[,16]
  hh.45.64 = pums.agehhincr[,3] # Householder 44-64 years 036
  hh.45.64.less.than.10k = hh.45.64 * pums.hhincr[,1]
  hh.45.64.10k.14k = hh.45.64 * pums.hhincr[,2]
  hh.45.64.15k.19k = hh.45.64 * pums.hhincr[,3]
  hh.45.64.20k.24k = hh.45.64 * pums.hhincr[,4]
  hh.45.64.25k.29k = hh.45.64 * pums.hhincr[,5]
  hh.45.64.30k.34k = hh.45.64 * pums.hhincr[,6]
  hh.45.64.35k.39k = hh.45.64 * pums.hhincr[,7]
  hh.45.64.40k.44k = hh.45.64 * pums.hhincr[,8]
  hh.45.64.45k.49k = hh.45.64 * pums.hhincr[,9]
  hh.45.64.50k.59k = hh.45.64 * pums.hhincr[,10]
  hh.45.64.60k.74k = hh.45.64 * pums.hhincr[,11]
  hh.45.64.75k.99k = hh.45.64 * pums.hhincr[,12]
  hh.45.64.100k.124k = hh.45.64 * pums.hhincr[,13]
  hh.45.64.125k.149k = hh.45.64 * pums.hhincr[,14]
  hh.45.64.150k.199k = hh.45.64 * pums.hhincr[,15]
  hh.45.64.200k.more = hh.45.64 * pums.hhincr[,16]
  hh.65.over = pums.agehhincr[,4] # Householder 65 years or over 053
  hh.65.over.less.than.10k = hh.65.over * pums.hhincr[,1]
  hh.65.over.10k.14k = hh.65.over * pums.hhincr[,2]
  hh.65.over.15k.19k = hh.65.over * pums.hhincr[,3]
  hh.65.over.20k.24k = hh.65.over * pums.hhincr[,4]
  hh.65.over.25k.29k = hh.65.over * pums.hhincr[,5]
  hh.65.over.30k.34k = hh.65.over * pums.hhincr[,6]
  hh.65.over.35k.39k = hh.65.over * pums.hhincr[,7]
  hh.65.over.40k.44k = hh.65.over * pums.hhincr[,8]
  hh.65.over.45k.49k = hh.65.over * pums.hhincr[,9]
  hh.65.over.50k.59k = hh.65.over * pums.hhincr[,10]
  hh.65.over.60k.74k = hh.65.over * pums.hhincr[,11]
  hh.65.over.75k.99k = hh.65.over * pums.hhincr[,12]
  hh.65.over.100k.124k = hh.65.over * pums.hhincr[,13]
  hh.65.over.125k.149k = hh.65.over * pums.hhincr[,14]
  hh.65.over.150k.199k = hh.65.over * pums.hhincr[,15]
  hh.65.over.200k.more = hh.65.over * pums.hhincr[,16]
  
  pums.b19037 = cbind(hh,
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
  
  colnames(pums.b19037)=paste0('B19037',sprintf('%03d',1:ncol(pums.b19037)))
  
  #### B25003: Tenure by Race/Ethnicity of Householder ####
  cat('B25003: Tenure by Race/Ethnicity of Householder','\n')
  
  occ.hu = rep(1, nrow(pums)) # Total 001
  white.occ.hu = pums.racer[,1] # householder who is White alone A001
  white.occ.hu.own = white.occ.hu * pums.tenr[,1]
  white.occ.hu.rent = white.occ.hu * pums.tenr[,2]
  black.occ.hu = pums.racer[,2] # householder who is Black alone B001
  black.occ.hu.own = black.occ.hu * pums.tenr[,1]
  black.occ.hu.rent = black.occ.hu * pums.tenr[,2]
  aiana.occ.hu = pums.racer[,3] # householder who is American Indian/Alaska Native alone C001
  aiana.occ.hu.own = aiana.occ.hu * pums.tenr[,1]
  aiana.occ.hu.rent = aiana.occ.hu * pums.tenr[,2]
  asian.occ.hu = pums.racer[,4] # householder who is Asian alone D001
  asian.occ.hu.own = asian.occ.hu * pums.tenr[,1]
  asian.occ.hu.rent = asian.occ.hu * pums.tenr[,2]
  nhopi.occ.hu = pums.racer[,5] # householder who is Native Hawaiian/Other Pacific Islander alone E001
  nhopi.occ.hu.own = nhopi.occ.hu * pums.tenr[,1]
  nhopi.occ.hu.rent = nhopi.occ.hu * pums.tenr[,2]
  other.occ.hu = pums.racer[,6] # householder who is Some Other Race alone F001
  other.occ.hu.own = other.occ.hu * pums.tenr[,1]
  other.occ.hu.rent = other.occ.hu * pums.tenr[,2]
  twomore.occ.hu = rowSums(pums.racer[,7:9]) # householder who is Two or More Races G001
  twomore.occ.hu.own = twomore.occ.hu * pums.tenr[,1]
  twomore.occ.hu.rent = twomore.occ.hu * pums.tenr[,2]
  white.non.hispanic.occ.hu = pums.racer[,1] * pums.hispanr[,2] # householder who is White Non-Hispanic alone H001
  white.non.hispanic.occ.hu.own = white.non.hispanic.occ.hu * pums.tenr[,1]
  white.non.hispanic.occ.hu.rent = white.non.hispanic.occ.hu * pums.tenr[,2]
  hisp.occ.hu = pums.hispanr[,1] # householder who is Hispanic/Latino I001
  hisp.occ.hu.own = hisp.occ.hu * pums.tenr[,1]
  hisp.occ.hu.rent = hisp.occ.hu * pums.tenr[,2]
  
  pums.b25003 = cbind(white.occ.hu,
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
  
  # generate column names
  names.b25003 = as.vector(sapply(toupper(letters[1:9]),function(i){
    
    sub_name = unname(paste0('B25003',i))
    
    sapply(1:3, function(j){
      paste0(sub_name,sprintf('%03d',j))
    })
    
  }))
  
  colnames(pums.b25003) = names.b25003
  
  #### B25007: Tenure by Age of Householder ####
  cat('B25007: Tenure by Age of Householder','\n')
  
  occ.hu = rep(1, nrow(pums)) # Total 001
  occ.hu.own = pums.tenr[,1] # Owner occupied 002
  occ.hu.own.15.24 = occ.hu.own * pums.agetenr[,2]
  occ.hu.own.25.34 = occ.hu.own * pums.agetenr[,3]
  occ.hu.own.35.44 = occ.hu.own * pums.agetenr[,4]
  occ.hu.own.45.54 = occ.hu.own * pums.agetenr[,5]
  occ.hu.own.55.59 = occ.hu.own * pums.agetenr[,6]
  occ.hu.own.60.64 = occ.hu.own * pums.agetenr[,7]
  occ.hu.own.65.74 = occ.hu.own * pums.agetenr[,8]
  occ.hu.own.75.84 = occ.hu.own * pums.agetenr[,9]
  occ.hu.own.85o = occ.hu.own * pums.agetenr[,10]
  occ.hu.rent = pums.tenr[,2] # Renter occupied 012
  occ.hu.rent.15.24 = occ.hu.rent * pums.agetenr[,2]
  occ.hu.rent.25.34 = occ.hu.rent * pums.agetenr[,3]
  occ.hu.rent.35.44 = occ.hu.rent * pums.agetenr[,4]
  occ.hu.rent.45.54 = occ.hu.rent * pums.agetenr[,5]
  occ.hu.rent.55.59 = occ.hu.rent * pums.agetenr[,6]
  occ.hu.rent.60.64 = occ.hu.rent * pums.agetenr[,7]
  occ.hu.rent.65.74 = occ.hu.rent * pums.agetenr[,8]
  occ.hu.rent.75.84 = occ.hu.rent * pums.agetenr[,9]
  occ.hu.rent.85o = occ.hu.rent * pums.agetenr[,10]
  
  pums.b25007 = cbind(occ.hu,
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
  
  colnames(pums.b25007)=paste0('B25007',sprintf('%03d',1:ncol(pums.b25007)))
  
  #### B25032: Tenure by Units in Structure ####
  cat('B25032: Tenure by Units in Structure','\n')
  
  occ.hu = rep(1, nrow(pums)) # Total 001
  occ.hu.own = pums.tenr[,1] # Owner occupied 002
  occ.hu.own.u1.detached = occ.hu.own * pums.unitsr[,3]
  occ.hu.own.u1.attached = occ.hu.own * pums.unitsr[,4]
  occ.hu.own.u2 = occ.hu.own * pums.unitsr[,5]
  occ.hu.own.u3.4 = occ.hu.own * pums.unitsr[,6]
  occ.hu.own.u5.9 = occ.hu.own * pums.unitsr[,7]
  occ.hu.own.u10.19 = occ.hu.own * pums.unitsr[,8]
  occ.hu.own.u20.49 = occ.hu.own * pums.unitsr[,9]
  occ.hu.own.u50.more = occ.hu.own * pums.unitsr[,10]
  occ.hu.own.mobile.home = occ.hu.own * pums.unitsr[,1]
  occ.hu.own.boat.rv.van.etc = occ.hu.own * pums.unitsr[,2]
  occ.hu.rent = pums.tenr[,2] # Renter occupied 013
  occ.hu.rent.u1.detached = occ.hu.rent * pums.unitsr[,3]
  occ.hu.rent.u1.attached = occ.hu.rent * pums.unitsr[,4]
  occ.hu.rent.u2 = occ.hu.rent * pums.unitsr[,5]
  occ.hu.rent.u3.4 = occ.hu.rent * pums.unitsr[,6]
  occ.hu.rent.u5.9 = occ.hu.rent * pums.unitsr[,7]
  occ.hu.rent.u10.19 = occ.hu.rent * pums.unitsr[,8]
  occ.hu.rent.u20.49 = occ.hu.rent * pums.unitsr[,9]
  occ.hu.rent.u50.more = occ.hu.rent * pums.unitsr[,10]
  occ.hu.rent.mobile.home = occ.hu.rent * pums.unitsr[,1]
  occ.hu.rent.boat.rv.van.etc = occ.hu.rent * pums.unitsr[,2]
  
  pums.b25032 = cbind(occ.hu,
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
  
  colnames(pums.b25032)=paste0('B25032',sprintf('%03d',1:ncol(pums.b25032)))
  
  #### B25036: Tenure by Year Structure Built ####
  cat('B25036: Tenure by Year Structure Built','\n')
  
  occ.hu = rep(1, nrow(pums)) # Total 001
  occ.hu.own = pums.tenr[,1] # Owner occupied 002
  occ.hu.own.built.2010.later = occ.hu.own * pums.builtr[,10]
  occ.hu.own.built.2000.2009 = occ.hu.own * pums.builtr[,9]
  occ.hu.own.built.1990.1999 = occ.hu.own * pums.builtr[,7]
  occ.hu.own.built.1980.1989 = occ.hu.own * pums.builtr[,6]
  occ.hu.own.built.1970.1979 = occ.hu.own * pums.builtr[,5]
  occ.hu.own.built.1960.1969 = occ.hu.own * pums.builtr[,4]
  occ.hu.own.built.1950.1959 = occ.hu.own * pums.builtr[,3]
  occ.hu.own.built.1940.1949 = occ.hu.own * pums.builtr[,2]
  occ.hu.own.built.1939.earlier = occ.hu.own * pums.builtr[,1]
  occ.hu.rent = pums.tenr[,2] # Renter occupied 012
  occ.hu.rent.built.2010.later = occ.hu.rent * pums.builtr[,10]
  occ.hu.rent.built.2000.2009 = occ.hu.rent * pums.builtr[,9]
  occ.hu.rent.built.1990.1999 = occ.hu.rent * pums.builtr[,7]
  occ.hu.rent.built.1980.1989 = occ.hu.rent * pums.builtr[,6]
  occ.hu.rent.built.1970.1979 = occ.hu.rent * pums.builtr[,5]
  occ.hu.rent.built.1960.1969 = occ.hu.rent * pums.builtr[,4]
  occ.hu.rent.built.1950.1959 = occ.hu.rent * pums.builtr[,3]
  occ.hu.rent.built.1940.1949 = occ.hu.rent * pums.builtr[,2]
  occ.hu.rent.built.1939.earlier = occ.hu.rent * pums.builtr[,1]
  
  pums.b25036 = cbind(occ.hu,
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
  
  colnames(pums.b25036)=paste0('B25036',sprintf('%03d',1:ncol(pums.b25036)))
  
  # options(na.action = 'na.omit') # reset the global na action
  
  list(pums = pums,
       pums_in = cbind(pums.b11005,pums.b11006,pums.b11012,pums.b11016,pums.b19037,
                       pums.b25003,pums.b25007,pums.b25032,pums.b25036))
  
}
