# script brings in detections, calculates capHist, and performs multistate model.  All developed by C. Holbrook.

# START- NECESSARY FUNCTIONS for script (functions developed for RMark):

# adds understandable parameter names:
addParmNames = function(my.ddl, modelDefs=list())
{
  if (length(modelDefs) == 0)
  {
    # Create full parm name field to map real and beta results to parm names in ddl
    my.ddl$S$fullParm = with(my.ddl$S, paste('S ','s',stratum, ' g',group,' t',time,sep=''))
    my.ddl$p$fullParm = with(my.ddl$p, paste('p ','s',stratum, ' g',group,' t',time,sep=''))
    my.ddl$Psi$fullParm = with(my.ddl$Psi, paste('Psi ','s',stratum, ' to',tostratum,' g',group,' t',time,sep=''))
  }
  
  if (length(modelDefs) > 0)
  {  
    #for S
    factorVars = names(my.ddl$S) #identify factor variables in design data frame
    useFactors = match(modelDefs$factorVars$S, factorVars)
    useFactors = modelDefs$factorVars$S
    
    parmName = paste("parm.",modelDefs$ID,sep='')
    
    my.ddl$S[,parmName] = 'S'
    
    for(i in useFactors)
    {
      my.ddl$S[,parmName] = paste(my.ddl$S[,parmName],my.ddl$S[,i],sep='.')
    }
    
    #for p
    factorVars = names(my.ddl$p) #identify factor variables in design data frame
    useFactors = match(modelDefs$factorVars$p, factorVars)
    useFactors = modelDefs$factorVars$p
    
    parmName = paste("parm.",modelDefs$ID,sep='')
    
    my.ddl$p[,parmName] = 'p'
    
    for(i in useFactors)
    {
      my.ddl$p[,parmName] = paste(my.ddl$p[,parmName],my.ddl$p[,i],sep='.')
    }
    
    #for Psi
    factorVars = names(my.ddl$Psi) #identify factor variables in design data frame
    useFactors = match(modelDefs$factorVars$Psi, factorVars)
    useFactors = modelDefs$factorVars$Psi
    
    parmName = paste("parm.",modelDefs$ID,sep='')
    
    my.ddl$Psi[,parmName] = 'Psi'
    
    for(i in useFactors)
    {
      my.ddl$Psi[,parmName] = paste(my.ddl$Psi[,parmName],my.ddl$Psi[,i],sep='.')
    }
  }
  
  return(my.ddl)
}

#####

# fix structural parameters:
fix.structural = function(my.ddl, parmDefs)
{
  
  cols.fromState = match(names(parmDefs)[substr(names(parmDefs),1,9) == 'fromState'], names(parmDefs))
  
  parmDefs.f = parmDefs[,cols.fromState] #subset only from states
  states = substr(names(parmDefs.f),10,nchar(names(parmDefs.f)))
  
  parms = data.frame(
    time = parmDefs$transition[row(parmDefs.f)[!is.na(parmDefs.f)]],
    stratum = states[col(parmDefs.f)[!is.na(parmDefs.f)]],
    tostratum = parmDefs$toState[row(parmDefs.f)[!is.na(parmDefs.f)]],
    label = parmDefs.f[!is.na(parmDefs.f)],
    group = parmDefs$group[row(parmDefs.f)[!is.na(parmDefs.f)]])
  
  parms = parms[with(parms,order(group,time,stratum,tostratum)),] #sort
  
  parms$parm.S = with(parms, paste('S ','s',stratum, ' g',group,' t',time,sep=''))
  parms$parm.Psi = with(parms, paste('Psi ','s',stratum, ' to',tostratum,' g',group,' t',time,sep=''))
  parms$parm.p = with(parms, paste('p ','s',tostratum, ' g',group,' t',time+1,sep=''))
  
  #make list of estimable parameters
  estimableParms = list(S='',Psi='',p='')
  estimableParms$S = unique(parms[parms$label=='S','parm.S'])
  estimableParms$Psi = unique(parms[parms$label %in% c('S','Phi'),'parm.Psi'])
  estimableParms$p = unique(parms[parms$label %in% c('S','Phi','L'),'parm.p'])
  estimableParms
  
  #  #fix p to 1 at L parms
  #  parms.L = parms$parm.p[parms$label=='L']
  #  parms.L.renamed = paste('L',substr(parms.L,2,nchar(parms.L)),sep='')
  #  my.ddl$p$fullParm[my.ddl$p$fullParm %in% parms.L] = parms.L.renamed #for lambda
  #  estimableParms$p[estimableParms$p %in% parms.L] = parms.L.renamed
  
  #fix s to 1 at Phi parms
  parms.Phi.Psi = parms$parm.Psi[parms$label=='Phi'] #for changing Psi label to Phi
  parms.Phi.Psi = my.ddl$Psi$fullParm[my.ddl$Psi$fullParm %in% parms.Phi.Psi] #exclude any parms not in ddl
  parms.Phi.Psi.renamed = paste('Phi',substr(parms.Phi.Psi,4,nchar(parms.Phi.Psi)),sep='')
  my.ddl$Psi$fullParm[my.ddl$Psi$fullParm %in% parms.Phi.Psi] = parms.Phi.Psi.renamed
  estimableParms$Psi[estimableParms$Psi %in% parms.Phi.Psi] = parms.Phi.Psi.renamed
  
  #subset only estimable parameters from design data
  my.ddl$S = my.ddl$S[my.ddl$S$fullParm %in% estimableParms$S,] #for S
  my.ddl$Psi = my.ddl$Psi[my.ddl$Psi$fullParm %in% estimableParms$Psi,] #for Psi
  my.ddl$p = my.ddl$p[my.ddl$p$fullParm %in% estimableParms$p,] #for p
  
  return(my.ddl)
}

# fix boundary parameters
fix.boundary = function(my.ddl, fix.bnd, parmID)
{
  #identify indices of fixed parameters
  fix1 = list('S'='','p'='','Psi'='')
  fix0 = list('S'='','p'='','Psi'='')
  
  fix1$S = as.numeric(row.names(my.ddl$S[my.ddl$S[,parmID] %in% fix.bnd$fixS.1.name,]))
  fix0$S = as.numeric(row.names(my.ddl$S[my.ddl$S[,parmID] %in% fix.bnd$fixS.0.name,]))
  
  fix1$p = as.numeric(row.names(my.ddl$p[my.ddl$p[,parmID] %in% fix.bnd$fixp.1.name,]))
  fix0$p = as.numeric(row.names(my.ddl$p[my.ddl$p[,parmID] %in% fix.bnd$fixp.0.name,]))
  
  fix1$Psi = as.numeric(row.names(my.ddl$Psi[my.ddl$Psi[,parmID] %in% fix.bnd$fixPsi.1.name,]))
  fix0$Psi = as.numeric(row.names(my.ddl$Psi[my.ddl$Psi[,parmID] %in% fix.bnd$fixPsi.0.name,]))
  
  fixParms = list(fix1=fix1,fix0=fix0)
  
  return(fixParms)
}

#####

# defineModel for each parameter:
defineModel = function(parm, formula, fixedParms=fixed, link)
{                           
  myModel = list(formula=formula,
                 fixed=list(index=c(with(fixedParms$fix1,get(parm)),with(fixedParms$fix0,get(parm))),
                            value=c(rep(1,with(fixedParms$fix1,length(get(parm)))),rep(0,with(fixedParms$fix0,length(get(parm)))))),
                 link = link)
  
  return(myModel)  
}

######

# identifies singular parms
identifySingularParms = function(my.results,modelNum)
{
  #identify "singular" parameters (unestimable or nonidentifable)
  if(!is.null(my.results[[modelNum]]$results$singular)) 
  { warning('CONVERGENCE PROBLEMS...RUN function identifySingularParms') } 
  
  singularParms = my.results[[modelNum]]$results$singular
  singularBetas = my.results[[modelNum]]$results$beta[singularParms,]
  singularReals = my.results[[modelNum]]$results$real[singularParms,]
  return(singularReals)
  
}
##

#query function:
query <- function(x,y){
  dbSendQuery(x, sprintf("COPY (%s) TO 'C:\\Users\\Public\\Documents\\res.csv' WITH DELIMITER AS ',' NULL AS 'NA' CSV HEADER;", y))
  read.csv('C:\\Users\\Public\\Documents\\res.csv', as.is = TRUE, strip.white = TRUE, header = TRUE)
}
##
# END OF necessary FUNCTIONS:



# code extracts capture histories from telemetry data for use in program Mark.  Code relies on three tables- sites (describes model occasions, and model states),
# tag (tagging information), and dtc (detections)

# This analysis of HECWL detections includes fish tagged in Tittabawassee River in 2011 for a 3 state model.

library(RPostgreSQL)
library(plyr)
library(RMark)
setwd("~/HECWL_analysis/multi-state mark-recap-20130305")

drv <- dbDriver("PostgreSQL")
db <- dbConnect(drv, dbname="hecwlsp", user = "postgres")

###############################################
# this threshold represents 2-months.  This should be enough time to eliminate multiple detection events 
# caused by short term movements away from receiver and still detect a returning fish (i.e., to SGR for spawning.
###############################################
thresh <- 5184000

# pull in 2011 Titta releases. Uses glatos_final table to include observations in HEC (Specifically the SCU line)
dtc <- query(db, "SELECT * FROM (SELECT a.glatos_array, a.detection_timestamp_utc AS detection_date_time_utc, a.animal_id, b.length, b.age, b.sex, a.release_location, a.utc_release_date_time AS release_date_time 
             FROM glatos_final a LEFT OUTER JOIN tagging b ON a.animal_id = b.animal_id) c WHERE c.release_location = 'Dow Chemical-Tittabawassee River' AND c.release_date_time < timestamp '2012-01-01 00:00:00'")

# pull in tags released.  Capture histories are based on tagging information (not fish recorded in detections)
tag <- query(db,"SELECT AGE, ANIMAL_ID, LENGTH, SEX, RELEASE_DATE_TIME, SURGERY_LOCATION FROM tagging WHERE surgery_location = 'Tittabawassee' and release_date_time < timestamp '2012-01-01 00:00:00+00'") 

# pull in model structure (created in excel):
sites <- read.csv("C:\\Users\\Todd\\Documents\\HECWL_analysis\\multi-state mark-recap-20130305\\sites.csv", as.is = TRUE, head = TRUE)

#############################################

# fix time in dtcs:
dtc$detection_date_time_utc <- paste(dtc$detection_date_time_utc, "00", sep = "")
dtc$detection_date_time_utc <- as.POSIXct(strptime(dtc$detection_date_time_utc, "%Y-%m-%d %H:%M:%S%z", tz = "GMT"),tz = "GMT")

# convert POSIX time to numeric in dtcs:
dtc$time <- as.numeric(dtc$detection_date_time_utc)
dtc <- dtc[order(dtc$animal_id, dtc$time),]

# function assigns arrival event number within a fish
arrive <- function(x){
  x$difference <- c(NA,diff(x$time))
  x$arrival <- c(1,x$glatos_array[2:nrow(x)] != x$glatos_array[1:(nrow(x)-1)]) | x$difference > thresh
  x$arrival <- as.numeric(x$arrival)
  x$arrival[1] <- 1
  x$event <- cumsum(x$arrival)
  return(x)
}
# plyr ddply function is much faster than loop.
dtc <- ddply(dtc, .(animal_id), arrive)

dtc <- dtc[order(dtc$animal_id, dtc$event, dtc$detection_date_time_utc),]
dtc$depart <- 0

# function assigns departure event within fish and event
depart <- function(x){
  x$depart[(nrow(x))] <- 1
  return(x)
}

dtc <- ddply(dtc, .(animal_id, event), depart)

# archive dtc so far.
det <- dtc

# Recode 'glatos_array' to distinguish outgoing fish movement past SGR and TTB receivers and incoming fish movement past SGR and TTB.  All TTB and SGR without event number 1 or 2
# are recoded to TTB1 and SGR1, respectively.
dtc$glatos_array1 <- ifelse((dtc$glatos_array == 'TTB' & dtc$event != 1) & (dtc$glatos_array == 'TTB' & dtc$event != 2), dtc$glatos_array1 <- 'TTB1', 
                            ifelse((dtc$glatos_array == 'SGR' & dtc$event != 1) & (dtc$glatos_array == 'SGR' & dtc$event != 2), dtc$glatos_array1 <- 'SGR1', dtc$glatos_array1 <- dtc$glatos_array ))

# Recode fish ages to groups (in tagging data) (used for making .inp file).  
#tag$age_group <- ifelse(tag$age < 6, tag$age_group <- 1, 
#                   ifelse(tag$age == 6, tag$age_group <- 2, 
#                    ifelse(tag$age == 7, tag$age_group <- 3, 
#                     ifelse(tag$age == 8, tag$age_group <- 4, tag$age_group <- 5))))

# Recode fish ages to groups (in tagging data) (used for making .txt file).  
tag$age_group <- ifelse(tag$age < 6, tag$age_group <- "<age6", 
                       ifelse(tag$age == 6, tag$age_group <- "age6", 
                             ifelse(tag$age == 7, tag$age_group <- "age7", 
                                   ifelse(tag$age == 8, tag$age_group <- "age8", tag$age_group <- "age9+"))))

#tag$age_group <- ifelse(tag$age < 6, tag$age_group <- "<age6", 
#                       ifelse(tag$age == 6 | tag$age == 7 | tag$age == 8, tag$age_group <- "age6,7,8", tag$age_group <- "age9+"))
# subset arrivals
dtc <- dtc[dtc$arrival == 1,]
dtc <- dtc[order(dtc$animal_id, dtc$detection_date_time_utc),]

# adds states and occasions to dtc based on info in sites data frame
dtc$occasion <- NA
dtc$state <- NA

# loop below pulls out each location id for each occasion and state in "sites" dataframe and then identifies "Occasion" and "state" of each detection.
for(i in 1:nrow(sites)){
  for(j in 2:ncol(sites)){
    sites.ij <- sites[i,j]
    sites.ij <- unlist(strsplit(sites.ij," " ))
    #below loop links sites location ID with detection based on common array IDs (i.e., GLATOS_ARRAY)
    for(k in 1:length(sites.ij)){
      if(!is.na(sites.ij[k])){
        dtc$occasion[grep(sites.ij[k], dtc$glatos_array1)] <- i  
        dtc$state[grep(sites.ij[k], dtc$glatos_array1)] <- names(sites)[j]
      } 
    }
  }
}


# loop below assigns "1" for a reversal (i.e., fish doubles back), based on the max occasion for each row to start within fish.
# this mark-recap model only considers fish movements away from release site and then the return of fish for spawning the following spring.

tags <- sort(unique(dtc$animal_id))
dtc <- dtc[order(dtc$animal_id, dtc$detection_date_time_utc),]
dtc$maxDtc <- NA
for(i in 1:length(tags)){
  fish.i <- dtc[dtc$animal_id == tags[i],]
  for(j in 1:nrow(fish.i)){
    fish.i$maxDtc[j] <- max(fish.i$occasion[1:j], na.rm = TRUE)
    dtc[match(row.names(fish.i), row.names(dtc)),] <- fish.i
  }
}

dtc$rev <- ifelse(dtc$maxDtc > dtc$occasion | is.na(dtc$occasion) == TRUE, 1,0)

#reversals = 1 and non-reversal = 0
dtc <- dtc[dtc$rev == 0,]

# All fish were released at single location.  Therefore all capture histories start with state "A".  
# Next line creates a vector of "A" for all tagged fish.

tag$capHist <- "A"

# Loop below selects each tagged animal and then extracts vector 1: max occasion from dtc and creates string of corresponding capture states. 
for(i in 1:nrow(tag)){
  dtc.i <- dtc[dtc$animal_id == tag$animal_id[i],]
  if(nrow(dtc.i) > 0){
    dtc.i <- dtc.i[1:match(max(dtc.i$occasion), dtc.i$occasion),]
  }
  for(j in 2:12){
    if(j %in% dtc.i$occasion){  
      tag$capHist[i] <- paste(tag$capHist[i], dtc.i$state[match(j,dtc.i$occasion)],sep = "")
    } else tag$capHist[i] <- paste(tag$capHist[i], 0, sep = "")
    
  }
}
# add covariates to capture history
# add dummy variables for sex to capHist
# remove 1 undertermined sex:
tag <- tag[tag$sex != 'U',]
tag$males <- as.numeric(tag$sex == 'M')
tag$females <- as.numeric(tag$sex == 'F')

# dummy variables for age groups (use when making .inp file):
#tag$age_group1 <- ifelse(tag$age_group == 1, 1, 0)
#tag$age_group2 <- ifelse(tag$age_group == 2, 1, 0)
#tag$age_group3 <- ifelse(tag$age_group == 3, 1, 0)
#tag$age_group4 <- ifelse(tag$age_group == 4, 1, 0)
#tag$age_group5 <- ifelse(tag$age_group == 5, 1, 0)


# combine sex and age groups with capHist
#markCH <-  paste(tag$capHist," ",tag$males," ",tag$females," ",tag$age_group1," ",tag$age_group2," ",tag$age_group3," ", tag$age_group4," ",tag$age_group5, " ;","/* ", tag$animal_id, " */" ,sep = "")
#markCH <- paste(tag$capHist," ",tag$males," ",tag$females," ;","/* ", tag$animal_id, " */" ,sep = "")

RMark <- tag
RMark$comments <- paste("/* ", tag$animal_id, " */")
RMark$freq <- 1

RMark <- RMark[, names(RMark) != "release_date_time"]
names(RMark)[1:2] <- c("ch", "freq")
colnames(RMark)[colnames(RMark) == "capHist"] <- "ch"
row.names(RMark) <- RMark$comments
RMark <- RMark[c("ch", "freq", "age", "animal_id", "length", "sex", "surgery_location", "age_group", "males", "females")]

write.table(RMark, "HECWL_sex.txt", quote = FALSE, sep = "\t", row.names = TRUE, col.names = TRUE)


#no sex, etc
#markCH <- paste(tag$capHist, "1;")
#markCH <-  paste(tag$capHist," ","1;", "/* ", tag$animal_id, " */" ,sep = "")

# write this table for import in to Program MARK.

#write.table(markCH, "hecwl_sex_20130305.inp", quote = FALSE, col.names = FALSE, row.names = FALSE)

################



# RMark analysis of HECWL data.  Uses functions-approach developed by C. Holbrook.





# matrix of structural parameters for fixing:
#strct <- read.csv("struct.csv", as.is = TRUE)

# load in data as .inp Mark file (this is problematic!  Easier to load as .txt file)
#hec <- convert.inp("hecwl_sex_age.inp", group.df=data.frame(sex = rep(c('male','female'),5), age = rep(c('<6','age6', 'age7', 'age8', 'age9+'), 2), use.comments=TRUE) #convert to data frame for RMark

# loads in data from .txt file- easier format with multiple grouping factors.  Use field.types to manipulate what factors will be used in analysis.  "S" = skip
hec <- import.chdata("HECWL_SEX.txt", header = TRUE, use.comments = TRUE, field.types=c('n','s','s', 's', 'f','s','f','s','s'))

# adds in group designation (if needed)
# hec$group <- 1
# hec$group <- as.factor(hec$group)  

# creates processed data:
hec.processed <- process.data(hec, groups=c('sex', 'age_group'), model = "Multistrata")


# creates design data- setting pim.type = "time" results in model with single release cohort.
hec.ddl <- make.design.data(hec.processed, parameters = list(S=list(pim.type="time"), p=list(pim.type="time"), Psi=list(pim.type="time", subtract.stratum=c('A','B','C'))))



# adds ParmNames to ddl:
hec.ddl <- addParmNames(hec.ddl)

# fixes structural parameters by subsetting .ddl

# estimable parms- these need to be checked prior to running model:
# S
stratSA <- c("A")
estTimeSA <- c(1,2,3)

stratSB <- c("B")
estTimeSB <- c(4,5,6,7,8,9)

stratSC <- c("C")
estTimeSC <- c(5)

# P
stratpA <- c("A")
estTimepA <- c(2,3)

stratpB <- c("B")
estTimepB <- c(4,5,6,7,8,9,10)

stratpC <- c("C")
estTimepC <- c(5,6)

# Psi
Psi.A.B <- c(3)
Psi.B.A <- c(4,5,6,7,8,9,10)
Psi.B.C <- c(4)
Psi.C.A <- c(5,6)
###

hec.ddl$S <- hec.ddl$S[(hec.ddl$S$stratum %in% stratSA & hec.ddl$S$time %in% estTimeSA) | 
                         (hec.ddl$S$stratum %in% stratSB & hec.ddl$S$time %in% estTimeSB) | 
                         (hec.ddl$S$stratum %in% stratSC & hec.ddl$S$time %in% estTimeSC), ]

hec.ddl$p <- hec.ddl$p[(hec.ddl$p$stratum %in% stratpA & hec.ddl$p$time %in% estTimepA) | 
                         (hec.ddl$p$stratum %in% stratpB & hec.ddl$p$time %in% estTimepB) | 
                         (hec.ddl$p$stratum %in% stratpC & hec.ddl$p$time %in% estTimepC), ]

hec.ddl$Psi <- hec.ddl$Psi[(hec.ddl$Psi$A == 1 & hec.ddl$Psi$toB ==1 & hec.ddl$Psi$time %in% Psi.A.B) |
                             (hec.ddl$Psi$B == 1 & hec.ddl$Psi$toA ==1 & hec.ddl$Psi$time %in% Psi.B.A) |
                             (hec.ddl$Psi$B == 1 & hec.ddl$Psi$toC ==1 & hec.ddl$Psi$time %in% Psi.B.C) |
                             (hec.ddl$Psi$C == 1 & hec.ddl$Psi$toA ==1 & hec.ddl$Psi$time %in% Psi.C.A) , ]
# verify .ddl
hec.ddl

# define models(full):

# full model:
modelDefs = list(
  ID = 1,
  name = list(
    S = 'S.timexsexxstrataxage_group',
    p = 'p.timexsexxstrataxage_group',
    Psi = 'Psi.timexsexxstrata_age_group'),
  formula = list(
    S = ~-1+time:sex:age_group:stratum,
    p = ~-1+time:sex:age_group:stratum,
    Psi = ~-1+time:sex:age_group:stratum:tostratum),
  factorVars = list(
    S = c('time','sex','age_group','stratum'),
    p = c('time','sex','age_group','stratum'),
    Psi= c('time','sex','age_group','stratum','tostratum')),
  useFixed = list(
    S = TRUE,
    p = TRUE,
    Psi = TRUE),
  link = list(
    S = 'sin',
    p = 'sin',
    Psi = 'mlogit'))

# Identify, by name, parameters to be fixed for boundary conditions:
fixBnd = list(fixS.1.name = c(
                              #'S.9.M.B',
                              #'S.9.F.B',
                              #'S.5.F.C',
                              #'S.5.M.C'
                              
                              
),
              
              fixp.0.name = c('p.4.F.A',
                              'p.4.M.A',
                              'p.5.F.A',
                              'p.5.M.A',
                              'p.6.F.A',
                              'p.6.M.A',
                              'p.7.M.A',
                              'p.7.F.A',
                              'p.8.M.A',
                              'p.8.F.A',
                              'p.9.F.A',
                              'p.9.M.A',
                              'p.10.M.A',
                              'p.10.F.A'
                              
              ),
              
              fixPsi.0.name = c('Psi.9.F.B.A',
                                'Psi.9.M.B.A',
                                'Psi.6.M.C.A',
                                'Psi.6.F.C.A'
                                
                                
              ),
              fixp.1.name = c('p.2.F.A',
                              'p.2.M.A',
                              'p.3.F.A',
                              'p.3.M.A',
                              'p.11.F.A',
                              'p.11.M.A',
                              'p.7.F.B',
                              'p.7.M.B',
                              'p.8.F.B',
                              'p.8.M.B',
                              'p.9.F.B',
                              'p.9.M.B',
                              'p.10.M.B',
                              'p.10.F.B',
                              'p.5.M.C',
                              'p.5.F.C'
                              
              )
)

#-------------------------------------------
# FROM HERE DOWN ALL THE SAME FOR EVERY MODEL- does not change with new model specs but does need to be included.

parmID = paste('parm.',modelDefs$ID,sep='')

# Add model-specific parameter names
hec.ddl = addParmNames(hec.ddl, modelDefs)

# Identify fixed parameter indices
fixed = fix.boundary(hec.ddl,fixBnd,parmID)

# Define models for each parameters  
assign(as.character(modelDefs$name$S), defineModel(parm='S',formula=modelDefs$formula$S,link=modelDefs$link$S))
assign(as.character(modelDefs$name$p), defineModel(parm='p',formula=modelDefs$formula$p,link=modelDefs$link$p))
assign(as.character(modelDefs$name$Psi), defineModel(parm='Psi',formula=modelDefs$formula$Psi,link=modelDefs$link$Psi))


########

# define models(no sex for p):

# S{time,sex,strata},p{time,strata},Psi{time,sex,strata}:
modelDefs = list(
  ID = 2,
  name = list(
    S = 'S.timexsexxstrata',
    p = 'p.timexstrata',
    Psi = 'Psi.timexsexxstrata'),
  formula = list(
    S = ~-1+time:sex:stratum,
    p = ~-1+time:stratum,
    Psi = ~-1+time:sex:stratum:tostratum),
  factorVars = list(
    S = c('time','sex','stratum'),
    p = c('time','stratum'),
    Psi= c('time','sex','stratum','tostratum')),
  useFixed = list(
    S = TRUE,
    p = TRUE,
    Psi = TRUE),
  link = list(
    S = 'sin',
    p = 'sin',
    Psi = 'mlogit'))

# Identify, by name, parameters to be fixed for boundary conditions:
fixBnd = list(fixS.1.name =                             c('S.4.F.A',
                                                          'S.4.M.A',
                                                          'S.5.F.A',
                                                          'S.5.M.A',
                                                          'S.6.F.A',
                                                          'S.6.M.A',
                                                          'S.7.F.A',
                                                          'S.7.M.A',
                                                          'S.8.M.A',
                                                          'S.8.F.A',
                                                          'S.9.M.A',
                                                          'S.9.F.A',
                                                          'S.10.M.A',
                                                          'S.10.F.A',
                                                          'S.11.F.A',
                                                          'S.11.M.A',
                                                          'S.9.M.B',
                                                          'S.9.F.B',
                                                          'S.5.F.C',
                                                          'S.5.M.C',
                                                          'S.5.F.B',
                                                          'S.5.M.B'
                                                          
                                                          
),
              
              fixp.0.name = c('p.4.A',
                              'p.5.A',
                              'p.6.A',
                              'p.7.A',
                              'p.8.A',
                              'p.9.A',
                              'p.10.A'
                              
              ),
              
              fixPsi.0.name = c('Psi.9.F.B.A',
                                'Psi.9.M.B.A',
                                'Psi.6.M.C.A',
                                'Psi.6.F.C.A'
                                
                                
              ),
              fixp.1.name = c('p.2.A',
                              'p.3.A',
                              'p.11.A',
                              'p.7.B',
                              'p.8.B',
                              'p.9.B',
                              'p.10.B',
                              'p.5.C'
                              
              )
)

#-------------------------------------------
# FROM HERE DOWN ALL THE SAME FOR EVERY MODEL- does not change with new model specs but does need to be included.

parmID = paste('parm.',modelDefs$ID,sep='')

# Add model-specific parameter names
hec.ddl = addParmNames(hec.ddl, modelDefs)

# Identify fixed parameter indices
fixed = fix.boundary(hec.ddl,fixBnd,parmID)

# Define models for each parameters  
assign(as.character(modelDefs$name$S), defineModel(parm='S',formula=modelDefs$formula$S,link=modelDefs$link$S))
assign(as.character(modelDefs$name$p), defineModel(parm='p',formula=modelDefs$formula$p,link=modelDefs$link$p))
assign(as.character(modelDefs$name$Psi), defineModel(parm='Psi',formula=modelDefs$formula$Psi,link=modelDefs$link$Psi))


######

# model 3 p{time,strata}, S{time,strata}, Psi{time,sex,strata}:
modelDefs = list(
  ID = 3,
  name = list(
    S = 'S.timexstrata',
    p = 'p.timexstrata',
    Psi = 'Psi.timexsexxstrata'),
  formula = list(
    S = ~-1+time:stratum,
    p = ~-1+time:stratum,
    Psi = ~-1+time:sex:stratum:tostratum),
  factorVars = list(
    S = c('time','stratum'),
    p = c('time','stratum'),
    Psi= c('time','sex','stratum','tostratum')),
  useFixed = list(
    S = TRUE,
    p = TRUE,
    Psi = TRUE),
  link = list(
    S = 'sin',
    p = 'sin',
    Psi = 'mlogit'))

# Identify, by name, parameters to be fixed for boundary conditions:
fixBnd = list(fixS.1.name = c(  'S.4.A',
                                
                                'S.5.A',
                                
                                'S.6.A',
                                
                                'S.7.A',
                                
                                
                                'S.8.A',
                                
                                'S.9.A',
                                'S.10.A',
                                
                                #'S.11.A',
                                
                                'S.9.B',
                                
                                'S.5.C',
                                'S.5.B'
                                
                                
                                
),
              
              fixp.0.name = c('p.4.A',
                              #'p.4.M.A',
                              'p.5.A',
                              #'p.5.M.A',
                              'p.6.A',
                              #'p.6.M.A',
                              'p.7.A',
                              #'p.7.F.A',
                              'p.8.A',
                              #'p.8.F.A',
                              'p.9.A',
                              #'p.9.M.A',
                              'p.10.A'
                              #'p.10.F.A'
                              
              ),
              
              fixPsi.0.name = c('Psi.9.F.B.A',
                                'Psi.9.M.B.A',
                                'Psi.6.M.C.A',
                                'Psi.6.F.C.A'
                                
                                
              ),
              fixp.1.name = c('p.2.A',
                              #'p.2.M.A',
                              'p.3.A',
                              #'p.3.M.A',
                              'p.11.A',
                              #'p.11.M.A',
                              'p.7.B',
                              #'p.7.M.B',
                              'p.8.B',
                              #'p.8.M.B',
                              'p.9.B',
                              #'p.9.M.B',
                              'p.10.B',
                              #'p.10.F.B',
                              'p.5.C'
                              #'p.5.F.C'
                              
              )
)

#-------------------------------------------
# FROM HERE DOWN ALL THE SAME FOR EVERY MODEL- does not change with new model specs but does need to be included.

parmID = paste('parm.',modelDefs$ID,sep='')

# Add model-specific parameter names
hec.ddl = addParmNames(hec.ddl, modelDefs)

# Identify fixed parameter indices
fixed = fix.boundary(hec.ddl,fixBnd,parmID)

# Define models for each parameters  
assign(as.character(modelDefs$name$S), defineModel(parm='S',formula=modelDefs$formula$S,link=modelDefs$link$S))
assign(as.character(modelDefs$name$p), defineModel(parm='p',formula=modelDefs$formula$p,link=modelDefs$link$p))
assign(as.character(modelDefs$name$Psi), defineModel(parm='Psi',formula=modelDefs$formula$Psi,link=modelDefs$link$Psi))


#####

# define models(no sex for S):

# model 4:
modelDefs = list(
  ID = 4,
  name = list(
    S = 'S.timexstrata',
    p = 'p.timexstrata',
    Psi = 'Psi.timexstrata'),
  formula = list(
    S = ~-1+time:stratum,
    p = ~-1+time:stratum,
    Psi = ~-1+time:stratum:tostratum),
  factorVars = list(
    S = c('time','stratum'),
    p = c('time','stratum'),
    Psi= c('time','stratum','tostratum')),
  useFixed = list(
    S = TRUE,
    p = TRUE,
    Psi = TRUE),
  link = list(
    S = 'sin',
    p = 'sin',
    Psi = 'mlogit'))

# Identify, by name, parameters to be fixed for boundary conditions:
fixBnd = list(fixS.1.name = c(
  'S.4.A',
  'S.5.A',
  'S.6.A',
  
  'S.7.A',
  
  'S.8.A',
  
  'S.9.A',
  
  'S.10.A',
  
  'S.11.A',
  
  'S.9.B',
  
  'S.5.C',
  'S.5.B',
  'S.1.B',
  'S.2.B',
  'S.3.B',
  'S.10.B',
  'S.11.B',
  'S.1.C',
  'S.2.C',
  'S.3.C',
  'S.4.C',
  'S.6.C',
  'S.7.C',
  'S.8.C',
  'S.9.C',
  'S.10.C',
  'S.11.C'
  
  
  
  
  
  
  
),
              
              fixp.0.name = c('p.4.A',
                              #'p.4.M.A',
                              'p.5.A',
                              #'p.5.M.A',
                              'p.6.A',
                              #'p.6.M.A',
                              'p.7.A',
                              #'p.7.F.A',
                              'p.8.A',
                              #'p.8.F.A',
                              'p.9.A',
                              #'p.9.M.A',
                              'p.10.A'
                              
                              
                              
                              
              ),
              
              fixPsi.0.name = c('Psi.9.B.A',
                                
                                'Psi.6.C.A'
                                #'Psi.6.F.C.A'
                                
                                
              ),
              fixp.1.name = c('p.2.A',
                              #'p.2.M.A',
                              'p.3.A',
                              #'p.3.M.A',
                              'p.11.A',
                              #'p.11.M.A',
                              'p.7.B',
                              #'p.7.M.B',
                              'p.8.B',
                              #'p.8.M.B',
                              'p.9.B',
                              #'p.9.M.B',
                              'p.10.B',
                              #'p.10.F.B',
                              'p.5.C'
                              #'p.5.F.C'
                              
              )
)
#-------------------------------------------
# FROM HERE DOWN ALL THE SAME FOR EVERY MODEL- does not change with new model specs but does need to be included.

parmID = paste('parm.',modelDefs$ID,sep='')

# Add model-specific parameter names
hec.ddl = addParmNames(hec.ddl, modelDefs)

# Identify fixed parameter indices
fixed = fix.boundary(hec.ddl,fixBnd,parmID)
fixed

# Define models for each parameters  
assign(as.character(modelDefs$name$S), defineModel(parm='S',formula=modelDefs$formula$S,link=modelDefs$link$S))
assign(as.character(modelDefs$name$p), defineModel(parm='p',formula=modelDefs$formula$p,link=modelDefs$link$p))
assign(as.character(modelDefs$name$Psi), defineModel(parm='Psi',formula=modelDefs$formula$Psi,link=modelDefs$link$Psi))


##########


model.list=create.model.list("Multistrata") #automatically pulls all model parameters in list for model selection.

#verify model.list:
model.list

# fits model from model.list
hec.results <- mark.wrapper(model.list, data = hec.processed, ddl=hec.ddl, invisible = FALSE, title = 'hec_test', realvcv = TRUE, profile.int = FALSE)

# Identify best model and view estimates
bestModel = as.numeric(rownames(hec.results$model.table[hec.results$model.table$weight == max(hec.results$model.table$weight),]))

# Check for and identify "singular" parms in best model
identifySingularParms(hec.results, bestModel)

hec.results

# View summary for best model  
summary.mark(hec.results[[bestModel]], se= TRUE, showall = TRUE)

#extract real results:
hec.results[[bestModel]]$results$real

#extract real vcv:
hec.results[[bestModel]]$results$real.vcv
ark <- hec.results

### calculate derived parms:

# assign reasonable parm names:
re <- hec.results[[bestModel]]$results$real

re$Parm <-row.names(re)
row.names(re) <- seq(from = 1, to = (nrow(re)), by = 1)
re$Index <- seq(from = 1, to = (nrow(re)), by = 1)
rePsi <- re[c(grep("Psi", re$Parm)),]
reS <- re[c(grep("S", re$Parm)),]
reP <- re[c(grep("p", re$Parm)),]

reS_split <- data.frame(do.call(rbind, strsplit(as.vector(reS$Parm), split =" ")))
reP_split <- data.frame(do.call(rbind,strsplit(as.vector(reP$Parm), split = " ")))
rePsi_split <- data.frame(do.call(rbind, strsplit(as.vector(rePsi$Parm), split = " ")))

reS_split$Parm1 <- with(reS_split, paste(X1,".",(substr(X4,2,3)),".", (substr(X2,2,3)), sep = ""))
reP_split$Parm1 <- with(reP_split, paste(X1,".",(substr(X4,2,3)), ".",(substr(X2,2,3)), sep = ""))
rePsi_split$Parm1 <- with(rePsi_split, paste(X1, ".", (substr(X5,2,3)), ".", (substr(X2,2,3)),".", (substr(X3,3,4)),sep = ""))

reS_split <- cbind(reS, reS_split)
reP_split <- cbind(reP, reP_split)
rePsi_split <- cbind(rePsi, rePsi_split)

reS_split <- reS_split[,c("Index","Parm1")]
reP_split <- reP_split[,c("Index","Parm1")]
rePsi_split <- rePsi_split[,c("Index","Parm1")]

all <- rbind(reS_split,reP_split,rePsi_split)

re <- merge(re, all, by = "Index", all.x = TRUE)

re <- re[,-c(6,7,8)]
names(re)[c(2,6)] <- c("Estimate","Parm")

# pull in vcv matrix:
vcv <- hec.results[[bestModel]]$results$real.vcv

# Estimate derived parameters from 2011 HECWL mark recapture model.  Uses real paramter output from MARK and variance-covariance matrix (of real paramters) inputs.
# SE estimates are calculated using the Delta Method.

#Estimate derived parameters from 2011 HECWL mark recapture model

library(msm) #for deltamethod
#re <- hec.results[[1]]$results.real


#re1 <- read.csv("~/HECWL_analysis/multi-state mark-recap-20130305/results_20130310.csv",as.is=T) #read in real parameter estimates
#vcv1 <- as.matrix(read.csv("~/HECWL_analysis/multi-state mark-recap-20130305/vcv_20130310.csv",as.is=T,header=F)) #read variance covariance matrix

#define the derived parameters. S.5.B, S.5.C were changed to 1. (these were fixed Parms that were not included in RMark results)
derParm <- list(
  phi.1.A.A="S.1.A",
  phi.1.A.x="1-S.1.A",
  
  phi.2.A.A="S.2.A",
  phi.2.A.x="1-S.2.A",
  
  phi.3.A.A="S.3.A*(1-Psi.3.A.B)",
  phi.3.A.B="S.3.A*Psi.3.A.B",
  phi.3.A.x="1-S.3.A",
  
  phi.4.B.A="S.4.B*Psi.4.B.A",
  phi.4.B.B="S.4.B*(1-Psi.4.B.A-Psi.4.B.C)",
  phi.4.B.C="S.4.B*Psi.4.B.C",
  phi.4.B.x="1-S.4.B",
  
  phi.5.B.A="1*Psi.5.B.A",
  phi.5.B.B="1*(1-Psi.5.B.A)",
  phi.5.B.x="1-1",
  phi.5.C.A="1*Psi.5.C.A",
  phi.5.C.C="1*(1-Psi.5.C.A)",
  phi.5.C.x="(1-1)",
  
  phi.6.B.A="S.6.B*Psi.6.B.A",
  phi.6.B.B="S.6.B*(1-Psi.6.B.A)",
  phi.6.B.x="1-S.6.B",
  phi.6.C.A="1*Psi.6.C.A",
  phi.6.C.x="1-Psi.6.C.A",
  
  phi.7.B.A="S.7.B*Psi.7.B.A",
  phi.7.B.B="S.7.B*(1-Psi.7.B.A)",
  phi.7.B.X="1-S.7.B",
  
  phi.8.B.A="S.8.B*Psi.8.B.A",
  phi.8.B.B="S.8.B*(1-Psi.8.B.A)",
  phi.8.B.X="1-S.8.B",
  
  phi.9.B.A="1*Psi.9.B.A",
  phi.9.B.B="1*(1-Psi.9.B.A)",
  phi.9.B.X="1-1",
  
  phi.10.B.A="1*Psi.10.B.A",
  phi.10.B.x="1-Psi.10.B.A"
  
  
)

derParm$WHT_ret <- with(derParm, paste(phi.5.C.A,"+",phi.5.C.C,"*(",phi.6.C.A,")",sep=""))
derParm$BBI_ret <- with(derParm, paste(phi.10.B.A, sep = ""))
derParm$FMP_ret <- with(derParm, paste(phi.9.B.A,"+",phi.9.B.B,"*(",BBI_ret,")",sep=""))

derParm$PRS_ret <- with(derParm, paste(phi.8.B.A,"+",phi.8.B.B,"*(",FMP_ret,")",sep=""))
derParm$THB_ret <- with(derParm, paste(phi.7.B.A,"+",phi.7.B.B,"*(",PRS_ret,")",sep=""))
derParm$STG_ret <- with(derParm, paste(phi.6.B.A,"+",phi.6.B.B,"*(",THB_ret,")",sep=""))
derParm$OSC_ret <- with(derParm, paste(phi.5.B.A,"+",phi.5.B.B,"*(",STG_ret,")",sep=""))
derParm$SBO_ret <- with(derParm, paste(phi.4.B.A,"+",phi.4.B.B,"*(",OSC_ret,")+",phi.4.B.C,"*(",WHT_ret,")",sep=""))
derParm$SGR_ret <- with(derParm, paste(phi.3.A.A,"+",phi.3.A.B,"*(",SBO_ret,")",sep=""))

derParm$rel_SGR <- with(derParm, paste(phi.1.A.A,"*(",phi.2.A.A,")", sep="" ))
derParm$rel_SBI <- with(derParm, paste(phi.1.A.A,"*(",phi.2.A.A,")","*(",phi.3.A.B,")",sep = ""))
derParm$BWB_ret <- with(derParm, paste(phi.6.C.A,sep=""))

derParm$SGR_OSC <- with(derParm, paste(phi.3.A.B,"*(",phi.4.B.B,")", sep = ""))
derParm$SGR_STG <- with(derParm, paste(phi.3.A.B,"*(",phi.4.B.B,")","*(",phi.5.B.B,")", sep = ""))
derParm$SGR_THB <- with(derParm, paste(phi.3.A.B,"*(",phi.4.B.B,")","*(",phi.5.B.B,")","*(",phi.6.B.B,")", sep = ""))
derParm$SGR_PRS <- with(derParm, paste(phi.3.A.B,"*(",phi.4.B.B,")","*(",phi.5.B.B,")","*(",phi.6.B.B,")","*(",phi.7.B.B,")",sep = ""))
derParm$SGR_WHT <- with(derParm, paste(phi.3.A.B,"*(",phi.4.B.C,")",sep = ""))
derParm$SGR_BWB <- with(derParm, paste(phi.3.A.B,"*(",phi.4.B.C,")","*(",phi.5.C.C,")",sep = ""))

derParm$SBI_OSC <- with(derParm, paste("(",phi.4.B.B,")", sep = ""))
derParm$SBI_STG <- with(derParm, paste(phi.4.B.B,"*(",phi.5.B.B,")", sep = ""))
derParm$SBI_THB <- with(derParm, paste(phi.4.B.B,"*(",phi.5.B.B,")","*(",phi.6.B.B,")", sep = ""))
derParm$SBI_PRS <- with(derParm, paste(phi.4.B.B,"*(",phi.5.B.B,")","*(",phi.6.B.B,")","*(",phi.7.B.B,")",sep = ""))
derParm$SBI_FMP <- with(derParm, paste(phi.4.B.B,"*(",phi.5.B.B,")","*(",phi.6.B.B,")","*(",phi.7.B.B,")","*(",phi.8.B.B,")",sep=""))
derParm$SBI_BBI <- with(derParm, paste(phi.4.B.B,"*(",phi.5.B.B,")","*(",phi.6.B.B,")","*(",phi.7.B.B,")","*(",phi.8.B.B,")","*(",phi.9.B.B,")",sep=""))
derParm$SBI_WHT <- with(derParm, paste(phi.4.B.C))
derParm$SBI_BWB <- with(derParm, paste(phi.4.B.C,"*(",phi.5.C.C,")"))
derParm$SGR_BWB <- with(derParm, paste(phi.3.A.B,"*(",phi.4.B.C,")","*(",phi.5.C.C,")",sep = ""))



#make data frame to hold results
derEst <- as.data.frame(matrix(NA,nrow=length(derParm),ncol=5))
names(derEst) <- c("Parm","Estimate","SE","LCI","UCI") 


#create objects with model parameter estimates as names
for(i in 1:nrow(re)){
  assign(re$Parm[i],re$Estimate[i])
}

derEst$Parm <- names(derParm) #set names in results set


#evaluate the expression to calculate derived estimate
for(i in 1:length(derParm)){
  derEst$Estimate[i] <- eval(parse(text=derParm[i]))
} 

#make formula list 
form <- derParm

for(i in 1:length(form)){
  for(j in 1:nrow(re)){
    form[[i]] <- gsub(re$Parm[j],paste("x",re$Index[j],sep=""),form[[i]])
  }
  form[[i]] <- as.formula(paste("~",form[[i]]))
}

derEst$SE <- deltamethod(form,re$Estimate,vcv) #calculate standard error

derEst$LCI <- with(derEst, Estimate-1.96*SE) 
derEst$UCI <- with(derEst, Estimate+1.96*SE)

#write.csv(derEst, "DerivedParms_todd_final.csv")


# cleanup unused files in working directory:
cleanup(ask=FALSE)

# export data file for MARK (as .inp)
export.chdata(hec.processed, filename = 'hec')

# export model results- will produce three files for each model in model list- see details in ?export.model for importing in to MARK.
export.model(hec.results)
