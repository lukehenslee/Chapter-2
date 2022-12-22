#==================================================================================================
# Thesis chapter 2
# Date: September 7, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: Assign detection histories to coho salmon with paired sex, stock, and
# capture timing data. 
#
# Model movement probabilities using RMARK
#==================================================================================================
#NOTES: Multistate mark recapture detection histories
#
# States:
# A: Shaktoolik
# V: Entered stream draining into Shaktoolik subdistrict
# B: Unalakleet
# W: Entered stream draining into Unalakleet subdistrict
# X: Outside
#
# Detection histories are unrestricted in length, but only transitions between
# two states is recorded.
# 
# The following are impossible transitions:
# X<->V
# X<->W
# V<->W
# A<->W
# B<->V
#
#==================================================================================================

# Load packages ####
library(readr)
library(tidyverse)
library(data.table)
library(lubridate)
library(openxlsx)
library(magrittr)
library(RMark)

# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/git_repo/Chapter-2")

# Multi-state modeling ####

# In this model, there are three moves allowed after capture event (four total
# steps in detection history), and the last step in the history will reflect 
# 'stock' grouping. What we want to know is, 'what is the probability of a fish 
# in group XX moving to area XX in step XX?' 

## Import data ####
ch <- read.csv('data/ch/stock_sex_week_year_ABXVW0.csv', 
               colClasses = c('numeric', 'character', 'factor', 'factor',
                              'numeric', 'factor'))

  ## Detection history data paired with sex, stock, year and week of capture
  ## I want day of year as a continuous variable, so let's modify the dataset

col <- read.csv('data/tag_colClasses.csv')
tag <- read.csv('data/tag.csv', colClasses = paste(col[1,]))

tag$day.of.year <- yday(mdy(tag$capture.date))

ch <- left_join(ch, tag[c(1, 25)], by = 'tag.ID')

  ## So now 'stat.week' is a factor variable and 'day.of.year' is continuous

  ## We are first going to model with 'stock' groups, and some of the rows 
  ## contain NA's. We'll make a detection history with only complete cases

ch.stock <- ch[complete.cases(ch),]

## Modeling ####

## Start with stock grouping

## Make process data
coho.proc <- process.data(ch.stock, model = 'Multistrata', 
                          groups = c('sex', 'year', 'stock', 'stat.week'))
  ## Setting 'sex', 'year', 'stat.week', and 'stock' to grouping variables

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix survival in 'V' to 1 ## 
# coho.ddl$S$fix <- NA
# coho.ddl$S$fix[coho.ddl$S$stratum == 'V'] <- 1
  # This was originally done because 'survival' was used as a proxy for movement
  # In this version all fish survive to be detected in homestreams
  # May need to fix all strata to '1'

  #coho.ddl$S$fix#

## Fix detection probabilities 
  # These are based on all coho salmon detections
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum == 'A'] <- 0.94
coho.ddl$p$fix[coho.ddl$p$stratum == 'B'] <- 0.96
coho.ddl$p$fix[coho.ddl$p$stratum == 'X'] <- 0.95
coho.ddl$p$fix[coho.ddl$p$stratum == 'V'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'W'] <- 0.98
  # These values are based on observed detection probabilities of all tagged 
  # coho
  # Detections of only coho tracked to streams is ~1

coho.ddl$p$fix

## Fix psi prob to zero for times and states
coho.ddl$Psi$fix <- NA

# Can't move FROM 'X', 'V', or 'W' in the first move
## Remove if there is no time structure
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'X' & coho.ddl$Psi$time == 1] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$time == 1] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$time == 1] <- 0

# Can't move directly between 'X' and 'V' and 'W'
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'X' & coho.ddl$Psi$tostratum == 'V'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'X' & coho.ddl$Psi$tostratum == 'W'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$tostratum == 'X'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$tostratum == 'W'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$tostratum == 'V'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$tostratum == 'X'] <- 0

# Can't move from 'A' to 'W'
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$tostratum == 'A'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'A' & coho.ddl$Psi$tostratum == 'W'] <- 0

# Can't move from 'B' to 'V'
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'B' & coho.ddl$Psi$tostratum == 'V'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$tostratum == 'B'] <- 0

# And you can't stay in the same state
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'A' & coho.ddl$Psi$tostratum == 'A'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'B' & coho.ddl$Psi$tostratum == 'B'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'X' & coho.ddl$Psi$tostratum == 'X'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$tostratum == 'V'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$tostratum == 'W'] <- 0

coho.ddl$Psi$fix

#### Model selection ####

coho.models <- function ()
{
  ## phi structures 
  S.time <- list(formula = ~time)
  S.state <- list(formula = ~stratum)
  S.sex <- list(formula = ~sex)
  S.stock <- list(formula = ~stock)
  #S.day <- list(formula = ~day.of.year)
  S.week <- list(formula = ~stat.week)
  S.year <- list(formula = ~year)
  S.time.state <- list(formula = ~time + stratum)
  S.time.state.sex <- list(formula = ~time + stratum + sex)
  S.time.state.stock <- list(formula = ~time + stratum + stock)
  S.time.state.sex.stock <- list(formula = ~time + stratum + sex + stock)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  ## Let RMARK estimate detection probabilities 
  # p.int <- list(formula = ~1)
  
  ## psi
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + sex)
  Psi.state.tostate.stock <- list(formula = ~stratum + tostratum + stock)
  #Psi.state.tostate.day <- list(formula = ~stratum + tostratum + day.of.year)
  Psi.state.tostate.week <- list(formula = ~stratum + tostratum + stat.week)
  Psi.state.tostate.year <- list(formula = ~stratum + tostratum + year)
  Psi.state.tostate.stock.time <- list(formula = ~stratum + tostratum + stock + time)
  Psi.state.tostate.sex.stock.time <- list(formula = ~stratum + tostratum + sex + stock + time)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results <- coho.models()  
### coho.results ####

#                                                                                    model npar     AICc   DeltaAICc       weight
#57                            S(~time + stratum + stock)p(~fix)Psi(~stratum + tostratum)   18 2232.881    0.000000 3.675559e-01
#49                      S(~time + stratum + sex + stock)p(~fix)Psi(~stratum + tostratum)   19 2234.888    2.007589 1.347042e-01
#58                      S(~time + stratum + stock)p(~fix)Psi(~stratum + tostratum + sex)   19 2234.977    2.096189 1.288670e-01
#64                     S(~time + stratum + stock)p(~fix)Psi(~stratum + tostratum + year)   19 2234.977    2.096189 1.288670e-01
#50                S(~time + stratum + sex + stock)p(~fix)Psi(~stratum + tostratum + sex)   20 2236.990    4.109032 4.710408e-02
#56               S(~time + stratum + sex + stock)p(~fix)Psi(~stratum + tostratum + year)   20 2236.990    4.109032 4.710408e-02
#60                    S(~time + stratum + stock)p(~fix)Psi(~stratum + tostratum + stock)   20 2237.079    4.197632 4.506292e-02
#62                     S(~time + stratum + stock)p(~fix)Psi(~stratum + tostratum + time)   20 2237.079    4.197632 4.506292e-02

# Let's run with just the models within ~4 AIC values for clarity

coho.models.2 <- function ()
{
  ## phi structures 
  S.time.state.stock <- list(formula = ~time + stratum + stock)
  S.time.state.sex.stock <- list(formula = ~time + stratum + sex + stock)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + sex)
  Psi.state.tostate.stock <- list(formula = ~stratum + tostratum + stock)
  Psi.state.tostate.year <- list(formula = ~stratum + tostratum + year)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results.2 <- coho.models.2()  

### coho.results.2 ####

# Let's rerun one more time with S(~time+stratum+stock only)
## Make process data
coho.proc <- process.data(ch.stock, model = 'Multistrata', 
                          groups = c('sex', 'year', 'stock'))
## Setting 'sex', 'year', 'stat.week', and 'stock' to grouping variables

## Design data
coho.ddl <- make.design.data(coho.proc)
coho.models.3 <- function ()
{
  ## phi structures 
  S.time.state.stock <- list(formula = ~time + stratum + stock)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + sex)
  Psi.state.tostate.year <- list(formula = ~stratum + tostratum + year)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results.3 <- coho.models.3()  

# Okay, now we have a nice list of three models, now what?
# Let's look at unadjusted AICc 

coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate$results$AICc.unadjusted
coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate.sex$results$AICc.unadjusted
coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate.year$results$AICc.unadjusted

# They all have the same AIC score?
# Let's look at unadjusted npar

coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate$results$npar.unadjusted
coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate.sex$results$npar.unadjusted
coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate.year$results$npar.unadjusted

# If we want to see total output
coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate$output # mark305
coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate.sex$output # mark306
coho.results.3$S.time.state.stock.p.fixed.Psi.state.tostate.year$output # mark307

# Fletcher chat
# Model 1: 1.086
# Model 2: 1.086
# Model 3: 1.086





#                                                        model npar     AICc  DeltaAICc       weight Deviance
#3                         S(~1)p(~1)Psi(~stratum + tostratum)   11 2232.036   0.000000 7.291350e-01 2192.535
#4                   S(~1)p(~1)Psi(~stratum + tostratum + sex)   12 2234.128   2.091854 2.561932e-01 2192.535
#7             S(~1)p(~1)Psi(~stratum + tostratum + stat.week)   15 2240.452   8.415231 1.085085e-02 2192.535
#5       S(~1)p(~1)Psi(~stratum + tostratum + sex + stat.week)   16 2242.576  10.539113 3.752050e-03 2192.535
#8        S(~1)p(~1)Psi(~stratum + tostratum + stat.week:year)   20 2251.153  19.116330 5.149317e-05 2192.535
#6  S(~1)p(~1)Psi(~stratum + tostratum + sex + stat.week:year)   21 2253.318  21.281292 1.744352e-05 2192.535
#1                                           S(~1)p(~1)Psi(~1)    3 2574.392 342.355238 0.000000e+00 2551.345
#2                                         S(~1)p(~1)Psi(~sex)    4 2576.422 344.385172 0.000000e+00 2551.345
#11                                       S(~1)p(~1)Psi(~year)    4 2576.422 344.385172 0.000000e+00 2551.345
#9                                       S(~1)p(~1)Psi(~stock)    5 2578.459 346.422695 0.000000e+00 2551.345
#10                                  S(~1)p(~1)Psi(~stat.week)    7 2582.557 350.520677 0.000000e+00 2551.345


### phi and p are fixed, one model for each grouping variable, 'stat.week' is continuous ####
## Make process data
coho.proc <- process.data(ch, model = 'Multistrata', 
                          groups = c('sex', 'year', 'stock'))
## Setting 'sex', 'year', and 'stock' to grouping variables

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix survival ito 1 ## 
coho.ddl$S$fix <- NA
coho.ddl$S$fix[coho.ddl$S$stratum == 'A'] <- 1
coho.ddl$S$fix[coho.ddl$S$stratum == 'B'] <- 1
coho.ddl$S$fix[coho.ddl$S$stratum == 'V'] <- 1
coho.ddl$S$fix[coho.ddl$S$stratum == 'W'] <- 1
coho.ddl$S$fix[coho.ddl$S$stratum == 'X'] <- 1
# In this version all fish survive to be detected in homestreams
# May need to fix all strata to '1'

coho.ddl$S$fix#

## Fix detection probabilities 
# These are based on all coho salmon detections which was ~1
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum == 'A'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'B'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'X'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'V'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'W'] <- 1

coho.ddl$p$fix

## Fix psi prob to zero for times and states
coho.ddl$Psi$fix <- NA

# Can't move FROM 'X', 'V', or 'W' in the first move
## Remove if there is no time structure
#coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'X' & coho.ddl$Psi$time == 1] <- 0
#coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$time == 1] <- 0
#coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$time == 1] <- 0

# Can't move directly between 'X' and 'V' and 'W'
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'X' & coho.ddl$Psi$tostratum == 'V'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'X' & coho.ddl$Psi$tostratum == 'W'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$tostratum == 'X'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$tostratum == 'W'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$tostratum == 'V'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$tostratum == 'X'] <- 0

# Can't move from 'A' to 'W'
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$tostratum == 'A'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'A' & coho.ddl$Psi$tostratum == 'W'] <- 0

# Can't move from 'B' to 'V'
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'B' & coho.ddl$Psi$tostratum == 'V'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$tostratum == 'B'] <- 0

# And you can't stay in the same state
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'A' & coho.ddl$Psi$tostratum == 'A'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'B' & coho.ddl$Psi$tostratum == 'B'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'X' & coho.ddl$Psi$tostratum == 'X'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'V' & coho.ddl$Psi$tostratum == 'V'] <- 0
coho.ddl$Psi$fix[coho.ddl$Psi$stratum == 'W' & coho.ddl$Psi$tostratum == 'W'] <- 0

coho.ddl$Psi$fix

#### Model selection ####

coho.models <- function ()
{
  ## phi structures 
  #S.state <- list(formula = ~stratum)
  #S.sex <- list(formula = ~sex)
  #S.sex.state <- list(formula = ~sex + stratum)
  ## All tagged coho survived so set phi to intercept
  #S.int <- list(formula = ~1)
  #phi set to 1
  S.fixed <- list(formula = ~fix)
  
  ## fixed values for p based on our earlier analyses
  # p.fixed <- list(formula = ~fix)
  ## Let RMARK estimate detection probabilities 
  #p.int <- list(formula = ~1)
  # Detection probability set to 1
  p.fixed <- list(formula = ~fix)
  
  ## psi
  #Psi.int <- list(formula = ~1)
  #Psi.sex <- list(formula = ~sex)
  #Psi.stock <- list(formula = ~stock)
  #Psi.week <- list(formula = ~stat.week)
  #Psi.year <- list(formula = ~year)
  # Movement dependent on state you are in and which you are moving to
  # so 'state.tostate' will be in every model 
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + sex)
  Psi.state.tostate.stock <- list(formula = ~stratum + tostratum + stock)
  Psi.state.tostate.year <- list(formula = ~stratum + tostratum + year)
  Psi.state.tostate.week <- list(formula = ~stratum + tostratum + stat.week)
  Psi.state.tostate.sex.year <- list(formula = ~stratum + tostratum + sex + year)
  Psi.state.tostate.sex.week <- list(formula = ~stratum + tostratum + sex + 
                                   stat.week)
  
  #Psi.state.tostate.year <- list(formula = ~stratum + tostratum + stat.week
  #                               + sex + stock)
  
  
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results <- coho.models()  
coho.results

# coho.results

#                                                       model npar     AICc DeltaAICc     weight Deviance
#1                   S(~fix)p(~fix)Psi(~stratum + tostratum)    9 2227.876  0.000000 0.41030607 2173.977
#2             S(~fix)p(~fix)Psi(~stratum + tostratum + sex)   10 2229.952  2.076111 0.14530686 2173.977
#6       S(~fix)p(~fix)Psi(~stratum + tostratum + stat.week)   10 2229.952  2.076111 0.14530686 2209.537
#7            S(~fix)p(~fix)Psi(~stratum + tostratum + year)   10 2229.952  2.076111 0.14530686 2173.977
#3 S(~fix)p(~fix)Psi(~stratum + tostratum + sex + stat.week)   11 2232.036  4.160072 0.05125779 2209.537
#4      S(~fix)p(~fix)Psi(~stratum + tostratum + sex + year)   11 2232.036  4.160072 0.05125779 2173.977
#5           S(~fix)p(~fix)Psi(~stratum + tostratum + stock)   11 2232.036  4.160072 0.05125779 2173.977


# Params of 'best' model
coho.results$S.sex.state.p.fixed.Psi.state.tostate$results$real

# Transition matricies 
Psilist <- get.real(coho.results$S.sex.state.p.fixed.Psi.state.tostate,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$sex == 'M',])
coho.psi


# Build movement estimates and incorporate survival
S.coho.F <- coho.results$S.sex.state.p.fixed.Psi.state.tostate$results$real[c(1:2,9,3:4), c(1:4)]
S.coho.M <- coho.results$S.sex.state.p.fixed.Psi.state.tostate$results$real[c(5:6,9,7:8), c(1:4)]

# FEMALES
coho.psi.F.est <- coho.psi
coho.psi.F.lcl <- coho.psi
coho.psi.F.ucl <- coho.psi

# Movement from A
coho.psi.F.est[1,] <- coho.psi[1,] * S.coho.F[1,1] # est
coho.psi.F.lcl[1,] <- coho.psi[1,] * S.coho.F[1,3] # lcl
coho.psi.F.ucl[1,] <- coho.psi[1,] * S.coho.F[1,4] # ucl
coho.psi.F.est[1,1] <- 1 - sum(coho.psi[1,] * S.coho.F[1,1]) # Stop migration
coho.psi.F.lcl[1,1] <- 1 - sum(coho.psi[1,] * S.coho.F[1,3]) # Stop migration lcl
coho.psi.F.ucl[1,1] <- 1 - sum(coho.psi[1,] * S.coho.F[1,4]) # Stop migration ucl

# Movement from B
coho.psi.F.est[2,] <- coho.psi[2,] * S.coho.F[2,1] # est
coho.psi.F.lcl[2,] <- coho.psi[2,] * S.coho.F[2,3] # lcl
coho.psi.F.ucl[2,] <- coho.psi[2,] * S.coho.F[2,4] # ucl
coho.psi.F.est[2,2] <- 1 - sum(coho.psi[2,] * S.coho.F[2,1]) # Stop migration
coho.psi.F.lcl[2,2] <- 1 - sum(coho.psi[2,] * S.coho.F[2,3]) # Stop migration lcl
coho.psi.F.ucl[2,2] <- 1 - sum(coho.psi[2,] * S.coho.F[2,4]) # Stop migration ucl

# Movement from V
coho.psi.F.est[3,] <- coho.psi[3,] * S.coho.F[3,1] # est
coho.psi.F.lcl[3,] <- coho.psi[3,] * S.coho.F[3,3] # lcl
coho.psi.F.ucl[3,] <- coho.psi[3,] * S.coho.F[3,4] # ucl
coho.psi.F.est[3,3] <- 1 - sum(coho.psi[3,] * S.coho.F[3,1]) # Stop migration
coho.psi.F.lcl[3,3] <- 1 - sum(coho.psi[3,] * S.coho.F[3,3]) # Stop migration lcl
coho.psi.F.ucl[3,3] <- 1 - sum(coho.psi[3,] * S.coho.F[3,4]) # Stop migration ucl

# Movement from W
coho.psi.F.est[4,] <- coho.psi[4,] * S.coho.F[4,1] # est
coho.psi.F.lcl[4,] <- coho.psi[4,] * S.coho.F[4,3] # lcl
coho.psi.F.ucl[4,] <- coho.psi[4,] * S.coho.F[4,4] # ucl
coho.psi.F.est[4,4] <- 1 - sum(coho.psi[4,] * S.coho.F[4,1]) # Stop migration
coho.psi.F.lcl[4,4] <- 1 - sum(coho.psi[4,] * S.coho.F[4,3]) # Stop migration lcl
coho.psi.F.ucl[4,4] <- 1 - sum(coho.psi[4,] * S.coho.F[4,4]) # Stop migration ucl

# Movement from X
coho.psi.F.est[5,] <- coho.psi[5,] * S.coho.F[5,1] # est
coho.psi.F.lcl[5,] <- coho.psi[5,] * S.coho.F[5,3] # lcl
coho.psi.F.ucl[5,] <- coho.psi[5,] * S.coho.F[5,4] # ucl
coho.psi.F.est[5,5] <- 1 - sum(coho.psi[5,] * S.coho.F[5,1]) # Stop migration
coho.psi.F.lcl[5,5] <- 1 - sum(coho.psi[5,] * S.coho.F[5,3]) # Stop migration lcl
coho.psi.F.ucl[5,5] <- 1 - sum(coho.psi[5,] * S.coho.F[5,4]) # Stop migration ucl

round(coho.psi.F.est, 3)
round(coho.psi.F.lcl, 3)
round(coho.psi.F.ucl, 3)

# MALES
coho.psi.M.est <- coho.psi
coho.psi.M.lcl <- coho.psi
coho.psi.M.ucl <- coho.psi

# Movement from A
coho.psi.M.est[1,] <- coho.psi[1,] * S.coho.M[1,1] # est
coho.psi.M.lcl[1,] <- coho.psi[1,] * S.coho.M[1,3] # lcl
coho.psi.M.ucl[1,] <- coho.psi[1,] * S.coho.M[1,4] # ucl
coho.psi.M.est[1,1] <- 1 - sum(coho.psi[1,] * S.coho.M[1,1]) # Stop migration
coho.psi.M.lcl[1,1] <- 1 - sum(coho.psi[1,] * S.coho.M[1,3]) # Stop migration lcl
coho.psi.M.ucl[1,1] <- 1 - sum(coho.psi[1,] * S.coho.M[1,4]) # Stop migration ucl

# Movement from B
coho.psi.M.est[2,] <- coho.psi[2,] * S.coho.M[2,1] # est
coho.psi.M.lcl[2,] <- coho.psi[2,] * S.coho.M[2,3] # lcl
coho.psi.M.ucl[2,] <- coho.psi[2,] * S.coho.M[2,4] # ucl
coho.psi.M.est[2,2] <- 1 - sum(coho.psi[2,] * S.coho.M[2,1]) # Stop migration
coho.psi.M.lcl[2,2] <- 1 - sum(coho.psi[2,] * S.coho.M[2,3]) # Stop migration lcl
coho.psi.M.ucl[2,2] <- 1 - sum(coho.psi[2,] * S.coho.M[2,4]) # Stop migration ucl

# Movement from V
coho.psi.M.est[3,] <- coho.psi[3,] * S.coho.M[3,1] # est
coho.psi.M.lcl[3,] <- coho.psi[3,] * S.coho.M[3,3] # lcl
coho.psi.M.ucl[3,] <- coho.psi[3,] * S.coho.M[3,4] # ucl
coho.psi.M.est[3,3] <- 1 - sum(coho.psi[3,] * S.coho.M[3,1]) # Stop migration
coho.psi.M.lcl[3,3] <- 1 - sum(coho.psi[3,] * S.coho.M[3,3]) # Stop migration lcl
coho.psi.M.ucl[3,3] <- 1 - sum(coho.psi[3,] * S.coho.M[3,4]) # Stop migration ucl

# Movement from W
coho.psi.M.est[4,] <- coho.psi[4,] * S.coho.M[4,1] # est
coho.psi.M.lcl[4,] <- coho.psi[4,] * S.coho.M[4,3] # lcl
coho.psi.M.ucl[4,] <- coho.psi[4,] * S.coho.M[4,4] # ucl
coho.psi.M.est[4,4] <- 1 - sum(coho.psi[4,] * S.coho.M[4,1]) # Stop migration
coho.psi.M.lcl[4,4] <- 1 - sum(coho.psi[4,] * S.coho.M[4,3]) # Stop migration lcl
coho.psi.M.ucl[4,4] <- 1 - sum(coho.psi[4,] * S.coho.M[4,4]) # Stop migration ucl

# Movement from X
coho.psi.M.est[5,] <- coho.psi[5,] * S.coho.M[5,1] # est
coho.psi.M.lcl[5,] <- coho.psi[5,] * S.coho.M[5,3] # lcl
coho.psi.M.ucl[5,] <- coho.psi[5,] * S.coho.M[5,4] # ucl
coho.psi.M.est[5,5] <- 1 - sum(coho.psi[5,] * S.coho.M[5,1]) # Stop migration
coho.psi.M.lcl[5,5] <- 1 - sum(coho.psi[5,] * S.coho.M[5,3]) # Stop migration lcl
coho.psi.M.ucl[5,5] <- 1 - sum(coho.psi[5,] * S.coho.M[5,4]) # Stop migration ucl

round(coho.psi.M.est, 3)
round(coho.psi.M.lcl, 3)
round(coho.psi.M.ucl, 3)

# Now just the models within 4 AIC units
ms.models.3 <- function ()
{
  ## phi structures 
  S.state.time <- list(formula = ~stratum + time)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  Psi.state.tostate.sex.time <- list(formula = ~stratum + tostratum + sex + time)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE)
  return(results)
}

ms.results.3 <- ms.models.3()  

ms.results.3

# Okay, these look pretty good- let's model average our param estimates

coho.mod.avg <- model.average(ms.results.2, vcv = T)
coho.Psi.mod.avg <- model.average(ms.results.2, 'Psi', vcv = T, drop = FALSE)
coho.Psi.mod.avg.est <- coho.Psi.mod.avg$estimates

# The problem is- using 'time' as a Psi covariate doesn't really mean anything



