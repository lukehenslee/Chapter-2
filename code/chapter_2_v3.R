#==================================================================================================
# Thesis chapter 2
# Date: September 22, 2022
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
# B: Unalakleet
# X: Outside
#
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
setwd("H:/git_repo/Chapter-2")
      
# Import data ####

  ## Detection history data paired with sex, and stock
ch <- read.csv('data/ch/stock_sex_ABX0.csv', 
               colClasses = c('numeric', 'character', 'factor', 'factor'))

  ## We are first going to model with 'stock' groups, and some of the rows 
  ## contain NA's. We'll make a detection history with only complete cases

ch.stock <- ch[complete.cases(ch),]
# Multi-state modeling ####

  ## In this model, there are three moves allowed after capture event (four total
  ## steps in detection history), and the last step in the history will reflect 
  ## 'stock' grouping. What we want to know is, 'what is the probability of a fish 
  ## in group XX moving to area XX in step XX?' 

## Make process data ####
coho.proc <- process.data(ch.stock, model = 'Multistrata', 
                          groups = c('stock', 'sex'))
  ## Setting 'sex', and 'stock' to grouping variables

## Design data ####
coho.ddl <- make.design.data(coho.proc)

## Fixing design data ####

  # Detection probabilities in each strata are ~1, we set them at 1 to prevent
  # overdispersion of parameter values near boundaries
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum == 'A'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'B'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'X'] <- 1

coho.ddl$p$fix

## Model selection ####

coho.models <- function ()
{
  ## phi structures 
  #S.time.state <- list(formula = ~time + stratum)
  S.time.state.stock <- list(formula = ~time + stratum + stock)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  #Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + time + sex)
  #Psi.state.tostate.stock <- list(formula = ~stratum + tostratum + stock)
  #Psi.state.tostate.stock <- list(formula = ~stratum + tostratum + stock + sex)
  Psi.state.tostate.time.stock <- list(formula = ~stratum + tostratum + time + stock)
  Psi.state.tostate.time.stock.sex <- list(formula = ~stratum + tostratum + time + stock + sex)
  #Psi.state.tostate.stat.week <- list(formula = ~stratum + tostratum + stat.week)
  #Psi.state.tostate.year <- list(formula = ~stratum + tostratum + year)
  #Psi.state.tostate.stock.stat.week <- list(formula = ~stratum + tostratum + stock + stat.week)
  #Psi.state.tostate.stock.sex <- list(formula = ~stratum + tostratum + stock + sex)
  #Psi.state.tostate.stock.year <- list(formula = ~stratum + tostratum + stock + year)
  #Psi.state.tostate.stock.stat.week.year <- list(formula = ~stratum + tostratum + stock + stat.week + year)
  #Psi.state.tostate.stock.stat.week.sex <- list(formula = ~stratum + tostratum + stock + stat.week + sex)
  #Psi.state.tostate.stock.sex.year <- list(formula = ~stratum + tostratum + stock + sex + year)
  #Psi.state.tostate.stock.sex.year <- list(formula = ~stratum + tostratum + stock + sex + year + stat.week)
  #Psi.state.tostate.stock.stat.week.year <- list(formula = ~stratum + tostratum + stock + stat.week + year)
  #Psi.state.tostate.stock.stat.week.sex.year <- list(formula = ~stratum + tostratum + stock + stat.week + sex + year)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results <- coho.models()  

coho.results

## Build 'best' model

S.time.state.stock <- list(formula = ~time + stratum + stock)

p.fixed <- list(formula = ~fix)

Psi.state.tostate.time.stock.sex <- list(formula = ~stratum + tostratum + time + stock + sex)

coho.mod <- mark(coho.proc, coho.ddl, model.parameters = list(
  S = S.time.state.stock, p = p.fixed, Psi = Psi.state.tostate.time.stock.sex))

coho.mod.results <- coho.mod$results$real

## Average model results ####

coho.mod.avg <- model.average(coho.results, vcv = T)
coho.mod.avg.vcv <- coho.mod.avg$vcv.real

S <- model.average(coho.results, 'S')
S

p <- model.average(coho.results, 'p')

Psi <- model.average(coho.results, 'Psi', drop = F)
Psi

coho.mod.avg.est <- rbind(coho.mod.avg.S[ ,c(1:3)], 
                      coho.mod.avg.p[ ,c(1:3)], 
                      coho.mod.avg.Psi[ ,c(1:3)])

write.csv(coho.mod.avg.est, 'data/coho.mod.avg.csv')

## Delta method ####
deltamethod(~x1 * x2, mean = coho.mod.avg$estimates[c(1:2),2], 
            cov = coho.mod.avg$vcv.real[c(1:2), c(1:2)])

# Movement estimates ####

Psi <- Psi[which(Psi[,7] %in% c('2', '3') == F),]
S <- S[which(S[,7] %in% c('2', '3') == F),]

list <- list()

for(i in 1:length(Psi.list)) {
  list[[i]] <- Psi.list[[i]]
}


## Females ####
### Unalakleet ####
#### Time 1 ####
fu1 <- TransitionMatrix(Psi[Psi$sex == 'F' & Psi$time == '1' & 
                              Psi$stock == 'Unalakleet',])

fu2 <- TransitionMatrix(Psi[Psi$sex == 'F' & Psi$time == '2' & 
                              Psi$stock == 'Unalakleet',])

fu2[1,] <- fu2[1,] * S[S$stratum == 'A' & S$sex == 'F' & S$time == '2' 
                       & S$stock == 'Unalakleet', 2]
fu2[2,] <- fu2[2,] * S[S$stratum == 'B' & S$sex == 'F' & S$time == '2' 
                      & S$stock == 'Unalakleet', 2]
fu2[3,] <- fu2[3,] * S[S$stratum == 'X' & S$sex == 'F' & S$time == '2' 
                       & S$stock == 'Unalakleet', 2]

fu3 <- TransitionMatrix(Psi[Psi$sex == 'F' & Psi$time == '3' & 
                              Psi$stock == 'Unalakleet',])

fu3[1,] <- fu3[1,] * S[S$stratum == 'A' & S$sex == 'F' & S$time == '3' 
                       & S$stock == 'Unalakleet', 2]
fu3[2,] <- fu3[2,] * S[S$stratum == 'B' & S$sex == 'F' & S$time == '3' 
                       & S$stock == 'Unalakleet', 2]
fu3[3,] <- fu3[3,] * S[S$stratum == 'X' & S$sex == 'F' & S$time == '3' 
                       & S$stock == 'Unalakleet', 2]



fu1.est <- matrix(NA, 3, 3)

# AtoA = 1 - (AtoB + AtoC)
fu1.est[1,1] <- 1- (est[289] + est[295])
# AtoB
fu1.est[2,1] <- est[289]
# AtoZ
fu1.est[3,1] <- est[295]

# BtoA = 1 - (BtoB + BtoC)
fu1.est[1,2] <- est[301]
# BtoB
fu1.est[2,2] <- 1- (est[289] + est[295])
# BtoZ
fu1.est[3,2] <- est[295]

# XtoA = 1 - (XtoB + XtoC)
fu1.est[1,1] <- 1- (est[289] + est[295])
# XtoB
fu1.est[2,1] <- est[289]
# XtoZ
fu1.est[3,1] <- est[295]



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

