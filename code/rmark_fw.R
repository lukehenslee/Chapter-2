#==================================================================================================
# RMARK
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#Purpose: 
#==================================================================================================
#NOTES: 
#==================================================================================================

# Load packages ####

library(tidyverse)
library(RMark)

# Set working directory ####
setwd("C:/Users/lukeh/Desktop/Git_repos/Chapter-2")

# Import data ####
coho <- read.csv('ch_coho.csv', colClasses = c('numeric', 'character',
                    rep('factor', 4)))

# Modeling ####

## Make process data
coho.proc <- process.data(coho, model = 'Multistrata', group = 'sex')

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix detection probabilities 
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum=="A"] <- 0.94
coho.ddl$p$fix[coho.ddl$p$stratum=="B"] <- 0.96
coho.ddl$p$fix[coho.ddl$p$stratum=="X"] <- 0.95
coho.ddl$p$fix[coho.ddl$p$stratum=="V"] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum=="W"] <- 0.98

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
coho.ddl$Psi$fix

# Model selection ####


# All the best models have a 'state' and 'time' structure for phi (but I don't want any time structure), 
# and 'stratum', 'tostratum', 'sex' and 'time' for psi- let's build a 'marklist'

ms.models.2 <- function ()
{
  ## phi structures 
  S.state.time <- list(formula = ~stratum)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  Psi.time.sex <- list(formula = ~time + sex)
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + sex)
  Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  Psi.state.tostate.sex.time <- list(formula = ~stratum + tostratum + sex + time)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

ms.results.2 <- ms.models.2()  
ms.results.2

Psilist=get.real(ms.results.2$S.state.time.p.fixed.Psi.state.tostate.sex,"Psi",vcv=TRUE)
Psivalues=Psilist$estimates
TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$sex == 'M',])

ms.results.2$S.state.time.p.fixed.Psi.state.tostate.sex$results$real

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
