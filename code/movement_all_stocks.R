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
coho <- read.csv('data/ch/stock_sex_week_year_ABXVW0.csv', colClasses = c('numeric', 'character',
                    rep('factor', 4)))

# Modeling ####

## Make process data
coho.proc <- process.data(coho, model = 'Multistrata')

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix survival in 'V' to 1
coho.ddl$S$fix <- NA
coho.ddl$S$fix[coho.ddl$S$stratum == 'V'] <- 1

coho.ddl$S$fix

## Fix detection probabilities 
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum == 'A'] <- 0.94
coho.ddl$p$fix[coho.ddl$p$stratum == 'B'] <- 0.96
coho.ddl$p$fix[coho.ddl$p$stratum == 'X'] <- 0.95
coho.ddl$p$fix[coho.ddl$p$stratum == 'V'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'W'] <- 0.98

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

# Model selection ####


# All the best models have a 'state' and 'time' structure for phi (but I don't want any time structure), 
# and 'stratum', 'tostratum', 'sex' and 'time' for psi- let's build a 'marklist'

coho.models <- function ()
{
  ## phi structures 
  #S.int <- list(formula = ~1)
  S.state <- list(formula = ~stratum)
  #S.sex <- list(formula = ~sex)
  #S.sex.state <- list(formula = ~sex + stratum)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  #Psi.time.sex <- list(formula = ~time + sex)
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + sex)
  #Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  #Psi.state.tostate.sex.time <- list(formula = ~stratum + tostratum + sex + time)
  Psi.state.tostate.week <- list(formula = ~stratum + tostratum + stat.week)
  Psi.state.tostate.year <- list(formula = ~stratum + tostratum + year)
  Psi.state.tostate.sex.week <- list(formula = ~stratum + tostratum + sex + stat.week)
  Psi.state.tostate.sex.year <- list(formula = ~stratum + tostratum + sex + year)
  Psi.state.tostate.week.year <- list(formula = ~stratum + tostratum + stat.week + year)
  Psi.state.tostate.sex.week.year <- list(formula = ~stratum + tostratum + sex + stat.week + year)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results <- coho.models()  
coho.results

# Look at beta params of full model
coho.results$S.state.p.fixed.Psi.state.tostate

# Params of 'best' model
coho.mod <- coho.results$S.state.p.fixed.Psi.state.tostate

# Build best model
coho.mod <- mark(coho.proc, coho.ddl, model.parameters = list(S = S.state,
                                                              p = p.fixed,
                                                              Psi = Psi.state.tostate))

# Transition matricies 
Psilist <- get.real(coho.mod,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi <- TransitionMatrix(Psivalues[Psivalues$time==1,])
coho.psi

Slist <- get.real(coho.mod, 'S', vcv = T)
Svalues <- Slist$estimates
coho.S.vcv <- Slist$vcv.real


coho.psi.vcv <- Psilist$vcv.real


# Build movement estimates and incorporate survival
S.coho <- coho.mod$results$real[c(1:5), c(1:4)]

# Psi
coho.psi.est <- coho.psi


# Movement from A
coho.psi.est[1,] <- coho.psi[1,] * S.coho[1,1] # est
coho.psi.est[1,1] <- 1 - sum(coho.psi[1,] * S.coho.F[1,1]) # Stop migration


# Movement from B
coho.psi.est[2,] <- coho.psi[2,] * S.coho[2,1] # est
coho.psi.est[2,2] <- 1 - sum(coho.psi[2,] * S.coho[2,1]) # Stop migration


# Movement from V
coho.psi.est[3,] <- coho.psi[3,] * S.coho[5,1] # est
coho.psi.est[3,3] <- 1 - sum(coho.psi[3,] * S.coho[5,1]) # Stop migration


# Movement from W
coho.psi.est[4,] <- coho.psi[4,] * S.coho[3,1] # est
coho.psi.est[4,4] <- 1 - sum(coho.psi[4,] * S.coho[3,1]) # Stop migration


# Movement from X
coho.psi.est[5,] <- coho.psi[5,] * S.coho[4,1] # est
coho.psi.est[5,5] <- 1 - sum(coho.psi[5,] * S.coho[4,1]) # Stop migration


round(coho.psi.est, 3)


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
