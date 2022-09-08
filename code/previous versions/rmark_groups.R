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
# Remove NA
coho <- coho[complete.cases(coho),]

# Modeling ####

## Make process data
coho.proc <- process.data(coho, model = 'Multistrata', groups = c('stock', 'sex',
                                                                  'stat.week'))

## Design data
coho.ddl <- make.design.data(coho.proc)

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
  ### single
  S.state <- list(formula = ~stratum)
  S.stock <- list(formula = ~stock)
  S.sex <- list(formula = ~sex)
  S.week <- list(formula = ~stat.week)
  
  ### double
  S.stock.state <- list(formula = ~stock + stratum)
  S.sex.state <- list(formula = ~sex + stratum)
  S.week.state <- list(formula = ~stat.week + stratum)
  S.week.stock <- list(formula = ~stock + stat.week)
  S.week.sex <- list(formula = ~stock + sex)
  
  ### triple 
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  Psi.time.stock <- list(formula = ~time + stock)
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.stock <- list(formula = ~stratum + tostratum + stock)
  Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  Psi.state.tostate.stock.time <- list(formula = ~stratum + tostratum + stock + time)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results <- coho.models()  
coho.results

# Params of 'best' model
coho.results$S.stock.state.p.fixed.Psi.state.tostate$results$real

# Transition matricies 
Psilist <- get.real(coho.results$S.stock.state.p.fixed.Psi.state.tostate,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$stock == 'Shaktoolik',])
coho.psi


# Build movement estimates and incorporate survival
S.coho.S <- coho.results$S.stock.state.p.fixed.Psi.state.tostate$results$real[c(1:2,13,3:4), c(1:4)]
S.coho.T <- coho.results$S.stock.state.p.fixed.Psi.state.tostate$results$real[c(5:6,13,7:8), c(1:4)]
S.coho.U <- coho.results$S.stock.state.p.fixed.Psi.state.tostate$results$real[c(9:10,13,11:12), c(1:4)]

# Unalakleet
coho.psi.U.est <- coho.psi
coho.psi.U.lcl <- coho.psi
coho.psi.U.ucl <- coho.psi

# Movement from A
coho.psi.U.est[1,] <- coho.psi[1,] * S.coho.U[1,1] # est
coho.psi.U.lcl[1,] <- coho.psi[1,] * S.coho.U[1,3] # lcl
coho.psi.U.ucl[1,] <- coho.psi[1,] * S.coho.U[1,4] # ucl
coho.psi.U.est[1,1] <- 1 - sum(coho.psi[1,] * S.coho.U[1,1]) # Stop migration
coho.psi.U.lcl[1,1] <- 1 - sum(coho.psi[1,] * S.coho.U[1,3]) # Stop migration lcl
coho.psi.U.ucl[1,1] <- 1 - sum(coho.psi[1,] * S.coho.U[1,4]) # Stop migration ucl

# Movement from B
coho.psi.U.est[2,] <- coho.psi[2,] * S.coho.U[2,1] # est
coho.psi.U.lcl[2,] <- coho.psi[2,] * S.coho.U[2,3] # lcl
coho.psi.U.ucl[2,] <- coho.psi[2,] * S.coho.U[2,4] # ucl
coho.psi.U.est[2,2] <- 1 - sum(coho.psi[2,] * S.coho.U[2,1]) # Stop migration
coho.psi.U.lcl[2,2] <- 1 - sum(coho.psi[2,] * S.coho.U[2,3]) # Stop migration lcl
coho.psi.U.ucl[2,2] <- 1 - sum(coho.psi[2,] * S.coho.U[2,4]) # Stop migration ucl

# Movement from V
coho.psi.U.est[3,] <- coho.psi[3,] * S.coho.U[3,1] # est
coho.psi.U.lcl[3,] <- coho.psi[3,] * S.coho.U[3,3] # lcl
coho.psi.U.ucl[3,] <- coho.psi[3,] * S.coho.U[3,4] # ucl
coho.psi.U.est[3,3] <- 1 - sum(coho.psi[3,] * S.coho.U[3,1]) # Stop migration
coho.psi.U.lcl[3,3] <- 1 - sum(coho.psi[3,] * S.coho.U[3,3]) # Stop migration lcl
coho.psi.U.ucl[3,3] <- 1 - sum(coho.psi[3,] * S.coho.U[3,4]) # Stop migration ucl

# Movement from W
coho.psi.U.est[4,] <- coho.psi[4,] * S.coho.U[4,1] # est
coho.psi.U.lcl[4,] <- coho.psi[4,] * S.coho.U[4,3] # lcl
coho.psi.U.ucl[4,] <- coho.psi[4,] * S.coho.U[4,4] # ucl
coho.psi.U.est[4,4] <- 1 - sum(coho.psi[4,] * S.coho.U[4,1]) # Stop migration
coho.psi.U.lcl[4,4] <- 1 - sum(coho.psi[4,] * S.coho.U[4,3]) # Stop migration lcl
coho.psi.U.ucl[4,4] <- 1 - sum(coho.psi[4,] * S.coho.U[4,4]) # Stop migration ucl

# Movement from X
coho.psi.U.est[5,] <- coho.psi[5,] * S.coho.U[5,1] # est
coho.psi.U.lcl[5,] <- coho.psi[5,] * S.coho.U[5,3] # lcl
coho.psi.U.ucl[5,] <- coho.psi[5,] * S.coho.U[5,4] # ucl
coho.psi.U.est[5,5] <- 1 - sum(coho.psi[5,] * S.coho.U[5,1]) # Stop migration
coho.psi.U.lcl[5,5] <- 1 - sum(coho.psi[5,] * S.coho.U[5,3]) # Stop migration lcl
coho.psi.U.ucl[5,5] <- 1 - sum(coho.psi[5,] * S.coho.U[5,4]) # Stop migration ucl

round(coho.psi.U.est, 3)
round(coho.psi.U.lcl, 3)
round(coho.psi.U.ucl, 3)

# Shaktoolik
coho.psi.S.est <- coho.psi
coho.psi.S.lcl <- coho.psi
coho.psi.S.Scl <- coho.psi

# Movement from A
coho.psi.S.est[1,] <- coho.psi[1,] * S.coho.S[1,1] # est
coho.psi.S.lcl[1,] <- coho.psi[1,] * S.coho.S[1,3] # lcl
coho.psi.S.Scl[1,] <- coho.psi[1,] * S.coho.S[1,4] # ucl
coho.psi.S.est[1,1] <- 1 - sum(coho.psi[1,] * S.coho.S[1,1]) # Stop migration
coho.psi.S.lcl[1,1] <- 1 - sum(coho.psi[1,] * S.coho.S[1,3]) # Stop migration lcl
coho.psi.S.Scl[1,1] <- 1 - sum(coho.psi[1,] * S.coho.S[1,4]) # Stop migration ucl

# Movement from B
coho.psi.S.est[2,] <- coho.psi[2,] * S.coho.S[2,1] # est
coho.psi.S.lcl[2,] <- coho.psi[2,] * S.coho.S[2,3] # lcl
coho.psi.S.Scl[2,] <- coho.psi[2,] * S.coho.S[2,4] # ucl
coho.psi.S.est[2,2] <- 1 - sum(coho.psi[2,] * S.coho.S[2,1]) # Stop migration
coho.psi.S.lcl[2,2] <- 1 - sum(coho.psi[2,] * S.coho.S[2,3]) # Stop migration lcl
coho.psi.S.Scl[2,2] <- 1 - sum(coho.psi[2,] * S.coho.S[2,4]) # Stop migration ucl

# Movement from V
coho.psi.S.est[3,] <- coho.psi[3,] * S.coho.S[3,1] # est
coho.psi.S.lcl[3,] <- coho.psi[3,] * S.coho.S[3,3] # lcl
coho.psi.S.Scl[3,] <- coho.psi[3,] * S.coho.S[3,4] # ucl
coho.psi.S.est[3,3] <- 1 - sum(coho.psi[3,] * S.coho.S[3,1]) # Stop migration
coho.psi.S.lcl[3,3] <- 1 - sum(coho.psi[3,] * S.coho.S[3,3]) # Stop migration lcl
coho.psi.S.Scl[3,3] <- 1 - sum(coho.psi[3,] * S.coho.S[3,4]) # Stop migration ucl

# Movement from W
coho.psi.S.est[4,] <- coho.psi[4,] * S.coho.S[4,1] # est
coho.psi.S.lcl[4,] <- coho.psi[4,] * S.coho.S[4,3] # lcl
coho.psi.S.Scl[4,] <- coho.psi[4,] * S.coho.S[4,4] # ucl
coho.psi.S.est[4,4] <- 1 - sum(coho.psi[4,] * S.coho.S[4,1]) # Stop migration
coho.psi.S.lcl[4,4] <- 1 - sum(coho.psi[4,] * S.coho.S[4,3]) # Stop migration lcl
coho.psi.S.Scl[4,4] <- 1 - sum(coho.psi[4,] * S.coho.S[4,4]) # Stop migration ucl

# Movement from X
coho.psi.S.est[5,] <- coho.psi[5,] * S.coho.S[5,1] # est
coho.psi.S.lcl[5,] <- coho.psi[5,] * S.coho.S[5,3] # lcl
coho.psi.S.Scl[5,] <- coho.psi[5,] * S.coho.S[5,4] # ucl
coho.psi.S.est[5,5] <- 1 - sum(coho.psi[5,] * S.coho.S[5,1]) # Stop migration
coho.psi.S.lcl[5,5] <- 1 - sum(coho.psi[5,] * S.coho.S[5,3]) # Stop migration lcl
coho.psi.S.Scl[5,5] <- 1 - sum(coho.psi[5,] * S.coho.S[5,4]) # Stop migration ucl

round(coho.psi.S.est, 3)
round(coho.psi.S.lcl, 3)
round(coho.psi.S.Scl, 3)

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
