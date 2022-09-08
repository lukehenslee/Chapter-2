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
coho.proc <- process.data(coho, model = 'Multistrata', groups = 'stock')

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
#coho.ddl$Psi$fix

# Model selection ####


# All the best models have a 'state' and 'time' structure for phi (but I don't want any time structure), 
# and 'stratum', 'tostratum', 'sex' and 'time' for psi- let's build a 'marklist'

coho.models <- function ()
{
  ## phi structures 
  S.state.time <- list(formula = ~stratum)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  Psi.stock <- list(formula = ~stock)
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

# MALES
Psilist=get.real(coho.results$S.state.time.p.fixed.Psi.state.tostate.stock,"Psi",vcv=TRUE)
Psivalues=Psilist$estimates
coho.psi.U <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$stock == 'Unalakleet',])
coho.psi.S <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$stock == 'Shaktoolik',])
coho.psi.T <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$stock == 'Transitory',])



coho.results$S.state.time.p.fixed.Psi.stock$results$real

# Build movement estimates and incorporate survival
S.coho <- coho.results$S.state.time.p.fixed.Psi.state.tostate.sex$results$real[c(1:5), c(1:4)]

# Movement from A
coho.psi[1,] * S.coho[1,1] # est
coho.psi[1,] * S.coho[1,3] # lcl
coho.psi[1,] * S.coho[1,4] # ucl
1 - sum(coho.psi[1,] * S.coho[1,1]) # Stop migration
1 - sum(coho.psi[1,] * S.coho[1,3]) # Stop migration lcl
1 - sum(coho.psi[1,] * S.coho[1,4]) # Stop migration ucl

# Movement from B
coho.psi[2,] * S.coho[2,1] # est
coho.psi[2,] * S.coho[2,3] # lcl
coho.psi[2,] * S.coho[2,4] # ucl
1 - sum(coho.psi[2,] * S.coho[2,1]) # Stop migration
1 - sum(coho.psi[2,] * S.coho[2,3]) # Stop migration lcl
1 - sum(coho.psi[2,] * S.coho[2,4]) # Stop migration ucl

# Movement from V
coho.psi[3,] * S.coho[3,1] # est
coho.psi[3,] * S.coho[3,3] # lcl
coho.psi[3,] * S.coho[3,4] # ucl
1 - sum(coho.psi[3,] * S.coho[3,1]) # Stop migration
1 - sum(coho.psi[3,] * S.coho[3,3]) # Stop migration lcl
1 - sum(coho.psi[3,] * S.coho[3,4]) # Stop migration ucl

# Movement from W
coho.psi[4,] * S.coho[4,1] # est
coho.psi[4,] * S.coho[4,3] # lcl
coho.psi[4,] * S.coho[4,4] # ucl
1 - sum(coho.psi[4,] * S.coho[4,1]) # Stop migration
1 - sum(coho.psi[4,] * S.coho[4,3]) # Stop migration lcl
1 - sum(coho.psi[4,] * S.coho[4,4]) # Stop migration ucl

# Movement from X
coho.psi[5,] * S.coho[5,1] # est
coho.psi[5,] * S.coho[5,3] # lcl
coho.psi[5,] * S.coho[5,4] # ucl
1 - sum(coho.psi[5,] * S.coho[5,1]) # Stop migration
1 - sum(coho.psi[5,] * S.coho[5,3]) # Stop migration lcl
1 - sum(coho.psi[5,] * S.coho[5,4]) # Stop migration ucl

# FEMALES
Psilist=get.real(coho.results$S.state.time.p.fixed.Psi.state.tostate.sex,"Psi",vcv=TRUE)
Psivalues=Psilist$estimates
coho.psi.F <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$sex == 'F',])

coho.results$S.state.time.p.fixed.Psi.state.tostate.sex$results$real

# Build movement estimates and incorporate survival
S.coho <- coho.results$S.state.time.p.fixed.Psi.state.tostate.sex$results$real[c(1:5), c(1:4)]

# Movement from A
coho.psi[1,] * S.coho[1,1] # est
coho.psi[1,] * S.coho[1,3] # lcl
coho.psi[1,] * S.coho[1,4] # ucl
1 - sum(coho.psi[1,] * S.coho[1,1]) # Stop migration
1 - sum(coho.psi[1,] * S.coho[1,3]) # Stop migration lcl
1 - sum(coho.psi[1,] * S.coho[1,4]) # Stop migration ucl

# Movement from B
coho.psi[2,] * S.coho[2,1] # est
coho.psi[2,] * S.coho[2,3] # lcl
coho.psi[2,] * S.coho[2,4] # ucl
1 - sum(coho.psi[2,] * S.coho[2,1]) # Stop migration
1 - sum(coho.psi[2,] * S.coho[2,3]) # Stop migration lcl
1 - sum(coho.psi[2,] * S.coho[2,4]) # Stop migration ucl

# Movement from V
coho.psi[3,] * S.coho[3,1] # est
coho.psi[3,] * S.coho[3,3] # lcl
coho.psi[3,] * S.coho[3,4] # ucl
1 - sum(coho.psi[3,] * S.coho[3,1]) # Stop migration
1 - sum(coho.psi[3,] * S.coho[3,3]) # Stop migration lcl
1 - sum(coho.psi[3,] * S.coho[3,4]) # Stop migration ucl

# Movement from W
coho.psi[4,] * S.coho[4,1] # est
coho.psi[4,] * S.coho[4,3] # lcl
coho.psi[4,] * S.coho[4,4] # ucl
1 - sum(coho.psi[4,] * S.coho[4,1]) # Stop migration
1 - sum(coho.psi[4,] * S.coho[4,3]) # Stop migration lcl
1 - sum(coho.psi[4,] * S.coho[4,4]) # Stop migration ucl

# Movement from X
coho.psi[5,] * S.coho[5,1] # est
coho.psi[5,] * S.coho[5,3] # lcl
coho.psi[5,] * S.coho[5,4] # ucl
1 - sum(coho.psi[5,] * S.coho[5,1]) # Stop migration
1 - sum(coho.psi[5,] * S.coho[5,3]) # Stop migration lcl
1 - sum(coho.psi[5,] * S.coho[5,4]) # Stop migration ucl

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
