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
#coho.ddl$S$fix <- NA
#coho.ddl$S$fix[coho.ddl$S$stratum == 'V'] <- 1

#coho.ddl$S$fix

## Fix detection probabilities 
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum == 'A'] <- 0.94
coho.ddl$p$fix[coho.ddl$p$stratum == 'B'] <- 0.96
coho.ddl$p$fix[coho.ddl$p$stratum == 'X'] <- 0.95
coho.ddl$p$fix[coho.ddl$p$stratum == 'V'] <- 0.99
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

#coho.models <- function ()
#{
  ## phi structures 
  #S.int <- list(formula = ~1)
  #S.state <- list(formula = ~stratum)
  #S.sex <- list(formula = ~sex)
  #S.sex.state <- list(formula = ~sex + stratum)
  
  ## fixed values for p based on our earlier analyses
  #p.fixed <- list(formula = ~fix)
  
  ## psi
  #Psi.time.sex <- list(formula = ~time + sex)
  #Psi.state.tostate <- list(formula = ~stratum + tostratum)
  #Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + sex)
  #Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  #Psi.state.tostate.sex.time <- list(formula = ~stratum + tostratum + sex + time)
  #Psi.state.tostate.week <- list(formula = ~stratum + tostratum + stat.week)
  #Psi.state.tostate.year <- list(formula = ~stratum + tostratum + year)
  #Psi.state.tostate.sex.week <- list(formula = ~stratum + tostratum + sex + stat.week)
  #Psi.state.tostate.sex.year <- list(formula = ~stratum + tostratum + sex + year)
  #Psi.state.tostate.week.year <- list(formula = ~stratum + tostratum + stat.week + year)
  #Psi.state.tostate.sex.week.year <- list(formula = ~stratum + tostratum + sex + stat.week + year)
  
 # cml=create.model.list("Multistrata")
  #results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  #return(results)
#}

#coho.results <- coho.models()  
#coho.results


# Build best model
coho.mod <- mark(coho.proc, coho.ddl, model.parameters = list(S = list(formula = ~stratum),
                                                              p = list(formula = ~stratum),
                                                              Psi = list(formula = ~stratum + tostratum)),
                 realvcv = T)

# Transition matricies 
Psilist <- get.real(coho.mod,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi <- TransitionMatrix(Psivalues[Psivalues$time==1,])


Slist <- get.real(coho.mod, 'S', vcv = T)
Svalues <- Slist$estimates

# Build movement estimates and incorporate survival
S.coho <- coho.mod$results$real[c(1:5), c(1:4)]

# Psi
coho.psi.est <- coho.psi

# Movement from A
coho.psi.est[1,] <- coho.psi[1,] * S.coho[1,1] # est
coho.psi.est[1,1] <- 1 - sum(coho.psi[1,] * S.coho[1,1]) # Stop migration

# Movement from B
coho.psi.est[2,] <- coho.psi[2,] * S.coho[2,1] # est
coho.psi.est[2,2] <- 1 - sum(coho.psi[2,] * S.coho[2,1]) # Stop migration

# Movement from V
coho.psi.est[3,] <- coho.psi[3,] * S.coho[3,1] # est
coho.psi.est[3,3] <- 1 - sum(coho.psi[3,] * S.coho[3,1]) # Stop migration

# Movement from W
coho.psi.est[4,] <- coho.psi[4,] * S.coho[4,1] # est
coho.psi.est[4,4] <- 1 - sum(coho.psi[4,] * S.coho[4,1]) # Stop migration

# Movement from X
coho.psi.est[5,] <- coho.psi[5,] * S.coho[5,1] # est
coho.psi.est[5,5] <- 1 - sum(coho.psi[5,] * S.coho[5,1]) # Stop migration

round(coho.psi.est, 3)

# The fucking delta method
coho.mod.vcv <- coho.mod$results$real.vcv

coho.mod.real <- coho.mod$results$real

se.matrix <- matrix(data = NA, nrow= 5, ncol = 5)

# A
# Survival A
se.matrix[1,1] <- coho.mod.real[1,2]
# A to B 
se.matrix[1,2] <- deltamethod.special('prod', coho.mod.real[c(1,12),1], abs(coho.mod.vcv[c(1,12), c(1,12)]))

# A to V 
se.matrix[1,3] <- deltamethod.special('prod', coho.mod.real[c(1,19),1], abs(coho.mod.vcv[c(1,19), c(1,19)]))

# A to W
se.matrix[1,4] <- 0

# A to X 
se.matrix[1,5] <- deltamethod.special('prod', coho.mod.real[c(1,24),1], abs(coho.mod.vcv[c(1,24), c(1,24)]))

## B 
# B to A
se.matrix[2,1] <- deltamethod.special('prod', coho.mod.real[c(2,30),1], abs(coho.mod.vcv[c(2,30), c(2,30)]))

# Survival B
se.matrix[2,2] <- coho.mod.real[2,2]

# B to V 
se.matrix[2,3] <- 0

# B to W
se.matrix[2,4] <- deltamethod.special('prod', coho.mod.real[c(2,36),1], abs(coho.mod.vcv[c(2,36), c(2,36)]))

# B to X 
se.matrix[2,5] <- deltamethod.special('prod', coho.mod.real[c(2,42),1], abs(coho.mod.vcv[c(2,42), c(2,42)]))

## V
# V to A
se.matrix[3,1] <- deltamethod.special('prod', coho.mod.real[c(3,48),1], abs(coho.mod.vcv[c(3,48), c(3,48)]))

# V to B
se.matrix[3,2] <- 0

# Surival V
se.matrix[3,3] <- coho.mod.real[3,2]

# V to W
se.matrix[3,4] <- 0

# V to X
se.matrix[3,5] <- 0

## W
# W to A
se.matrix[4,1] <- 0

# W to B
se.matrix[4,2] <- deltamethod.special('prod', coho.mod.real[c(4,54),1], abs(coho.mod.vcv[c(4,54), c(4,54)]))

# W to V
se.matrix[4,3] <- 0

# Survival W
se.matrix[4,4] <- coho.mod.real[4,2]

# W to X
se.matrix[4,5] <- 0

## X 
# X to A
se.matrix[5,1] <- deltamethod.special('prod', coho.mod.real[c(5,60),1], abs(coho.mod.vcv[c(5,60), c(5,60)]))

# X to B
se.matrix[5,2] <- deltamethod.special('prod', coho.mod.real[c(5,66),1], abs(coho.mod.vcv[c(5,66), c(5,66)]))

# X to V 
se.matrix[5,3] <- 0

# X to W
se.matrix[5,4] <- 0

# Survival X
se.matrix[5,5] <- coho.mod.real[5,2]

## CI ####
ucl <- coho.psi.est + (1.96 * se.matrix)
lcl <- coho.psi.est + (-1.96 * se.matrix)

# Results ####
round(coho.psi.est, 3)
round(ucl, 3)
round(lcl, 3)
