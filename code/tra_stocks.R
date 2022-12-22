#==================================================================================================
# RMARK
# Date: September 23, 2022
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
setwd("H:/git_repo/Chapter-2")

# Import data ####
coho <- read.csv('data/ch/stock_sex_week_year_ABX0.csv')
coho <- coho[which(coho$stock == 'Transitory'),]
# Modeling ####

## Make process data
coho.proc <- process.data(coho, model = 'Multistrata')

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix detection probabilities 
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum == 'A'] <- 0.94
coho.ddl$p$fix[coho.ddl$p$stratum == 'B'] <- 0.96
coho.ddl$p$fix[coho.ddl$p$stratum == 'X'] <- 0.95

coho.ddl$p$fix

## Fix survival probabilities 
coho.ddl$S$fix <- NA
coho.ddl$S$fix[coho.ddl$S$stratum == 'A'] <- 1
coho.ddl$S$fix[coho.ddl$S$stratum == 'B'] <- 1
#coho.ddl$S$fix[coho.ddl$S$stratum == 'X'] <- 1
coho.ddl$S$fix


coho.mod <- mark(coho.proc, coho.ddl, model.parameters = list(S = list(formula = ~stratum),
                                                              p = list(formula = ~fix),
                                                              Psi = list(formula = ~stratum + tostratum)),
                 realvcv = T)

# Transition matricies 
Psilist <- get.real(coho.mod,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi <- TransitionMatrix(Psivalues[Psivalues$time==1,], vcv.real = Psilist$vcv.real)

# Build movement estimates and incorporate survival
S.coho <- coho.mod$results$real[1, 1]

# Psi
coho.psi.mat <- coho.psi$TransitionMat
coho.psi.est <- coho.psi$TransitionMat

# Movement from A
#coho.psi.est[1,] <- coho.psi[1,] * S.coho[1,1] # est
#coho.psi.est[1,1] <- 1 - sum(coho.psi[1,] * S.coho[1,1]) # Stop migration

# Movement from B
#coho.psi.est[2,] <- coho.psi[2,] * S.coho[2,1] # est
#coho.psi.est[2,2] <- 1 - sum(coho.psi[2,] * S.coho[2,1]) # Stop migration

# Movement from V
#coho.psi.est[3,] <- coho.psi[3,] * S.coho[3,1] # est
#coho.psi.est[3,3] <- 1 - sum(coho.psi[3,] * S.coho[3,1]) # Stop migration

# Movement from W
#coho.psi.est[4,] <- coho.psi[4,] * S.coho[4,1] # est
#coho.psi.est[4,4] <- 1 - sum(coho.psi[4,] * S.coho[4,1]) # Stop migration

# Movement from X
coho.psi.est[3,] <- coho.psi.mat[3,] * S.coho # est
coho.psi.est[3,3] <- 1 - sum(coho.psi.mat[3,] * S.coho) # Stop migration

round(coho.psi.est, 3)

# The fucking delta method
coho.mod.real <- coho.mod$results$real

coho.mod.vcv <- coho.mod$results$real.vcv

se.matrix <- coho.psi$se.TransitionMat

## X 
# X to A
se.matrix[3,1] <- deltamethod.special('prod', coho.mod.real[c(1,30),1], abs(coho.mod.vcv[c(1,30), c(1,30)]))

# X to B
se.matrix[3,2] <- deltamethod.special('prod', coho.mod.real[c(1,36),1], abs(coho.mod.vcv[c(1,36), c(1,36)]))

# Survival X
se.matrix[3,3] <- coho.mod.real[1,2]

## CI ####
ucl <- coho.psi.est + (1.96 * se.matrix)
lcl <- coho.psi.est + (-1.96 * se.matrix)

# Results ####
round(coho.psi.est, 3)
round(lcl, 3)
round(ucl, 3)
