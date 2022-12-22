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
coho <- coho[which(coho$stock == 'Shaktoolik'),]
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
coho.ddl$S$fix[coho.ddl$S$stratum == 'X'] <- 1
coho.ddl$S$fix


coho.mod <- mark(coho.proc, coho.ddl, model.parameters = list(S = list(formula = ~fix),
                                                              p = list(formula = ~fix),
                                                              Psi = list(formula = ~stratum + tostratum)),
                 realvcv = T)

# Transition matricies 
Psilist <- get.real(coho.mod,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi <- TransitionMatrix(Psivalues[Psivalues$time==1,], vcv.real = Psilist$vcv.real)


round(coho.psi$TransitionMat, 3)

# SE 
round(coho.psi$lcl.TransitionMat, 3)
round(coho.psi$ucl.TransitionMat, 3)
