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

# Modeling ####

## Make process data
coho.proc <- process.data(coho, model = 'Multistrata', groups = c('stock'))

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix survival in 'V' to 1
#coho.ddl$S$fix <- NA
#coho.ddl$S$fix[coho.ddl$S$stratum == 'A'] <- 1
#coho.ddl$S$fix[coho.ddl$S$stratum == 'B'] <- 1
#coho.ddl$S$fix[coho.ddl$S$stratum == 'X'] <- 1

#coho.ddl$S$fix

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


coho.mod <- mark(coho.proc, coho.ddl, model.parameters = list(S = list(formula = ~stratum),
                                                              p = list(formula = ~stratum),
                                                              Psi = list(formula = ~stratum + tostratum + stock)),
                 realvcv = T)

# Transition matricies 
Psilist <- get.real(coho.mod,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi.skk <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$stock == 'Shaktoolik',])
coho.psi.unk <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$stock == 'Unalakleet',])
coho.psi.tra <- TransitionMatrix(Psivalues[Psivalues$time==1&Psivalues$stock == 'Transitory',])

round(coho.psi.skk, 3)
round(coho.psi.unk, 3)
round(coho.psi.tra, 3)

# Shaktoolik ####
coho.skk <- coho[which(coho$stock == 'Shaktoolik'),]

## Make process data
coho.proc <- process.data(coho.skk, model = 'Multistrata')

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


coho.mod.skk <- mark(coho.proc, coho.ddl, model.parameters = list(S = list(formula = ~fix),
                                                              p = list(formula = ~fix),
                                                              Psi = list(formula = ~stratum + tostratum)),
                 realvcv = T)

# Transition matricies 
Psilist <- get.real(coho.mod.skk,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi.skk <- TransitionMatrix(Psivalues[Psivalues$time==1,])


round(coho.psi.skk, 3)

# Unalakleet ####
coho.unk <- coho[which(coho$stock == 'Unalakleet'),]

## Make process data
coho.proc <- process.data(coho.unk, model = 'Multistrata')

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


coho.mod.unk <- mark(coho.proc, coho.ddl, model.parameters = list(S = list(formula = ~fix),
                                                                  p = list(formula = ~fix),
                                                                  Psi = list(formula = ~stratum + tostratum)),
                     realvcv = T)

# Transition matricies 
Psilist <- get.real(coho.mod.unk,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi.unk <- TransitionMatrix(Psivalues[Psivalues$time==1,])


round(coho.psi.unk, 3)

# Transitory ####
coho.tra <- coho[which(coho$stock == 'Shaktoolik'),]

## Make process data
coho.proc <- process.data(coho.tra, model = 'Multistrata')

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


coho.mod.tra <- mark(coho.proc, coho.ddl, model.parameters = list(S = list(formula = ~fix),
                                                                  p = list(formula = ~fix),
                                                                  Psi = list(formula = ~stratum + tostratum)),
                     realvcv = T)

# Transition matricies 
Psilist <- get.real(coho.mod.tra,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi.tra <- TransitionMatrix(Psivalues[Psivalues$time==1,])


round(coho.psi.tra, 3)

# Results ####
round(coho.psi.skk, 3)
round(coho.psi.unk, 3)
round(coho.psi.tra, 3)

