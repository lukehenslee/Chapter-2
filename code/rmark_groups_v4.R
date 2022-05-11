#==================================================================================================
# RMARK
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#Purpose: 
#==================================================================================================
# NOTES: Three states-
# A: Shaktoolik
# B: Unalakleet
# C: Outside
#==================================================================================================

# Load packages ####

library(tidyverse)
library(RMark)

# Set working directory ####
setwd("C:/Users/lukeh/Desktop/Git_repos/Chapter-2")

# Import data ####
coho <- read.csv('data/ch_coho_v4.csv')

# Remove NA since we are only estimating for coho assigned to stocks
coho <- coho[complete.cases(coho),]

# Modeling ####

## Make process data
coho.proc <- process.data(coho, model = 'Multistrata', groups = c('stock', 'stat.week'))

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix detection probabilities 
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum == 'A'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'B'] <- 1
coho.ddl$p$fix[coho.ddl$p$stratum == 'X'] <- 1

# Model selection ####

coho.models <- function ()
{
  ## phi structures 
  S.int <- list(formula = ~1)
  S.state <- list(formula = ~stratum)
  S.stock <- list(formula = ~stock)
  S.stock.state <- list(formula = ~stock + stratum)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  ### single
  Psi.state <- list(formula = ~stratum)
  Psi.tostate <- list(formula = ~tostratum)
  Psi.stock <- list(formula = ~stock)
  Psi.week <- list(formula = ~stat.week)
  
  ### double
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.stock.tostate <- list(formula = ~stock + tostratum)
  Psi.week.tostate <- list(formula = ~stat.week + tostratum)
  
  ### triple
  Psi.stock.state.tostate <- list(formula = ~stock + stratum + tostratum)
  Psi.week.state.tostate <- list(formula = ~stat.week + stratum + tostratum)
  Psi.stock.week.tostate <- list(formula = ~stock + stat.week + tostratum)
  
  ### quad
  Psi.stock.state.tostate.week <- list(formula = ~stock + stratum + tostratum + stat.week)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results <- coho.models()
coho.results

# S(~stock + stratum)p(~fix)Psi(~stock + stratum + tostratum + stat.week) 
# Weight:9.984279e-01 Deviance: 1219.526

# Model averaging 
## Don't need model averaging since the 'best' model has 0.998 of the weight

# Params of 'best' model
coho.results$S.stock.state.p.fixed.Psi.stock.state.tostate.week$results$real

# Transition matricies 
Psilist <- get.real(coho.results$S.int.p.fixed.Psi.stock.state.tostate.week,"Psi",vcv=TRUE)
Psivalues <- Psilist$estimates
coho.psi <- TransitionMatrix(Psivalues[Psivalues$time== 1 & Psivalues$stock == 'Transitory'
                                       & Psivalues$stat.week == 32,])
coho.psi


# Build movement estimates and incorporate survival ####
S.coho.S <- coho.results$S.stock.state.p.fixed.Psi.stock.state.tostate.week$results$real[c(1:3), c(1:4)]
S.coho.U <- coho.results$S.stock.state.p.fixed.Psi.stock.state.tostate.week$results$real[c(4:6), c(1:4)]
S.coho.T <- coho.results$S.stock.state.p.fixed.Psi.stock.state.tostate.week$results$real[c(7:9), c(1:4)]

## Unalakleet ####
### Week 31 ####
coho.psi.U.31 <- TransitionMatrix(Psivalues[Psivalues$time== 1 & Psivalues$stock == 'Unalakleet'
                                         & Psivalues$stat.week == 31,])
coho.psi.U.31.est <- coho.psi.U.31
coho.psi.U.31.lcl <- coho.psi.U.31
coho.psi.U.31.ucl <- coho.psi.U.31

# Movement from A
coho.psi.U.31.est[1,] <- coho.psi.U.31[1,] * S.coho.U[1,1] # est
coho.psi.U.31.lcl[1,] <- coho.psi.U.31[1,] * S.coho.U[1,3] # lcl
coho.psi.U.31.ucl[1,] <- coho.psi.U.31[1,] * S.coho.U[1,4] # ucl
coho.psi.U.31.est[1,1] <- 1 - sum(coho.psi.U.31[1,] * S.coho.U[1,1]) # Stop migration
coho.psi.U.31.lcl[1,1] <- 1 - sum(coho.psi.U.31[1,] * S.coho.U[1,3]) # Stop migration lcl
coho.psi.U.31.ucl[1,1] <- 1 - sum(coho.psi.U.31[1,] * S.coho.U[1,4]) # Stop migration ucl

# Movement from B
coho.psi.U.31.est[2,] <- coho.psi.U.31[2,] * S.coho.U[2,1] # est
coho.psi.U.31.lcl[2,] <- coho.psi.U.31[2,] * S.coho.U[2,3] # lcl
coho.psi.U.31.ucl[2,] <- coho.psi.U.31[2,] * S.coho.U[2,4] # ucl
coho.psi.U.31.est[2,2] <- 1 - sum(coho.psi.U.31[2,] * S.coho.U[2,1]) # Stop migration
coho.psi.U.31.lcl[2,2] <- 1 - sum(coho.psi.U.31[2,] * S.coho.U[2,3]) # Stop migration lcl
coho.psi.U.31.ucl[2,2] <- 1 - sum(coho.psi.U.31[2,] * S.coho.U[2,4]) # Stop migration ucl

# Movement from X
coho.psi.U.31.est[3,] <- coho.psi.U.31[3,] * S.coho.U[3,1] # est
coho.psi.U.31.lcl[3,] <- coho.psi.U.31[3,] * S.coho.U[3,3] # lcl
coho.psi.U.31.ucl[3,] <- coho.psi.U.31[3,] * S.coho.U[3,4] # ucl
coho.psi.U.31.est[3,3] <- 1 - sum(coho.psi.U.31[3,] * S.coho.U[3,1]) # Stop migration
coho.psi.U.31.lcl[3,3] <- 1 - sum(coho.psi.U.31[3,] * S.coho.U[3,3]) # Stop migration lcl
coho.psi.U.31.ucl[3,3] <- 1 - sum(coho.psi.U.31[3,] * S.coho.U[3,4]) # Stop migration ucl

round(coho.psi.U.31.est, 3)
round(coho.psi.U.31.lcl, 3)
round(coho.psi.U.31.ucl, 3)

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
