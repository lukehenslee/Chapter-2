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
coho <- read.csv('data/ch/stock_sex_ABX0.csv')
coho <- coho[which(coho$stock == 'Shaktoolik'),]
# Modeling ####

## Make process data
coho.proc <- process.data(coho, model = 'Multistrata', groups = c('sex'))

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


coho.models <- function ()
{
  ## phi structures 
  #S.int <- list(formula = ~1)
  S.state <- list(formula = ~stratum)
  S.sex <- list(formula = ~sex)
  #S.sex.state <- list(formula = ~sex + stratum)
  #S.stock.state <- list(formula = ~stock + stratum)
  #S.stock.sex.state <- list(formula = ~sex + stock + stratum)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  #Psi.time.sex <- list(formula = ~time + sex)
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.state.tostate.sex <- list(formula = ~stratum + tostratum + sex)
  #Psi.state.tostate.stock <- list(formula = ~stratum + tostratum + stock)
  #Psi.state.tostate.sex.stock <- list(formula = ~stratum + tostratum + sex.stock)
  #Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  #Psi.state.tostate.sex.time <- list(formula = ~stratum + tostratum + sex + time)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE, mlogit0 = TRUE)
  return(results)
}

coho.results <- coho.models()  
coho.results

# Params of 'best' model
#coho.results$S.stock.sex.state.p.fixed.Psi.state.tostate.stock$group.labels

coho.mod.avg <- model.average(coho.results, vcv = T, drop = F)

vcv <- coho.mod.avg$vcv.real

S <- model.average(coho.results, 'S')
S <- S[which(S[ ,7] %in% c('2', '3') == F & S[ ,9] == 1),]
S

p <- model.average(coho.results, 'p')

Psi <- model.average(coho.results, 'Psi', drop = F)
Psi <- Psi[which(Psi[ ,7] %in% c('2','3') == F & Psi[ ,9] == 1),]
Psi

## Delta method ####
deltamethod(~x1 * x2, mean = coho.mod.avg$estimates[c(1:2),2], 
            cov = coho.mod.avg$vcv.real[c(1:2), c(1:2)])

# Unalakleet males
um <- TransitionMatrix(Psi[Psi$group == 'M',])

um[1, ] <- um[1, ] * S[S$group == 'MUnalakleet' & S$stratum == 'A', 2]
1 - sum(um[1, ]) #0.083
um[2, ] <- um[2, ] * S[S$group == 'MUnalakleet' & S$stratum == 'B', 2]
1 - sum(um[2, ])
um[3, ] <- um[3, ] * S[S$group == 'MUnalakleet' & S$stratum == 'X', 2]
1 - sum(um[3, ])

# Shaktoolik males
sm <- TransitionMatrix(Psi[Psi$group == 'MShaktoolik',])

sm[1, ] <- sm[1, ] * S[S$group == 'MShaktoolik' & S$stratum == 'A', 2]
sm[2, ] <- sm[2, ] * S[S$group == 'MShaktoolik' & S$stratum == 'B', 2]
sm[3, ] <- sm[3, ] * S[S$group == 'MShaktoolik' & S$stratum == 'X', 2]

# Transitory males
tm <- TransitionMatrix(Psi[Psi$group == 'MTransitory',])

tm[1, ] <- tm[1, ] * S[S$group == 'MTransitory' & S$stratum == 'A', 2]
tm[2, ] <- tm[2, ] * S[S$group == 'MTransitory' & S$stratum == 'B', 2]
tm[3, ] <- tm[3, ] * S[S$group == 'MTransitory' & S$stratum == 'X', 2]

# Unalakleet females
uf <- TransitionMatrix(Psi[Psi$group == 'F',])

uf[1, ] <- uf[1, ] * S[S$group == 'FUnalakleet' & S$stratum == 'A', 2]
uf[2, ] <- uf[2, ] * S[S$group == 'FUnalakleet' & S$stratum == 'B', 2]
uf[3, ] <- uf[3, ] * S[S$group == 'FUnalakleet' & S$stratum == 'X', 2]

# Shaktoolik females
sf <- TransitionMatrix(Psi[Psi$group == 'FShaktoolik',])

sf[1, ] <- sf[1, ] * S[S$group == 'FShaktoolik' & S$stratum == 'A', 2]
sf[2, ] <- sf[2, ] * S[S$group == 'FShaktoolik' & S$stratum == 'B', 2]
sf[3, ] <- sf[3, ] * S[S$group == 'FShaktoolik' & S$stratum == 'X', 2]

# Transitory females
tf <- TransitionMatrix(Psi[Psi$group == 'FTransitory',])

tf[1, ] <- tf[1, ] * S[S$group == 'FTransitory' & S$stratum == 'A', 2]
tf[2, ] <- tf[2, ] * S[S$group == 'FTransitory' & S$stratum == 'B', 2]
tf[3, ] <- tf[3, ] * S[S$group == 'FTransitory' & S$stratum == 'X', 2]







# Build movement estimates and incorporate survival
S.coho.F <- coho.results$S.sex.state.p.fixed.Psi.state.tostate$results$real[c(1:2,9,3:4), c(1:4)]
S.coho.M <- coho.results$S.sex.state.p.fixed.Psi.state.tostate$results$real[c(5:6,9,7:8), c(1:4)]

# FEMALES
coho.psi.F.est <- coho.psi
coho.psi.F.lcl <- coho.psi
coho.psi.F.ucl <- coho.psi

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
uf[2, ] <- uf[2, ] * S[S$group == 'FUnalakleet' & S$stratum == 'B', 2]
uf[3, ] <- uf[3, ] * S[S$group == 'FUnalakleet' & S$stratum == 'X', 2]