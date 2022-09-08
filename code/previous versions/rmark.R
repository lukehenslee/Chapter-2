#==================================================================================================
# RMARK
# Date: February 21, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
#Purpose: 
#==================================================================================================
#NOTES: ALL MORTALITY TAKES PLACE BEFORE MOVEMENT 
#
# Sum of all movement probabilities FROM a given state must = 1
## Therefore, if psiBB = psiBX, then psiBB = psiBX = 0.5
#
# When setting psi param to zero, use mlogit0=TRUE
#==================================================================================================

# Load packages ####

library(tidyverse)
library(RMark)

# Set working directory ####
setwd("C:/Users/lukeh/Desktop/Git_repos/Chapter-2")

# Import data ####
coho <- read.csv('data/ch_coho.csv', colClasses = c('numeric', 'character',
                    rep('factor', 2)))

# Modeling ####

## Sex ####
## Make process data
coho.proc <- process.data(coho, model = 'Multistrata')

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix detection probabilities 

### Reference indices and set values
p.A <- as.numeric(row.names(coho.ddl$p[coho.ddl$p$A == 1,]))
p.A.val <- rep(0.92, length(p.A))
p.B <- as.numeric(row.names(coho.ddl$p[coho.ddl$p$B == 1,]))
p.B.val <- rep(0.97, length(p.B))
p.X <- as.numeric(row.names(coho.ddl$p[coho.ddl$p$X == 1,]))
p.X.val <- rep(0.96, length(p.X))

p.fixed <- list(index = c(p.A, p.B, p.X), value = c(p.A.val, p.B.val, p.X.val))

# Model selection ####

ms.models <- function ()
{
  ## phi structures 
  S.dot <- list(formula = ~1)
  S.state <- list(formula = ~stratum)
  S.time <- list(formula = ~time)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(fixed = p.fixed)
  
  ## psi
  Psi.dot <- list(formula = ~1)
  Psi.state <- list(formula = ~stratum)
  Psi.tostate <- list(formula = ~tostratum)
  Psi.time <- list(formula = ~time)
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.stateXtostate <- list(formula = ~stratum*tostratum)
  Psi.state.int.tostate <- list(formula = ~stratum:tostratum)
  Psi.stateXtostate.time <- list(formula = ~ stratum*tostratum + time)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE)
  return(results)
}

ms.results<-ms.models()  

ms.results

# Look at param values for 'best' model

ms.results$S.time.Psi.stateXtostate$results$beta
ms.results$S.time.Psi.stateXtostate$results$real

# Look at the transition matrix
Psilist <- get.real(ms.results$S.time.Psi.stateXtostate, "Psi", vcv=TRUE)

Psivalues <- Psilist$estimates

TransitionMatrix(Psivalues[Psivalues$time==1,])

# Now model with a time structure to get cease movement probabilities between steps
coho.proc <- process.data(coho, model = 'Multistrata')

## Design data
coho.ddl <- make.design.data(coho.proc,)

## Fix detection probabilities 
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum=="A"] <- 0.92
coho.ddl$p$fix[coho.ddl$p$stratum=="B"] <- 0.97
coho.ddl$p$fix[coho.ddl$p$stratum=="X"] <- 0.96


psi.state.time <- list(formula = ~ stratum + time)
phi.state.time <- list(formula = ~ stratum + time)

S.state.time.p.fixed.Psi.state.time <- mark(coho.proc, coho.ddl, model.parameters=list(S = phi.state.time, 
                                                       Psi = psi.state.time, 
                                                       p = p.fixed)) 

psi.state <- list(formula = ~ stratum)
phi.state <- list(formula = ~ stratum)
p.state <- list(formula = ~ stratum)

S.state.p.state.Psi.time <- mark(coho.proc, coho.ddl, model.parameters=list(S = phi.state, 
                                                                                       Psi = psi.state, 
                                                                                       p = p.state)) 

psi.state <- list(formula = ~ stratum)
phi. <- list(formula = ~ 1)
p. <- list(formula = ~ 1)

S..p..Psi.state <- mark(coho.proc, coho.ddl, model.parameters=list(S = phi., 
                                                                            Psi = psi.state, 
                                                                            p = p.))

S.state.p..Psi.state <- mark(coho.proc, coho.ddl, model.parameters=list(S = phi.state, 
                                                                   Psi = psi.state, 
                                                                   p = p.))
