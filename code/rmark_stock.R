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
coho <- read.csv('data/ch_coho_stock.csv')
str(coho)
# Modeling ####

## Sex ####
## Make process data
coho.proc <- process.data(coho, model = 'Multistrata', group = 'stock')

## Design data
coho.ddl <- make.design.data(coho.proc)

## Fix parameters 

### Fix p
p.A <- as.numeric(row.names(coho.ddl$p[coho.ddl$p$A == 1,]))
p.A.val <- rep(0.92, length(p.A))
p.B <- as.numeric(row.names(coho.ddl$p[coho.ddl$p$B == 1,]))
p.B.val <- rep(0.97, length(p.B))
p.X <- as.numeric(row.names(coho.ddl$p[coho.ddl$p$X == 1,]))
p.X.val <- rep(0.96, length(p.X))

p.fixed <- list(index = c(p.A, p.B, p.X), value = c(p.A.val, p.B.val, p.X.val))

### Fix phi
S.A <- as.numeric(row.names(coho.ddl$S[coho.ddl$S$A == 1,]))
S.A.val <- rep(1, length(S.A))
S.B <- as.numeric(row.names(coho.ddl$S[coho.ddl$S$B == 1,]))
S.B.val <- rep(1, length(S.B))
S.X <- as.numeric(row.names(coho.ddl$S[coho.ddl$S$X == 1,]))
S.X.val <- rep(1, length(S.X))

S.fixed <- list(index = c(S.A, S.B, S.X), value = c(S.A.val, S.B.val, S.X.val))
# Model selection ####

ms.models <- function ()
{
  ## phi structures 
  S.time <- list(formula = ~time)
  S.state.time <- list(formula = ~time + stratum)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~stratum)
  
  ## psi
  ### Single variables
  Psi.dot <- list(formula = ~1)
  
  ### OG
  Psi.state.tostate <- list(formula = ~stratum + tostratum)
  Psi.stateXtostate <- list(formula = ~stratum*tostratum)
  Psi.state.tostate.time <- list(formula = ~stratum + tostratum + time)
  Psi.state.int.tostate.time <- list(formula = ~stratum*tostratum + time)
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE)
  return(results)
}

ms.results<-ms.models()  

ms.results

# Look at param values for 'best' model

ms.results$S.state.time.p.fixed.Psi.state.tostate$results$real

# Look at the transition matrix
Psilist <- get.real(ms.results$S.state.time.p.fixed.Psi.state.tostate.time$results, 'Psi', vcv = TRUE)

Psivalues <- Psilist$

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
