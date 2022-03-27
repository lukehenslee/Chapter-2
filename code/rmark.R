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
coho <- read.csv('data/ch_coho.csv', colClasses = c('numeric', 'character',
                    rep('factor', 2)))

# Modeling ####

## Sex ####
## Make process data
coho.proc <- process.data(coho, model = 'Multistrata', groups = 'sex')

## Design data
coho.ddl <- make.design.data(coho.proc,)

## Fix detection probabilities 
coho.ddl$p$fix <- NA
coho.ddl$p$fix[coho.ddl$p$stratum=="A"] <- 0.92
coho.ddl$p$fix[coho.ddl$p$stratum=="B"] <- 0.97
coho.ddl$p$fix[coho.ddl$p$stratum=="X"] <- 0.96

coho.ddl$p$fix

## Set model params ####
### p
p.fix <- list(formula = ~fix)

### phi
phi.state.sex <- list(formula = ~stratum + sex)

### psi 
psi.state.sex <- list(formula = ~stratum + sex)

# Make model ####

mod1<- mark(coho.proc, coho.ddl, model.parameters=list(S = phi.state.sex, 
                                                            Psi = psi.state.sex, 
                                                            p = p.fix)) 

mod1$results$beta

mod1$results$real

# Model selection ####

ms.models <- function ()
{
  ## phi structures 
  S.dot <- list(formula = ~1)
  S.state <- list(formula = ~stratum)
  S.sex <- list(formula = ~sex)
  S.state.sex <- list(formula = ~stratum + sex)
  
  ## fixed values for p based on our earlier analyses
  p.fixed <- list(formula = ~fix)
  
  ## psi
  Psi.dot <- list(formula = ~1)
  Psi.state <- list(formula = ~stratum)
  Psi.sex <- list(formula = ~sex)
  Psi.state.sex <- list(formula = ~stratum + sex) 
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=coho.proc, ddl=coho.ddl, output=FALSE)
  return(results)
}

ms.results<-ms.models()  

ms.results

# Look at param values for 'best' model

ms.results$S.state.sex.p.fixed.Psi.state$results$beta
ms.results$S.state.sex.p.fixed.Psi.state$results$real

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
