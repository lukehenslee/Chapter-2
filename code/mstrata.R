#==================================================================================================
# mstrata
# Date: February 23, 2022
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#

#==================================================================================================
#NOTES: 
#==================================================================================================

# Load packages ####
library(RMark)

# Set working directory ####
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter 2")
<<<<<<< HEAD
ch5 <- read.csv('ch5.csv')

ch5$ch <- str_pad(ch5$ch, width = 4, side = 'right', pad = '0')
ch5m <- ch5 %>% count(ch)

mstrata <- ch5m
=======


>>>>>>> 173152ced44f0ae880ddbc133fe4c91709ec4c2c
data(mstrata)

run.mstrata=function()
{
  #
  # Process data
  #
  mstrata.processed=process.data(mstrata,model="Multistrata")
  #
  # Create default design data
  #
  mstrata.ddl=make.design.data(mstrata.processed)
  #
  #  Define range of models for S; note that the betas will differ from the output
  #  in MARK for the ~stratum = S(s) because the design matrix is defined using
  #  treatment contrasts for factors so the intercept is stratum A and the other
  #  two estimates represent the amount that survival for B abd C differ from A.
  #  You can use force the approach used in MARK with the formula ~-1+stratum which
  #  creates 3 separate Betas - one for A,B and C.
  #
  S.stratum=list(formula=~stratum)
  S.stratumxtime=list(formula=~stratum*time)
  #
  #  Define range of models for p
  #
  p.stratum=list(formula=~stratum)
  #
  #  Define range of models for Psi; what is denoted as s for Psi
  #  in the Mark example for Psi is accomplished by -1+stratum:tostratum which
  #  nests tostratum within stratum.  Likewise, to get s*t as noted in MARK you
  #  want ~-1+stratum:tostratum:time with time nested in tostratum nested in
  #  stratum.
  #
  Psi.s=list(formula=~-1+stratum:tostratum)
  #
  # Create model list and run assortment of models
  #
  model.list=create.model.list("Multistrata")
  #
  # Add on specific model that is paired with fixed p's to remove confounding
  #
  p.stratumxtime=list(formula=~stratum*time)
  p.stratumxtime.fixed=list(formula=~stratum*time,fixed=list(time=4,value=1))
  model.list=rbind(model.list,c(S="S.stratumxtime",p="p.stratumxtime.fixed",
                                Psi="Psi.s"))
  #
  # Run the list of models
  #
  mstrata.results=mark.wrapper(model.list,data=mstrata.processed,ddl=mstrata.ddl,threads=2)
  #
  # Return model table and list of models
  #
  return(mstrata.results)
}
mstrata.results=run.mstrata()
mstrata.results

# Example of reverse Multistratum model
data(mstrata)
mod=mark(mstrata,model="Multistrata")
mod.rev=mark(mstrata,model="Multistrata",reverse=TRUE)
Psilist=get.real(mod,"Psi",vcv=TRUE)
Psilist.rev=get.real(mod.rev,"Psi",vcv=TRUE)
Psivalues=Psilist$estimates
Psivalues.rev=Psilist.rev$estimates
TransitionMatrix(Psivalues[Psivalues$time==1,])
TransitionMatrix(Psivalues.rev[Psivalues.rev$occ==1,], ch = TRUE)
TransitionMatrix(Psivalues.rev[Psivalues.rev$occ==1,])

