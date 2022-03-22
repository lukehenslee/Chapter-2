
library(RMark)

## import data 

SAGR.ms<-read.csv('data/SAGRms.csv', header=T)

## check to see that import successfully brought ch as a character string and our covariates
## as numeric.  

str(SAGR.ms)

## convert ch to a character string

SAGR.ms$ch<- as.character(SAGR.ms$ch)

## take a look at the file to get a feel for the data

View(SAGR.ms)

## make the process data 

SAGR.ms.proc<- process.data(SAGR.ms,model="Multistrata")

## and the design data

SAGR.ms.ddl<- make.design.data(SAGR.ms.proc)

## lets take a look at the PIM structure for each of the parameter types

## for psi

SAGR.ms.ddl$Psi


## for phi

SAGR.ms.ddl$S

## first, we will fix the detection probabilities for the successful females to 1.0, because there  
## were no known cases where we missed an available female during succesful.  We can use the following
## code to extract the index values for these parameters from the design data.  Take a careful look
## at this code and see how it is asking for the row names (PIM values) for all of the detection 
## parameters where the stratum is identified as S. 

p.S.indices=as.numeric(row.names(SAGR.ms.ddl$p[SAGR.ms.ddl$p$stratum=="S",]))

p.S.indices

## now, lets compare this to the PIMs, just to be sure. You should see that stratum switches to 
## S at par.index 22

SAGR.ms.ddl$p

## next, we will create a vector of 0s that are the same length as the vector of indices 
## we just created

p.S.values=rep(0,length(p.S.indices))

p.S.values

## now we can specify the parameter structures for each of the different parameter types; 
## p, phi, and phi.  In keeping with the approach I took in the paper, we will assume that 
## p only varies according to state, but that the p for the successful state is fixed to 1.0
## using our index and value terms we defined above.

p.state=list(formula=~stratum, fixed=list(index=p.S.indices, value=p.S.values))

## For the survival term (phi) in the paper I considerd only a single structure with additive 
## effects of State and Age.  This was informed in part by the known-fate analysis of the larger
## telemetry dataset, where we did not find support for annual variation in survival.  But as 
## mentioned in the handout, Age was likely confounded with transmitter failure, so was important 
## to incorporate here. 

phi.state.age=list(formula=~stratum + cov)

## and for the psi term this is where we can get a little more creative with how we 
## proceed through model selection

psi.state.time<-list(formula=~stratum + time) 

mod1<- mark(SAGR.ms.proc,SAGR.ms.ddl, model.parameters=list(S = phi.state.age, 
                                                          Psi = psi.state.time, 
                                                          p = p.state), output=FALSE)                                                         

## Notice the parameter warning here, which is being thrown as a result of lack of convergence 
## on the p's and also many of the psi terms.

mod1$results$beta

mod1$results$real

## the lack of convergence here could be a result of p being too close to a value of 1.0 for the
## failed breeding state, which is a realistic fact of a telemetry-based dataset. 
## or it could be due to time terms within the psi's.  One thing we should try to do is to 
## fix all the values for of the ps (across both states).  This is more simple than 
## our earlier approach 

p.fixed=list(formula=~1, fixed=1)

## and re-run the model replacing p.state with p.fixed

mod2<- mark(SAGR.ms.proc,SAGR.ms.ddl, model.parameters=list(S = phi.state.age, 
                                                            Psi = psi.state.time, 
                                                            p = p.fixed), output=FALSE)                                                         

## Now we should see widespread convergence, including for the annual psi terms.  We also 
## see that the p terms (betas) are removed completely from the model (because they are fixed)

mod2$results$beta

mod2$results$real

## the last little bit of bookkeeping we need to do is change the definition of our state terms
## right now for the failed state, we are getting the probability of transitioning to success, 
## whereas for the succesful state we're getting the probability of transitioning to failure.  
## We can get the alternate values (failed to failed, ## success to success) by subtraction, but 
## to fit covariates to the transitions we need to have a common transition term. I.e. it makes 
## no sense to fit a covariate that affects probability of failure for one state and probability
## of success for the other. We need to re-define which term is estimated by subtraction, and 
## we will redefine such that both cases estimate the transition into the Successful state, such
## that our state transitions represent the probability of succesfully fledging young. 

## this is accomplished by modifying the way our design data is constructed to include the 
## subtract.stratum() command, and specifying "F" (failed) as the transition for subtraction
## for each of the two stratum.  If you had more than two states, you would include one term 
## for each of the n states in that staement

SAGR.ms.ddl<- make.design.data(SAGR.ms.proc,parameters=
                                 list(Psi=list(subtract.stratum=c("F","F"))))

## now re-run the model with the modified design data

mod3<- mark(SAGR.ms.proc,SAGR.ms.ddl, model.parameters=list(S = phi.state.age, 
                                                            Psi = psi.state.time, 
                                                            p = p.fixed), output=FALSE)                                                         

## and we should see a common transition direction on the real estimates of psi

mod3$results$real

## now that we've got our base model structure correctly built, we can actually get around 
## to doing some hypothesis testing.  There are two different covariate types we'll be implementing
## in this analysis; one is our individual time-varying covariate for age structure, and the second 
## is a group time-varying covariate of annual precipitation. 

## the individual covariate is already embedded in the data file and is part of our processed 
## data.  

head(SAGR.ms.proc$data)

## the process used by RMark here is such that when we specify a =~cov structure in a model, 
## it reads through the 1-7 covariate values based on the numeric index in their covariate 
## name, and applies them to the appropriate location in the design matrix that corresponds 
## with the same numeric interval (1, 2, 3, ...etc).  It is pretty slick and after the data 
## management stage requires nothing additional from you that differs from a normal covariate.  

## for the group covariate, we've got to do a little work to incorporate the values into our 
## design data.   First lets create this term, using the values provided in the handout.  While
## you could import this as its own table, since we're working with a small number of values 
## it is easy enough to build the data.frame by hand

time<- seq(1,7,1)
precip<- c(1.90364648216, -0.855768464, -0.52976143, 0.029107771, -0.59962008, 0.052393988, -0.28648)

G.cov<- data.frame(cbind(time, precip))
G.cov

## now we can use the merge_design.covariates() to add the group-level covariate to our 
## design data. There are two critical pieces of inforamtion here provided by the bygroup=
## and bytime= statements.  For bytime=, you need to have a time column in your group covar.
## data frame (which we do) and that is what the data will be merged by.  If bygroup= were 
## true you would also need a Group column, which also implies that your covariate values
## may need to replicate across each group; we do not have that scenario so only need bytime=

SAGR.ms.ddl$Psi<- merge_design.covariates(SAGR.ms.ddl$Psi,G.cov,bygroup=FALSE, bytime=TRUE)

## take a look at the new design data

SAGR.ms.ddl$Psi

## Now we can proceed with model sellection.  I am going to approach this using my standard 
## sequential approach, as I defined in the supplemental handout I gave you earlier in the 
## semester.  The model structure for p is fixed, so this is really just a matter of first 
## evaluating the best model structure for phi, and then for psi.  First I will remove the 
## three models I ran earlier

remove(mod1)
remove(mod2)
remove(mod3)

## and now define the alternative model structures for phi(S), which include effects of state, 
## age (cov), and time, plus a dot structure, and then run those models using a function with mark.wrapper

ms.phi.models=function ()
{
 
  ## phi structures for single terms
  
  S.dot=list(formula=~1)
  S.state=list(formula=~stratum)
  S.time=list(formula=~time)
  S.age=list(formula=~cov)
  
  ## and joint additive models
  
  S.state.time=list(formula=~stratum + time)
  S.state.age=list(formula=~stratum + cov)
  S.age.time=list(formula=~age + time)
  S.state.time.age=list(formula=~stratum + time + cov)
  
  ## fixed values for p based on our earlier analyses
  
  p.fixed = list(formula=~1, fixed=1)
  
  ## fully general state and time structure for psi
  
  Psi.state.time = list(formula=~stratum + time) 
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=SAGR.ms.proc, ddl=SAGR.ms.ddl, output=FALSE)
  return(results)
} ## end function

## Run function and name the results of the function "ms.phi.results"
ms.phi.results<-ms.phi.models()  

## These models all ran well with only 1 convergence issues;  best not 
## to worry too much about convergence at this point until we have a chance to  
## address constraints on the other terms

ms.phi.results 

## So the best model has a state + age structure for phi, with one including time as 
## a competative alternative. Take a look at the betas and real estiamtes for these 
## models, which as a reminder can be accessed as follows: 

ms.phi.results$S.state.time.age.p.fixed.Psi.state.time$results$beta
ms.phi.results$S.state.time.age.p.fixed.Psi.state.time$results$real

## there is a non-trivial decision point to be made here, whether to go with the 
## more general, but less-well-supported model with a full time structure, or 
## to use the better-supported model with just state and age effects.  Ultimately
## I went with the later for my dissertation because there was another analysis for 
## this manuscript that was more data rich to suggest lack of time effects on survival,
## plus dropping the extra parameters will make estimation a bit more efficient. 

## given a best time structure for phi, and with p fixed, we can work through the 
## various model structures for psi using a new function that will apply all of the 
## psi structures.  Again here for expediency, I just included all of the additive 
## model combinations I was interested in 

ms.psi.models=function ()
{
  
  ## phi structures from step 1
  
  S.state.age=list(formula=~stratum + cov)
  
  ## fixed values for p based on our earlier analyses
  
  p.fixed = list(formula=~1, fixed=1)
  
  ## models for psi that consider state, time, and precip covariates
  
  Psi.state.time = list(formula=~stratum + time) 
  Psi.state = list(formula=~stratum) 
  Psi.time = list(formula=~time) 
  Psi.state.precip = list(formula=~stratum + precip) 
  Psi.precip = list(formula=~precip) 
  Psi.state.age = list(formula=~stratum + cov) 
  Psi.age = list(formula=~cov) 
  Psi.dot = list(formula=~1) 
  
  ## plus one interaction model to test a specific hypothesis 
  
  Psi.state.by.precip = list(formula=~stratum * precip) 
  
  cml=create.model.list("Multistrata")
  results=mark.wrapper(cml, data=SAGR.ms.proc, ddl=SAGR.ms.ddl, output=FALSE)
  return(results)
} ## end function

## Run function and name the results of the function "ms.psi.results"
ms.psi.results<-ms.psi.models()  

## These models all ran well without convergence issue, and suggest general support for both 
## state and precip effects on transition probability.

ms.psi.results 

## again, explore the results of the best model

ms.psi.results$S.state.age.p.fixed.Psi.state.precip$results$beta
ms.psi.results$S.state.age.p.fixed.Psi.state.precip$results$real

## you will notice here that the 95% confidence intervals of the state effect overlap 0.0
## if we adhere to recommendations by Arnold (2010) 85% CIs may be more appropriate to use
## in conjunction with a criteria of 2.0 delta AIC for model selection.  These can be 
## be approximated as 1.44 times the standard error (vs 1.96*SE for 95%).

## lets close with some graphing of these results.  How about a graph of probability of 
## success by state through time.  First extract the real estimates for intrevals 1 through 6 
## for each state.  This requires a bit of careful thought about how the real estimates relate
## to the PIMs

Psi.Fail<- ms.psi.results$S.state.age.p.fixed.Psi.state.precip$results$real[14:19,]
Psi.Success<- ms.psi.results$S.state.age.p.fixed.Psi.state.precip$results$real[35:40,]

## note that all of the nonsense before the estimate column is a row label, not columnar 
## data, so rows 1:4 are the estimate, se, and cls. These can be combined using rbind()

Psi.est<- data.frame(rbind(Psi.Fail[,1:4], Psi.Success[,1:4])) 

## and then add year and state variables for graphing

Psi.est$Year<- rep(seq(2006,2011,1), times=2)
Psi.est$Prior.State<- rep(c("Failed", "Success"), each=6)

## and now onto ggplot

library(ggplot2)

ms.year.fig<- ggplot(data=Psi.est, aes(x=Year, y=estimate, color=Prior.State))+
  geom_errorbar(aes(ymax=(estimate+se),ymin=(estimate-se)), width = 0.3, position=position_dodge(width=0.3))+
  geom_point(position=position_dodge(width=0.3), size=2.5)+
  geom_line(position=position_dodge(width=0.3), size=0.5) +
  ylab ("Probability of brood success") +
  xlab ("")+ 
  theme(text= element_text(size=16)) 

ms.year.fig

## just for fun, lets add an additional time series with the full annual estiamtes from the year 
## model.  This will give a good indication for how well the precip variable predicts annual 
## variability in reproductive success.

Psi.Annual<- ms.psi.results$S.state.age.p.fixed.Psi.time$results$real[14:19,]
Psi.Annual$Year<- rep(seq(2006,2011,1), times=1)
Psi.Annual$Prior.State<- rep(c("Failed"), each=6)

## and we can add one addition geom_line() to the figure code to depict this relationship

ms.year.fig<- ggplot(data=Psi.est, aes(x=Year, y=estimate, color=Prior.State))+
  geom_errorbar(aes(ymax=(estimate+se),ymin=(estimate-se)), width = 0.3, position=position_dodge(width=0.3))+
  geom_point(position=position_dodge(width=0.3), size=2.5)+
  geom_line(position=position_dodge(width=0.3), size=0.5) +
  geom_line(data=Psi.Annual, aes(x=Year, y=estimate), linetype="dashed", color="black", size=0.75) +
  ylab ("Probability of brood success") +
  xlab ("")+ 
  theme(text= element_text(size=16)) 

ms.year.fig

## neat-o.  This shows that the psi estiamtes from the state + precip model, which are constrained
## through time to perfectly follow annual precip values, track the full annual estimates, which are
## unconstrained, very well. We can actually formally quatify this rate of agreement using an approach
## that is superficially similar to computing an R-squared value in a normal linear regression.  This 
## is refered to as R-squared dev, and it is described in greater detail in the handout. 

## the model deviances can be accessed from the model selection results table, and I will use the 
## Psi(State) model as my null equivalent

Dev.cov<- ms.psi.results$model.table[1,9]
Dev.time<- ms.psi.results$model.table[8,9]
Dev.dot<- ms.psi.results$model.table[3,9]

## and we can compute R2_Dev using the equation contained on page 6 of the lab handout

R2_Dev<- (Dev.dot - Dev.cov)/(Dev.dot - Dev.time)

## and if we did this correctly, we should get an estimate somewhere between 0 and 1, hopefully
## closer to 1 than 0.

R2_Dev

## Huzzah! It appears as though ~ 82% of the annual variability in sage-grouse reproductive 
## success is explained by annual patterns in rainfall.


