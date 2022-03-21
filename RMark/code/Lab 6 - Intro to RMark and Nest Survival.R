#########################################################################
######################## Part 1 #########################################
######################## First Steps ####################################

## install (if necessary) and load the necessary packages

#install.packages("RMark")
#install.packages("rgl")
library(RMark)
library(rgl)

## import data 
 
nest.df<-read.csv("C:/Users/Erik/Desktop/R/SAGRNest.csv", header=T)

## First lets take a look at the data structure Note the different categories 
## and determine if you need to change the type of any of the variables

str(nest.df)

## convert "Year" to a factor

nest.df$Year<- factor(nest.df$Year)

## Check data structure again - Make sure "Year" is a factor

str(nest.df)

## The first step in running a new RMark analysis will be to processing the data 
## This is similar to what you do on the 1st screen in Program MARK to define 
## the number of occassions, analysis type, how many groups, etc. 

ns1.process<- process.data(nest.df, model="Nest", nocc=44, groups=c("Year", "HenAge"))

## Next we need to make our 'design data', which is similar to setting the PIM structure
## at the beginning of a MARK analysis.  In this case our PIMS are fine as is and there
## is no need to tweak them from the normal group and time structure, so the code is 
## pretty strait-forward.

ns1.ddl<- make.design.data(ns1.process)

##Run the null (.) model
S.dot<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~1)))

## Show PIM structure for previously run model
PIMS(S.dot, "S", simplified=T)

## View results of S.dot model in text window
S.dot

## View just the real estimates

S.dot$results$real

## Single variable model for Sage
S.Sage <- mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~Sage)))

## Show PIM structure for previously run model
PIMS(S.Sage, "S", simplified=T)

## View results of model in text window
S.Sage

## Model incorporating group effect of Year
S.Year<- mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~Year)))

## Show PIM structure for previously run model
PIMS(S.Year, "S", simplified=T)

## Model testing group effect of age

S.Age<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~HenAge)))

## Collect models run so far to append into an AIC table

GSG.results=collect.models()

GSG.results

## Additive model using the variables Dlek and All1000
S.Dlek.and.All1000<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~Dlek+All1000)))

## Show PIM structure for previously run model
PIMS(S.Dlek.and.All1000, "S", simplified=T)

## model with interaction between Dlek and All1000
S.Dlek.X.All1000<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~Dlek*All1000)))

## Show PIM structure for previously run model
PIMS(S.Dlek.X.All1000, "S", simplified=T)

## Estimates for each day separately - Note: the Lowercase "t" indicates 
  #that You're not restricting the data to a trend
S.time<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~time)))

## Show PIM structure for previously run model
PIMS(S.time, "S", simplified=T)

## Linear Time trend - Note: the capitalized "T" indicates that it's a time trend
S.Time<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~Time)))

##Produce an additive model with a time trend and NonSage
S.Time.and.NonSage<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~Time+NonSage)))

# Non-linear Time trend - Note how the quadratic term was added.
##You can do this for any continuous covariate
##Whenever you want to make a quadratic term you follow this general formula:
##VariableX + I(VariableX^2)

S.Time2<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~Time+I(Time^2))))

## Create a design structure that allows for testing the difference between survival 
  ##during laying and incubation By modifying the .ddl
## All capture histories start at the day the 1st egg was laid (Day 1)
  ## Laying occurs during the 1st 10 days, after that it is considered the incubation stage

ns1.ddl<-add.design.data(ns1.process, ns1.ddl, parameter="S", type="time", bins=c(1,11,44), name="NestStage", right=F, replace=T)

## This variable that you are creating ("NestStage") uses the information from the ns1.ddl to 
  ##separate laying from incubation stages 
## This shows you how it breaks the occasions into a laying and incubation period.
##It is important to note the difference between the "[" and "(" ... See handout/appendix C

summary(ns1.ddl$S$NestStage)

## Run model testing the effect of the stage of nesting (e.g. laying versus incubation)
S.NestStage<-mark(ns1.process, ns1.ddl, model.parameters=list(S=list(formula=~NestStage)))

## View the PIM structure of the model that was just run
PIMS(S.NestStage, "S", simplified=T)

## View model results in a text window
S.NestStage

## Compile the results from the models that have been run
GSG.results=collect.models()
## View the AIC table of results
GSG.results



################################################################
########################## Part 2 ##############################
################################################################

##Simplifying analysis by using a function

## processing data - This is similar to what you do on the 1st screen in program MARK
ns.process<-process.data(nest.df, model="Nest", nocc=44, groups=c("Year", "HenAge"))

## This is similar to setting the Pims
ns.ddl<-make.design.data(ns.process)


## Giving the function the name "nest.models"
nest.models=function ()
{
## Null model
S.dot=list(formula=~1)

##Note what pieces of information 
  ##are included/excluded when writing this formula compared to what you did previously

## one-variable models
S.Year=list(formula=~Year)
S.JD=list(formula=~JD)
S.HenAge=list(formula=~HenAge)
S.Dlek=list(formula=~Dlek)
S.Sage=list(formula=~Sage)
S.NonSage=list(formula=~NonSage)
S.RoadD500=list(formula=~RoadD500)
S.All1000=list(formula=~All1000)
S.NMG_Ht=list(formula=~NMG_Ht)
S.AvgGrsHt100=list(formula=~AvgGrsHt100)
## end of 1-variable models
##Time-varying Models

## Linear Time trend - Note: the capitalized "T" indicates that it's a time trend
S.Time=list(formula=~Time)
## Non-linear Time trend - Note how the quadratic term was added.
S.Time2=list(formula=~Time+I(Time^2))
## Separate survival estimates for each time step - note: the lowercase "t"
S.time=list(formula=~time)


##You are now providing the information that applies to all models in this function
  ## Therefore, it was not necessary to add this information to each model as done before
  
  cml=create.model.list("Nest")
  results=mark.wrapper(cml, data=ns.process,)
  return(results)
} ## end function

## Run function and name the results of the function "nest.results"
nest.results<-nest.models()  

## View table of results from all models
nest.results 

##View beta results for a single model 
nest.results$S.NonSage$results$beta

## Compute model-averaged daily nest survival estimates

Avg.est<- model.average(nest.results) 

## Extract just the first 44 parameters

Avg.est.44<- Avg.est[1:44, ] 

## Calculate nest success probability for a 37-day nesting interval 

NS.37<- product(Avg.est.44[1:44, 2])
NS.37

## Compute real parameter values for survival as a function of
  # NonSage which are model-averaged over the fitted models.

## Create a vector for the values of NonSage to calculate real parameter estimates
NonSage.values<-seq(-0.64, 6.24, by=0.2)

## or alternatively 
NonSage.values<-seq(min(nest.df$NonSage),max(nest.df$NonSage), by=0.2)

## Check to make sure this was created correctly
summary(NonSage.values)
length(NonSage.values)

## Compute DSR estimates from the NonSage Model
DSRbyNonSage<-covariate.predictions(nest.results$S.NonSage, data=data.frame(NonSage=NonSage.values), indices=c(1))
## Look at the data
DSRbyNonSage

## Extract just the estimates, SE, and CIs from these results and append into their
## own data.frame.  Note that the output contains both the estimates and the vcv 
## matrix, so the $estimates component is a necessary addition to the statement

NonSage.pred<- data.frame(DSRbyNonSage$estimates[,5:8])

## now we can append the z-transformed values to this data frame

NonSage.pred$NonSage.z<- NonSage.values

## and calculate the back-transformed real values from the Z-standardized values
## where the standard deviation = 0.104026872 and mean = 0.16546597 for NonSage

NonSage.pred$NonSage<- NonSage.pred$NonSage.z*0.104026872+0.16546597

## now we can create a sexy plot showing the effect of non-sage cover on the 
## daily nest survival rate using ggplot

library(ggplot2)

Nonsage.plot<- ggplot(data=NonSage.pred, aes(x=NonSage, y=estimate))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill= "gray")+
  geom_line() +
  ylab ("Daily Nest Survival") +
  xlab ("Non-Sagebrush Shrub Cover")+ 
  theme(text= element_text(size=16)) 

Nonsage.plot

  

