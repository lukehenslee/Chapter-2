
## First read in the data file.  For whatever reason, R wants to convert the capture
## history to a number, which eliminates leading 0s and is incredibly annoying.  The
## colCloasses="character" statement fixes this, and then the following statements 
## convert the Group and frequency columns back to appropriate format.

ch.df.f<- read.csv("data/Lab7file.csv", colClasses = "character")
ch.df.f$Group<- as.factor(ch.df.f$Group)
ch.df.f$freq<- as.numeric(ch.df.f$freq)


## load RMark
#install.packages("RMark")
library(RMark)

## process our mark data

cc.process<- process.data(ch.df.f, begin.time = 1, model="Closed", groups="Group")

## make the design data

cc.ddl<- make.design.data(cc.process)

## Lets run a simple model based on all of RMark's defaults. Viewing the model 
## output with allow you to intrepret the specifics of the structure of the model. 

mod1<- mark(cc.process,cc.ddl, model="Closed")

## Note that in this case the model is estimating 3 parameters - p, c, and f0
## f0 is an estimate of the number of unmarked animals in the population.  The 
## total population size estimate for each group, as a function of the model, 
## is derived as the sum of the estiamted unmarked population size and the 
## known number of marked animals.  We can have RMark produce this derivation 
## for us fairly easily by asking for the derived parameters

mod1$results$derived

## specify parameter structures for each parameter type

## this first we will use for models where p and c are considered equivelent; 
## i.e. where ther is no behavior effect.  This is what the share=TRUE statement
## is accomplishing; it tells RMark that p and c should share a common structure.

dot=list(formula=~1, share=TRUE)
time=list(formula=~time, share=TRUE)
group=list(formula=~Group, share=TRUE)

## and now we can do the same, but using share=FALSE so that p and c are modeled
## differently within each model.  Note that now you will need to specify both 
## the p and c structure in the mark() statement, whereas when assuming p=c you 
## only need specify the p structure

pc.dot=list(formula=~1, share=FALSE)
pc.time=list(formula=~time, share=FALSE)
pc.group=list(formula=~Group, share=FALSE)

## Lastly we need to define the group structure on N.  Because here we are interested
## in abundance estimates for each study area, I am going to use a Group structure 
## in all models.

N.group<- list(formula=~Group)

## now we can run models.  I am going to label them individually and correspond 
## the lableing to the model notation by Otis et al. (1976), contained in the 
## handout on page 2. I will also use "g" to incorporate group structure.  

M0<- mark(cc.process,cc.ddl, model="Closed", model.parameters=list(p=dot, f0=N.group), output=FALSE)
Mt<- mark(cc.process,cc.ddl, model="Closed", model.parameters=list(p=time, f0=N.group), output=FALSE)
Mb<- mark(cc.process,cc.ddl, model="Closed", model.parameters=list(p=pc.dot, c=pc.dot, f0=N.group), output=FALSE)
Mbt<- mark(cc.process,cc.ddl, model="Closed", model.parameters=list(p=pc.time, c=pc.time, f0=N.group), output=FALSE)
Mg<- mark(cc.process,cc.ddl, model="Closed", model.parameters=list(p=group, f0=N.group), output=FALSE)
Mbg<- mark(cc.process,cc.ddl, model="Closed", model.parameters=list(p=pc.group, c=pc.group, f0=N.group), output=FALSE)

## Note that the output=FALSE command suppresses the full results output for each model.
## this helps to tidy up your workspace but don't use it as an excuse to be complacent with 
## careful checking of model results. 

## before we look at results, I want to remove the test model I ran earlier so that my 
## AIC table is based only on the above 

remove(mod1)

## Compile the results from the models that have been run
CC.results=collect.models()

## View the AIC table of results.  When viewing the results, those models that lack 
## a behavior effect will be listed with a c() notation, indicating no structure for
## the recapture parameter.  Those with some specification for c (e.g. c(~Group)) are
## models with a behavior effect

CC.results

## you should have noticed that the Mbt model had convergence issues

Mbt$results$real
Mbt$results$beta

## these results illustrate that there are convergence issues on the last detection parameter, 
## and they are carrying over to the f0s too.  The best advice I have here is that one should
## not draw inference from models in which parameters are having convergence issues, so, I recommend 
## deleting the Mbt model from your candidate set before proceeding. 

remove(Mbt)

## Compile the results from the models that have been run
CC.results.pruned=collect.models()

## View the AIC table of results
CC.results.pruned

## Now we see we have three competative model structures - one for the Mgb model, 
## one for the Mb model, and a third for the Mt model.  We should take a look at 
## the real and beta estimates for each of these

Mbg$results$real
Mbg$results$beta

## Notice for the real estimates, there are different detection probabilities (p and c)
## for each of the three groups.  Don't confuse these with the three time intervals - 
## remember when in doubt always check the model PIM structure

PIMS(Mbg, parameter = "p")

## repeat for the other two competative models

Mb$results$real
Mb$results$beta

Mt$results$real
Mt$results$beta

## Notice there are some fairly big differences in these results and their real 
## parameter estimates, illustrating that how we incorporate variability (or not)
## in detection can have a pretty big affect on estimates.  Lets visualise differences 
## in the derived abundance parameters to illustrate this point.  First extract N-hat 
## as an object from each model and add a model name

Mbg.nhat<- data.frame(Mbg$results$derived)
Mbg.nhat$model<- "Mbg"

Mb.nhat<- data.frame(Mb$results$derived)
Mb.nhat$model<- "Mg"

Mt.nhat<- data.frame(Mt$results$derived)
Mt.nhat$model<- "Mt"

## now group them together using rbind()

Nhat<- data.frame(rbind(Mbg.nhat, Mb.nhat, Mt.nhat))
colnames(Nhat)<- c("Estimate", "SE", "lcl", "ucl", "model")

## and add a group number label

Nhat$Group<- rep(c("A", "B", "C"), times=3, lenght.out=nrow(Nhat))

Nhat

## Now we can plot the estimates using ggplot

library(ggplot2)

Nhat.graph<- ggplot(data=Nhat, aes(x=Group, y=Estimate, group=model, color=model))+
            geom_errorbar(aes(ymax=ucl,ymin=lcl), width = 0.3, position=position_dodge(width=0.3))+
            geom_point(position=position_dodge(width=0.3))
  
Nhat.graph

## here we see there is quite a bit more agreement with the Mbg and Mg models, 
## while the Mt model is giving us very different results.  This is precisely
## a place where model-averaging would be our friend if we were most interested 
## in producing robust estimates of N.  

ma<- model.average(CC.results.pruned)
ma

## the last three parameters are our estimate of the unmarked population, where 
## as stated in the handout N-hat is simply the unmarked population estimate plus 
## the known number of marked animals.  For this reason we can simply add the 
## number of marked animals, by group, to these model-averaged values.  First, 
## extract just the f0s 

f0.ma<- data.frame(ma[16:18,])
f0.ma

## now we need a count of marked individuals.  This is just the count of each 
## for each group in the original data frame.  We can extract this using the 
## aggregate() command

count<- aggregate(ID ~ Group, data = ch.df.f, FUN = function(x){NROW(x)})

## and the model-averaged estimates of N-hat are just those values added to 
## f0.  because we are adding a constant to each estimate, the SE will remain 
## the same as the model-averaged estimate

f0.ma$Nhat<- f0.ma$estimate+count$ID 

## and add this back to our graph - if this works correctly we should get estimates
## that are intermediate relative to our originals.  Needs a bit of somewhat clunky
## code to start that appends these values to our earlier Nhat data frame.  This could 
## probably all be accomplished more eloquently, but it gets the job done.

Nhat[10:12,1]<-f0.ma$Nhat
Nhat[10:12,2]<-f0.ma$se
Nhat[10:12,3]<-f0.ma$Nhat+f0.ma$se*1.96
Nhat[10:12,4]<-f0.ma$Nhat-f0.ma$se*1.96 
Nhat[10:12,5]<-"ModAvg"
Nhat[10:12,6]<-c("A", "B", "C")

Nhat.graph<- ggplot(data=Nhat, aes(x=Group, y=Estimate, group=model, color=model))+
  geom_errorbar(aes(ymax=ucl,ymin=lcl), width = 0.3, position=position_dodge(width=0.3))+
  geom_point(position=position_dodge(width=0.3))
  
Nhat.graph




