library(RMark)

## here we will use a simulated data file that comes with the MARK/RMark documentation,
## and can be called directly from the RMark package

data(robust)

## take a look at the first few lines

head(robust)

## these data look mostly identical to a CJS history, and also notice that some records 
## relect histories for multiple individuals (freq=2). There are 15 occassions total, 
## which coincidentally we can count efficiently using the nchar() command 

nchar(robust$ch[1])

## the first thing we need to do is define the breakdown of primary and secondary 
## occassions, which in RMark we accomplish by defining which intervals among occassions
## are open, and which are closed.  In regular MARK, this option is specified based on the 
## 'easy robust design times' option in the analysis specifications stage of starting an 
## analysis.  For these data there were 5 primary occassions with a variable number of 
## secondary occassions for each primary that correspond with the following sequence,  
## where the "|" indicates the open period.  

 ## 1 1 | 2 2 | 3 3 3 3 | 4 4 4 4 4 | 5 5

## this order is signified by ones in the following time.intrevals object.
## where the ones identify intervals that are open (i.e. occur in between 
## primary occassions) and the 0s indicate intervals that are closed (i.e. 
## occur between secondary occassions within a primary.  
## Make sure you see the connection here:

time.intervals=c(0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0)

## once time intervals are defined we can processs the data

rd.proc<- process.data(data = robust,
                       model = "Robust",
                       time.intervals = time.intervals)

## for this case we are not manipulating the design data outside of the defaults
## so there is no need to create design data at this point and we can just start 
## running models.  As stated in the handout, the robust design in the full likelihood
## implementation has 5 distinct parameters, and as such is more complex than other 
## analyses we've conducted thus far. Review the information in the handout to remind 
## yourself what each of these parameters represent. 

## for this lab, we will simplify things slightly by making an assumption that there 
## is no behavioral response to capture, such that model structures for p and c are 
## always equal to each other.  Review the closed capture lab for methods to evaluate 
## behavioral effects - the process is the same for the robust design. Instead here we 
## will test for time effects on survival (phi), for both time effects and markovian 
## vs random process on temporary emigration, and for time effects on detection (p=c).

## Below is this approach achieved using a function with the mark.wrapper= feature. 
## Because we are running a modest number of models here, I elected to implement them 
## all in a single function.  If you were doing a more complex analysis, for example 
## that included group and covariate structures, you might elect to conduct the analysis
## in a more systematic approach, such as what we implemented with the multi-state. 

S.dot=list(formula=~1)
S.time=list(formula=~time)

GammaDoublePrime.random.time=list(formula=~time, share=TRUE)
GammaDoublePrime.random.dot=list(formula=~1, share=TRUE)

GammaDoublePrime.Markov.time=list(formula=~time, share=FALSE)
GammaDoublePrime.Markov.dot=list(formula=~1, share=FALSE)

GammaPrime.Markov.time=list(formula=~time, share=FALSE)
GammaPrime.Markov.dot=list(formula=~1, share=FALSE)

p.time = list(formula=~time, share=TRUE)
p.session.time = list(formula=~time + session, share=TRUE)
p.dot = list(formula=~1, share=TRUE)


mod1.markov<- mark(data=rd.proc, model="Robust", model.parameters=list(
                                            S = S.dot,
                                            p = p.session.time,
                                            GammaDoublePrime = GammaDoublePrime.Markov.time,
                                            GammaPrime = GammaPrime.Markov.time),
                                            output=FALSE)

mod1.markov$results$beta
mod1.markov$results$real


## lets take a second to explore the real estimates from this model and get a sense for
## what these gammas are telling us.  First I will extract rows 2 thorugh 8 from the results
## which contain the Gamma's

Gamma.mod1<- mod1.markov$results$real[2:8,]

## and then add a column for the time intervals/occassions that these values correspond with
## note that the gamma primes are offset by one interval, for reasons that are explained
## in the handout. 

Gamma.mod1$Interval<- c(2, 3, 4, 5, 3, 4, 5)

## and add a column for Gamma" vs Gamma'

Gamma.mod1$Parameter<- c(rep("Gamma.double", 4), rep("Gamma.single", 3))

## now we can graph and visualize the difference in the probability of single-year (Gamma")
## vs. multi-year (Gamma') absence


library(ggplot2)

gamma.fig<- ggplot(data=Gamma.mod1, aes(x=as.factor(Interval), y=estimate, color=Parameter))+
  geom_errorbar(aes(ymax=(estimate+se),ymin=(estimate-se)), width = 0.3, position=position_dodge(width=0.3))+
  geom_point(position=position_dodge(width=0.3), size=2.5)+
  geom_line(position=position_dodge(width=0.3), size=0.5) +
  ylab ("Probability of Absence") +
  xlab ("Occassion No")+ 
  theme(text= element_text(size=16)) 

gamma.fig  # don't worry about the error message being thrown here


## now lets run a second competing model with identical structure, but where temporary emigration
## is assumed to be random in nature.  To do this we eliminate the GammaPrime= from the model.parameters()
## list and replace the model structure with the GammaDoublePrime.ranom.time list.  This will 
## fit a model where temporary emigration is allowed to vary through time, but where the probability
## of single- vs multi-year absence is identical. 

mod2.random <- mark(data=rd.proc, model="Robust", model.parameters=list(
                                            S = S.dot,
                                            p = p.session.time,
                                            GammaDoublePrime = GammaDoublePrime.random.time),
                                            output=FALSE)

mod2.random$results$beta
mod2.random$results$real

## now notice we get only a single series of gamma estiamtes, and the probability of absence 
## is assumed to follow a random structure. First lets compare the AIC of this model with 
## that of the Markovian structure

mod1.markov$results$AICc
mod1.markov$results$AICc

delta.mod1<- mod2.random$results$AICc - mod1.markov$results$AICc
delta.mod1

## the random temporary emigration structure is ~14 AICc units better supported than markovian
## with only k=3 parameters contributing to ~6AICc units of that difference (i.e. 2K = 2*3)
## note that we could have used collect.models() to create a complete AIC table for these 2 
## models, but that seemed unesseccary to me at this stage. Lets add the estimates from this 
## model to our figure for comparison purposes

Gamma.random<- mod2.random$results$real[2:5,]
Gamma.random$Interval<- c(2, 3, 4, 5)

gamma.fig + geom_point(data=Gamma.random, aes(x=as.factor(Interval), y=estimate), color="Black", size=2.5)+
  geom_errorbar(data=Gamma.random, aes(x=as.factor(Interval), ymax=(estimate+se),ymin=(estimate-se)),color="Black", width = 0.1)



