#############################################################################################
######################################## Part 1 #############################################
###################################### First Steps ##########################################
#############################################################################################

## First we will import a data file using the read.csv() command.  You should find the RUGRCap
## file on Blackboard.  I've saved that file on a folder on my desktop called 'R'.  You will 
## need to modify the file pathway to make the command work based on where you've downloaded 
## and saved the file too.  Alternatively you can use the import data commant in the Global
## Environment window, BUT if you go this route be careful to name the file correctly as RUGR
## or the rest of the script will not execute. I recommend getting comfortable importing files
## via command line.

RUGR <- read.csv("C:/Users/Erik/Desktop/R/RUGRCap.csv")

## In general you should spend a lot of time exploring your data as you work with it. 
## The summary command is a useful general tool to evaluate your data once you 
## import it.  Look for issues like numeric data being read as factors or characters.

summary(RUGR)

## We will start with some simple data management proceedures.  First, I want to remove
## all of the recaptures from the dataset. This will allow me to work with all unique 
## individuals and the weights and measurements from the first time they were captured.
## I will use the subset() command to do this. Notice how I gave the new object a new 
## unique name.  I recommend always renaming objects as you go for consistency.

RUGR.N<- subset(RUGR, Recap=="N")

## Here are a few commands that are generally useful for looking at data
## We've already seen the summary() command, but the str() command can be equally useful
## as it gives you a description of the underlying structure of a data frame

str(RUGR.N)

## View will open the data frame in a new windown within R studio

View(RUGR.N)

## and utils:: in front of View will open a new data viewer window.  This is useful
## because it will show >1000 rows, whereas View() will truncate your data

utils::View(RUGR.N)

## the head command will return just the top rows of the data - super useful 
## for reminding yourself what data are in each column.  The number specifies 
## how many rows you want to see; default is 6 which is normally fine

head(RUGR.N)

head(RUGR.N, 10)

## We can delete elements of a data frame using subset() and by adding a '-' value in front 
## the object we want to delete.  For example, the bag mass is not needed in our data frame
## so we can delete it.

RUGR.N<- subset(RUGR.N, select=-(Bag))

head(RUGR.N)

## There are two ways we can interact with elements of the data frame. If we want to extract 
## only 1 column, we can create a new object and extract just that column

RUGR.Mass<- RUGR.N$Mass
summary(RUGR.Mass)

## above the '$' is what tells r that Mass is a column within the dataframe RUGR.N
## we can also use a [row,column] notation, where the left value gives the row position and 
## the right value gives the column position.  Leaving either the row value blank will  
## extract all rows within the column. So to extract an entire column:

RUGR.Site<- RUGR.N[,1]
summary(RUGR.Site)

## to extract a single row:

Obs.two<- RUGR.N[2,]
View(Obs.two)

## to extract a range of rows:

Obs.two<- RUGR.N[2:12,]
View(Obs.two)


## and finally to extract a single value:

RUGR.N[3,5]

## First you'll probably want to do some visualization of your data.  R has some convenient 
## base graphing features.  the histogram feature can be useful for visualizing distributions.
## breaks= tells us how many bins to include in the histogram.

hist(RUGR.N$Mass, breaks=20)

##You can use the plot() feature for scatter plots.  Lets look at 
## the relationship between mass and ordinal (Julian) date

plot(RUGR.N$Julian, RUGR.N$Mass)

## lets extract just the birds from the fall and try it again.

RUGR.NF<- subset(RUGR.N, Julian>200)

plot(x=RUGR.NF$Julian, y=RUGR.NF$Mass)

## Here are a few more basic features for working with data.  You may find occassion to summarize 
## values across different levels within your data.  E.g. what is the average temperature among 
## a series of sites that you've sampled.  The aggregate() feature is handy for this.  From the 
## capture data, lets compute the mean body mass of birds by site.  The operation we ask aggregate
## to perform is included in the FUN= portion of the statement, in this case the mean.
## the syntax of the model statement here is Value~Group for how the aggregation is performed.

mass.site<- aggregate(RUGR.NF$Mass~RUGR.NF$Site, FUN = sum, na.action = na.omit)

mass.site

## or if we wanted to look at differences between sites and years

mass.site.year<- aggregate(RUGR.NF$Mass~RUGR.NF$Site+RUGR.NF$Year, FUN = mean, na.action = na.omit)

mass.site.year

## if we create this as a data.frame(), the resulting object is a new data frame we can work with.
## for example:

mass.site.year<- data.frame(aggregate(RUGR.NF$Mass~RUGR.NF$Site+RUGR.NF$Year, FUN = mean, na.action = na.omit))

## use colnames() to give the new data frame its own column names.  This is a fairly important step
## as you will regularly use the column names to reference variables within the dataframe
## and R is not always good at naming them for you.

colnames(mass.site.year)<- c("Site", "Year", "Mass")
mass.site.year

## and plot changes in mass by year

plot(mass.site.year$Mass~mass.site.year$Year)


########################################################################
############################### Part 2 ################################# 
#################### GLM and Model Selection Basics ####################
########################################################################


## Here we will ask a more formal question about how ruffed grouse mass differs among age and  
## sex classes and across sites and years.  We will use this excercise as a tool to get 
## generally familar with using regressions in R and intrepreting their results, as we'll 
## see commonalities with most of the demographic analyses we complete.

## I happen to know there is temporal variation in the grouse mass data (date trend on mass) that
## will possibly obscure some of the mass relations, so first I want to statistically 
## remove the effect of date from the data.  This can be accomplished using a GLM and extracting
## residual terms from the model.  

## First I'm going to remove records with no mass recorded because they will impede our 
## ability to append the residuals

RUGR.NF<- subset(RUGR.NF, Mass>0)

## then we can fit the GLM.  We will create it as an object, mass.julian.glm. 
## Use the glm() function, specify the data, and then use the y~x notation
## to specify the model.  the family= command tells R which distribution to assume for the 
## response variable.  gaussian is r-speak for a normal distribution.

mass.julian.glm<-glm(data=RUGR.NF, Mass~Julian, family=gaussian) 

## the summary() command will give us the model results

summary(mass.julian.glm)

## because we've created our GLM as an object, we can extract the residuals and add them as a 
## new variable to our data frame

RUGR.NF$dateresid<- resid(mass.julian.glm)
head(RUGR.NF)

## and just to check, lets plot this new value against date

plot(RUGR.NF$Julian, RUGR.NF$dateresid)

## and for reference add a line to the graph with an intercept=0 and slope=0

abline(0,0)

## before we move forward I also want to remove individuals of unknown age and sex

RUGR.NF<- subset(RUGR.NF, Age!="U")
RUGR.NF<- subset(RUGR.NF, Sex!="U")


## Now, I want to ask whether age, sex, year, and study area affected mass, given that we've
## corrected the underlying date effect. This is where we'll incorporate multiple GLM models 
## and a model selection framework using AIC.  First I am going to run a series of models 
## that contain each of these effects.  Notice that a normal/gaussian response is the default
## option for glm(), so we can skip the family= command.  


age<- glm(data=RUGR.NF, dateresid~Age)
sex<- glm(data=RUGR.NF, dateresid~Sex)
year<- glm(data=RUGR.NF, dateresid~as.factor(Year))
site<-glm(data=RUGR.NF, dateresid~Site)

## and you might recall from WLE 411 we normally want to evaluate a 'null' model defined 
## only by the intercept.  In R we use the ~1 notation to specify that model

null<- glm(data=RUGR.NF, dateresid~1)

## now we've got a list of five models we can compare to see which is best supported by the 
## data, and which outcompete our null model which is the model selection equivalent to a 
## null hypothesis test.  We will use the package MuMIn as a tool for performing the model 
## selection.  You will probably have to 'turn on' the install.packages() command by deleting 
## the hashtag in front of that command.  Once you've installed a package once, I recommend 
## turning the code off again as it will slow down the speed of running your script, and is 
## not necessary.  You will likely need to run the library() command regularly to turn on the 
## package.  Both commands are needed as a starting point. 

#install.packages("MuMIn")
library(MuMIn)

## once installeed, use the model.sel() command to aggregate your model AIC scores into a single
## object. 

AIC.sel<-model.sel(age,sex,year,site,null)
AIC.sel

## this shows good support for effects of age and sex and less so for effects of 
## site and year.  Take a look at the summary statistics for each model

## I have reason to want to test the site effect along with age and sex. So
## lets run a few more models where we build slightly more complex sturctures.  Here is one
## with combined additive effects of age, sex and site

Age.Sex.Site<-glm(data=RUGR.NF, dateresid~Age + Sex + Site)

## and another with an interaction between age and Site

Age.Sex.Site.int<- glm(data=RUGR.NF, dateresid~Age + Sex + Site + Age*Site)

## and one with no site effect but additive effects of Age and Sex

Age.Sex<-glm(data=RUGR.NF, dateresid~Age + Sex)

## now lets update our AIC list

AIC.sel<-model.sel(age,sex,year,site,null, Age.Sex, Age.Sex.Site, Age.Sex.Site.int)
AIC.sel

## The interaction between site and age seems well-supported by the data based on AIC
## but you should always also look at the parameter coefficients

summary(Age.Sex.Site.int)

## lets create a more complex graphic showing these results using the ggplot package
## its likely I'll include ggplot2 code in a number of labs because that is what I am
## most comfortable using for graphing.  I've added a supplemental script to Blackboard 
## with a more formal tutorial for ggplot.  We won't cover this in class, but you are 
## welcome to work through it on your own. 

#install.packages("ggplot2")
library(ggplot2)

## to simplify things, I am going to create a new column that aggregates age and sex

RUGR.NF$AgeSex<- paste(RUGR.NF$Age,RUGR.NF$Sex)

## Now I will use a boxplot to show differences in mass among age and sex classes, by site

mass.box<- ggplot(RUGR.NF, aes(as.factor(AgeSex), color=as.factor(Site), dateresid)) +
  geom_boxplot() +
  ylab ("mass") +
  xlab ("Age")+ 
  theme_grey(base_size = 18)
mass.box


#######################################################################################
#################################### Part 3 ###########################################
################################ Data Management ######################################
#######################################################################################


## Traditionally one of the toughest challenges for a mark-recapture analysis was reltated 
## to data formatting; taking your capture data and converting it into an encounter history
## Using R programming for data management makes this once arduous task relatively simple.  
## We will illustrate this process using capture data from greater sage-grouse.  First read
## in the following file and create the dataframe object SAGR

SAGR<- read.csv("C:/Users/Erik/Desktop/R/SAGR.csv")
SAGR<- data.frame(SAGR)

## Take a look at the file to get a sense of what we are working with here.  These are capture
## data from Erik's dissertation research on greater sage-grouse.  The dataframe contains all 
## age and sex classes captured over an 8-year period.

head(SAGR)
summary(SAGR)

## This file contains all age and sex classes, as well as captures that were conducted during 
## both spring and fall.  I want to build an encounter history for only males captured on and 
## around leks in the spring.  So, first I am going to subset to only males and then for only
## spring captures that occured before day 136 (May 15th). 

SAGR.M<- subset(SAGR, Sex=="M")
SAGR.M.Spring<- subset(SAGR.M, Julian<136)

## and its always good practice to explore your files as you go and make modifications.  Here we see
## that we're going to be working with 1069 capture records, including 756 newly captured males that 
## were recaptured a total of 313 times. 

summary(SAGR.M.Spring)


## Now we will use the reshape2 package to create an encounter history for each bird during 
## each year of the study.

#install.packages(reshape2)
library(reshape2)

## First I am going to add a sequence of 1's that will serve as our 'detection' value in 
## the capture history

SAGR.M.Spring$det<- 1

## and then I will use the dcast() function to 'pivot' the long format of the capture data
## into a wide format based on year. There are a few optional features that I am using here
## to remove some steps in the process.  Firstly, the as.factor() command ensures that R 
## reads Year as a factor rather than numeric value.  fun.aggregate=mean says that if two 
## records exist for the same year, use the mean of those values.  Since these are all 
## 1s then 1 will always be the mean, but the default function is to return a count of the 
## duplicates so we don't want that.  fill=0 will replace any non-applicable cells (years 
## when birds were not caught) with 0s, which is what we want. 

SAGR.Hist<- dcast(SAGR.M.Spring, Color ~ as.factor(Year), fun.aggregate=mean, fill=0, value.var="det")

## Lets look at this new object in its entirety. At this point with your own data you should 
## spend a LARGE amount of time checking the input file against your raw data.  Double check 
## a number of individuals throughout the capture history to make sure that all of the histories
## are accurate and that nothing got lost in the shuffle.  

View(SAGR.Hist)

## So now we have the history but lost all of our ancillary information about the individual 
## birds.  It is easy enough to add that back in using the merge() function.  Lets add the 
## ancillary data back into the history to act as a grouping variable.  

SAGR.Hist.group<- merge(SAGR.Hist, SAGR.M.Spring, by="Color")

## and take a look at this

View(SAGR.Hist.group)

## you should see that we just re-expanded our dataframe back to 1069 observations.  This
## is because the capture history was duplicated for every observation in the original data 
## file in order to be compatible with the merge.  From here it is easy enough though to 
## remove the duplicated rows using the duplicated() function, but we need to think a bit 
## about the order of operations because some characteristics (like age) will change for 
## individuals through time.  In the case of age we want to retain the value at first capture
## because we can always incorporate age structure via the PIMS.  There are probably a number
## of ways to do this - but I simply sorted the data frame by color band and year, which should
## cause duplicated() to retain the first row (i.e. at capture).

SAGR.Hist.group<- SAGR.Hist.group[order(SAGR.Hist.group$Year),]

## Notice that the syntax to drop the duplicated values is the !duplicated() - without the 
## exclamation it would retain the only duplicate values rather than drop them. 

SAGR.Hist.group2<- SAGR.Hist.group[!duplicated(SAGR.Hist.group$Color),]

## If the above worked, you should find that the Year value in the ancillary data matches the 
## first year of observation in the history for each bird. 

View(SAGR.Hist.group2)

## Now that we have a full data frame with a wide format capture history, we can build an 
## input file data frame that can be read by RMark.  First, we need to take the history 
## columns and paste them together into a single column (i.e. the capture history).  RMark
## will want this to be labeled 'ch'

ch<- apply(SAGR.Hist.group2[,2:9], 1, paste, collapse ="")

## this can be converted to a data.frame, which I'll call SAGR.inp for convention with the 
## MARK input files, although in RMark the .inp extension is no longer needed.

SAGR.inp<- data.frame(ch)

## and now we can add back into the input data.frame any ancillary data (groups, covariates, 
## etc).  In this case because we didn't do any sorting or other data management once we 
## extracted the capture history, we can similarly pull the Age column and tack it onto the
## data frame.  You just want to be careful taking this approach to avoid any errors - when in
## doubt, you could always retain your ID variable and use the merge feature.

SAGR.inp$Age<- SAGR.Hist.group2$Age

## Now SAGR.inp would be ready to go into RMark, and we could analyze both time and group 
## effects.

View(SAGR.inp)

#######################################################################################
################################## Part 4 #############################################
############################ First Look at RMark ######################################
#######################################################################################

## We will finish this week with a brief teaser on running basic analyses using RMark, and
## revisiting the dipper data.  Next week we'll move into more advanced features of the 
## package, this week we will just run a few models and look at some results.

#install.packages(RMark)
library(RMark)

## in RMark the MARK data files are included in the base package and can be accessed with
## the data() command

data(dipper)

## here you can see the general format of the data file, which looks similar to the .inp
## file we just built above.  Notice that for RMark we have lost most of the more specific
## conventions.  In particular there is no need for semicolons, and you can include groups 
## as a character string (defined as a factor) rather than as a series of 1,0 columns, which
## is quite convenient. 

head(dipper)

summary(dipper)

## to run a model we will use the mark() function to create a new model object.
## This will run a mark analysis based on the simplest model structure - Phi(.) p(.)
## and it works because the CJS is the default model type for the mark() command.  
## We will dissect this command in much greater detail next week; for this week I just 
## want to run a simple model and illustrate how to work with it as an object
## note that the output=F component of the model will suppress it from printing the 
## model results every time it runs. 

Example<- mark(dipper, output=F)

## We can check the PIM structure of this model using the PIMS() command.  Here you will
## here you can see taht the PIMs are set to a constant model structure. 

PIMS(Example, parameter="Phi")

PIMS(Example, parameter="p")

## We can print the full model results using the summary command 

summary(Example)

## Or using the following

Example$results

## and if you wanted to report individual model componets, you can extract subsets of object
## for example, lets pull just the real estimates

Example$results$real

## and the betas

Example$results$beta

## we will stop here and next week will explore building and running multiple models and model sets 