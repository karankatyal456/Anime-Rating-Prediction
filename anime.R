# Step-1
# Identify the Problem Statement, What are you trying to solve?

##"Predict the rating of (movie/web series/etc.) based on its characteristics"

# Step-2
# Identify the Target variable, What value will be predicted?
##"Rating"

# Step-3
#Loading the raw Data
InputData=read.csv(choose.files(),na.strings=c("","","NA","null"))
View(InputData)

#out of the 16 columns, which one is the target variable?
#Rating column

##start looking at the data from different aspects

##Now lets understand out of 15 columns which are useful and which aren't.

# Step-4
# Exploring the dataset
str(InputData)
head(InputData,10)

###Title:
## continuous variable
#names of anime releases,all are different name.
#Just the name of a anime doesn't drive the rating of it

#it is not helpful from the business perspective


###Media type
##categorical variable
#it helps to understand in which media type anime is popular
#so media type can affect the popularity of anime.
#so we dont reject this column in the first phase of filteration.

###episodes
##continuous variable
#it tells the number of episodes the anime have
#it can help to predict the rating.
#so  dont reject this column in the first phase of filteration.

### duration
##continuous variable
#it tells about duration.
#if the duration is less many people get interest to watch,so it can help to predict the rating
#so  dont reject this column in the first phase of filteration.

###ongoing
##categorical variable
#consists of yes/no values
table(InputData$ongoing)
#no-6942,yes-87
#it has very less value of yes compare to no
#so it doesn't help in the prediction of rating

### sz of release
##categorical variable
#consits of:
            #Fall,spring,summer,winter
table(InputData$sznOfRelease)
#fall-613,spring-645,summer-424,winter-431
#it has too many missing value,so it cant help in the prediction of rating
 

###description
##continuous variable
# it tells about the synoposis of plot,all the descriptions are different
#it doesn't help in the prediction of rating
#so reject this column.

###studios
##continuous variable
#see how many unique variables are there
length(InputData$studios)
#7029
#all are different studios,it doesnt derive in the prediction of rating



###tags
##continuous variable
#it is  representing the tags,genre i.e category of artistic,music etc.of the  anime
#it can't help to predict the rating of it


###content warn
##continuous variable
# it is telling about the contents of the anime,
#it doesn't derive to predict the rating.

#they are not helpful from buisness prespective.


###watched
##continuous variable
#it tells the number of people who completed watching it.
#it would help in prediction of rating.


###watching
##continuous variable
#it tells the number of people are still watching the shows.
#it would help in prediction of rating.


###want watch
##continuous variable
#it tells the number of people want to watch the shows.
#it would help in prediction of rating.


###dropped
##continuous variable
#it tells the number of people have dropped before the completion of shows..
#it would help in prediction of rating.


###votes
##continuous variable
#it would help in prediction of rating.


# Removing useless columns
InputData[, c("title", "sznOfRelease", "ongoing", "description",
              "studios","tags","contentWarn")] = NULL
head(InputData,10)

# Step-5
# Whether it is a Regression problem or Classification?
#target variable-rating
#continuous-regression


# Step-6
# Checking and treating missing values

# Checking missing values
colSums(is.na(InputData))


#### Imputing missing values for CATEGORICAL columns

FunctionMode=function(inpData){
  ModeValue=names(table(inpData)[table(inpData)==max(table(inpData))])
  return(ModeValue)
}

# Calling the mode function to get the mode value
FunctionMode(InputData$mediaType)


# Imputing missing values for Categorical columns
InputData$mediaType[is.na(InputData$mediaType)] = "TV"

# Checking missing values after imputation
colSums(is.na(InputData))


#### Imputing missing values for ContinuousL columns
InputData$watched[is.na(InputData$watched)] = round(median(InputData$watched,na.rm=TRUE),digits=0)
InputData$duration[is.na(InputData$duration)] =median(InputData$duration,na.rm=TRUE)


# Checking missing values after imputation
colSums(is.na(InputData))


###treating the outliers###
############see for which of the columns you have to treat the outliers#####


###eps
boxplot(InputData$eps,horizontal=T)
#here, there are some very high values in the end which are outliers
##see what is last value in the data, which you won't consider as the outlier

quantiles=quantile(InputData$eps,c(0.99,0.995,0.997,0.999,0.9993,0.9996))
quantiles

#check which quantile it approx corresponds to
#and all the values above that will be replaced
quantiles_final=quantile(InputData$eps,0.999)
quantiles_final
max(InputData$eps)

InputData$eps = ifelse(InputData$eps > quantiles_final , quantiles_final, InputData$eps)
#check the boxplot again and see whether outliers are removed or not.
boxplot(InputData$eps, horizontal = T)


###duration
boxplot(InputData$duration,horizontal=T)
#here, there are some very high values in the end which are outliers
##see what is last value in the data, which you won't consider as the outlier

quantiles=quantile(InputData$duration,c(0.99,0.997,0.999,0.9993,0.9995,0.9997,0.9999))
quantiles

#check which quantile it approx corresponds to
#and all the values above that will be replaced
quantiles_final=quantile(InputData$duration,0.9995)
quantiles_final
max(InputData$duration)

InputData$duration = ifelse(InputData$duration > quantiles_final , quantiles_final, InputData$duration)
#check the boxplot again and see whether outliers are removed or not.
boxplot(InputData$duration, horizontal = T)


###watched
boxplot(InputData$watched,horizontal=T)
#here, there are some very high values in the end which are outliers
##see what is last value in the data, which you won't consider as the outlier

quantiles=quantile(InputData$watched,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995))
quantiles

#check which quantile it approx corresponds to
#and all the values above that will be replaced
quantiles_final=quantile(InputData$watched,0.963)
quantiles_final
max(InputData$watched)

InputData$watched = ifelse(InputData$watched > quantiles_final , quantiles_final, InputData$watched)
#check the boxplot again and see whether outliers are removed or not.
boxplot(InputData$watched, horizontal = T)

###watching
boxplot(InputData$watching,horizontal=T)
#here, there are some very high values in the end which are outliers
##see what is last value in the data, which you won't consider as the outlier

quantiles=quantile(InputData$watching,c(0.95,0.96,0.963,0.965,0.97,0.98,0.99,0.995))
quantiles

#check which quantile it approx corresponds to
#and all the values above that will be replaced
quantiles_final=quantile(InputData$watching,0.963)
quantiles_final
max(InputData$watching)

InputData$watching = ifelse(InputData$watching > quantiles_final , quantiles_final, InputData$watching)
#check the boxplot again and see whether outliers are removed or not.
boxplot(InputData$watching, horizontal = T)


###wantwatch
boxplot(InputData$wantWatch,horizontal=T)
#here, there are some very high values in the end which are outliers
##see what is last value in the data, which you won't consider as the outlier

quantiles=quantile(InputData$wantWatch,c(0.99,0.995,0.997,0.999,0.9993,0.9995,0.9997,0.9999))
quantiles

#check which quantile it approx corresponds to
#and all the values above that will be replaced
quantiles_final=quantile(InputData$wantWatch,0.9997)
quantiles_final
max(InputData$wantWatch)

InputData$wantWatch = ifelse(InputData$wantWatch > quantiles_final , quantiles_final, InputData$wantWatch)
#check the boxplot again and see whether outliers are removed or not.
boxplot(InputData$wantWatch, horizontal = T)


###dropped
boxplot(InputData$dropped,horizontal=T)
#here, there are some very high values in the end which are outliers
##see what is last value in the data, which you won't consider as the outlier

quantiles=quantile(InputData$dropped,c(0.995,0.997,0.999,0.9993,0.9995,0.9997,0.9999))
quantiles

#check which quantile it approx corresponds to
#and all the values above that will be replaced
quantiles_final=quantile(InputData$dropped,0.997)
quantiles_final
max(InputData$dropped)

InputData$dropped = ifelse(InputData$dropped > quantiles_final , quantiles_final, InputData$dropped)
#check the boxplot again and see whether outliers are removed or not.
boxplot(InputData$dropped, horizontal = T)

###rating
boxplot(InputData$rating,horizontal=T)
##it doesnt have any higher value.
#it doesnt need to treat outliers.

###votes
boxplot(InputData$votes,horizontal=T)
#here, there are some very high values in the end which are outliers
##see what is last value in the data, which you won't consider as the outlier

quantiles=quantile(InputData$votes,c(0.93,0.935,0.94,0.943,0.945,0.947,0.95,0.953))
quantiles

#check which quantile it approx corresponds to
#and all the values above that will be replaced
quantiles_final=quantile(InputData$votes,0.94)
quantiles_final
max(InputData$votes)

InputData$votes = ifelse(InputData$votes > quantiles_final , quantiles_final, InputData$votes)
#check the boxplot again and see whether outliers are removed or not.
boxplot(InputData$votes, horizontal = T)


# Step-7
# Explore each "Potential" predictor for distribution and Quality
##Univariate analysis


##continuous column- histogram
##categorical column- bar plot


# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("eps","duration","watched","watching","wantWatch","dropped", "votes","rating")

#Splitting the plot window
par(mfrow=c(2,4))

# library to generate professional colors
library(RColorBrewer)

# looping to create the histograms for each column
for (contCol in ColsForHist){
  hist(InputData[,c(contCol)], main=paste('Histogram of:', contCol), 
       col=brewer.pal(8,"Paired"))
}


############################################################
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("mediaType")
par(mfrow=c(1,1))


# looping to create the Bar-Plots for each column
for (catCol in ColsForBar){
  barplot(table(InputData[,c(catCol)]), main=paste('Barplot of:', catCol), 
          col=brewer.pal(8,"Paired"))
}

# Step-8

##bivariate analysis

# Visual Relationship between predictors and target variable
##Regression- 2 scenarios
# Continuous Vs Continuous ---- Scatter Plot
# Continuous Vs Categorical --- Box Plot

# Continuous Vs Continuous --- Scatter plot

# For multiple columns at once
ContinuousCols = c("rating","eps","duration","watched","watching","wantWatch","dropped", "votes")
par(mfrow=c(1,1))
plot(InputData[, ContinuousCols], col='blue')

# Continuous Vs Categorical Visual analysis: Boxplot

CategoricalCols = c("mediaType")
library(RColorBrewer) 

for (bar_cols in CategoricalCols){
  boxplot(rating~ (InputData[,c(bar_cols)]), data = InputData, 
          main=paste('Box plot of:',bar_cols),col=brewer.pal(8,"Paired"))
  
}



# Step-9
# Strength of Relationship between predictor and target variable
# Continuous Vs Continuous ---- Correlation test
# Continuous Vs Categorical---- ANOVA test

# Continuous Vs Continuous : Correlation analysis
# Correlation for multiple columns at once

ContinuousCols = c("rating","eps","duration","watched","watching","wantWatch","dropped", "votes_new")
cor(InputData[, ContinuousCols], use = "complete.obs")
CorrData=cor(InputData[, ContinuousCols], use = "complete.obs")
CorrData

# Final columns which has high correlation with the target variable
names(CorrData[,'rating'][abs(CorrData[,'rating'])>0.4])

# Selecting below continuous columns for Model
#watched- good variable
#wantWatch-good variable
#watching-good variable
#votes- good variable


# Continuous Vs Categorical correlation strength: ANOVA
# Analysis of Variance(ANOVA)

# H0: Variables are NOT correlated
# Small P-Value--> Variables are correlated(H0 is rejected)
# Large P-Value--> Variables are NOT correlated (H0 is accepted)

# as we have only one column so no need to use for loop .

summary(aov(rating ~ InputData$mediaType, data = InputData))


# Selecting below columns based on ANOVA results(p value < 0.05)
#"mediaType"

#########################################################################
#########################################################################

# Step-10
# Generating the Data frame for machine learning
TargetVariableName=c('rating')

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis
BestPredictorName= c("mediaType","watched","watching","wantWatch","votes")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable

PredictorVariable=InputData[, BestPredictorName]
str(PredictorVariable)

DataForML=data.frame(TargetVariable,PredictorVariable)
head(DataForML)

#########################################################################

# Step-11

# Sampling | Splitting data into 70% for training 30% for testing
set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)
head(DataForMLTrain)
head(DataForMLTest)

# Creating Predictive models on training data to check the accuracy of each algorithm
###### Linear Regression #######

Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)

#multiple r-squared-0.441,adjusted r-sq-0.4398
#the fitness of model is not good.


# Checking Accuracy of model on Testing data
DataForMLTest$Pred_LM=predict(Model_Reg, DataForMLTest)
head(DataForMLTest)

# Calculating the Absolute Percentage Error for each prediction
DataForMLTest$LM_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

####test for multicollinearity#####

Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)
library(car)
VIF=vif(Model_Reg)
data.frame(VIF)

#the fitness of model is not good as here  value of r square is very low.


########################################################################
# Additional Data Transformations

#might reduce skewness and increase the correlation with the target variable
# logarithmic is the most common one
hist(InputData$votes)
cor(x=InputData$votes , y=InputData$rating)
min(InputData$votes)
#Treating the Zeros in the Columns

log(0)
#Votes- the number of people voted
#apart from 0, minimum no. of people voted =1 
InputData$votes[InputData$votes==0]=1
# Log Transformation
hist(log(InputData$votes))
cor(x=log(InputData$votes) , y=InputData$rating)

#so  create a new column votes_new inside inputdata and use it for the model
#instead of votes
InputData$votes_new=log(InputData$votes)
head(InputData,10)

ColsForHist=c("eps","duration","watched","watching","wantWatch","dropped", "votes_new","rating")

#Splitting the plot window
par(mfrow=c(2,4))

# library to generate professional colors
library(RColorBrewer)

# looping to create the histograms for each column
for (contCol in ColsForHist){
  hist(InputData[,c(contCol)], main=paste('Histogram of:', contCol), 
       col=brewer.pal(8,"Paired"))
}

ContinuousCols = c("rating","watched","watching","wantWatch","votes_new")
par(mfrow=c(1,1))
plot(InputData[, ContinuousCols], col='blue')

#run the whole model and use votes_new instead of votes.

TargetVariableName=c('rating')

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis
BestPredictorName= c("mediaType","watched","watching","wantWatch","votes_new")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)

# Selecting all other columns as Predictors apart from target variable

PredictorVariable=InputData[, BestPredictorName]
str(PredictorVariable)

DataForML=data.frame(TargetVariable,PredictorVariable)
head(DataForML)

set.seed(123)
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)
head(DataForMLTrain)
head(DataForMLTest)

Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)

Model_Reg_1=lm(TargetVariable~watched+watching+wantWatch+votes_new+
               I(mediaType=="Movie")+I(mediaType=="Other")+I(mediaType=="OVA")+I(mediaType=="TV")
             +I(mediaType=="TV Special")+I(mediaType=="Web"),data=DataForMLTrain)
summary(Model_Reg_1)



#coefficient estimates
#for continuous variables:
#postive estimate suggests positive relationship with rating
#negative estimate suggests positive relationship with rating
#wantwatch-if wantwatch increases by 1 units then rating would increases by 5 units
# watched- negative denotes fall (negative relatioship between watched and rating.)

#for categorical variable
#mediatypemovie: coefficient is positive
#reference category is DVD Special
#it says that it will give more rating on the movie platform than DVD Special platform
#mediatypeother:coefficient is negative
#it says that it will give less rating on the other platform than DVD special platform.

#similarly we can say for others variable by seeing the coefficient estimate.



# Checking Accuracy of model on Testing data
DataForMLTest$Pred_LM=predict(Model_Reg_1, DataForMLTest)
head(DataForMLTest)

# Calculating the Absolute Percentage Error for each prediction
DataForMLTest$LM_APE= 100 *(abs(DataForMLTest$TargetVariable-DataForMLTest$Pred_LM)/DataForMLTest$TargetVariable)
head(DataForMLTest)

MeanAPE=mean(DataForMLTest$LM_APE)
MedianAPE=median(DataForMLTest$LM_APE)

print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - MeanAPE))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - MedianAPE))

"### Mean Accuracy of Linear Regression Model is:  80.3654391740629"
"### Median Accuracy of Linear Regression Model is:  87.441303065507"

####test for multicollinearity#####

Model_Reg_1==lm(TargetVariable~watched+watching+wantWatch+votes_new+
                  I(mediaType=="Movie")+I(mediaType=="Other")+I(mediaType=="OVA")+I(mediaType=="TV")
                +I(mediaType=="TV Special")+I(mediaType=="Web"),data=DataForMLTrain)
summary(Model_Reg_1)
library(car)
VIF=vif(Model_Reg_1)
data.frame(VIF)

Model_Reg_1=lm(TargetVariable~watched+watching+wantWatch+votes_new+
                  I(mediaType=="Movie")+I(mediaType=="Other")+I(mediaType=="OVA")+I(mediaType=="TV")
                +I(mediaType=="TV Special")+I(mediaType=="Web"),data=DataForMLTrain)
summary(Model_Reg_1)

#multiple R-squared- 0.5657,Adjusted R-squared- 0.5648(lies between 0.5 to 1)
# by transforming the variable and here it is seen that the fitness of model is good.

########################################################################
########################################################################
########################################################################
###Business Recommendation####

#the anime should release more on the platform movie,TV special,in this platform they can get more ratings.
#they should avoid to release on the platform like web,ova,as all are not so much familiar with the technologies.
#specially the old people cant see the anime in web,ova.
#most of the people preferred to movie,tv special.













