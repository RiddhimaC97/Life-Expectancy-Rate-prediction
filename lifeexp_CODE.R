LifeExpectancyData=read.csv('C:/Users/RIDDHIMA/Downloads/Life Expectancy Data.csv')
UniqueFunc=function(inpColumn){
           return(length(unique(inpColumn)))
}
sapply(LifeExpectancyData, UniqueFunc)
summary(LifeExpectancyData)
dim(LifeExpectancyData)


colSums=(is.na(LifeExpectancyData))
colSums

table(LifeExpectancyData$Country)
mode(LifeExpectancyData$Country)

LifeExpectancyData[c("Country")] <- list(NULL)
LifeExpectancyData$Status=factor(LifeExpectancyData$Status)
str(LifeExpectancyData)
ColsForHist=c('Adult_Mortality','Infant_Deaths','BMI','Under.five_Deaths','HIV.AIDS','Per_Capita_GDP',' Income_Composition_of_Resources
','Schooling','Year')
par(mfrow=c(3,2))
library(RColorBrewer) 
for (contCol in ColsForHist){
	hist(LifeExpectancyData[,c(contCol)], main=paste('Histogram of:', contCol), 
	col=brewer.pal(8,"Paired"))
}
plot(x=LifeExpectancyData$Life_Expectancy, y=LifeExpectancyData$Alcohol, col='blue')
is.na(LifeExpectancyData)
LifeExpectancyData[!complete.cases(LifeExpectancyData),]
LifeExpectancydta=na.omit(LifeExpectancyData)
LifeExpectancydta[!complete.cases(LifeExpectancydta),]

#################################

ContinuousCols = c("Year","Total_Expenditure","Adult_Mortality","Infant_Deaths","BMI","Under.five_Deaths","HIV.AIDS",
"Per_Capita_GDP","Income_Composition_of_Resources",
"Schooling")
CorrData=cor(LifeExpectancydta[, ContinuousCols])

###################################
# H0=Average Life Expectancy is 60
t.test(x=LifeExpectancydta$Life_Expectancy, mu=60, alternative=c("two.sided"), conf.level=0.95)
#H0 is rejected
###########################################
boxplot(Life_Expectancy ~ Status, data =LifeExpectancydta , col=brewer.pal(8,"Paired"))


TargetVariableName=c('Life_Expectancy')
BestPredictorName=names(LifeExpectancydta[, !names(LifeExpectancydta) %in% TargetVariableName])
TargetVariable=LifeExpectancydta[, c(TargetVariableName)]
str(TargetVariable)
PredictorVariable=LifeExpectancydta[, BestPredictorName]
str(PredictorVariable)
DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)
startTime=Sys.time()
Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
endTime=Sys.time()
endTime-startTime


install.packages("lmtest")
library(lmtest)
dwtest(Model_Reg)
bptest(Model_Reg)
#################
summary(Model_Reg)

install.packages("ggplot2")
library(ggplot2)
plot(Model_Reg)


DataForMLTest$Pred_LM=predict(Model_Reg, DataForMLTest)
head(DataForMLTest)


###################

LM_APE= 100 *(abs(DataForMLTest$Pred_LM - DataForMLTest$TargetVariable)/DataForMLTest$TargetVariable)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - mean(LM_APE)))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - median(LM_APE)))
######################
plot(x=LifeExpectancydta$Life_Expectancy, y=LifeExpectancydta$Adult_Mortality, col='blue')
str(LifeExpectancydta)

install.packages("Random Forest")


library(party)
startTime=Sys.time()
Model_CTREE=ctree(TargetVariable ~ . , data=DataForMLTrain)
plot(Model_CTREE)
endTime=Sys.time()
endTime-startTime

# Checking Accuracy of model on Testing data
DataForMLTest$Pred_CTREE=as.numeric(predict(Model_CTREE, DataForMLTest))
head(DataForMLTest)

# Calculating the Absolute Percentage Error for each prediction
CTREE_APE= 100 *(abs(DataForMLTest$Pred_CTREE - DataForMLTest$TargetVariable)/DataForMLTest$TargetVariable)
print(paste('### Mean Accuracy of ctree Model is: ', 100 - mean(CTREE_APE)))
print(paste('### Median Accuracy of ctree  Model is: ', 100 - median(CTREE_APE)))





