rm(list=ls())

#load the dataset

importrushi<-read.csv(file="F://kDD/Claim.csv",header=TRUE)

newrushi<-as.data.frame(importrushi)


#create a sample set of 2000 rows at random
?sample()
index<-sample(1:nrow(importrushi),17000)
rushi<-as.data.frame(importrushi[index,])


#drop column 1, 2 (hosp name & provider id)
newrushi<-as.data.frame(importrushi[index,3:11])

#recode Period with 100 to 400 as 4 levels are there
newrushi[,2] <- ifelse(newrushi[,2] == "1 through 30 days After Discharge from Index Hospital Admission", 100, ifelse(newrushi[,2] == "1 to 3 days Prior to Index Hospital Admission", 200, ifelse(newrushi[,2] == "During Index Hospital Admission", 300, ifelse(newrushi[,2] == "Complete Episode", 400, 000))))

#recode claim type with 1 to 8 as 8 levels are there
newrushi[,3] <- ifelse(newrushi[,3] == "Carrier", 1, ifelse(newrushi[,3] == "Durable Medical Equipment", 2, ifelse(newrushi[,3] == "Home Health Agency", 3, ifelse(newrushi[,3] == "Hospice", 4, ifelse(newrushi[,3] == "Inpatient", 5, ifelse(newrushi[,3] == "Outpatient", 6, ifelse(newrushi[,3] == "Skilled Nursing Facility", 7, ifelse(newrushi[,3] == "Total", 8, 000))))))))

#source("http://goo.gl/UUyEzD")
#outlierKD(newrushi, Avg.Spending.Per.Episode..Hospital. )




ToNumber<- function(X)
{
  A <- gsub("%","*1e-2",gsub("K","*1e+3",gsub("M","*1e+6",gsub("\\$|,","",as.character(X)),fixed=TRUE),fixed=TRUE),fixed=TRUE)
  B <- try(sapply(A,function(a){eval(parse(text=a))}),silent=TRUE)
  if (is.numeric(B)) return (as.numeric(B)) else return(X)
}

numX <-as.data.frame(lapply(as.list(newrushi),ToNumber))




#######################################################################
#   C4.5
#
str(newrushi)
#install.packages("C50")  #execute this statement to install that package
numX$Percent_of_Spending_Hospital<-ifelse(numX$Percent_of_Spending_Hospital>0.49,"yes","no")
idx<-sample(nrow(numX),as.integer(.75*nrow(numX)))
training<-numX[idx,1:7]
test<-numX[-idx,1:7]
library(C50)
str(training)
training$Percent_of_Spending_Hospital<-factor(training$Percent_of_Spending_Hospital)
c45_model <- C5.0(training[,-(4:7)],training$Percent_of_Spending_Hospital)
summary(c45_model)
c45_pred <- predict(c45_model, test)
summary(c45_pred)
tab<-table(c45_pred,test$Percent_of_Spending_Hospital)
## boost model
library(gmodels)
CrossTable(test$Percent_of_Spending_Hospital, c45_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted '))


# accuracy

#install.packages("caret")
# takes a long time
#install.packages("e1071")
library(caret)
library(ggplot2)
library(lattice)

library(e1071)
confusionMatrix(tab)
#################################
### CART
library(rpart)
library(rattle)
views_tree <- rpart(training$Percent_of_Spending_Hospital~ Period + Claim_Type,data=training,method="class")
fancyRpartPlot(views_tree)

my_prediction <- predict(views_tree,test,type="class")

(conf <- table(test$Percent_of_Spending_Hospital,my_prediction))
(acc <- sum(diag(conf))/sum(conf))




