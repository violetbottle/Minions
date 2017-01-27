
# Team    :    4
# Purpose :    Project
# Course  :    Kdd
# Section :    B  
# Term    :    FALL 2016

rm(list=ls())
install.packages("pryr")
library(pryr)
mem_used()

#Change the link
raw_input =read.csv(file="F://kDD/dataset.csv",header =TRUE)


# Step 1 : DATA CLEANSING

df <- data.frame(raw_input[3:9],raw_input[11:16])
summary(df)
#boxplot(new)
View(df)
names(df)

# check if New offence classification is blank,recidivism type is tech & return to prison is Yes than replace the blank row values with convicting.offence.classification
df$New.Conviction.Offense.Classification <- ifelse(df$New.Conviction.Offense.Classification == "" & df$Recidivism.Type == 'Tech' & df$Recidivism...Return.to.Prison == 'Yes', as.character(df$Convicting.Offense.Classification), as.character(df$New.Conviction.Offense.Classification))

# inserting none if new offence classification is blank, recidivism type is No recidivism & return to prism is No
df$New.Conviction.Offense.Classification[df$New.Conviction.Offense.Classification == ""] <- "NONE"

# check if New conviction offence type is blank,recidivism type is tech & return to prison is Yes than replace the blank row values with convicting.offence.type
df$New.Conviction.Offense.Type <- ifelse(df$New.Conviction.Offense.Type == "" & df$Recidivism.Type == 'Tech' & df$Recidivism...Return.to.Prison == 'Yes', as.character(df$Convicting.Offense.Type), as.character(df$New.Conviction.Offense.Type))

# inserting none if new conviction offence type is blank, recidivism type is No recidivism & return to prism is No
df$New.Conviction.Offense.Type[df$New.Conviction.Offense.Type == ""] <- "NONE"


# check if New offence sub type is blank,recidivism type is tech & return to prison is Yes than replace the blank row values with convicting.offence.subtype
df$New.Conviction.Offense.Sub.Type <- ifelse(df$New.Conviction.Offense.Sub.Type == "" & df$Recidivism.Type == 'Tech' & df$Recidivism...Return.to.Prison == 'Yes', as.character(df$Convicting.Offense.Subtype), as.character(df$New.Conviction.Offense.Sub.Type))

# inserting none if new conviction offence sub type is blank, recidivism type is No recidivism & return to prism is No
df$New.Conviction.Offense.Sub.Type[df$New.Conviction.Offense.Sub.Type == ""] <- "NONE"


df$Days.to.Recidivism<-ifelse(is.na(df$Days.to.Recidivism),0,df$Days.to.Recidivism)
df$Sex<-ifelse(df$Sex=="F",1,0)
df$Sex<-ifelse(is.na(df$Sex),max(df$Sex),df$Sex)



df$Race...Ethnicity <- ifelse(is.na(df$Race...Ethnicity),max(df$Race...Ethnicity),df$Race...Ethnicity)
df$Age.At.Release   <- ifelse(is.na(df$Age.At.Release), max(df$Age.At.Release),df$Age.At.Release)


#boxplot(df,las=2,xlim(0,12),ylim(0,20))

summary(df)


df<-df[,-(11:13)]
crx<-sample(nrow(df),as.integer(.70*nrow(df)))
training<-df[crx,1:8]
test<-df[-crx,1:8]

##############################################
#C4.5
#install.packages("C50")
library(C50)

treeModel <- C5.0(training[,-8], training$Recidivism...Return.to.Prison)
summary(treeModel)
c45_pred <- predict(treeModel, test)
C5imp(treeModel)
C5imp(treeModel, metric = "splits")

library(caret)
library(lattice)
library(ggplot2)
library(e1071)
library(gmodels)
confusionMatrix(c45_pred,test$Recidivism...Return.to.Prison)

CrossTable(test$Recidivism...Return.to.Prison, c45_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted '))

c45_boost10 <- C5.0(training[,-8], training$Recidivism...Return.to.Prison,
                       trials = 10)
summary(c45_boost10)
c45_boost10

c45_boost_pred10 <- predict(c45_boost10, test)

confusionMatrix(c45_boost_pred10,test$Recidivism...Return.to.Prison)
CrossTable(test$Recidivism...Return.to.Prison, c45_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted'))

######################################################################
#
#
#install.packages("inTrees")
library(randomForest)
library(inTrees)
#rf <- randomForest(Recidivism...Return.to.Prison ~ .,data=df)
target <- training[,"Recidivism...Return.to.Prison"]  # target: class
X <- training[, 1:(ncol(training) - 1)]
rf <- randomForest(X, as.factor(target))
getTree(rf, 1)
importance(rf)
varImpPlot(rf)
####################

#treeList <- RF2List(rf)  # transform rf object to an inTrees' format
#exec <- extractRules(treeList, X)  # R-executable conditions
#exec[1:2,]
#ruleMetric <- getRuleMetric(exec,X,target)  # get rule metrics
#ruleMetric[1:2,]
#ruleMetric <- pruneRule(ruleMetric, X, target)
#ruleMetric[1:2,]
#(ruleMetric <- selectRuleRRF(ruleMetric, X, target))
#(learner <- buildLearner(ruleMetric, X, target))
#readableRules <- presentRules(ruleMetric, colnames(X))
#readableRules[1:2, ]


Prediction <- predict(rf, test)

confusionMatrix(Prediction,test$Recidivism...Return.to.Prison)
CrossTable(test$Recidivism...Return.to.Prison, Prediction,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual ', 'predicted'))

submit <- data.frame(Release = test$Release.Type,Classification=test$Convicting.Offense.Classification,Conviction=test$Convicting.Offense.Subtype,Age=test$Age.At.Release,Race=test$Race...Ethnicity , target= Prediction)
 
write.csv(submit, file = "firstforest.csv", row.names = FALSE)


