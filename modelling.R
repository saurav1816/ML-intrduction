library(rpart)
library(rpart.plot)
library(randomForest)
library(ranger)
library(tidyr)
library(ggplot2)

### setting up the home directory and reading the input file
setwd("C:/Users/Manu/Desktop/R programming/Discussion self/Delloit/DSAT/DSAT")
# creditData=read.csv("ModelingData.csv", header=TRUE)
creditData=read.csv("ModelingDataUpdated.csv", header=TRUE)
nrow(creditData)


#Checking and dropping and missing values, but dataset is not having any missing values
summary(creditData)
subset(creditData,is.na(RESPONSE))
new_cr <- creditData[rowSums(is.na(creditData)) > 0,]
creditData <- na.omit(creditData)

#Dropping the OBS. column 
drops <- c("OBS.")
creditData <- creditData[, !(names(creditData) %in% drops)]
str(creditData)

creditData<-tbl_df(creditData)
glimpse(creditData)
#Changing variable data types as per the variable description
creditData$CHK_ACCT <- factor(creditData$CHK_ACCT)
creditData$HISTORY <- factor(creditData$HISTORY)
creditData$NEW_CAR <- factor(creditData$NEW_CAR)
creditData$USED_CAR <- factor(creditData$USED_CAR)
creditData$FURNITURE <- factor(creditData$FURNITURE)
creditData$CHK_ACCT <- factor(creditData$FURNITURE)
creditData$RADIO.TV <- factor(creditData$RADIO.TV)
creditData$EDUCATION <- factor(creditData$EDUCATION)
creditData$RETRAINING <- factor(creditData$RETRAINING)
creditData$SAV_ACCT <- factor(creditData$SAV_ACCT)
creditData$EMPLOYMENT <- factor(creditData$EMPLOYMENT)
creditData$MALE_DIV <- factor(creditData$MALE_DIV)
creditData$MALE_SINGLE <- factor(creditData$MALE_SINGLE)
creditData$MALE_MAR_or_WID <- factor(creditData$MALE_MAR_or_WID)
creditData$CO.APPLICANT <- factor(creditData$CO.APPLICANT)
creditData$GUARANTOR <- factor(creditData$GUARANTOR)
creditData$PRESENT_RESIDENT <- factor(creditData$PRESENT_RESIDENT)
creditData$REAL_ESTATE <- factor(creditData$REAL_ESTATE)
creditData$PROP_UNKN_NONE <- factor(creditData$PROP_UNKN_NONE)
creditData$OTHER_INSTALL <- factor(creditData$OTHER_INSTALL)
creditData$RENT <- factor(creditData$RENT)
creditData$OWN_RES <- factor(creditData$OWN_RES)
creditData$JOB <- factor(creditData$JOB)
creditData$TELEPHONE <- factor(creditData$TELEPHONE)
creditData$FOREIGN <- factor(creditData$FOREIGN)
creditData$RESPONSE <- factor(creditData$RESPONSE)
# str(creditData)

### plotting Age and Duration fields to check if they can be gouped into Bins
 barplot(table(creditData$DURATION))
 creditData$DURATION <- ifelse((creditData$DURATION %in% c("12","18","24","36","48")),"A",ifelse((creditData$DURATION %in% c("10","15","21","27","30","42","6","60","9")),"B","C"))
 barplot(table(creditData$AGE))
 creditData$AGE <- ifelse((creditData$AGE %in% c("19","20","21","22")),"A",creditData$AGE) 
  creditData$AGE <- ifelse((creditData$AGE %in% c("23","24","25","26","27","28","29","30")),"B",creditData$AGE) 
 creditData$AGE <- ifelse((creditData$AGE %in% c("31","32","33","34","35","36","37","38","39","40")),"C",creditData$AGE) 
 creditData$AGE <- ifelse((creditData$AGE %in% c("41","42","43","44","45","46","47","48","49","50")),"D",creditData$AGE)
 creditData$AGE <- ifelse((creditData$AGE %in% c("51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","70","74","75")),"E",creditData$AGE)

#########
#Sampling data into training and test randomly
set.seed(729375)
forTest <- rbinom(n = nrow(creditData), size = 1, prob = 0.2) > 0
dTrain <- subset(creditData, !forTest)
dTest <- subset(creditData, forTest)
?rbinom
#Selecting column names in a variable
colNames <- colnames(creditData)
colNames
#Selecting categorical variables
catVars <- colNames[sapply(creditData[,colNames], class) %in% c('factor','character')]

#Selecting numeric variables
numVars <- colNames[sapply(creditData[,colNames], class) %in% c('numeric', 'integer')]

#Removing some of the unwanted variables
rm(list=c('creditData', 'new_cr'))

#Choosing positive response
pos <- '1'

### Selecting important variables ###

#Random Forest package for selecting important variables
mo1 <- ranger(RESPONSE ~., data = dTrain, num.trees = 101, write.forest = TRUE, importance = "impurity")
mo1$importance

data.frame(as.list(mo1$variable.importance)) %>%  gather() %>%
  ggplot(aes(x = reorder(key, value), y = value)) +
  geom_bar(stat = "identity", width = 0.6, fill = "grey") +
  coord_flip() +
  theme_minimal() +
  ggtitle("Variable Importance (Gini Index)") +
  theme(axis.title.y = element_blank())


#Boruta package for selecting important variables
library(Boruta)
br.train <- Boruta(RESPONSE~., data = dTrain, doTrace = 2)
print(br.train)
#Plot boruta variables importance chart
plot(br.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(br.train$ImpHistory),
           function(i)br.train$ImpHistory[is.finite(br.train$ImpHistory[,i]),i])
names(lz) <- colnames(br.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(br.train$ImpHistory), cex.axis = 0.5)

#Decide on tentative attributes
f.boruta <- TentativeRoughFix(br.train)
print(f.boruta)
#Plot after the classification of tentative variables
plot(f.boruta, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(f.boruta$ImpHistory),
           function(i)f.boruta$ImpHistory[is.finite(f.boruta$ImpHistory[,i]),i])
names(lz) <- colnames(f.boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(f.boruta$ImpHistory), cex.axis = 0.5)

#List of confirmed attributes
confVars <- getSelectedAttributes(f.boruta)

dim(dTrain)
dim(dTest)
#creditData <- creditData[, !(names(creditData) %in% drops)]
dTrainF <- dTrain[,(names(dTrain) %in% c(confVars, 'RESPONSE'))]
#Adding the 3 additional fields derived from Random Forest
dTrainF$INSTALL_RATE=dTrain$INSTALL_RATE
dTrainF$PRESENT_RESIDENT=dTrain$PRESENT_RESIDENT
dTrainF$JOB=dTrain$JOB

# Test dataset
dTestL= dTest[,(names(dTest) %in% c(confVars, 'RESPONSE'))]
dTestL$INSTALL_RATE=dTest$INSTALL_RATE
dTestL$PRESENT_RESIDENT=dTest$PRESENT_RESIDENT
dTestL$JOB=dTest$JOB

dTestF <- dTest[,(names(dTest) %in% confVars)]
#Adding the 3 additional fields derived from Random Forest
dTestF$INSTALL_RATE=dTest$INSTALL_RATE
dTestF$PRESENT_RESIDENT=dTest$PRESENT_RESIDENT
dTestF$JOB=dTest$JOB


### Running 4 differnet models on the trainning dataset and validating on test dataset

# 1)
# Running Random forest model on training data
modelRF <- ranger(RESPONSE ~., data = dTrainF, num.trees = 100, write.forest = TRUE, importance = "impurity")

trainPred <- predict(modelRF, dTrain[,-31])
table(trainPred$predictions, dTrain$RESPONSE)

#Predicting the test data points
testPred <- predict(modelRF, dTestF)
testPred <- testPred$predictions
t=table(testPred, dTest$RESPONSE)
testAccuracy <- mean(testPred == dTest$RESPONSE)
print (testAccuracy)

prob <- predict(modelRF, dTestL[,-13], type="prob")
pr=prob$predictions
pr1=as.data.frame(pr)
pr1
pr2=as.data.frame(dTestL[,13])
pred <- prediction(pr1[,1], pr2)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
plot(perf)

# 2)
#Decision Tree Model
library(rpart)
modelDT <- rpart(RESPONSE ~., data = dTrainF, method = "class")
# plot(modelDT)
# text(modelDT, pretty=0)
predDT <- predict(modelDT, dTestF)
predVal <- ifelse(predDT[,1] >= 0.5, 1, 0)
table(predVal,dTest$RESPONSE)
DTAccuracy <- mean(predVal == dTest$RESPONSE)
DTAccuracy

# 3)
# SVM 
library("e1071")
modelSVM <- svm(RESPONSE~., data = dTrainF) 
summary(modelSVM)
predSVM <- predict(modelSVM, dTestF)
table(predSVM, dTest[,31])
mean(predSVM == dTest[,31])
####

# 4)
#Logistic Regression
modelLR <- glm(RESPONSE ~
                 DURATION           
               + HISTORY            
               # + NEW_CAR                	
               + USED_CAR            
               + AMOUNT              
               + SAV_ACCT            
               + EMPLOYMENT          
               + REAL_ESTATE         
               # + PROP_UNKN_NONE      
               + AGE                 
               + OTHER_INSTALL       
               + OWN_RES             
               + INSTALL_RATE            
               # + PRESENT_RESIDENT         
               # + JOB                  
               ,data = dTrainF, family = "binomial")
summary(modelLR)
# getting the least significant variable and removed them in below order
# JOB --> PROP_UNKN_NONE --> PRESENT_RESIDENT 
modelLR_pval=as.data.frame((summary(modelLR))$coefficients)
subset(modelLR_pval,modelLR_pval[,4]==max(modelLR_pval[-c(1),4]))
# checking Nagelkerke R square
NagelkerkeR2(modelLR)

predLR <- predict(modelLR, newdata = dTestF, type = 'response')
head(predLR)
table(dTest[,31], predLR > 0.5)
predLRVal <- ifelse(predLR >= 0.5, 1, 0)
mean(predLRVal == dTest$RESPONSE)
####

### KNN
library("class")
str(dTrain)
dim(dTrainF)
str(dTest)
dim(dTestL)
dTrain_labels=dTrain[,31]
dTest_labels=dTest[,31]
model_knn=knn(dTrainF, dTestL, dTrain_labels, k = 20, prob=TRUE)
#validation
library("gmodels")
CrossTable(dTest_labels,model_knn, prop.chisq = FALSE)

# GBM
library(gbm)
gbm_model<-gbm(RESPONSE ~ ., dTrainF, distribution = "bernoulli", n.trees= 5,
               shrinkage= 0.1, interaction.depth= 3)

## Variable importance
summary(gbm_model,
        cBars=length(gbm_model$var.names),
        n.trees=gbm_model$n.trees,
        plotit=TRUE,
        order=TRUE,
        method=relative.influence,
        normalize=TRUE)
summary(gbm_model)
predict_gbm<-predict(gbm_model, testing, n.trees=5, type="response")
summary(predict_gbm)

b <-transform(predict_gbm)
testing$predgbm<-b$X_data
head(testing)

#Subset the Data for the Relevant Variables
testing1_gbm <-subset(testing, select = c(predgbm,DEF,Num))
#Compute the GINI
library(Hmisc)
rcorr.cens(testing1_gbm$predgbm,testing1_gbm$DEF)

#Merge the Relevant Files
Scored_testing1 <-merge(testing1_lg,testing1_rf ,testing1_gbm by="Num")
