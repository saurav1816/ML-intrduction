rm(list = ls())
dir<-"C:/Users/smehta36/Documents/Analytics/Kaggle Titanic/"
train<-read.csv(paste(dir,"train.csv",sep=""),header = T,na.strings = c("","NA"))
test<-read.csv(paste(dir,"test.csv",sep=""),header = T,na.strings = c("","NA"))
#kaggle Titanic
##* `tbl_df` creates a "local data frame"
##* Local data frame is simply a wrapper for a data frame that prints nicely
# convert to local data frame
train=tbl_df(train)
# you can specify that you want to see more rows
print(train, n=20)
glimpse(train)
summary(train)
row_number(train[62,])
full=bind_rows(train,test)
str(full)
# convert to a normal data frame to see all of the columns
data.frame(head(full,10))
##remove col
full=full[,c(-11)]
glimpse(full)
c=train[train$Age>0 & train$Sex=="male", ]



# Grab title from passenger names
#full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Show title counts by sex
#table(full$Sex, full$Title)
full[c(62, 830), 'Embarked']

str(full$Embarked)
full$Embarked[c(62, 830)] <- 'C'
#missing age by mice

md.pattern(full)
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Name')
full[factor_vars]=lapply(full[factor_vars], function(x) as.factor(x))

# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

mice_plot <- aggr(full, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(full), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

imputed_data=mice(full[,!names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')],m=3,maxit=5,method='rf',seed=501)

summary(imputed_data)

#check imputed values
imputed_out=imputed_data$imp$Age
imputed_out
#get complete data ( 2nd out of 5)
completeData <- complete(imputed_data,2)

# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(completeData$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))
# Replace Age variable from the mice model.
full$Age <- completeData$Age
str(full$Survived)
glimpse(full)
#relationship b/w age and survived and sex
ggplot(full[1:891,],aes(Age,fill = factor(Survived)))+
  geom_histogram()+
  facet_grid(.~Sex)+
  theme_few()

#P Model
# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                           Fare + Embarked ,
                         data = train)
impotance=importance(rf_model)
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

# Write the solution to file
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
