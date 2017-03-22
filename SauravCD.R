rm(list = ls())
dir<-"C:/Users/smehta36/Documents/Analytics/Kaggle Titanic/"
train<-read.csv(paste(dir,"train.csv",sep=""),header = T,na.strings = c("","NA"))
test<-read.csv(paste(dir,"test.csv",sep=""),header = T,na.strings = c("","NA"))

# remove passengerid
head(train,1)
train$PassengerId<-NULL
test$PassengerId<-NULL

#add Survived to test
test.dum<-data.frame(Survived=rep("None",nrow(test)),test[,])
head(test.dum,1)
head(test,1)

#combine data
data.combined<-rbind(train,test.dum)
str(data.combined)
str(test.dum)

data.combined$Survived<-as.factor(data.combined$Survived)
data.combined$Pclass<-as.factor(data.combined$Pclass)

#cols remove 
data.combined<-data.combined[,-c(11)]

#check dependent vairable
table(data.combined$Survived)

#hypothesis that rich people were survivied 
train$Pclass<-as.factor(train$Pclass)
ggplot(train,aes(x = Pclass, fill=factor(Survived ))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("count") +
  labs("Survived")

#hypothesis on gender survive rate
length(unique(as.character(data.combined$Name)))
dup.names<-as.character(data.combined[which(duplicated(as.character(data.combined$Name))),"Name"])
data.combined[which(data.combined$Name %in% dup.names),]

# Any correlation with other variables (e.g., sibsp)?
#What is up with the 'Miss.' and 'Mr.' thing?
misses<- data.combined[which(str_detect(data.combined$Name,"Miss")),]

# Hypothesis - Name titles correlate with age
mrses<-data.combined[which(str_detect(data.combined$Name,"Mrs")),]
mrses[1:5,]


# Check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"), ]
males[1:5,]

# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
# NOTE - Using the grep function here, but could have used the str_detect function as well.

extractTitle<- function(Name){
  Name<-as.character(Name)
  
  if (length(grep("Mr.",Name))>0)
    {
      return ("Mr.")
    }   else if (length(grep("Miss.",Name))>0)
    {
      return ("Miss.")
    }    else if (length(grep("Mrs.",Name))>0)
    {
      return ("Mrs.") 
    }    else if (length(grep("Master.",Name))>0)
    {
      return ("Master.") 
    }    else  
    {
      return ("Other") 
    }
    
}

titles<- NULL 
 for (i in 1:nrow(data.combined)) {
  titles<-c(titles,extractTitle(data.combined[i,"Name"]))
}

summary(titles)
data.combined$title <- as.factor(titles)
head(data.combined)

# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")


# What's the distribution of females to males across train & test?
table(data.combined$Sex)

# Visualize the 3-way relationship of sex, pclass, and survival, compare to analysis of title
ggplot(data.combined[1:891,], aes(x=Sex,fill=Survived)) +
stat_count(width=0.5) +
facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("sex") +
  ylab("t.count") +
  labs(fill="Survived")



# OK, age and sex seem pretty important as derived from analysis of title, let's take a closer 
# look at the distibutions of age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891,"Age"])

# Just to be thorough, take a look at survival rates broken out by sex, pclass, and age
ggplot(data.combined[1:891,], aes(x = Age, fill =Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")


# Validate that "Master." is a good proxy for male children
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)


# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") + 
  xlab("Age") +
  ylab("Total Count")


# OK, appears female children may have different survival rate, 
# could be a candidate for feature engineering later
misses.alone<-misses[which(misses$SibSp==0 & misses$Parch==0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))


# Move on to the sibsp variable, summarize the variable
summary(data.combined$SibSp)


# Can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We believe title is predictive. Visualize survival reates by sibsp, pclass, and title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  stat_count(width = 1) +
  facet_wrap(~Pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Treat the parch vaiable as a factor and visualize
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")


# Let's try some feature engineering. What about creating a family size feature?
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)


# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  stat_count(width = 1) +
  facet_wrap(~pclass + title) + 
  ggtitle("Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")
str(data.combined)

?boxcox
