setwd("XXXX") # set directory
library(ggplot2)
library(rpart)
library(plyr)
library(randomForest)
library(ROCR)

# Read Data
train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")
gendersubmission <- read.csv("./data/gender_submission.csv")
test$Survived <- NA
combdata <- rbind(train,test)
combdata$Name <- as.character(combdata$Name)

if(!file.exists("plots")){
  dir.create("plots")
}

# Feature engineering for Title
combdata$Title <- (strsplit(combdata$Name,"[,.]"))[[1]][2]
combdata$Title <- sapply(combdata$Name, function(x) strsplit(x,"[,.]")[[1]][2])
combdata$Title <- gsub(" ", "",combdata$Title)

combdata$Title[combdata$Title %in% c("Major","Capt","Don")] <- "Sir"
combdata$Title[combdata$Title %in% c("Mlle","Ms")] <- "Miss"
combdata$Title[combdata$Title %in% c("Dona","Jonkheer","theCountess")] <- "Lady"
combdata$Title[combdata$Title %in% c("Mme")] <- "Mrs"
table((combdata$Title))

# Feature engineering for First Name
combdata$FirstName <- sapply(combdata$Name,function(x) strsplit(x,"[.]")[[1]][2])
combdata$FirstName <- sapply(combdata$FirstName,function(x) strsplit(x," "))
for (i in 1:nrow(combdata)){
  combdata$FirstName[i] <- combdata$FirstName[[i]][2]
}
combdata$FirstName <- as.character(combdata$FirstName)
combdata$FirstName <- gsub("\\(","",combdata$FirstName)
combdata$FirstName <- gsub(")","",combdata$FirstName)

# Feature engineering for first name repetition
repname.df <- as.data.frame(table(combdata$FirstName))
colnames(repname.df) <- c("Name","Frequency")
repname.df <- repname.df[repname.df$Frequency>1,]
combdata$RepName <- as.factor(combdata$FirstName %in% repname.df$Name)

# Feature engineering for family size
combdata$familysize <- combdata$SibSp + combdata$Parch +1

# Fitting age for missing data
agefit <- rpart(Age~Pclass+Sex+SibSp + Parch + Fare + Embarked + Title + familysize, data=combdata[!is.na(combdata$Age),],method="anova")
combdata$Age[is.na(combdata$Age)] <- predict(agefit,combdata[is.na(combdata$Age),])
combdata$Age <- round(combdata$Age, digits = 0)

# Fitting fare for missing data
farefit <- rpart(Fare~Age + Pclass+Sex+SibSp + Parch  + Embarked + Title + familysize, data=combdata[!is.na(combdata$Fare),],method="anova")
combdata$Fare[is.na(combdata$Fare)] <- predict(agefit,combdata[is.na(combdata$Fare),])

# Setting embark location for missing data
table(combdata$Embarked)
combdata$Embarked[combdata$Embarked==""] <- "S"
combdata$Embarked <- as.factor(combdata$Embarked)

combdata$Title <- as.factor(combdata$Title)
combdata$Pclass <- as.factor(combdata$Pclass)
combdata$Survived <- as.factor(combdata$Survived)
newtrain <- combdata[1:891,]
newtest <- combdata[892:1309,]

# EDA
g <- ggplot(newtrain,aes(Age))
g+geom_histogram(binwidth = 5) + facet_grid(.~Sex)+ aes(color=Survived,fill=Survived) + labs(title="Survival Count by Age & Gender",y="Count")+theme(plot.title = element_text(size=10,hjust = 0.5),axis.title = element_text(size=9),legend.title = element_text(size=9))
ggsave("./plots/Survival Count by Age & Gender.png")

g <- ggplot(newtrain,aes(Fare))
g+geom_histogram(binwidth = 25) + aes(color=Survived,fill=Survived) + labs(title="Survival Count by Fare",y="Count")+theme(plot.title = element_text(size=10,hjust = 0.5),axis.title = element_text(size=9),legend.title = element_text(size=9))
ggsave("./plots/Survival Count by Fare.png")

g <- ggplot(newtrain,aes(Fare))
g + geom_histogram()+ aes(color=Survived,fill=Survived) + facet_wrap(~Pclass,scales = "free") + labs(title="Survival Count by Fare & Passenger Class",y="Count")+theme(plot.title = element_text(size=10,hjust = 0.5),axis.title = element_text(size=9),legend.title = element_text(size=9))
ggsave("./plots/Survival Count by Fare & Passenger Class.png")

g <- ggplot(newtrain,aes(familysize))
g+geom_histogram(binwidth = 1)+aes(color=Survived,fill=Survived) + labs(title="Survival Count by Family Size",x="Family Size", y="Count")+theme(plot.title = element_text(size=10,hjust = 0.5),axis.title = element_text(size=9),legend.title = element_text(size=9))
ggsave("./plots/Survival Count by Family Size.png")

g <- ggplot(newtrain,aes(Title))
g+geom_bar()+aes(color=Survived,fill=Survived) + labs(title="Survival Count by Title",x="Title", y="Count")+theme(plot.title = element_text(size=10,hjust = 0.5),axis.title = element_text(size=9),legend.title = element_text(size=9))
ggsave("./plots/Survival Count by Title.png")

g <- ggplot(newtrain,aes(Pclass))
g+geom_bar()+aes(color=Survived,fill=Survived) + facet_grid(.~Sex)+  labs(title="Survival Count by Passenger Class & Gender",x="Passenger Class", y="Count")+theme(plot.title = element_text(size=10,hjust = 0.5),axis.title = element_text(size=9),legend.title = element_text(size=9))
ggsave("./plots/Survival Count by Passenger Class & Gender.png")

# Random Forest
set.seed(323)
survivalfit_rf <- randomForest(as.factor(Survived)~Pclass + Sex + Age + SibSp + Parch +Fare + Embarked + Title + familysize + RepName, data=newtrain,importance=TRUE)

plot(survivalfit_rf, main="Error Rate v No. of Trees")
legend("topright", colnames(survivalfit_rf$err.rate),fill=1:3)

varImpPlot(survivalfit_rf,main="Variable Importance Plot")

importance <- as.data.frame(importance(survivalfit_rf))
importance$variables <- row.names(importance)
row.names(importance) <- NULL
importance <- importance[,c(5,1:4)]
importance$variables <- factor(importance$variables,levels = importance$variables[order(importance$MeanDecreaseGini)])

library(RColorBrewer)
pal <- colorRampPalette(brewer.pal(9,"Blues"))(nrow(importance))
g <- ggplot(data=importance, aes(x=variables,y=MeanDecreaseGini))
g+ geom_col(aes(fill=variables))+scale_fill_manual(values  = pal) +coord_flip() + labs(title="Variable Importance Plot",x="Variables",y="Mean Decrease Gini")+ theme_classic()+theme(plot.title = element_text(hjust = 0.5,size = 11),axis.title = element_text(size=9),axis.title.y = element_text(size=9))+guides(fill=FALSE) 
ggsave("./plots/Variable Importance Plot.png")
