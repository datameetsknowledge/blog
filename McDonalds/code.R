library(ggplot2)
library(corrplot)
source("Multiplot.R") # Multiplot function
setwd("XXXX") # set local directory
rawdata <- read.csv("menu.csv")
rawdata <- rawdata[c(1:82,84:nrow(rawdata)),] #removes 40 chicken nuggets
names(rawdata) <- gsub("\\.","",names(rawdata))

# Feature engineering
rawdata$TransFatDailyValue <- round(rawdata$TransFat/2*100, digits=0)
rawdata$SugarsDailyValue <- round(rawdata$Sugars/30*100,digits=0)
rawdata$ProteinDailyValue <- round(rawdata$Protein/50*100, digits=0)
rawdata$CaloriesDailyValue <- round(rawdata$Calories/2250*100, digits=0)

# EDA
aggregate(x=rawdata$CaloriesDailyValue,by=list(rawdata$Category),FUN=mean)

g <- ggplot(rawdata,aes(Category,CaloriesDailyValue))
g1 <- g + geom_boxplot() + theme(axis.text.y = element_text(size=8),axis.text.x = element_text(angle=45,hjust=1,size=8), axis.title.y = element_text(size=8),axis.title.x = element_blank()) 

g <- ggplot(rawdata,aes(Category,CarbohydratesDailyValue))
g2 <- g + geom_boxplot() + theme(axis.text.y = element_text(size=8),axis.text.x = element_text(angle=45,hjust=1,size=8), axis.title.y = element_text(size=8),axis.title.x = element_blank()) 

g <- ggplot(rawdata,aes(Category,ProteinDailyValue))
g3 <- g + geom_boxplot() + theme(axis.text.y = element_text(size=8),axis.text.x = element_text(angle=45,hjust=1,size=8), axis.title.y = element_text(size=8),axis.title.x = element_blank()) 

g <- ggplot(rawdata,aes(Category,TotalFatDailyValue))
g4 <- g + geom_boxplot() + theme(axis.text.y = element_text(size=8),axis.text.x = element_text(angle=45,hjust=1,size=8), axis.title.y = element_text(size=8),axis.title.x = element_blank()) 

g <- ggplot(rawdata,aes(Category,SaturatedFatDailyValue))
g5 <- g + geom_boxplot() + theme(axis.text.y = element_text(size=8),axis.text.x = element_text(angle=45,hjust=1,size=8), axis.title.y = element_text(size=8),axis.title.x = element_blank())

g <- ggplot(rawdata,aes(Category,CholesterolDailyValue))
g6 <- g + geom_boxplot() + theme(axis.text.y = element_text(size=8),axis.text.x = element_text(angle=45,hjust=1,size=8), axis.title.y = element_text(size=8),axis.title.x = element_blank()) 

g <- ggplot(rawdata,aes(Category,SugarsDailyValue))
g7 <- g + geom_boxplot() + theme(axis.text.y = element_text(size=8),axis.text.x = element_text(angle=45,hjust=1,size=8), axis.title.y = element_text(size=8),axis.title.x = element_blank()) 

g <- ggplot(rawdata,aes(Category,IronDailyValue))
g8 <- g + geom_boxplot() + theme(axis.text.y = element_text(size=8),axis.text.x = element_text(angle=45,hjust=1,size=8), axis.title.y = element_text(size=8),axis.title.x = element_blank()) 

multiplot(g1,g2,g3,g4,g5,g6,g7,g8,cols=4)

# Carbs investigation
carbdata <- rawdata[rawdata$Category=="Smoothies & Shakes",c(1:4,15:16)]
carbdata$ServingSize <- gsub(".*?\\(","",carbdata$ServingSize)
carbdata$ServingSize <- gsub("[ ]g\\)","",carbdata$ServingSize)
write.csv(carbdata,"carbdata.csv",row.names=FALSE)

# CholesterolInvestigation
choldata <- rawdata[rawdata$Category=="Breakfast",c(1:4,11:12)]
choldata$ServingSize <- gsub(".*?\\(","",choldata$ServingSize)
choldata$ServingSize <- gsub("[ ]g\\)","",choldata$ServingSize)
colnames(choldata)[3] <- "ServingSize(grams)"
write.csv(choldata,"choldata.csv",row.names = FALSE)

#Unhealthiness Index
rawdata$UnhealthinessIndex <- rawdata$TotalFat + rawdata$SaturatedFat + rawdata$TransFat + rawdata$Cholesterol + rawdata$Sodium + rawdata$Sugars
unhealthydata <- rawdata[,c(1:4,7,9,12,14,25,26,28,29)]
unhealthydata$UnhealthinessIndexDailyValue <- round(unhealthydata$UnhealthinessIndex/(65+20+2+30+2420+299)*100,0)
g <- ggplot(unhealthydata,aes(Category,UnhealthinessIndex))
g9 <- g + geom_boxplot() + labs(y="Unhealtiness Index") +theme(axis.text.y = element_text(size=6),axis.text.x = element_text(angle=45,hjust=1,size=6), axis.title.y = element_text(size=8),axis.title.x = element_blank()) 
ggsave("UnhealthinessIndex.png",height=3.2, width=4.7)
write.csv(unhealthydata,"unhealthydata.csv",row.names = FALSE)

