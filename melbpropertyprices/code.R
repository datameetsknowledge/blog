library(ggplot2)
setwd("XXXXX") # set local directory
melbprices <- read.csv("Melbourne_housing.csv")
melbprices$Date <- as.Date(melbprices$Date, format="%d-%m-%Y")
melbprices$DistanceCat <- NA

# Creating categorical factors for distance
for (i in 1:nrow(melbprices)){
  if(melbprices$Distance[i]>=1.2 & melbprices$Distance[i]<4.65){
    melbprices$DistanceCat[i] <- "1.2 - 4.65"
  }
  else if (melbprices$Distance[i]>=4.65 & melbprices$Distance[i]<8.1){
    melbprices$DistanceCat[i] <- "4.65 - 8.1"
  }
  else if (melbprices$Distance[i]>=8.1 & melbprices$Distance[i]<11.55){
    melbprices$DistanceCat[i] <- "8.1 - 11.55"
  }
  else if (melbprices$Distance[i]>=11.55 & melbprices$Distance[i]<=15){
    melbprices$DistanceCat[i] <- "11.55 - 15"
  }
}
melbprices$DistanceCat <- as.factor(melbprices$DistanceCat)
melbprices$DistanceCat <- factor(melbprices$DistanceCat,levels(melbprices$DistanceCat)[c(1,3,4,2)])

# Inputting full name of property types
melbprices$Type <- as.character(melbprices$Type)
for (i in 1:nrow(melbprices)){
  if(melbprices$Type[i]=="h"){
    melbprices$Type[i]<-"House"
  }
  else if(melbprices$Type[i]=="t"){
    melbprices$Type[i]<-"Townhouse"
  }
  else if(melbprices$Type[i]=="u"){
    melbprices$Type[i]<-"Unit"
  }
}

# EDA of house prices across time
g <- ggplot(melbprices,aes(Date,Price))
g + geom_smooth(method=lm) + facet_grid(.~Type) + theme(axis.text.x = element_text(angle=90,hjust=1,size = 8),axis.title.y = element_text(size=9),axis.title.x = element_text(size=9), plot.title = element_text(size=10,hjust = 0.5)) + labs(title = "Property Prices Across Time")
g + geom_smooth(method=lm,aes(col=Type)) + facet_grid(.~DistanceCat)+ theme(axis.text.x = element_text(angle=90,hjust=1,size = 8),axis.title.y = element_text(size=9),axis.title.x = element_text(size=9), plot.title = element_text(size=10,hjust = 0.5), legend.title = element_text(size=9),legend.text = element_text(size=8)) + labs(title = "Property Prices Across Time at Various Distance from CBD")
ggsave("propertydistance.png",width=10.6,height=7.24,units = "in")

# EDA of townhouses to investigate relationship
townhouses <- subset(melbprices, melbprices$Type=="Townhouse")
g <- ggplot(townhouses,aes(Date,Price))
g + geom_point() + geom_smooth(method=lm) + facet_grid(.~DistanceCat) + theme(axis.text.x = element_text(angle=90,hjust=1,size = 8),axis.title.y = element_text(size=9),axis.title.x = element_text(size=9), plot.title = element_text(size=10,hjust = 0.5), legend.title = element_text(size=9),legend.text = element_text(size=8)) + labs(title = "Townhouse Prices Across Time at Various Distance from CBD")
table(townhouses$DistanceCat)

# Investigation into real estate agents performance 
melbpricesclean <- melbprices[complete.cases(melbprices),]
sellerlist <- aggregate(melbpricesclean$Type,by=list(melbpricesclean$SellerG),FUN=length)
names(sellerlist) <- c("Seller","Count")
sellerlisttop <- subset(sellerlist,sellerlist$Count>200)
topsellers <- apply(sellerlisttop,2,as.list)
topsellers <- topsellers[1]
topsellers <- unlist(topsellers)

selleraveragescount <- aggregate(melbpricesclean$Price, by=list(melbpricesclean$Suburb,melbpricesclean$Rooms,melbpricesclean$Type),FUN=length)
names(selleraveragescount) <- c("Suburb","Rooms","Type","Count")
selleraveragesprice <- aggregate(melbpricesclean$Price, by=list(melbpricesclean$Suburb,melbpricesclean$Rooms,melbpricesclean$Type),FUN=mean,na.rm=TRUE)
names(selleraveragesprice) <- c("Suburb","Rooms","Type","Avg Price")
selleraverages <- merge(selleraveragescount,selleraveragesprice)

melbpricesnew <- merge(melbpricesclean,selleraverages)
sellermelbprices <- melbpricesnew[melbpricesnew$SellerG %in% topsellers,]
sellermelbprices <- sellermelbprices[complete.cases(sellermelbprices),]
sellermelbprices$SellerDiff <- sellermelbprices$Price - sellermelbprices$`Avg Price`
sellermelbprices <- subset(sellermelbprices,sellermelbprices$Count>1)
write.csv(sellermelbprices,"sellermelbprices.csv",row.names = FALSE)
