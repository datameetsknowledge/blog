library(readxl)
setwd(dir = "/Users/stevenliu/Dropbox/Further Education/Own Projects/Fuel Prices")

### Download data ###
if(!file.exists("Raw_data")){
  dir.create("Raw_data")
}
augurl <- "http://data.nsw.gov.au/data/dataset/a97a46fc-2bdd-4b90-ac7f-0cb1e8d7ac3b/resource/efebafce-5ddf-4f85-9840-07654b01a7a2/download/Service-Station-and-Price-History--August-2016.xlsx"
sepurl <- "http://data.nsw.gov.au/data/dataset/a97a46fc-2bdd-4b90-ac7f-0cb1e8d7ac3b/resource/6d0a644f-83d8-49b2-beef-4fb180e4f6d1/download/Service-Station-and-Price-History--September-2016.xlsx"
octurl <- "http://data.nsw.gov.au/data/dataset/a97a46fc-2bdd-4b90-ac7f-0cb1e8d7ac3b/resource/7b09946e-ffa8-45f0-90b9-36b90af6e510/download/Service-Stations-and-Price-History-October-2016.xlsx"
novurl <- "http://data.nsw.gov.au/data/dataset/a97a46fc-2bdd-4b90-ac7f-0cb1e8d7ac3b/resource/d8e32bc9-9561-4971-abd5-21862f50d60d/download/PriceHistoryNov2016.xlsx"
decurl <- "http://data.nsw.gov.au/data/dataset/a97a46fc-2bdd-4b90-ac7f-0cb1e8d7ac3b/resource/2a7128ae-02fa-40f7-b9de-a75479ebc9e4/download/PriceHistoryDec2016.xlsx"

download.file(augurl,destfile = "./Raw_data2/august.xlsx",method="curl")
download.file(sepurl,destfile = "./Raw_data2/september.xlsx",method="curl")
download.file(octurl,destfile = "./Raw_data2/october.xlsx",method="curl")
download.file(novurl,destfile = "./Raw_data2/november.xlsx",method="curl")
download.file(decurl,destfile = "./Raw_data2/december.xlsx",method="curl")

### Reads data and fixes date formatting ###
Aug16 <- read_excel("./Raw_data2/august.xlsx")
Aug16$PriceUpdatedDate <- as.Date(Aug16$PriceUpdatedDate)
colnames(Aug16)[6]<- "FuelCode" 

Sep16 <- read_excel("./Raw_data2/september.xlsx")
Sep16$PriceUpdatedDate <- as.Date(Sep16$PriceUpdatedDate)

Oct16 <- read_excel("./Raw_data2/october.xlsx")
Oct16$PriceUpdatedDate <- as.Date(Oct16$PriceUpdatedDate)

Nov16 <- read_excel("./Raw_data2/november.xlsx",skip=1)
Nov16$PriceUpdatedDate<- as.Date(Nov16$PriceUpdatedDate)

Dec16 <- read_excel("./Raw_data2/december.xlsx",skip=1)
Dec16$PriceUpdatedDate<- as.Date(Dec16$PriceUpdatedDate)

### All months data ### 
total <- rbind(Aug16,Sep16,Oct16,Nov16,Dec16)
total$PriceUpdatedDate <- as.Date(total$PriceUpdatedDate)
total <- total[!is.na(total$PriceUpdatedDate),]
colnames(total)[7] <- "Date"
plot(total$Date,total$Price) ##check for outliers

### Subset data to exclude outliers ### 
total <- subset(total, total$Price<=200)

### Create new data frame mean fuel price based on postcode, fuel code & date ###
postcodeavgprice <- aggregate(total$Price, by=list(total$Postcode,total$FuelCode,total$Date),FUN=mean)
colnames(postcodeavgprice)[1:4] <- c("Postcode", "FuelCode", "Date", "Average Price")

### Merge average price and all months data ###
mergedata <- merge(x=total, y=postcodeavgprice, by.x = c("Postcode", "FuelCode", "Date"), all.x=TRUE)
mergedata$FuelCode <- as.character(mergedata$FuelCode)
mergedata <- mergedata[complete.cases(mergedata),]

### Subset to only 4 main types of fuel ###
fuelcodedata <- subset(mergedata, subset = FuelCode %in% c("E10","U91","P95","P98"))

### subset to only include Sydney region based on postcode, new column based on month & formatting ###
sydneyfuel <- subset(fuelcodedata,Postcode >="2000" & Postcode<"2250")
sydneyfuel$Month <- format.Date(sydneyfuel$Date,"%m")
sydneyfuel$Day <- weekdays(as.Date(sydneyfuel$Date))
sydneyfuel$FuelCode <- factor(sydneyfuel$FuelCode,levels = c("E10","U91","P95","P98"))
sydneyfuel$Postcode <- as.numeric(sydneyfuel$Postcode)

### Plots ### 
if(!file.exists("Graphs")){
  dir.create("Graphs")
}
library(ggplot2)
g2 <- ggplot(subset(sydneyfuel,sydneyfuel$Month=="08"), aes(Date,Price))
g2 + geom_point(alpha=0.05,aes(color=FuelCode))+geom_smooth(aes(color=FuelCode),se=0) + coord_cartesian(ylim=c(90,160))+ labs(title="Fuel Prices during August 2016") +theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_aug_fuelprices.png")


g2 <- ggplot(subset(sydneyfuel,sydneyfuel$Month=="09"), aes(Date,Price))
g2 + geom_point(alpha=0.05,aes(color=FuelCode))+geom_smooth(aes(color=FuelCode),se=0)+ coord_cartesian(ylim=c(90,160))+ labs(title="Fuel Prices during September 2016") +theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_sep_fuelprices.png")

g2 <- ggplot(subset(sydneyfuel,sydneyfuel$Month=="10"), aes(Date,Price))
g2 + geom_point(alpha=0.05,aes(color=FuelCode))+geom_smooth(aes(color=FuelCode),se=0) + coord_cartesian(ylim=c(90,160))+ labs(title="Fuel Prices during October 2016") +theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_oct_fuelprices.png")

g2 <- ggplot(subset(sydneyfuel,sydneyfuel$Month=="11"), aes(Date,Price))
g2 + geom_point(alpha=0.05,aes(color=FuelCode))+geom_smooth(aes(color=FuelCode),se=0)+ coord_cartesian(ylim=c(90,160))+ labs(title="Fuel Prices during November 2016") +theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_nov_fuelprices.png")

g2 <- ggplot(subset(sydneyfuel,sydneyfuel$Month=="12"), aes(Date,Price))
g2 + geom_point(alpha=0.05,aes(color=FuelCode))+geom_smooth(aes(color=FuelCode),se=0)+ coord_cartesian(ylim=c(90,160))+ labs(title="Fuel Prices during December 2016") +theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_dec_fuelprices.png")

g<- ggplot(sydneyfuel,aes(Postcode,Price))
g+geom_smooth(method="lm",se=0,aes(color=FuelCode))+facet_grid(.~Month) + coord_cartesian(ylim=c(100,150)) + labs(title="Regression between Fuel Prices & Postcodes in Sydney Regions for Aug - Dec 2016")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_postcode_fuelprices_lm.png")

g+geom_smooth(se=0,aes(color=FuelCode))+facet_grid(.~Month) + coord_cartesian(ylim=c(100,150)) + ggtitle("Relationship between Fuel Prices & Postcodes in Sydney Regions for Aug - Dec 2016") +theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_postcode_fuelprices.png")

sydneyfuel$Day <- factor(sydneyfuel$Day, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
g3 <- ggplot(sydneyfuel,aes(jitter(as.numeric(Day)),Price))
g3+ geom_smooth(aes(color=FuelCode),se=0) + coord_cartesian(ylim=c(100,150))+ xlab("Day of Week")+ ggtitle("Relationship between Day of Week & Fuel Prices")+theme(plot.title=element_text(hjust=0.5))+theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_week_fuelprices.png")

sydneyfuel$Day <- factor(sydneyfuel$Day, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
g3 <- ggplot(sydneyfuel,aes(as.numeric(Day),Price))
g3+geom_smooth(method="lm",aes(color=FuelCode),se=0) + coord_cartesian(ylim=c(100,150))+ xlab("Day of Week")+ ggtitle("Regression between Day of Week & Fuel Prices")+theme(plot.title=element_text(hjust=0.5))+theme(plot.title=element_text(hjust=0.5,size=10),axis.title.x = element_text(size=9),axis.title.y = element_text(size=9),legend.title = element_text(size=9))
ggsave("./Graphs/sydney_week_fuelprices_lm.png")

### Regression ###
library(stargazer)

sydneyfuel$Day <- factor(sydneyfuel$Day, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
sydneyfuel$DayNum <- as.numeric(sydneyfuel$Day)

E10regress <- lm(Price~DayNum, data=sydneyfuel,sydneyfuel$FuelCode=="E10")
U91regress <- lm(Price~DayNum, data=sydneyfuel,sydneyfuel$FuelCode=="U91")
P95regress <- lm(Price~DayNum, data=sydneyfuel,sydneyfuel$FuelCode=="P95")
P98regress <- lm(Price~DayNum, data=sydneyfuel,sydneyfuel$FuelCode=="P98")
stargazer(E10regress,U91regress,P95regress,P98regress,align=TRUE,dep.var.labels = c("Fuel Types"),column.labels = c("E10","U91","P95","P98"),covariate.labels="Day of the Week",type="html",single.row = TRUE,out = "fuelcoderegression.txt")

residual <-resid(E10regress) ##quick residual analysis for E10 regression
E10residlm <- lm(residual~sydneyfuel[sydneyfuel$FuelCode=="E10",]$DayNum, data=sydneyfuel[sydneyfuel$FuelCode=="E10",])
plot(jitter(sydneyfuel[sydneyfuel$FuelCode=="E10",]$DayNum),residual,main = "Residual Plot against Day of Week", xlab = "Day of Week",ylab = "Residual") + abline(0,0)
