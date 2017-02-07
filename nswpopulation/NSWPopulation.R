setwd("ABC") ##please set local directory
nswpop <- read.csv("NSW.csv", skip=1,header=TRUE)

############# Extract & clean data ############# 

malestart <- which(colnames(nswpop)=="X0")
maleend <- which(colnames(nswpop)=="X85.Plus")
femalestart <- which(colnames(nswpop)=="X0.1")
femaleend <- which(colnames(nswpop)=="X85.Plus.1")
afterlastcol <- which(colnames(nswpop)=="Total.Population")+1
agegap = maleend - malestart

nswpop[ ,afterlastcol:(afterlastcol+agegap)] = NA ##creating columns for population taking into account both male & female figs.

colnames(nswpop)[afterlastcol:(afterlastcol+agegap)] = paste(colnames(nswpop)[malestart:maleend],".2",sep="") ##renaming sum of male & female population columns

totalstart <- which(colnames(nswpop)=="X0.2")
totalend <- which(colnames(nswpop)=="X85.Plus.2")

nswpop[ ,totalstart:totalend] = nswpop[ ,malestart:maleend] + nswpop[ ,femalestart:femaleend] ##summing male & female population.

nswpop <- nswpop[ ,c(1:3,totalstart:totalend)] #subsetting total male & female population

############# Creating total population and population growth over year for individual SA2 location ############# 

newstart <- which(colnames(nswpop)=="X0.2")
newend <- which(colnames(nswpop)=="X85.Plus.2")

any(is.na(nswpop[ ,newstart:newend])) ##check if any fields are NA

nswpop <- nswpop[order(nswpop$SA2.Code,nswpop$Year),] ##order based on SA2 code and year 

nswpop[ ,"totalpop"] <- apply(nswpop[newstart:newend],1,sum,na.rm=TRUE) ##calculate total population per SA2 & year

############# Formatting ############# 

nswpop$Year <- as.Date(ISOdate(nswpop$Year,1,1)) ##formatting of year integer to date format.

############# Total pop growth rate between 2013 and 2027 ############# 
popgrowth <- subset(nswpop, nswpop$Year=='2016-01-01' | nswpop$Year=='2027-01-01') ##create new data frame based on data between 2016-2027.
popgrowth <- subset(popgrowth[, c(1:3,which(colnames(popgrowth)=="totalpop"))]) ##subset data for population data only

popgrowth$popchange <- rep(NA,n=nrow(popgrowth)) ##calculates net pop change between 2016-27
for (i in 2:nrow(popgrowth)){ 
  if (popgrowth[i,which(colnames(popgrowth)=="SA2.Code")] == popgrowth[i-1,which(colnames(popgrowth)=="SA2.Code")]){
    popgrowth[i,which(colnames(popgrowth)=="popchange")] = (popgrowth[i,which(colnames(popgrowth)=="totalpop")]-popgrowth[i-1,which(colnames(popgrowth)=="totalpop")])
  }
  else {
    popgrowth[i,which(colnames(popgrowth)=="popchange")] = NA
  }
}

popgrowth$popchangerate <- rep(NA,n=nrow(popgrowth)) ##calculates pop growth rate between 2016-27
for (i in 2:nrow(popgrowth)){
  if (popgrowth[i,which(colnames(popgrowth)=="SA2.Code")] == popgrowth[i-1,which(colnames(popgrowth)=="SA2.Code")]){
    popgrowth[i,which(colnames(popgrowth)=="popchangerate")] = (popgrowth[i,which(colnames(popgrowth)=="totalpop")]/popgrowth[i-1,which(colnames(popgrowth)=="totalpop")])-1
  }
  else {
    popgrowth[i,which(colnames(popgrowth)=="popchangerate")] = NA
  }
}

popgrowth <- popgrowth[complete.cases(popgrowth),] ##removes 2016 line for each SA2 as pop change reflected in new columns.
popgrowth <- subset(popgrowth[,c(2:6)]) ##removes date yield as reflected in data already. 

popgrowth$'2016pop'<- popgrowth$totalpop-popgrowth$popchange ##new column showing 2016 population

popgrowth$Rank16 <- rep(NA,nrow(popgrowth)) ##create ranking for SA2 areas based on 2016 population. 
popgrowth <- popgrowth[order(popgrowth$`2016pop`,decreasing = TRUE),]
for (i in 1:nrow(popgrowth)){
  popgrowth[i,7] <- i
}

popgrowth$Rank27 <- rep(NA, nrow(popgrowth)) ##create ranking for SA2 areas based on population growth rate between 2016-27 
popgrowth <- popgrowth[order(popgrowth$popchangerate,decreasing = TRUE),]
for (i in 1:nrow(popgrowth)){
  popgrowth[i,8] <- i
}

popgrowth <- popgrowth[,c(8,1,2,6,3,4,5,7)] ##tidying data frame
colnames(popgrowth) <- c("Overall % Growth Rank","SA2 Code", "SA2 Suburb", "2016 Pop","2027 Pop","Net Pop Change 2016-2027", "% Change", "2016 Pop Rank")

############# Match ABS postcode to SA2 locations for Tableau mapping ############# 
postcodes <- read.csv("SA2_Postcode.csv",header = TRUE)
postcodes <- postcodes[order(postcodes$SA2_NAME_2011),]
mappopgrowth <- merge(x=postcodes,y=popgrowth, by.x="SA2_NAME_2011", by.y = "SA2 Suburb", all.x=TRUE) ##new data frame, left join ABS postcode data to population growth data
mappopgrowth <- mappopgrowth[complete.cases(mappopgrowth),] ##remove any NA fields as SA2 postcodes contain whole of Australia. 
mappopgrowth <- mappopgrowth[order(mappopgrowth$`Overall % Growth Rank`),]

############# Write results ############# 
write.csv(popgrowth,file="popgrowth.csv",row.names = FALSE)
write.csv(mappopgrowth, file="mappopgrowth.csv", row.names = FALSE)
