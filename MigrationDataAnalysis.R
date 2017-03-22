setwd('A:/CBA/R/PracticumFIles')
#install.packages('readxl')
#install.packages("ggplot2")
#install.packages("plotrix")
#install.packages('dplyr')
#install.packages('caroline')
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)
library(dplyr)
library(reshape2)
library(plotrix)
library(caroline)
library(stringr)


#People Sentiment analysis on EU migration problem association with Unemployment.

# datacsv <- read.csv('PeopleInterestOverTime_Unemployment.csv',nrows=155, skip=2)
# colnames(datacsv) <- c("MonthYear","SUnemployment","SEUemployment","SIndiaemployment","SPakistanemployment")
# Year <- substr(datacsv$MonthYear,1,4)
# datacsv <- cbind(datacsv,Year)
# write.csv(datacsv,"PeopleInterest.csv")

# datacsvCampaign <- read.csv('CampaignEffect.csv',nrows=155, skip=2)
# colnames(datacsvCampaign) <- c("MonthYear","CLeaveEU","CVoteLeave","CGetBritainOut","CBetterOffOut","Brexit")
# CYear <- substr(datacsvCampaign$MonthYear,1,4)
# datacsvCampaign <- cbind(datacsvCampaign,CYear)
# dftranspose <- melt(datacsv)
# Cdftranspose <- melt(datacsvCampaign)
# write.csv(dftranspose,"PeopleInterest.csv")
# write.csv(Cdftranspose,"CampaignImpact.csv")


##2015
dataxlsMigrants2015 <- data.frame(read_excel('Migrant2015.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2015 <- dataxlsMigrants2015[, -grep("\\NA.*", colnames(dataxlsMigrants2015))]
MigrantsData2015 <- MigrantsData2015[, -grep("\\All*", colnames(MigrantsData2015))]
MigrantsData2015 = MigrantsData2015[,c(1:3,5,7,9:32)]
names(MigrantsData2015)[1]<-"ZipCode"
names(MigrantsData2015)[2]<-"AreaName"
names(MigrantsData2015)[3]<-"TotalPopulation"
names(MigrantsData2015)[4]<-"NativePopulation"
names(MigrantsData2015)[5]<-"MigrantPopulation"
names(MigrantsData2015)[6]<-"EU"

MigrantsData2015 <- MigrantsData2015[, -grep("\\Var.", colnames(MigrantsData2015))]

modMigrtData2015 <- data.frame()
MigrantDataFrame2015 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2015)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2015 = MigrantsData2015[i,, drop=F]
    MigrantDataFrame2015 = rbind(MigrantDataFrame2015,modMigrtData2015)
    
  }
}
MigrantDataFrame2015 <- cbind(MigrantDataFrame2015,Year = c("2015"))
MigrantDataFrame2015 <- subset(MigrantDataFrame2015,!is.na(MigrantDataFrame2015$AreaName))
MigrantDataFrame2015 <- data.frame(apply(MigrantDataFrame2015,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2015 <- data.frame(apply(MigrantDataFrame2015,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2015 <- data.frame(apply(MigrantDataFrame2015,2,function(x)gsub('\\.',0,x)))

##2014
dataxlsMigrants2014 <- data.frame(read_excel('Migrant2014.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2014 <- dataxlsMigrants2014[, -grep("\\NA.*", colnames(dataxlsMigrants2014))]
MigrantsData2014 <- MigrantsData2014[, -grep("\\All*", colnames(MigrantsData2014))]
MigrantsData2014 = MigrantsData2014[,c(1:3,5,7,9:32)]
names(MigrantsData2014)[1]<-"ZipCode"
names(MigrantsData2014)[2]<-"AreaName"
names(MigrantsData2014)[3]<-"TotalPopulation"
names(MigrantsData2014)[4]<-"NativePopulation"
names(MigrantsData2014)[5]<-"MigrantPopulation"
names(MigrantsData2014)[6]<-"EU"

MigrantsData2014 <- MigrantsData2014[, -grep("\\Var.", colnames(MigrantsData2014))]

modMigrtData2014 <- data.frame()
MigrantDataFrame2014 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2014)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2014 = MigrantsData2014[i,, drop=F]
    MigrantDataFrame2014 = rbind(MigrantDataFrame2014,modMigrtData2014)
    
  }
}
MigrantDataFrame2014 <- cbind(MigrantDataFrame2014,Year = c("2014"))
MigrantDataFrame2014 <- subset(MigrantDataFrame2014,!is.na(MigrantDataFrame2014$AreaName))
MigrantDataFrame2014 <- data.frame(apply(MigrantDataFrame2014,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2014 <- data.frame(apply(MigrantDataFrame2014,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2014 <- data.frame(apply(MigrantDataFrame2014,2,function(x)gsub('\\.',0,x)))

##2013
dataxlsMigrants2013 <- data.frame(read_excel('Migrant2013.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2013 <- dataxlsMigrants2013[, -grep("\\NA.*", colnames(dataxlsMigrants2013))]
MigrantsData2013 <- MigrantsData2013[, -grep("\\All*", colnames(MigrantsData2013))]
MigrantsData2013 = MigrantsData2013[,c(1:3,5,7,9:32)]
names(MigrantsData2013)[1]<-"ZipCode"
names(MigrantsData2013)[2]<-"AreaName"
names(MigrantsData2013)[3]<-"TotalPopulation"
names(MigrantsData2013)[4]<-"NativePopulation"
names(MigrantsData2013)[5]<-"MigrantPopulation"
names(MigrantsData2013)[6]<-"EU"

MigrantsData2013 <- MigrantsData2013[, -grep("\\Var.", colnames(MigrantsData2013))]


modMigrtData2013 <- data.frame()
MigrantDataFrame2013 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2013)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2013 = MigrantsData2013[i,, drop=F]
    MigrantDataFrame2013 = rbind(MigrantDataFrame2013,modMigrtData2013)
    
  }
}
MigrantDataFrame2013 <- cbind(MigrantDataFrame2013,Year = c("2013"))
MigrantDataFrame2013 <- subset(MigrantDataFrame2013,!is.na(MigrantDataFrame2013$AreaName))
MigrantDataFrame2013 <- data.frame(apply(MigrantDataFrame2013,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2013 <- data.frame(apply(MigrantDataFrame2013,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2013 <- data.frame(apply(MigrantDataFrame2013,2,function(x)gsub('\\.',0,x)))

##2012
dataxlsMigrants2012 <- data.frame(read_excel('Migrant2012.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2012 <- dataxlsMigrants2012[, -grep("\\NA.*", colnames(dataxlsMigrants2012))]
MigrantsData2012 <- MigrantsData2012[, -grep("\\All*", colnames(MigrantsData2012))]
MigrantsData2012 = MigrantsData2012[,c(1:3,5,7,9:32)]
names(MigrantsData2012)[1]<-"ZipCode"
names(MigrantsData2012)[2]<-"AreaName"
names(MigrantsData2012)[3]<-"TotalPopulation"
names(MigrantsData2012)[4]<-"NativePopulation"
names(MigrantsData2012)[5]<-"MigrantPopulation"
names(MigrantsData2012)[6]<-"EU"

MigrantsData2012 <- MigrantsData2012[, -grep("\\Var.", colnames(MigrantsData2012))]


modMigrtData2012 <- data.frame()
MigrantDataFrame2012 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2012)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2012 = MigrantsData2012[i,, drop=F]
    MigrantDataFrame2012 = rbind(MigrantDataFrame2012,modMigrtData2012)
    
  }
}
MigrantDataFrame2012 <- cbind(MigrantDataFrame2012,Year = c("2012"))
MigrantDataFrame2012 <- subset(MigrantDataFrame2012,!is.na(MigrantDataFrame2012$AreaName))
MigrantDataFrame2012 <- data.frame(apply(MigrantDataFrame2012,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2012 <- data.frame(apply(MigrantDataFrame2012,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2012 <- data.frame(apply(MigrantDataFrame2012,2,function(x)gsub('\\.',0,x)))

##2011
dataxlsMigrants2011 <- data.frame(read_excel('Migrant2011.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2011 <- dataxlsMigrants2011[, -grep("\\NA.*", colnames(dataxlsMigrants2011))]
MigrantsData2011 <- MigrantsData2011[, -grep("\\All*", colnames(MigrantsData2011))]
MigrantsData2011 = MigrantsData2011[,c(1:3,5,7,9:32)]
names(MigrantsData2011)[1]<-"ZipCode"
names(MigrantsData2011)[2]<-"AreaName"
names(MigrantsData2011)[3]<-"TotalPopulation"
names(MigrantsData2011)[4]<-"NativePopulation"
names(MigrantsData2011)[5]<-"MigrantPopulation"
names(MigrantsData2011)[6]<-"EU"
MigrantsData2011 <- MigrantsData2011[, -grep("\\Var.", colnames(MigrantsData2011))]
modMigrtData2011 <- data.frame()
MigrantDataFrame2011 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2011)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2011 = MigrantsData2011[i,, drop=F]
    MigrantDataFrame2011 = rbind(MigrantDataFrame2011,modMigrtData2011)
    
  }
}
MigrantDataFrame2011 <- cbind(MigrantDataFrame2011,Year = c("2011"))
MigrantDataFrame2011 <- subset(MigrantDataFrame2011,!is.na(MigrantDataFrame2011$AreaName))
MigrantDataFrame2011 <- data.frame(apply(MigrantDataFrame2011,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2011 <- data.frame(apply(MigrantDataFrame2011,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2011 <- data.frame(apply(MigrantDataFrame2011,2,function(x)gsub('\\.',0,x)))

##2010
dataxlsMigrants2010 <- data.frame(read_excel('Migrant2010.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2010 <- dataxlsMigrants2010[, -grep("\\NA.*", colnames(dataxlsMigrants2010))]
MigrantsData2010 <- MigrantsData2010[, -grep("\\All*", colnames(MigrantsData2010))]
MigrantsData2010 = MigrantsData2010[,c(1:3,5,7,9:32)]
names(MigrantsData2010)[1]<-"ZipCode"
names(MigrantsData2010)[2]<-"AreaName"
names(MigrantsData2010)[3]<-"TotalPopulation"
names(MigrantsData2010)[4]<-"NativePopulation"
names(MigrantsData2010)[5]<-"MigrantPopulation"
names(MigrantsData2010)[6]<-"EU"
MigrantsData2010 <- MigrantsData2010[, -grep("\\Var.", colnames(MigrantsData2010))]
modMigrtData2010 <- data.frame()
MigrantDataFrame2010 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2010)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2010 = MigrantsData2010[i,, drop=F]
    MigrantDataFrame2010 = rbind(MigrantDataFrame2010,modMigrtData2010)
    
  }
}
MigrantDataFrame2010 <- cbind(MigrantDataFrame2010,Year = c("2010"))
MigrantDataFrame2010 <- subset(MigrantDataFrame2010,!is.na(MigrantDataFrame2010$AreaName))
MigrantDataFrame2010 <- data.frame(apply(MigrantDataFrame2010,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2010 <- data.frame(apply(MigrantDataFrame2010,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2010 <- data.frame(apply(MigrantDataFrame2010,2,function(x)gsub('\\.',0,x)))


##2009
dataxlsMigrants2009 <- data.frame(read_excel('Migrant2009.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2009 <- dataxlsMigrants2009[, -grep("\\NA.*", colnames(dataxlsMigrants2009))]
MigrantsData2009 <- MigrantsData2009[, -grep("\\All*", colnames(MigrantsData2009))]
MigrantsData2009 = MigrantsData2009[,c(1:3,5,7,9:32)]
names(MigrantsData2009)[1]<-"ZipCode"
names(MigrantsData2009)[2]<-"AreaName"
names(MigrantsData2009)[3]<-"TotalPopulation"
names(MigrantsData2009)[4]<-"NativePopulation"
names(MigrantsData2009)[5]<-"MigrantPopulation"
names(MigrantsData2009)[6]<-"EU"
MigrantsData2009 <- MigrantsData2009[, -grep("\\Var.", colnames(MigrantsData2009))]
modMigrtData2009 <- data.frame()
MigrantDataFrame2009 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2009)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2009 = MigrantsData2009[i,, drop=F]
    MigrantDataFrame2009 = rbind(MigrantDataFrame2009,modMigrtData2009)
    
  }
}
MigrantDataFrame2009 <- cbind(MigrantDataFrame2009,Year = c("2009"))
MigrantDataFrame2009 <- subset(MigrantDataFrame2009,!is.na(MigrantDataFrame2009$AreaName))
MigrantDataFrame2009 <- data.frame(apply(MigrantDataFrame2009,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2009 <- data.frame(apply(MigrantDataFrame2009,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2009 <- data.frame(apply(MigrantDataFrame2009,2,function(x)gsub('\\.',0,x)))

##2008
dataxlsMigrants2008 <- data.frame(read_excel('Migrant2008.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2008 <- dataxlsMigrants2008[, -grep("\\NA.*", colnames(dataxlsMigrants2008))]
MigrantsData2008 <- MigrantsData2008[, -grep("\\All*", colnames(MigrantsData2008))]
MigrantsData2008 = MigrantsData2008[,c(1:3,5,7,9:32)]
names(MigrantsData2008)[1]<-"ZipCode"
names(MigrantsData2008)[2]<-"AreaName"
names(MigrantsData2008)[3]<-"TotalPopulation"
names(MigrantsData2008)[4]<-"NativePopulation"
names(MigrantsData2008)[5]<-"MigrantPopulation"
names(MigrantsData2008)[6]<-"EU"
MigrantsData2008 <- MigrantsData2008[, -grep("\\Var.", colnames(MigrantsData2008))]
modMigrtData2008 <- data.frame()
MigrantDataFrame2008 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2008)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2008 = MigrantsData2008[i,, drop=F]
    MigrantDataFrame2008 = rbind(MigrantDataFrame2008,modMigrtData2008)
    
  }
}
MigrantDataFrame2008 <- cbind(MigrantDataFrame2008,Year = c("2008"))
MigrantDataFrame2008 <- subset(MigrantDataFrame2008,!is.na(MigrantDataFrame2008$AreaName))
MigrantDataFrame2008 <- data.frame(apply(MigrantDataFrame2008,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2008 <- data.frame(apply(MigrantDataFrame2008,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2008 <- data.frame(apply(MigrantDataFrame2008,2,function(x)gsub('\\.',0,x)))

##2007
dataxlsMigrants2007 <- data.frame(read_excel('Migrant2007.xls',sheet = 9, skip=7,col_names = TRUE))
MigrantsData2007 <- dataxlsMigrants2007[, -grep("\\NA.*", colnames(dataxlsMigrants2007))]
MigrantsData2007 <- MigrantsData2007[, -grep("\\All*", colnames(MigrantsData2007))]
MigrantsData2007 = MigrantsData2007[,c(1:3,5,7,9:32)]
names(MigrantsData2007)[1]<-"ZipCode"
names(MigrantsData2007)[2]<-"AreaName"
names(MigrantsData2007)[3]<-"TotalPopulation"
names(MigrantsData2007)[4]<-"NativePopulation"
names(MigrantsData2007)[5]<-"MigrantPopulation"
names(MigrantsData2007)[6]<-"EU"
MigrantsData2007 <- MigrantsData2007[, -grep("\\Var.", colnames(MigrantsData2007))]
modMigrtData2007 <- data.frame()
MigrantDataFrame2007 <- data.frame()
p <- 0
for (i in 1:nrow(MigrantsData2007)){
  if(i >2 & i<=402) {
    p=p+1
    modMigrtData2007 = MigrantsData2007[i,, drop=F]
    MigrantDataFrame2007 = rbind(MigrantDataFrame2007,modMigrtData2007)
    
  }
}
MigrantDataFrame2007 <- cbind(MigrantDataFrame2007,Year = c("2007"))
MigrantDataFrame2007 <- subset(MigrantDataFrame2007,!is.na(MigrantDataFrame2007$AreaName))
MigrantDataFrame2007 <- data.frame(apply(MigrantDataFrame2007,2,function(x)gsub('\\c',0,x)))
MigrantDataFrame2007 <- data.frame(apply(MigrantDataFrame2007,2,function(x)gsub('.000000',"",x)))
MigrantDataFrame2007 <- data.frame(apply(MigrantDataFrame2007,2,function(x)gsub('\\.',0,x)))




FinalMigrantDF <- rbind(MigrantDataFrame2015,MigrantDataFrame2014,MigrantDataFrame2013,MigrantDataFrame2012,MigrantDataFrame2011,
                        MigrantDataFrame2010,MigrantDataFrame2009,MigrantDataFrame2008,MigrantDataFrame2007)
AreaNameUK <- gsub(" ", "",(gsub('0',"c",FinalMigrantDF$AreaName)))
FinalMigrantDF <- data.frame(cbind(FinalMigrantDF,AreaNameUK,SEAsia = 
                                     c(as.numeric(FinalMigrantDF$South.East.Asia))))



#write.csv(FinalMigrantDF,"MigrantPopulation.csv")



FinalMigrantUK <- melt(subset(FinalMigrantDF,AreaName=="United Kingdom"),id.vars =c("ZipCode","AreaName","Year","AreaNameUK"))
TotalMigrants = (ifelse(FinalMigrantUK$variable == "MigrantPopulation",as.numeric(str_trim(FinalMigrantUK$value)),0))
DFMigrantPopulation <- data.frame()
DFMigrantPopulation <- cbind(FinalMigrantUK[,c(1:4)],TotalMigrants)
DFMigrantPopulation <- subset(DFMigrantPopulation,DFMigrantPopulation$TotalMigrants != "0")
FinalMigrantUK <- merge.data.frame(x=FinalMigrantUK,y=DFMigrantPopulation,id.vars =c("ZipCode","AreaName","Year","AreaNameUK"))
#write.csv(FinalMigrantUK,"MigrantNationality.csv")
MigrantPopulationDF <- subset(FinalMigrantUK,str_trim(FinalMigrantUK$variable) == "EU")
MigrantPopulationDF = MigrantPopulationDF[,c(3,6)]


FinalMigrantDF2015<- subset(FinalMigrantDF,Year == as.numeric(2015))
sumofVal = sum(as.numeric(FinalMigrantDF2015$South.East.Asia))
PercentOfSEAsia <- round((FinalMigrantDF2015$SEAsia/sumofVal)*100,digits=2)
FinalMigrantDF2015 <- cbind(FinalMigrantDF2015,PercentOfSEAsia)

googlesearchcity <- read.csv("PeopleSerachByCity.csv")
googlesearchcity<- data.frame(cbind(googlesearchcity,Year = c("2015")))

FinalGoogleplusPop <- melt(merge.data.frame(x=FinalMigrantDF2015,y=googlesearchcity,by = c("AreaNameUK","Year")),id.vars =c("ZipCode","AreaName","Year","AreaNameUK"))
write.csv(FinalGoogleplusPop,"MergedGoogleRealPopulation.csv")


########unemployment levels and comparision
dataxlsUnemployment <- data.frame(read_excel('unemploymentlevelsbycountryofbirthandnationality.xlsx',sheet = 2, skip=5,col_names = TRUE))
names(dataxlsUnemployment)[2]<-"YearMonth"
dataxlsUnemployment = dataxlsUnemployment[,c(2,4,6,7,9:21)]
unemploymentData <- dataxlsUnemployment[, -grep("\\NA.*", colnames(dataxlsUnemployment))]
names(unemploymentData)[2]<-"TotalUnemployed"
names(unemploymentData)[5]<-"EUUnemployed"
names(unemploymentData)[9]<-"NonEUUnemployed"
names(unemploymentData)[14]<-"PakistanBangladesh"
modUnempoyedData <- data.frame()
UnemployedDF <- data.frame()
p <- 0
for (i in 1:nrow(unemploymentData)){
  if(i >83 & i<161) {
    p=p+1
    modUnempoyedData = unemploymentData[i,, drop=F]
    UnemployedDF = rbind(UnemployedDF,modUnempoyedData)
    
  }
}
Year <- substr(UnemployedDF$YearMonth,nchar(UnemployedDF$YearMonth)-4+1, nchar(UnemployedDF$YearMonth))
MonthUnEmp <- substr(UnemployedDF$YearMonth,5,7)
UnemployedDF <- cbind(UnemployedDF,Year,MonthUnEmp)
UnemployedDF <- subset(UnemployedDF,sapply(MonthUnEmp,tolower) == "dec" )
UnemployedDF <- data.frame(apply(UnemployedDF,2,function(x)gsub('\\*',0,x)))
UnempoyedDFFinal <- melt(UnemployedDF,c("YearMonth","Year","MonthUnEmp"))
#write.csv(UnempoyedDFFinal,"UnemployedData.csv")

##merging unemplyment with migrant
MigrantPopulationDF <- merge(MigrantPopulationDF,subset(UnempoyedDFFinal,str_trim(UnempoyedDFFinal$variable) == "TotalUnemployed"),by="Year")
write.csv(MigrantPopulationDF,"MigrantTotalYearwise.csv")

boxplot(as.numeric(str_trim(UnemployedDF$TotalUnemployed)),main="EU Data",xlab = "EU",col = (c("green")))

##finding correlation between unemployment levels and migrant flow
EUMigrantDataUK <- subset(FinalMigrantUK,str_trim(FinalMigrantUK$variable) == "EU")
EUMigrantDataUK <- EUMigrantDataUK[,c(3,6)]
DFForMigrantCorr <- FinalMigrantUK
DFForMigrantCorr = unique(FinalMigrantUK[,c(3,7)])
DFForMigrantCorrF <- data.frame()
DFForMigrantCorrF <- merge(DFForMigrantCorr,EUMigrantDataUK,by="Year")

DFForUnemplCorr <- UnemployedDF
DFForUnemplCorr = UnemployedDF[,c(3,17)]


DFForUnemplCorr <- subset(DFForUnemplCorr,as.numeric(str_trim(DFForUnemplCorr$Year))>=as.numeric(2007))
DFForUnemplCorr <- within(DFForUnemplCorr,UK <- as.numeric(str_trim(UK)))

DFForMigrantCorrF <- within(DFForMigrantCorrF,TotalMigrants <- as.numeric(str_trim(DFForMigrantCorrF$TotalMigrants))*1000)
DFForMigrantCorrF <- within(DFForMigrantCorrF,value <- as.numeric(str_trim(DFForMigrantCorrF$value))*1000)
NonEUMigrant <- DFForMigrantCorrF$TotalMigrants-DFForMigrantCorrF$value
DFForMigrantCorrF <- cbind(DFForMigrantCorrF,NonEUMigrant)
DFForUnemplCorr <- within(DFForUnemplCorr,UK <- as.numeric(str_trim(DFForUnemplCorr$UK)))

FinalDFCorr <- data.frame()
FinalDFCorr <- merge(DFForMigrantCorrF,DFForUnemplCorr,by="Year")


# between total migrants and unemployment

cor.test(FinalDFCorr$TotalMigrants,FinalDFCorr$UK,method="pearson")

# between total EU migrants and unemployment

cor.test(FinalDFCorr$value,FinalDFCorr$UK,method="pearson")

# between total non EU migrants and unemployment

cor.test(FinalDFCorr$NonEUMigrant,FinalDFCorr$UK,method="pearson")


###Skill mismatch
########unemployment levels and comparision
dataxlsSkills <- data.frame(read_excel('SkillMismatch.xls',sheet = 7, skip=3,col_names = TRUE))
names(dataxlsSkills)[1]<-"YearMonth"
names(dataxlsSkills)[3]<-"UK-OverEducated"
names(dataxlsSkills)[4]<-"UK-UnderEducated"
names(dataxlsSkills)[7]<-"EU14-OverEducated"
names(dataxlsSkills)[8]<-"EU14-UnderEducated"
names(dataxlsSkills)[11]<-"EU10-OverEducated"
names(dataxlsSkills)[12]<-"EU10-UnderEducated"
names(dataxlsSkills)[15]<-"ROW-OverEducated"
names(dataxlsSkills)[16]<-"ROW-UnderEducated"

dataskillDF <- dataxlsSkills[, -grep("\\Var.*", colnames(dataxlsSkills))]

modSkillData <- data.frame()
SkillDF <- data.frame()
p <- 0
for (i in 1:nrow(dataskillDF)){
  if(i >1 & i<57) {
    p=p+1
    modSkillData = dataskillDF[i,, drop=F]
    SkillDF = rbind(SkillDF,modSkillData)
    
  }
}
Year <- substr(SkillDF$YearMonth,nchar(SkillDF$YearMonth)-4+1, nchar(SkillDF$YearMonth))
MonthSkill <- substr(SkillDF$YearMonth,5,7)
SkillDF <- cbind(SkillDF,Year,MonthSkill)
SkillDF <- subset(SkillDF,sapply(SkillDF$MonthSkill,tolower) == "dec" )
SkillDF <- data.frame(apply(SkillDF,2,function(x)gsub('\\:',0,x)))
SkillFinalDF <- melt(SkillDF,id.vars =c("YearMonth","Year","MonthSkill"))
write.csv(SkillFinalDF,"SkilledDataForReporting.csv")

##Vacancies
dataxlsVacancy <- data.frame(read_excel('vacs02oct2016.xls',sheet = 1, skip=6,col_names = TRUE))
names(dataxlsVacancy)[1]<-"YearMonth"
names(dataxlsVacancy)[2]<-"AllVacancies"
dataxlsVacancy <- dataxlsVacancy[,c(1,2)]


#dataxlsVacancy <- dataxlsVacancy[, -grep("\\Var.*", colnames(dataxlsVacancy))]

modVacnData <- data.frame()
VacancyDF <- data.frame()
p <- 0
for (i in 1:nrow(dataxlsVacancy)){
  if(i >1 & i<186) {
    p=p+1
    modVacnData = dataxlsVacancy[i,, drop=F]
    VacancyDF = rbind(VacancyDF,modVacnData)
    
  }
}
Year <- substr(VacancyDF$YearMonth,nchar(VacancyDF$YearMonth)-4+1, nchar(VacancyDF$YearMonth))
MonthVacancy <- substr(VacancyDF$YearMonth,5,7)
VacancyDF <- cbind(VacancyDF,Year,MonthVacancy)
VacancyDF <- subset(VacancyDF,sapply(VacancyDF$MonthVacancy,tolower) == "dec" )
write.csv(VacancyDF,"VacancyDataForReporting.csv")
