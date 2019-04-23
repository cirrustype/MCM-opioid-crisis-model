#MCM 2019 
#Biana Ramirez, Jade williams, and Austin Schenk 

##### NFLS data#####
Data <- read.csv(file.choose())
View(Data)
attach(Data)
Data <- as.data.frame(Data)

State <- as.factor(State)
COUNTY <- as.factor(COUNTY)
SubstanceName <- as.factor(SubstanceName)

YYYY <- as.factor(YYYY)
FIPS_Combined <- as.factor(FIPS_Combined)
levels(YYYY)
dim(Data)

##############Data structure and renaming varaibles###########
levels(COUNTY)
County = unique(COUNTY)
Drug = unique(SubstanceName)
CountyCode = unique(FIPS_County) #county FIPS code
StateCode = unique(FIPS_State)#state FIPS code
CombinedCode = unique(FIPS_Combined) #combined county and state FIPS code

DR = DrugReports # counts of drug (opiate) incidents by substance for each county. 
TDRC = TotalDrugReportsCounty #sum of all incidents for each county 
TDRS = TotalDrugReportsState #sum of all incidents for each state

##########Year specific Data###############

#parsing by year

#Data2010 = Data[YYYY[YYYY = 2010]] was only pulling mason, kY or nothing. Fixed with which command below...
Data2010 = Data[which(YYYY == "2010"),]
Data2011 = Data[which(YYYY == "2011"),]
Data2012 = Data[which(YYYY == "2012"),]
Data2013 = Data[which(YYYY == "2013"),]
Data2014 = Data[which(YYYY == "2014"),]
Data2015 = Data[which(YYYY == "2015"),]
Data2016 = Data[which(YYYY == "2016"),]
Data2017 = Data[which(YYYY == "2017"),]

length(CombinedCode)



i = 0 
RateOI = numeric(length = 461)
OI2010 <- numeric(length = 461)
OI2017 <- numeric(length = 461)

############# Rate of Change Opiate Incidents ###############
CombinedCode
x = 0
for (x in 0:460)
{
  OI2010[x] = sum(Data$DrugReports[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2010")])
  OI2017[x] = sum(Data$DrugReports[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2017")])
  RateOI[x] = OI2010[x]/OI2017[x]
  x = x+1
}

OIData <- data.frame(CombinedCode,OI2010,OI2017,RateOI)
View(OIData)

###################### Drug Incidences (DI)#####################
RateDI = numeric(length = 461)
DI2010 <- numeric(length = 461)
DI2017 <- numeric(length = 461)

x = 0
for (x in 0:460)
{
  DI2010[x] = mean(Data$TotalDrugReportsCounty[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2010")])
  DI2017[x] = mean(Data$TotalDrugReportsCounty[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2017")])
  RateDI[x] = DI2010[x]/DI2017[x]
  x = x+1
}

DIData <- data.frame(CombinedCode,DI2010,DI2017,RateDI)
View(DIData)

########### DI for 2011-2016 #############
DI2011 <- numeric(length = 461)
for (x in 0:460)
{
  DI2011[x] = mean(Data$TotalDrugReportsCounty[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2011")])
  x = x+1
  }
DI2012 <- numeric(length = 461)

for (x in 0:460)
{
  DI2012[x] = mean(Data$TotalDrugReportsCounty[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2012")])
  x = x+1
}
DI2013 <- numeric(length = 461)
for (x in 0:460)
{
  DI2013[x] = mean(Data$TotalDrugReportsCounty[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2013")])
  x = x+1
}

DI2014 <- numeric(length = 461)
for (x in 0:460)
{
  DI2014[x] = mean(Data$TotalDrugReportsCounty[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2014")])
  x = x+1
}

DI2015 <- numeric(length = 461)
for (x in 0:460)
{
  DI2015[x] = mean(Data$TotalDrugReportsCounty[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2015")])
  x = x+1
}

DI2016 <- numeric(length = 461)
for (x in 0:460)
{
  DI2016[x] = mean(Data$TotalDrugReportsCounty[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2016")])
  x = x+1
}

########### OI for 2011-2016 #############

OI2011 <- numeric(length = 461)
for (x in 0:460)
{
  OI2011[x] = sum(Data$DrugReports[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2011")])
  x = x+1
}
OI2012 <- numeric(length = 461)
for (x in 0:460)
{
  OI2012[x] = sum(Data$DrugReports[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2012")])
  x = x+1
}
OI2013 <- numeric(length = 461)
for (x in 0:460)
{
  OI2013[x] = sum(Data$DrugReports[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2013")])
  x = x+1
}
OI2014 <- numeric(length = 461)
for (x in 0:460)
{
  OI2014[x] = sum(Data$DrugReports[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2014")])
  x = x+1
}
OI2015 <- numeric(length = 461)
for (x in 0:460)
{
  OI2015[x] = sum(Data$DrugReports[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2015")])
  x = x+1
}
OI2016 <- numeric(length = 461)
for (x in 0:460)
{
  OI2016[x] = sum(Data$DrugReports[which(Data$FIPS_Combined == CombinedCode[x] & YYYY == "2016")])
  x = x+1
}

############## OD Ratios #########
Diff_OIandDI <- RateDI - RateOI
ODratio2010 <- OI2010/DI2010
ODratio2011 <- OI2011/DI2011
ODratio2012 <- OI2012/DI2012
ODratio2013 <- OI2013/DI2013
ODratio2014 <- OI2014/DI2014
ODratio2015 <- OI2015/DI2015
ODratio2016 <- OI2016/DI2016
ODratio2017 <- OI2017/DI2017
Movement <- ODratio2017-ODratio2010

ODratioAVG <- (ODratio2010+ODratio2011+ODratio2012+ODratio2013+
                 ODratio2014+ODratio2015+ODratio2016+ODratio2017)/7
Diff_OIandDI
Movement
ODratioAVG

############### Incident DATA #########
IncidentData <- data.frame(CombinedCode,OI2010,OI2011,OI2012,OI2013,OI2014,OI2015,OI2016,OI2017,RateOI
                           ,DI2010,DI2011,DI2012,DI2013,DI2014,DI2015,DI2016,DI2017,RateDI,
                           Diff_OIandDI,ODratio2010,ODratio2017,Movement,ODratioAVG)
View(IncidentData)

################ Difference Density #############
logDiffOIDi <- log(Diff_OIandDI)
plot(density(Diff_OIandDI, na.rm = T))
plot(density(logDiffOIDi, na.rm = T)
     , main = "Density of Differences \n in All Counties"
     , xlab = "Logarithm of Difference")
# #Density plot of LOG differences between drug incident rates and
# # opiate incidents rates 
density(Diff_OIandDI, na.rm = T)

length(Diff_OIandDI)
boxplot(Diff_OIandDI)
summary(Diff_OIandDI)

############First round of county classification#########
attach(IncidentData)
dim(IncidentData)

quantile(Diff_OIandDI, .16, na.rm = T)
summary(Diff_OIandDI)

TFCrisis <- IncidentData$Diff_OIandDI < -0.8390523
TFCrisis
data.frame(IncidentData$CombinedCode, TFCrisis)

while(IncidentData$Diff_OIandDI <= -0.8390523) CombinedCode

install.packages("xlsx")
write.xlsx(IncidentData, "c:/incidentdata.xlsx")


############Second Round of County Classification######
plot(density(ODratioAVG, na.rm = T)
     , main = "Density of OD Ratio Averages \n of All Counties"
     , xlab = "OD Ratio Averages")
quantile(ODratioAVG, .84, na.rm = T)
AvgTFCrisis <- IncidentData$ODratioAVG > 0.5307335
data.frame(IncidentData$CombinedCode, AvgTFCrisis)

########CRISIS COUNTIES############
CRISIS <- list("21205","51169","21195","21175","21197",
           "21161","21127","21095","21089","21119",
           "51071","21063","21013")

#########interaction plots##############
par(mfrow = c(2,1))
par(mgp = c(2.5, 1, 0))

 
interaction.plot(Data$YYYY[which(Data$State == "KY" & Data$`O/D` > 0.15)]
                 ,FIPS_Combined[which(Data$State == "KY" & Data$`O/D` > 0.15)]
                 ,Data$`O/D`[which(Data$State == "KY" & Data$`O/D` > 0.15)]
                 , legend = F, xlab = "Year", ylab = "OD Ratios"
                 , las = 1)
title("Kentucky", line = -1)

interaction.plot(Data$YYYY[which(Data$State == "PA" & Data$`O/D` > 0.15)]
                 ,FIPS_Combined[which(Data$State == "PA" & Data$`O/D` > 0.15)]
                 ,Data$`O/D`[which(Data$State == "PA" & Data$`O/D` > 0.15)]
                 , legend = F, xlab = "Year", ylab = "OD Ratios"
                 , las = 1, ylim = c(0.15,0.7))
title("Pennsylvania", line = -1)

interaction.plot(Data$YYYY[which(Data$State == "WV" & Data$`O/D` > 0.15)]
                 ,FIPS_Combined[which(Data$State == "WV" & Data$`O/D` > 0.15)]
                 ,Data$`O/D`[which(Data$State == "WV" & Data$`O/D` > 0.15)]
                 , legend = F, xlab = "Year", ylab = "OD Ratios"
                 , las = 1)
title("West Virginia", line = -1)

interaction.plot(Data$YYYY[which(Data$State == "OH" & Data$`O/D` > 0.15)]
                 ,FIPS_Combined[which(Data$State == "OH" & Data$`O/D` > 0.15)]
                 ,Data$`O/D`[which(Data$State == "OH" & Data$`O/D` > 0.15)]
                 , legend = F, xlab = "Year", ylab = "OD Ratios"
                 , las = 1, ylim = c(0.15,0.8))
title("Ohio", line = -1)

interaction.plot(Data$YYYY[which(Data$State == "VA" & Data$`O/D` > 0.15)]
                 ,FIPS_Combined[which(Data$State == "VA" & Data$`O/D` > 0.15)]
                 ,Data$`O/D`[which(Data$State == "VA" & Data$`O/D` > 0.15)]
                 , legend = F, xlab = "Year", ylab = "OD Ratios"
                 , las = 1, ylim = c(0.15,0.9))
title("Virginia", line = -1)

###############Classifying County Status##########
###########Difference############
summary(IncidentData$Diff_OIandDI)
IncidentData$Diff_OIandDI[is.na(IncidentData$Diff_OIandDI)] <- 0.04561
quantile(IncidentData$Diff_OIandDI, .16, na.rm =T)
i = 0
DiffStatus = numeric(length = 461)
for (i in 1:461)
{
  if(IncidentData$Diff_OIandDI[i] <= -0.7444609){DiffStatus[i] = "PC"}
  else {DiffStatus[i] = "NR"}
  i = i+1
}
DiffStatus

##########Average############
IncidentData$ODratioAVG
summary(IncidentData$ODratioAVG)

IncidentData$ODratioAVG[is.na(IncidentData$ODratioAVG)] <- 0.36963
i = 0
ODStatus = numeric(length = 461)
for (i in 1:461)
{
  if(IncidentData$ODratioAVG[i] >= 0.5307335){ODStatus[i] = "PC"}
  else {ODStatus[i] = "NR"}
  i = i+1
}
ODStatus 

###########Movement#######
summary(IncidentData$Movement)
quantile(IncidentData$Movement, .84, na.rm = T)
IncidentData$Movement
IncidentData$Movement[is.na(IncidentData$Movement)] <- 0.02016

i = 0
MoveStatus = numeric(length = 461)
for (i in 1:461)
{
  if(IncidentData$Movement[i] >= 0.1981202){MoveStatus[i] = "PC"}
  else {MoveStatus[i] = "NR"}
  i = i+1
}
MoveStatus 

#####classification rules######
i = 0
Status = numeric(length = 461)
for (i in 1:461)
{
  if(DiffStatus[i]=="PC" & ODStatus[i]=="PC" & MoveStatus[i]=="PC"){Status[i] = "C"}
  else if(DiffStatus[i]=="PC" & ODStatus[i]=="PC" & MoveStatus[i]=="NR"){Status[i] = "C"}
  else if(DiffStatus[i]=="PC" & ODStatus[i]=="NR" & MoveStatus[i]=="PC"){Status[i] = "ER"}
  else if(DiffStatus[i]=="NR" & ODStatus[i]=="PC" & MoveStatus[i]=="PC"){Status[i] = "ER"}
  else if(DiffStatus[i]=="NR" & ODStatus[i]=="NR" & MoveStatus[i]=="PC"){Status[i] = "AR"}
  else if(DiffStatus[i]=="NR" & ODStatus[i]=="PC" & MoveStatus[i]=="NR"){Status[i] = "AR"}
  else if(DiffStatus[i]=="PC" & ODStatus[i]=="NR" & MoveStatus[i]=="NR"){Status[i] = "AR"}
  else {Status[i] = "NR"}
  i = i+1
}
View(Status)

length(Status[Status == "C"])
length(Status[Status == "ER"])
length(Status[Status == "AR"])
length(Status[Status == "NR"])

# ###########installation of mapping software##############
# install.packages("devtools")
# devtools::install_github("UrbanInstitute/urbnmapr")
# devtools::install_github("UI-Research/urbnthemes")
# 
# x <- c("ggmap", "rgdal", "rgeos", "maptools", "GGally", "dplyr", 
#        "urbnmapr", "tidycensus", "forcats", "urbnthemes","tmap", 
#        "mapdata", "maps", "tidyverse", "magrittr", "gridExtra", 
#        "RJSONIO", "leaflet", "stringr", "sf", "viridis")
# 
# install.packages(x) 
# 
# lapply(x, library, character.only = T) # load the required packages

##########Drug Identification##############
#this takes a long time to run.

CompleteStatus <- numeric(length = 24062)
i = 0
j = 0
for (i in 1:24062)
{
  for (j in 1:461)
  {
    if (FIPS_Combined[i] == CombinedCode[j])
    {CompleteStatus[i] = Status[j]}
    else j = j+1
  }
  i = i+1
}
View(CompleteStatus)
SubstanceData <- data.frame(Data$COUNTY
                            , Data$FIPS_Combined
                            , Data$SubstanceName
                            , CompleteStatus, Data$DrugReports)
View(SubstanceData)
install.packages("xlsx")

write.csv(SubstanceData, file = file.choose(new = T))
write.csv(IncidentData, file = file.choose(new = T))

########Substance Driver finders#############

SubstanceData <- read.csv(file.choose())
attach(SubstanceData)
DriverData <- data.frame(SubstanceData, Data$DrugReports, Data$`O/D`, Data$TotalDrugReportsCounty, Data$TotalDrugReportsState)
length(Drug)
DrugStatus <- numeric(length = 24062)
i= 0
j = 0
for (i in 1:24062)
{
    for (j in 1:69)
    {
      if (SubstanceData$Data.SubstanceName[i] == Drug[j])
      {
        if (CompleteStatus[i] == "NR") {DrugStatus[i] = "ND"}
        else if (CompleteStatus[i] == "AR") {DrugStatus[i] = "PD"}
        else if (CompleteStatus[i] == "C" | CompleteStatus[i] == "ER") {DrugStatus[i] = "D"}
      }
      else {j = j+1}
    }
    i = i+1
} 

length(DrugStatus[DrugStatus=="D"])
length(DrugStatus[DrugStatus=="PD"])       
length(DrugStatus[DrugStatus=="ND"])

DriverData <- data.frame(SubstanceData, Data$DrugReports, Data$`O/D`, Data$TotalDrugReportsCounty, Data$TotalDrugReportsState,
                         DrugStatus)

View(DriverData)
#############Count Comparisons of DRIVER in CRISIS ##################
attach(DriverData)

CrisisDriverData <- subset(DriverData[which((CompleteStatus == "C" | CompleteStatus == "ER") & DrugStatus == "D"),])
dim(CrisisDriverData)
View(CrisisDriverData)

df <- data.frame(CrisisDriverData$Data.SubstanceName, CrisisDriverData$Data.FIPS_Combined)
dim(df)
head(df)
plot(as.factor(df$CrisisDriverData.Data.SubstanceName))
table(as.factor(df$CrisisDriverData.Data.SubstanceName))
df
plot(as.factor(df$CrisisDriverData.Data.FIPS_Combined))
table(as.factor(df$CrisisDriverData.Data.FIPS_Combined))

##########Complicated#############
# #Future Research
# 
# DrugSums <- matrix(,nrow = 461,ncol = 69)
# i = 0
# x = 0
# j = 0
# 
# for (i in 1:461)
# {
#   for (j in 1:24062)
#   {
#     if (CombinedCode[i] == FIPS_Combined[j])
#     {
#       for (x in 1:69)
#       {
#       if (SubstanceName[j] == Drug[x])
#       {DrugSums[i,x] = sum(DrugReports)}
#       else {x = x+1}
#       }
#     }
#     else {j = j + 1}
#   }
#   i = i + 1
# }
# 
# View(DrugSums)

#############Part 2###############
install.packages("leaps")
library(leaps)
library(readxl)

data <- read_xlsx(file.choose())

attach(data)

mods <- regsubsets(Odavg ~., data=data, nbest = 5)

full <- lm(Odavg ~ HC01_VC40 +HC01_VC53 +HC01_VC150+ HC01_VC161+
              HC01_VC166 +HC01_VC174+HC01_VC176 +HC01_VC188+ HC01_VC189+
              HC01_VC193+ HC01_VC198+ HC01_VC199 +HC01_VC201+
              HC01_VC203+ HC01_VC206+ HC01_VC208+HC01_VC209+HC01_VC210)

par(mfrow = c(1,1))        
plot(full)

summary(full)
summary(full)$xnames
regsub
names(full)
plot(full, scale="adjr2")
install.packages("leaps")
library(leaps)

ODlm <- lm(Odavg ~ HC01_VC150+HC01_VC161+HC01_VC166+HC01_VC176
           +HC01_VC203+HC01_VC206+HC01_VC209+HC01_VC210, data=data)

plot(ODlm)

summary(ODlm)

corre <- data.frame(round(cor(data),2))
cor()
OD <- data.frame(corre[,1], corre[,2])
OD
ODneg <-as.vector(corre)


library(readxl)
final <- read_excel(file.choose())
View(final)


