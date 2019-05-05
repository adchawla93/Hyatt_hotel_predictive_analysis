#install packages
install.packages("sqldf")
install.packages("ggplot2")
install.packages("plyr")
install.packages("dplyr")
install.packages("maps")
install.packages("arules")
install.packages("arulesViz")
install.packages("ggmap")
install.packages("gridExtra")
install.packages("kernlab")
install.packages("e1071")
install.packages("moments")

#Load Packages
library(sqldf)
library(ggplot2)
library(plyr)
library(dplyr)
library(maps)
library(arules)
library(arulesViz)
library(ggmap) #for geocoding
library(gridExtra)
library(kernlab)
library(e1071)
library(moments)



#WINDOWS
setwd("E:\\OneDrive\\Documents\\School Work\\Syracuse\\Applied Data Science\\project_data\\totalData")
#or...
setwd("E:\\FreyGeospatial\\Documents\\IST687-data\\IST687-data")


#MAC
setwd("/Users/jordanfrey/Documents/OneDrive/Documents/School Work/Syracuse/Applied Data Science/project_data/totalData")


#READ DATA

febData <- read.csv("out-201402.csv")[,c("Room_Type_H", "Gender_H", "CHILDREN_NUM_C", "Laundry_PL", "NUM_ROOMS_R", "Pool.Indoor_PL", "Casino_PL", "Convention_PL", "Dry.Cleaning_PL", "Business.Center_PL",
                                         "Conference_PL","NPS_Type", "Likelihood_Recommend_H", "Fitness.Center_PL", "Golf_PL", "Limo.Service_PL","Mini.Bar_PL",
                                         "Pool.Outdoor_PL", "Resort_PL", "Spa_PL", "GP_Tier", "COUNTRY_CODE_R", "Country_PL", "State_PL", "City_PL", "Guest_Room_H", "Condition_Hotel_H", "Customer_SVC_H",
                                         "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Tranquility_H", "POV_CODE_C", "Brand_PL", "Property.Latitude_PL", "Property.Longitude_PL")]

mayData <- read.csv("out-201405.csv")[,c("Room_Type_H", "Gender_H", "CHILDREN_NUM_C", "Laundry_PL", "NUM_ROOMS_R", "Pool.Indoor_PL", "Casino_PL", "Convention_PL", "Dry.Cleaning_PL", "Business.Center_PL",
                                         "Conference_PL","NPS_Type", "Likelihood_Recommend_H", "Fitness.Center_PL", "Golf_PL", "Limo.Service_PL","Mini.Bar_PL",
                                         "Pool.Outdoor_PL", "Resort_PL", "Spa_PL", "GP_Tier", "COUNTRY_CODE_R", "Country_PL", "State_PL", "City_PL", "Guest_Room_H", "Condition_Hotel_H", "Customer_SVC_H",
                                         "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Tranquility_H", "POV_CODE_C", "Brand_PL", "Property.Latitude_PL", "Property.Longitude_PL")]

augData <- read.csv("out-201408.csv")[,c("Room_Type_H", "Gender_H", "CHILDREN_NUM_C", "Laundry_PL", "NUM_ROOMS_R", "Pool.Indoor_PL", "Casino_PL", "Convention_PL", "Dry.Cleaning_PL", "Business.Center_PL",
                                         "Conference_PL","NPS_Type", "Likelihood_Recommend_H", "Fitness.Center_PL", "Golf_PL", "Limo.Service_PL","Mini.Bar_PL",
                                         "Pool.Outdoor_PL", "Resort_PL", "Spa_PL", "GP_Tier", "COUNTRY_CODE_R", "Country_PL", "State_PL", "City_PL", "Guest_Room_H", "Condition_Hotel_H", "Customer_SVC_H",
                                         "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Tranquility_H", "POV_CODE_C", "Brand_PL", "Property.Latitude_PL", "Property.Longitude_PL")]

novData <- read.csv("out-201411.csv")[,c("Room_Type_H", "Gender_H", "CHILDREN_NUM_C", "Laundry_PL", "NUM_ROOMS_R", "Pool.Indoor_PL", "Casino_PL", "Convention_PL", "Dry.Cleaning_PL", "Business.Center_PL",
                                         "Conference_PL","NPS_Type", "Likelihood_Recommend_H", "Fitness.Center_PL", "Golf_PL", "Limo.Service_PL","Mini.Bar_PL",
                                         "Pool.Outdoor_PL", "Resort_PL", "Spa_PL", "GP_Tier", "COUNTRY_CODE_R", "Country_PL", "State_PL", "City_PL", "Guest_Room_H", "Condition_Hotel_H", "Customer_SVC_H",
                                         "Staff_Cared_H", "Internet_Sat_H", "Check_In_H", "Tranquility_H", "POV_CODE_C", "Brand_PL", "Property.Latitude_PL", "Property.Longitude_PL")]

#Interperet data
str(febData) #displays data frame structure, including the variables/factors


#make copies of data for distinct treatment of NAs and add month column
febData$Month <- "February"

mayData$Month <- "May"

augData$Month <- "August"

novData$Month <- "November"



#completely remove NA from likelihood to recommend without replacement

febData <- filter(febData, Likelihood_Recommend_H != "NA")
mayData <- filter(mayData, Likelihood_Recommend_H != "NA")
augData <- filter(augData, Likelihood_Recommend_H != "NA")
novData <- filter(novData, Likelihood_Recommend_H != "NA")

#Union the dataframes (without removing duplicates, as a union would in SQL)
totalData <- rbind(febData, mayData, augData, novData)

#Choose the location
myMapData <- sqldf("SELECT State_PL, COUNT(*) FROM totalData GROUP BY State_PL ORDER BY COUNT(*) DESC")
str(myMapData)
myMapData <- myMapData[myMapData$State_PL %in% state.name,]
emptyStates <- setdiff(state.name, myMapData$State_PL) #from dplyr?
emptyStates <- data.frame(emptyStates, 0)
colnames(emptyStates) <- c("State_PL", "COUNT(*)")
myMapData <- rbind(myMapData, emptyStates)
rownames(myMapData) <- NULL
myMapData$State_PL <- tolower(myMapData$State_PL)
colnames(myMapData) <- c("State_PL", "COUNT")
USA <- map_data("state")

responseMap <- ggplot(data = myMapData, aes(map_id=State_PL))+
  geom_map(map=USA, aes(fill=COUNT))+
  expand_limits(x=USA$long, y=USA$lat)+
  coord_fixed()+
  ggtitle("Survey Responses by State")
responseMap


california <- filter(USA, region=="california")
california <- california[,-6]
caliData <- filter(totalData, State_PL=="California")

caliData <- filter(caliData, Condition_Hotel_H != "NA")
caliData <- filter(caliData, Staff_Cared_H != "NA")
caliData <- filter(caliData, Internet_Sat_H != "NA")
caliData <- filter(caliData, Check_In_H != "NA")
caliData <- filter(caliData, Tranquility_H != "NA")
caliData <- filter(caliData, Customer_SVC_H != "NA")

View(caliData)

unique(caliData$Brand_PL)

ggplot(data=caliData, aes(x=Brand_PL))+
  geom_bar()

#Will choose to examine Hyatt Regency b/c has highest response and lowest score (not counting ambiguous 'Hyatt')
sqldf("SELECT Brand_PL, AVG(Likelihood_Recommend_H) FROM caliData GROUP BY Brand_PL ORDER BY AVG(Likelihood_Recommend_H) DESC")

brandGG <- ggplot(data=caliData, aes(x=NPS_Type)) +
  geom_bar()+
  facet_grid(.~Brand_PL)+
  ggtitle("NPS_Type by Brand")
brandGG

tapply(caliData$Likelihood_Recommend_H, caliData$Brand_PL, skewness)

row.names(caliData)<-1:nrow(caliData)

#filter by brand
caliData <- filter(caliData, Brand_PL == "Hyatt Regency")

#reorder months by factor levels
caliData$Month <- factor(caliData$Month, levels = month.name)

#replaces empty Limo values with "N"
caliData$Limo.Service_PL <- as.factor(gsub("^$", "N", caliData$Limo.Service_PL)) 



caliPoints <- data.frame(tapply(caliData$Likelihood_Recommend_H, caliData$City_PL, mean))
colnames(caliPoints) <- "Avg_Likelihood_Recommend_H"
caliPoints$City_PL <- rownames(caliPoints)
rownames(caliPoints) <- NULL
caliPoints$State_PL <- "california"
caliPoints <- na.omit(caliPoints)
caliPointsLatLong <- geocode(paste(caliPoints$City_PL, caliPoints$State_PL))
caliPoints$lon <- caliPointsLatLong$lon
caliPoints$lat <- caliPointsLatLong$lat

caliPoints

caliPoints$City_PL <- tolower(caliPoints$City_PL)

californiaMap <- ggplot(data = myMapData, aes(map_id=State_PL))+
  geom_map(map=california, aes(fill=COUNT))+
  expand_limits(x=california$long, y=california$lat)+
  coord_fixed()+
  geom_point(data = caliPoints, aes(x=caliPoints$lon, y=caliPoints$lat, color=Avg_Likelihood_Recommend_H), size=2)+
  scale_color_gradient(low="black", high="lightgrey")
californiaMap


#Analyze Data

#mean score likelihood_recommend score
tapply(caliData$Likelihood_Recommend_H, caliData$POV_CODE_C, mean) #leisure has slightly higher LTR

ggplot(data=caliData, aes(x=POV_CODE_C))+
  geom_bar()+
  ggtitle("Business vs Leisure Customers, California")


unique(caliData$GP_Tier)
caliData[caliData$GP_Tier=="GOLD", which(colnames(caliData)=="GP_Tier")] <- "Gold"
caliData[caliData$GP_Tier=="PLAT", which(colnames(caliData)=="GP_Tier")] <- "Platinum"
caliData[caliData$GP_Tier=="DIAM", which(colnames(caliData)=="GP_Tier")] <- "Diamond"

unique(caliData$GP_Tier)

View(caliData)

#lm (single linear regression)
#lm_condition and lm_customerSVC have highest coefficients of determination
(lm_condition <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Condition_Hotel_H))
summary(lm_condition)$r.squared
summary(lm_condition)$adj.r.squared

(lm_staff <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Staff_Cared_H))
summary(lm_staff)$r.squared
summary(lm_staff)$adj.r.squared

(lm_internet <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Internet_Sat_H))
summary(lm_internet)$r.squared
summary(lm_internet)$adj.r.squared

(lm_checkIn <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Check_In_H))
summary(lm_checkIn)$r.squared
summary(lm_checkIn)$adj.r.squared

(lm_tranquility <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Tranquility_H))
summary(lm_tranquility)$r.squared
summary(lm_tranquility)$adj.r.squared

(lm_customerSVC <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Customer_SVC_H))
summary(lm_customerSVC)$r.squared
summary(lm_customerSVC)$adj.r.squared


#multiple linear regression
lm_multi1 <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H)
summary(lm_multi1)$r.squared
summary(lm_multi1)$adj.r.squared

lm_multi2 <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H)
summary(lm_multi2)$r.squared
summary(lm_multi2)$adj.r.squared

lm_multi3 <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H + Staff_Cared_H)
summary(lm_multi3)$r.squared
summary(lm_multi3)$adj.r.squared

lm_multi4 <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H + Staff_Cared_H + Check_In_H)
summary(lm_multi4)$r.squared
summary(lm_multi4)$adj.r.squared

lm_multi5 <- lm(data = caliData, formula = Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H)
summary(lm_multi5)$r.squared
summary(lm_multi5)$adj.r.squared
summary(lm_multi5)

#strong P-values for all variables ***EXCEPT Check_In_H***
#Linear model explains >66% of variability in likelihood to recommend
summary(lm_multi5) 



########################
# aRules analysis
caliData[caliData$Casino_PL != "N", which(colnames(caliData)=="Casino_PL")] <- "Y"
caliData <- filter(caliData, Mini.Bar_PL != "")


business <- filter(caliData, POV_CODE_C == "BUSINESS")
leisure <- filter(caliData, POV_CODE_C == "LEISURE")

#in Apriori, only patterns up to a length of 10 are returned
#Also... No instance of casinos at Hyatt Regency Hotels in California...
#No instance of conference rooms at Hyatt Regency hotels in California...
businessRules <- business[,c("Room_Type_H","Convention_PL", "Business.Center_PL", 
                        "Golf_PL", "Limo.Service_PL","Mini.Bar_PL",
                        "NPS_Type")]


leisureRules <- leisure[, c("Room_Type_H","Pool.Indoor_PL", "Casino_PL",
                      "Golf_PL", "Limo.Service_PL","Mini.Bar_PL",
                      "Pool.Outdoor_PL", "Resort_PL", "Spa_PL", "NPS_Type")]


businessRulesApri <- apriori(businessRules,
                         parameter = list(minlen=5, supp=0.005, conf=0.05), #minlen refers to minimum number of variables being analyzed
                         appearance = list(rhs=c("NPS_Type=Passive", "NPS_Type=Detractor"), #restricting RHS to Passive, or Detractor NPS types
                                           default="lhs"), control = list(verbose=F))
businessRulesApri <- sort(businessRulesApri, by = "lift")
inspect(businessRulesApri[1:50])


#Proportion of services to NPS Type
businessProp <- businessRules
businessProp$one <- 1

businessProp <- ddply(businessProp, "NPS_Type", transform, percent = one / sum(one) * 100)

#Golf seems to be important
ggplot(businessProp, aes(x=NPS_Type, y=percent, fill=Golf_PL))+
  geom_bar(stat="identity")

ggplot(businessProp, aes(x=NPS_Type, y=percent, fill=Conference_PL))+
  geom_bar(stat="identity")

ggplot(businessProp, aes(x=NPS_Type, y=percent, fill=Limo.Service_PL))+
  geom_bar(stat="identity")

ggplot(businessProp, aes(x=NPS_Type, y=percent, fill=Casino_PL))+
  geom_bar(stat="identity")

#Mini Bar seems important
ggplot(businessProp, aes(x=NPS_Type, y=percent, fill=Mini.Bar_PL))+
  geom_bar(stat="identity")

#Business Center is important
ggplot(businessProp, aes(x=NPS_Type, y=percent, fill=Business.Center_PL))+
  geom_bar(stat="identity")

ggplot(businessProp, aes(x=NPS_Type, y=percent, fill=Convention_PL))+
  geom_bar(stat="identity")

ggplot(businessProp, aes(x=NPS_Type, y=percent, fill=Fitness.Center_PL))+
  geom_bar(stat="identity")


#not enough leisure surveys to make an informed decision using Apriori
leisureRulesApri <- apriori(leisureRules,
                            parameter = list(minlen=3, supp=0.005, conf=0.2), #minlen refers to minimum number of variables being analyzed
                            appearance = list(rhs=c("NPS_Type=Passive", "NPS_Type=Detractor"), #restricting RHS to Passive or Detractor NPS types
                                              default="lhs"), control = list(verbose=F))
leisureRulesApri <- sort(leisureRulesApri, by = "lift")
inspect(leisureRulesApri[1:20])
#################################################################

#KSVM
business$NPS_Num <- business$NPS_Type
any(business$Convention_PL=='Y')
any(business$Pool=='Y')

business$NPS_Num <- factor(business$NPS_Num,
       labels = c(1, 2, 3))

business$Pool.Outdoor_PL_Num <- factor(business$Pool.Outdoor_PL,
                           labels = c(0,1))
business[business$Pool.Outdoor_PL=="Y", which(colnames(business)=="Pool.Outdoor_PL_Num")] <- 1
business[business$Pool.Outdoor_PL=="N", which(colnames(business)=="Pool.Outdoor_PL_Num")] <- 0


business$Pool.Indoor_PL_Num <- factor(business$Pool.Indoor_PL,
                                      labels = c(0,1))
business[business$Pool.Indoor_PL=="Y", which(colnames(business)=="Pool.Indoor_PL_Num")] <- 1
business[business$Pool.Indoor_PL=="N", which(colnames(business)=="Pool.Indoor_PL_Num")] <- 0


business$Convention_PL_Num <- factor(business$Convention_PL,
                                      labels = c(0,1))
business[business$Convention_PL=="Y", which(colnames(business)=="Convention_PL_Num")] <- 1
business[business$Convention_PL=="N", which(colnames(business)=="Convention_PL_Num")] <- 0


business$Mini.Bar_PL_Num <- factor(business$Mini.Bar_PL,
                                     labels = c(0,1))
business[business$Mini.Bar_PL=="Y", which(colnames(business)=="Mini.Bar_PL_Num")] <- 1
business[business$Mini.Bar_PL=="N", which(colnames(business)=="Mini.Bar_PL_Num")] <- 0


business$Business.Center_PL_Num <- factor(business$Business.Center_PL,
                                   labels = c(0,1))
business[business$Business.Center_PL=="Y", which(colnames(business)=="Business.Center_PL_Num")] <- 1
business[business$Business.Center_PL=="N", which(colnames(business)=="Business.Center_PL_Num")] <- 0


business$Spa_PL_Num <- factor(business$Spa_PL,
                                          labels = c(0,1))
business[business$Spa_PL=="Y", which(colnames(business)=="Spa_PL_Num")] <- 1
business[business$Spa_PL=="N", which(colnames(business)=="Spa_PL_Num")] <- 0


business[business$NPS_Num=="Promoter", which(colnames(business)=="NPS_Num")] <- 3
business[business$NPS_Num=="Passive", which(colnames(business)=="NPS_Num")] <- 2
business[business$NPS_Num=="Detractor", which(colnames(business)=="NPS_Num")] <- 1


#Set "low-medium-high" values for hotel operations as 0,1,2 respectively

#Condition_Hotel_H_Num
business$Condition_Hotel_H_Num <- as.numeric(business$Condition_Hotel_H)
business[business$Condition_Hotel_H_Num==1, which(colnames(business)=="Condition_Hotel_H_Num")] <- 0
business[business$Condition_Hotel_H_Num==2, which(colnames(business)=="Condition_Hotel_H_Num")] <- 0
business[business$Condition_Hotel_H_Num==3, which(colnames(business)=="Condition_Hotel_H_Num")] <- 0
business[business$Condition_Hotel_H_Num==4, which(colnames(business)=="Condition_Hotel_H_Num")] <- 0

business[business$Condition_Hotel_H_Num==5, which(colnames(business)=="Condition_Hotel_H_Num")] <- 1
business[business$Condition_Hotel_H_Num==6, which(colnames(business)=="Condition_Hotel_H_Num")] <- 1
business[business$Condition_Hotel_H_Num==7, which(colnames(business)=="Condition_Hotel_H_Num")] <- 1

business[business$Condition_Hotel_H_Num==8, which(colnames(business)=="Condition_Hotel_H_Num")] <- 2
business[business$Condition_Hotel_H_Num==9, which(colnames(business)=="Condition_Hotel_H_Num")] <- 2
business[business$Condition_Hotel_H_Num==10, which(colnames(business)=="Condition_Hotel_H_Num")] <- 2

#Customer_SVC_H
business$Customer_SVC_H_Num <- as.numeric(business$Customer_SVC_H)
business[business$Customer_SVC_H_Num==1, which(colnames(business)=="Customer_SVC_H_Num")] <- 0
business[business$Customer_SVC_H_Num==2, which(colnames(business)=="Customer_SVC_H_Num")] <- 0
business[business$Customer_SVC_H_Num==3, which(colnames(business)=="Customer_SVC_H_Num")] <- 0
business[business$Customer_SVC_H_Num==4, which(colnames(business)=="Customer_SVC_H_Num")] <- 0

business[business$Customer_SVC_H_Num==5, which(colnames(business)=="Customer_SVC_H_Num")] <- 1
business[business$Customer_SVC_H_Num==6, which(colnames(business)=="Customer_SVC_H_Num")] <- 1
business[business$Customer_SVC_H_Num==7, which(colnames(business)=="Customer_SVC_H_Num")] <- 1

business[business$Customer_SVC_H_Num==8, which(colnames(business)=="Customer_SVC_H_Num")] <- 2
business[business$Customer_SVC_H_Num==9, which(colnames(business)=="Customer_SVC_H_Num")] <- 2
business[business$Customer_SVC_H_Num==10, which(colnames(business)=="Customer_SVC_H_Num")] <- 2

#Tranquility_H
business$Tranquility_H_Num <- as.numeric(business$Tranquility_H)
business[business$Tranquility_H_Num==1, which(colnames(business)=="Tranquility_H_Num")] <- 0
business[business$Tranquility_H_Num==2, which(colnames(business)=="Tranquility_H_Num")] <- 0
business[business$Tranquility_H_Num==3, which(colnames(business)=="Tranquility_H_Num")] <- 0
business[business$Tranquility_H_Num==4, which(colnames(business)=="Tranquility_H_Num")] <- 0

business[business$Tranquility_H_Num==5, which(colnames(business)=="Tranquility_H_Num")] <- 1
business[business$Tranquility_H_Num==6, which(colnames(business)=="Tranquility_H_Num")] <- 1
business[business$Tranquility_H_Num==7, which(colnames(business)=="Tranquility_H_Num")] <- 1

business[business$Tranquility_H_Num==8, which(colnames(business)=="Tranquility_H_Num")] <- 2
business[business$Tranquility_H_Num==9, which(colnames(business)=="Tranquility_H_Num")] <- 2
business[business$Tranquility_H_Num==10, which(colnames(business)=="Tranquility_H_Num")] <- 2

#Staff_Cared_H
business$Staff_Cared_H_Num <- as.numeric(business$Staff_Cared_H)
business[business$Staff_Cared_H_Num==1, which(colnames(business)=="Staff_Cared_H_Num")] <- 0
business[business$Staff_Cared_H_Num==2, which(colnames(business)=="Staff_Cared_H_Num")] <- 0
business[business$Staff_Cared_H_Num==3, which(colnames(business)=="Staff_Cared_H_Num")] <- 0
business[business$Staff_Cared_H_Num==4, which(colnames(business)=="Staff_Cared_H_Num")] <- 0

business[business$Staff_Cared_H_Num==5, which(colnames(business)=="Staff_Cared_H_Num")] <- 1
business[business$Staff_Cared_H_Num==6, which(colnames(business)=="Staff_Cared_H_Num")] <- 1
business[business$Staff_Cared_H_Num==7, which(colnames(business)=="Staff_Cared_H_Num")] <- 1

business[business$Staff_Cared_H_Num==8, which(colnames(business)=="Staff_Cared_H_Num")] <- 2
business[business$Staff_Cared_H_Num==9, which(colnames(business)=="Staff_Cared_H_Num")] <- 2
business[business$Staff_Cared_H_Num==10, which(colnames(business)=="Staff_Cared_H_Num")] <- 2

#Check_In_H
business$Check_In_H_Num <- as.numeric(business$Check_In_H)
business[business$Check_In_H_Num==1, which(colnames(business)=="Check_In_H_Num")] <- 0
business[business$Check_In_H_Num==2, which(colnames(business)=="Check_In_H_Num")] <- 0
business[business$Check_In_H_Num==3, which(colnames(business)=="Check_In_H_Num")] <- 0
business[business$Check_In_H_Num==4, which(colnames(business)=="Check_In_H_Num")] <- 0

business[business$Check_In_H_Num==5, which(colnames(business)=="Check_In_H_Num")] <- 1
business[business$Check_In_H_Num==6, which(colnames(business)=="Check_In_H_Num")] <- 1
business[business$Check_In_H_Num==7, which(colnames(business)=="Check_In_H_Num")] <- 1

business[business$Check_In_H_Num==8, which(colnames(business)=="Check_In_H_Num")] <- 2
business[business$Check_In_H_Num==9, which(colnames(business)=="Check_In_H_Num")] <- 2
business[business$Check_In_H_Num==10, which(colnames(business)=="Check_In_H_Num")] <- 2

#Internet_Sat_H
business$Internet_Sat_H_Num <- as.numeric(business$Internet_Sat_H)
business[business$Internet_Sat_H_Num==1, which(colnames(business)=="Internet_Sat_H_Num")] <- 0
business[business$Internet_Sat_H_Num==2, which(colnames(business)=="Internet_Sat_H_Num")] <- 0
business[business$Internet_Sat_H_Num==3, which(colnames(business)=="Internet_Sat_H_Num")] <- 0
business[business$Internet_Sat_H_Num==4, which(colnames(business)=="Internet_Sat_H_Num")] <- 0

business[business$Internet_Sat_H_Num==5, which(colnames(business)=="Internet_Sat_H_Num")] <- 1
business[business$Internet_Sat_H_Num==6, which(colnames(business)=="Internet_Sat_H_Num")] <- 1
business[business$Internet_Sat_H_Num==7, which(colnames(business)=="Internet_Sat_H_Num")] <- 1

business[business$Internet_Sat_H_Num==8, which(colnames(business)=="Internet_Sat_H_Num")] <- 2
business[business$Internet_Sat_H_Num==9, which(colnames(business)=="Internet_Sat_H_Num")] <- 2
business[business$Internet_Sat_H_Num==10, which(colnames(business)=="Internet_Sat_H_Num")] <- 2

View(business)

###########################
#Set training and test data
randIndex <- sample(1:dim(business)[1]) #randomize index of rows to prevent any unintended bias
head(randIndex)

train_cutpoint2_3 <- floor(2*nrow(business)/3) #set the number of rows that will be used in the training set (2/3 of data will be for training)
trainData <- business[randIndex[1:train_cutpoint2_3],] #applies 2/3 cut-off point for caliData
testData <- business[randIndex[train_cutpoint2_3+1:nrow(business)],] #applies the remainder for testData

#############################


ksvmOutputNum <- ksvm(NPS_Num ~ Condition_Hotel_H_Num + Customer_SVC_H_Num + Tranquility_H_Num + Staff_Cared_H_Num + Check_In_H_Num + Internet_Sat_H_Num,
                   data=trainData,
                   kernel = "rbfdot", 
                   kpar="automatic", 
                   C = 5, 
                   cross = 3,
                   prob.model = TRUE)
ksvmOutputNum


ksvmOutputLR <- ksvm(Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H,
                     data=trainData,
                     kernel = "rbfdot", 
                     kpar="automatic", 
                     C = 5, 
                     cross = 3,
                     prob.model = TRUE)
ksvmOutputLR


#histogram of ksvm
ksvmpredLR <- predict(ksvmOutputLR,testData)
ksvmpredLR
table(ksvmpredLR)
actual <- testData$Likelihood_Recommend_H
length(actual)
length(ksvmpredLR)
Kvar2 <- actual - as.vector(ksvmpredLR)
Kvar2 <- na.omit(Kvar2)
View(Kvar2)
hist(Kvar2, main = "Histogram of KSVM Model")



ksvmOutputNPS <- ksvm(NPS_Num ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H,
                      data=trainData,
                      kernel = "rbfdot", 
                      kpar="automatic", 
                      C = 5, 
                      cross = 3,
                      prob.model = TRUE)
ksvmOutputNPS


ksvmOutputCat <- ksvm(NPS_Num ~ Pool.Indoor_PL_Num + Pool.Outdoor_PL_Num + Convention_PL_Num + Mini.Bar_PL_Num + Business.Center_PL_Num + Spa_PL_Num,
                      data=trainData,
                      kernel = "rbfdot", 
                      kpar="automatic", 
                      C = 5, 
                      cross = 3,
                      prob.model = TRUE)
ksvmOutputCat


ksvmOutputCombined <- ksvm(NPS_Num ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H + Staff_Cared_H + Check_In_H + Internet_Sat_H +
                             Convention_PL_Num + Business.Center_PL_Num + Spa_PL_Num + Mini.Bar_PL_Num,
                           data=trainData,
                           kernel = "rbfdot", 
                           kpar="automatic", 
                           C = 5, 
                           cross = 3,
                           prob.model = TRUE)
ksvmOutputCombined


ksvmOutputCombined2 <- ksvm(NPS_Num ~ Condition_Hotel_H_Num + Customer_SVC_H_Num + Tranquility_H_Num + Staff_Cared_H_Num + Check_In_H_Num + Internet_Sat_H_Num +
                              Convention_PL_Num + Business.Center_PL_Num + Spa_PL_Num + Mini.Bar_PL_Num,
                            data=trainData,
                            kernel = "rbfdot", 
                            kpar="automatic", 
                            C = 5, 
                            cross = 3,
                            prob.model = TRUE)
ksvmOutputCombined2





########
#SVM
########
SVMOutput <- svm(Likelihood_Recommend_H ~ Condition_Hotel_H + Customer_SVC_H + Tranquility_H + 
                   Staff_Cared_H + Check_In_H + Internet_Sat_H, data=trainData)
SVMOutput


svm(NPS_Num ~ Pool.Indoor_PL_Num + Pool.Outdoor_PL_Num + Convention_PL_Num + Mini.Bar_PL_Num + Business.Center_PL_Num + Spa_PL_Num,
    data=trainData)

#trainData$Customer_SVC_H[is.na(trainData$Customer_SVC_H)] <- mean(trainData$Customer_SVC_H, na.rm = TRUE)
#any(is.na(testData$Internet_Sat_H))
#plot(scale(c),pch=16)

svmpred <- predict(SVMOutput,testData)
svmpred
table(svmpred)
actual <- testData$Likelihood_Recommend_H
actual
length(actual)
length(svmpred)
var2 <- actual - svmpred
View(var2)
var2 <- na.omit(var2)
hist(var2, main = "Histogram of SVM Model")








###########################################################################
#I experimented with these models, but are probably not too useful.

#Linear Model Based on Limo Service
Limo_conditionPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Condition_Hotel_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2) +
  geom_smooth(method="lm")+
  facet_grid(.~Limo.Service_PL)+
  ggtitle("Hotel Condition Rating")


Limo_staffPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Staff_Cared_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Limo.Service_PL)+
  ggtitle("Staff Rating")

Limo_internetPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Internet_Sat_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Limo.Service_PL)+
  ggtitle("Internet Rating")


Limo_checkInPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Check_In_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Limo.Service_PL)+
  ggtitle("Check-In Satisfaction Rating")

Limo_tranquilityPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Tranquility_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Limo.Service_PL)+
  ggtitle("Hotel Tranquility Rating")

Limo_customerPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, Customer_SVC_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Limo.Service_PL)+
  ggtitle("Customer Service Rating")

#look at Staff, check-in, cust.Service, tranquility, Internet.... All have lower scores w/out limo service
grid.arrange(Limo_conditionPlotRM, Limo_staffPlotRM, Limo_internetPlotRM, Limo_checkInPlotRM, 
             Limo_tranquilityPlotRM, Limo_customerPlotRM, ncol=2, nrow=3)


#gender
business[business$Gender_H=="", which(colnames(caliData)=="Gender_H")] <- "Prefer not to answer"

GenderconditionPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Condition_Hotel_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2) +
  geom_smooth(method="lm")+
  facet_grid(.~Gender_H)+
  ggtitle("Hotel Condition Rating")


GenderstaffPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Staff_Cared_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Gender_H)+
  ggtitle("Staff Rating")

GenderinternetPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Internet_Sat_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Gender_H)+
  ggtitle("Internet Rating")


GendercheckInPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Check_In_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Gender_H)+
  ggtitle("Check-In Satisfaction Rating")

GendertranquilityPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Tranquility_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Gender_H)+
  ggtitle("Hotel Tranquility Rating")

GendercustomerPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, Customer_SVC_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~Gender_H)+
  ggtitle("Customer Service Rating")

grid.arrange(GenderconditionPlotRM, GenderstaffPlotRM, GenderinternetPlotRM, GendercheckInPlotRM, GendertranquilityPlotRM, GendercustomerPlotRM, ncol=2, nrow=3)


#Tier
Tier_conditionPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Condition_Hotel_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2) +
  geom_smooth(method="lm")+
  facet_grid(.~GP_Tier)+
  ggtitle("Hotel Condition Rating")


Tier_staffPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Staff_Cared_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~GP_Tier)+
  ggtitle("Staff Rating")

Tier_internetPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Internet_Sat_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~GP_Tier)+
  ggtitle("Internet Rating")


Tier_checkInPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Check_In_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~GP_Tier)+
  ggtitle("Check-In Satisfaction Rating")

Tier_tranquilityPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, x=Tranquility_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~GP_Tier)+
  ggtitle("Hotel Tranquility Rating")

Tier_customerPlotRM <- ggplot(data=business, aes(y=Likelihood_Recommend_H, Customer_SVC_H)) +
  geom_point(position=position_jitter(width = .5, height = .5), alpha=0.2)+
  geom_smooth(method="lm")+
  facet_grid(.~GP_Tier)+
  ggtitle("Customer Service Rating")


grid.arrange(Tier_conditionPlotRM, Tier_staffPlotRM, Tier_internetPlotRM, Tier_checkInPlotRM, Tier_tranquilityPlotRM, Tier_customerPlotRM, ncol=2, nrow=3)
