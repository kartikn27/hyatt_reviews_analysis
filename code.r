file_path = '/Users/kartik/Documents/SYRACUSE_IM/SEM_1/IST_687/Final_Project/Data/out-201403.csv'
#setwd("/Users/kartik/Documents/SYRACUSE_IM/SEM_1/IST_687/Final_Project/Data/")
csvContent <- read.csv(file_path, header = TRUE, sep = ',')
csvContent

csvContent[which(csvContent$DUP_INDEX_C == 1),]

length(which(csvContent$DUP_INDEX_C == 0))

length(which(csvContent$LENGTH_OF_STAY_CATEGORY_R == '12-29'))

unique(csvContent$LENGTH_OF_STAY_CATEGORY_R)

tapply(csvContent$LENGTH_OF_STAY_CATEGORY_R, csvContent$LENGTH_OF_STAY_CATEGORY_R, length)

tapply(csvContent$NPS_Type, csvContent$NPS_Type, length)

tapply(csvContent$State_PL, csvContent$NPS_Type, length)

csvContent$State_NPS <- paste(csvContent$State_PL, csvContent$NPS_Type, sep = "--")
tapply(csvContent$State_NPS, csvContent$State_NPS, length)

csvContent$Country_NPS <- paste(csvContent$Country_PL, csvContent$NPS_Type, sep = "--")
tapply(csvContent$Country_NPS, csvContent$Country_NPS, length)


csvContent$Continent_NPS <- paste(csvContent$Sub.Continent_PL, csvContent$NPS_Type, sep = "--")
tapply(csvContent$Continent_NPS, csvContent$Continent_NPS, length)

csvContent$Guest.NPS.Goal_PL
factor(csvContent$Guest.NPS.Goal_PL)
tapply(csvContent$Guest.NPS.Goal_PL, csvContent$Guest.NPS.Goal_PL, length)

csvContent$CHECKOUT_HEADER_ID_C
library("ggplot2")
ggplot(subset(csvContent,NPS_Type=="Promoter"),aes(x=Sub.Continent_PL, y = length(which(csvContent$NPS_Type == "Promoter"))/length(csvContent$NPS_Type[!is.na(csvContent$NPS_Type)]), fill = "red")) + 
  geom_bar(stat="identity") + ggtitle("Continent wise Promoter Score") + 
  labs(x = "Continent", y = "Promoter Count") + coord_flip()

ggplot(subset(csvContent,NPS_Type=="Detractor"),aes(x=Sub.Continent_PL)) + 
  geom_bar() + ggtitle("Continent wise Detractor Score") + 
  labs(x = "Continent", y = "Detractor Count") + coord_flip()

ggplot(subset(csvContent,NPS_Type=="Passive"),aes(x=Sub.Continent_PL)) + 
  geom_bar() + ggtitle("Continent wise Passive Score") + 
  labs(x = "Continent", y = "Detractor Passive") + coord_flip()

ggplot(subset(csvContent,NPS_Type=="Promoter"),aes(x=Country_PL)) + 
  geom_bar() + ggtitle("Continent wise Promoter Score") + 
  labs(x = "Country", y = "Promoter Count") + coord_flip()

ggplot(subset(csvContent,NPS_Type=="Detractor"),aes(x=Country_PL)) + 
  geom_bar() + ggtitle("Continent wise Detractor Score") + 
  labs(x = "Country", y = "Detractor Count") + coord_flip()

ggplot(subset(csvContent,NPS_Type=="Passive"),aes(x=Country_PL)) + 
  geom_bar() + ggtitle("Continent wise Passive Score") + 
  labs(x = "Country", y = "Passive Count") + coord_flip()

ggplot(subset(csvContent,NPS_Type=="Promoter" & csvContent$State_PL != ""),aes(x=State_PL,fill = 'red')) + 
  geom_bar(stat = "identity") + ggtitle("State wise Promoter Score") + 
  labs(x = "State", y = "Promoter Count") + coord_flip()

ggplot(subset(csvContent,NPS_Type=="Detractor" & csvContent$State_PL != ""),aes(x=State_PL)) + 
  geom_bar() + ggtitle("State wise Detractor Score") + 
  labs(x = "State", y = "Detractor Count") + coord_flip()

ggplot(subset(csvContent,NPS_Type=="Passive" & csvContent$State_PL != ""),aes(x=State_PL)) + 
  geom_bar() + ggtitle("State wise Passive Score") + 
  labs(x = "State", y = "Passive Count") + coord_flip()

# ------ 11/09/2016 ------
ggplot(subset(csvContent,NPS_Type=="Promoter", csvContent$ROOM_TYPE_CODE_C),aes(x=State_PL)) + 
  geom_bar() + ggtitle("State wise Promoter Score") + 
  labs(x = "State", y = "Promoter Count") + coord_flip()

unique(csvContent$Relationship_PL)

tapply(csvContent$Relationship_PL, csvContent$Relationship_PL, findPercentage)

percentManage = (length(which(csvContent$Relationship_PL == "Managed"))/length(csvContent$Restaurant_PL))*100
percentManage

percentFranchised = (length(which(csvContent$Relationship_PL == "Franchised"))/length(csvContent$Restaurant_PL))*100
percentFranchised

percentLeased = (length(which(csvContent$Relationship_PL == "Leased"))/length(csvContent$Restaurant_PL))*100
percentLeased

percentOwned = (length(which(csvContent$Relationship_PL == "Owned"))/length(csvContent$Restaurant_PL))*100
percentOwned

#POC = subset(csvContent,csvContent$NPS_Type == "Promoter" & csvContent$Relationship_PL == "Owned" & csvContent$RESERVATION_STATUS_R == "CONFRM" & csvContent$State_PL != "")
#ggplot(subset(csvContent,csvContent$NPS_Type == "Promoter" & csvContent$Relationship_PL == "Owned" & csvContent$RESERVATION_STATUS_R == "CONFRM" & csvContent$State_PL != ""),aes(x=State_PL, fill = State_PL)) + 
#  geom_bar(aes(y = nrow(POC)/length(csvContent$State_PL)), stat = "identity") + ggtitle("State wise Promoter Owned and Confirm Score") + 
#  labs(x = "State", y = "Promoter Owned and Confirm Score Count") + coord_flip()

ggplot(subset(csvContent,csvContent$NPS_Type == "Promoter" & csvContent$Relationship_PL == "Owned" & csvContent$State_PL != ""),aes(x=State_PL, fill = State_PL)) + 
  geom_bar() + ggtitle("State wise Promoter Owned Score") + 
  labs(x = "State", y = "Promoter Owned Score Count") + coord_flip()

ggplot(subset(csvContent,csvContent$NPS_Type == "Promoter" & csvContent$Relationship_PL == "Leased" & csvContent$State_PL != ""),aes(x=State_PL, fill = State_PL)) + 
  geom_bar() + ggtitle("State wise Promoter Leased Score") + 
  labs(x = "State", y = "Promoter Leased Score Count") + coord_flip()

ggplot(subset(csvContent,csvContent$NPS_Type == "Promoter" & csvContent$Relationship_PL == "Managed" & csvContent$State_PL != ""),aes(x=State_PL, fill = State_PL)) + 
  geom_bar() + ggtitle("State wise Promoter Leased Score") + 
  labs(x = "State", y = "Promoter Leased Score Count") + coord_flip()




tapply(csvContent$State_PL, csvContent$State_PL, length)
typeof(tapply(csvContent$State_PL, csvContent$State_PL, length))


ggplot(subset(csvContent,csvContent$NPS_Type == "Promoter" & csvContent$Relationship_PL == "Owned" & csvContent$RESERVATION_STATUS_R == "CANCEL"),aes(x=State_PL, fill = State_PL)) + 
  geom_bar() + ggtitle("State wise Promoter Owned and Cancel Score") + 
  labs(x = "State", y = "Promoter Owned and Cancel Score Count") + coord_flip()

library(ggmap)

USAMap <- geocode("USA")
USA.map <- get_map(location = USAMap, zoom = 4)
gg<-ggmap(USA.map)
gg

cleanCsv <- subset(csvContent, csvContent$NPS_Type != "")
nrow(cleanCsv)
ggUSA <- gg + geom_point(data = cleanCsv, aes(x=cleanCsv$Property.Longitude_PL, y=cleanCsv$Property.Latitude_PL,colour = cleanCsv$NPS_Type))
ggUSA <- ggUSA + ggtitle("NPS Type in USA")
ggUSA

tapply(cleanCsv$NPS_Type,cleanCsv$NPS_Type, length)
tapply(cleanCsv$NPS_Type, cleanCsv$Relationship_PL, length)
unique(cleanCsv$NPS_Type)

tapply(cleanCsv$Location_PL, cleanCsv$Location_PL, length)
tapply(csvContent$Location_PL, csvContent$Location_PL, length)
#tapply(subset(csvContent,csvContent$Location_PL == "Resort"), csvContent$NPS_Type, length)

nrow(subset(csvContent,csvContent$Location_PL == "Airport" & csvContent$NPS_Type == "Promoter"))
nrow(subset(csvContent,csvContent$Location_PL == "Airport" & csvContent$NPS_Type == "Detractor"))
nrow(subset(csvContent,csvContent$Location_PL == "Airport" & csvContent$NPS_Type == "Passive"))

nrow(subset(csvContent,csvContent$Location_PL == "Resort" & csvContent$NPS_Type == "Promoter"))
nrow(subset(csvContent,csvContent$Location_PL == "Resort" & csvContent$NPS_Type == "Detractor"))
nrow(subset(csvContent,csvContent$Location_PL == "Resort" & csvContent$NPS_Type == "Passive"))

nrow(subset(csvContent,csvContent$Location_PL == "Urban" & csvContent$NPS_Type == "Promoter"))
nrow(subset(csvContent,csvContent$Location_PL == "Urban" & csvContent$NPS_Type == "Detractor"))
nrow(subset(csvContent,csvContent$Location_PL == "Urban" & csvContent$NPS_Type == "Passive"))

nrow(subset(csvContent,csvContent$Location_PL == "Suburban" & csvContent$NPS_Type == "Promoter"))
nrow(subset(csvContent,csvContent$Location_PL == "Suburban" & csvContent$NPS_Type == "Detractor"))
nrow(subset(csvContent,csvContent$Location_PL == "Suburban" & csvContent$NPS_Type == "Passive"))

unique(csvContent$Likelihood_Recommend_H)
length(which(csvContent$Likelihood_Recommend_H == 10))
length(csvContent$Likelihood_Recommend_H)

#---------------------#

lmodel2<-lm(csvContent$Likelihood_Recommend_H~csvContent$Tranquility_H, data = csvContent)
summary(lmodel2)


lmodel3<-lm(csvContent$Likelihood_Recommend_H~csvContent$DIRECT_NIGHTS_NUM_R, data = csvContent)
summary(lmodel3)

#-
lmodel4<-lm(csvContent$Likelihood_Recommend_H~csvContent$Condition_Hotel_H, data = csvContent)
summary(lmodel4)

lmodel5<-lm(csvContent$Likelihood_Recommend_H~csvContent$F.B_Overall_Experience_H, data = csvContent)
summary(lmodel5)

lmodel6<-lm(csvContent$Likelihood_Recommend_H~csvContent$Internet_Sat_H, data = csvContent)
summary(lmodel6)

#-
lmodel7<-lm(csvContent$Likelihood_Recommend_H~csvContent$Staff_Cared_H, data = csvContent)
summary(lmodel7)

#-
lmodel8<-lm(csvContent$Likelihood_Recommend_H~csvContent$Guest_Room_H, data = csvContent)
summary(lmodel8)

#-
lmodel9<-lm(csvContent$Likelihood_Recommend_H~csvContent$Overall_Sat_H, data = csvContent)
summary(lmodel9)

lmodel10<-lm(csvContent$Likelihood_Recommend_H~csvContent$REVENUE_USD_R, data = csvContent)
summary(lmodel10)

lmodel11<-lm(csvContent$Likelihood_Recommend_H~csvContent$Hotel.Inventory_PL, data = csvContent)
summary(lmodel11)


randnum <- sample(1:dim(csvContent)[1])
summary(randnum)
length(randnum)
head(randnum)

cutpoint2_3 <- floor(2*dim(csvContent)[1]/3)
cutpoint2_3 

#train data set:
train_data <- csvContent[randnum[1:cutpoint2_3],] 
train_data
head(train_data,5)

#test data set:
test_data <- csvContent[randnum[(cutpoint2_3+1):dim(csvContent)[1]],]

svmoutput <- ksvm(NPS_R~Shuttle.Service_PL+Restaurant_PL+All.Suites_PL+WALK_IN_FLG_C+Bell.Staff_PL+Boutique_PL+Business.Center_PL+Casino_PL+Conference_PL+Convention_PL+Dry.Cleaning_PL+Elevators_PL+Fitness.Center_PL+Fitness.Trainer_PL+Golf_PL+Indoor.Corridors_PL+Laundry_PL+Limo.Service_PL+Mini.Bar_PL+Pool.Indoor_PL+Pool.Outdoor_PL+Regency.Grand.Club_PL+Resort_PL
                  +Restaurant_PL
                  +Self.Parking_PL
                  +Shuttle.Service_PL
                  +Ski_PL
                  +Spa_PL
                  +Spa.services.in.fitness.center_PL
                  +Spa.online.booking_PL
                  +Spa.F.B.offering_PL
                  +Valet.Parking_PL
                  +GOLDPASSPORT_FLG_R
                  +ARRIVAL_FLG_R
                  +OFFER_FLG_R,data=train_data,kernel="rbfdot",kpar="automatic",C=5,cross=5, prob.model=TRUE)
svmOutput

#-----------------------------------------

tapply(subManaged$Likelihood_Recommend_H, subManaged$State_PL, mean, na.rm = TRUE)
tapply(subLeased$Likelihood_Recommend_H, subLeased$State_PL, mean, na.rm = TRUE)
tapply(subOwned$Likelihood_Recommend_H, subOwned$State_PL, mean, na.rm = TRUE)

subManaged <- subset(csvContent, csvContent$Relationship_PL == "Managed")
subManaged
head(subManaged)

subLeased <- subset(csvContent, csvContent$Relationship_PL == "Leased")
subLeased

subOwned <- subset(csvContent, csvContent$Relationship_PL == "Owned")
subOwned
head(subOwned)

subPromoter <- subset(csvContent, csvContent$NPS_Type == "Promoter")
subPromoter

subDetractor <- subset(csvContent, csvContent$NPS_Type == "Detractor")
subDetractor

subPassive <- subset(csvContent, csvContent$NPS_Type == "Passive")
subPassive

subPromoterT <- t(subPromoter)
View(subPromoterT)

subDetractorT <- t(subDetractor)
csvContent$All.Suites_PL <- ifelse(csvContent$All.Suites_PL=="N",0,1)
csvContent$WALK_IN_FLG_C <- ifelse(csvContent$WALK_IN_FLG_C=="N",0,1)
csvContent$Bell.Staff_PL <- ifelse(csvContent$Bell.Staff_PL=="N",0,1)
csvContent$Boutique_PL <- ifelse(csvContent$Boutique_PL=="N",0,1)
csvContent$Business.Center_PL <- ifelse(csvContent$Business.Center_PL=="N",0,1)
csvContent$Casino_PL <- ifelse(csvContent$Casino_PL=="N",0,1)
csvContent$Conference_PL <- ifelse(csvContent$Conference_PL=="N",0,1)
csvContent$Convention_PL <- ifelse(csvContent$Convention_PL=="N",0,1)
csvContent$Dry.Cleaning_PL <- ifelse(csvContent$Dry.Cleaning_PL=="N",0,1)
csvContent$Elevators_PL <- ifelse(csvContent$Elevators_PL=="N",0,1)
csvContent$Fitness.Center_PL <- ifelse(csvContent$Fitness.Center_PL=="N",0,1)
csvContent$Fitness.Trainer_PL <- ifelse(csvContent$Fitness.Trainer_PL=="N",0,1)
csvContent$Golf_PL <- ifelse(csvContent$Golf_PL=="N",0,1)
csvContent$Indoor.Corridors_PL <- ifelse(csvContent$Indoor.Corridors_PL=="N",0,1)
csvContent$Laundry_PL <- ifelse(csvContent$Laundry_PL=="N",0,1)
csvContent$Limo.Service_PL <- ifelse(csvContent$Limo.Service_PL=="N",0,1)
csvContent$Mini.Bar_PL <- ifelse(csvContent$Mini.Bar_PL=="N",0,1)
csvContent$Pool.Indoor_PL <- ifelse(csvContent$Pool.Indoor_PL=="N",0,1)
csvContent$Pool.Outdoor_PL <- ifelse(csvContent$Pool.Outdoor_PL=="N",0,1)
csvContent$Regency.Grand.Club_PL <- ifelse(csvContent$Regency.Grand.Club_PL=="N",0,1)
csvContent$Resort_PL <- ifelse(csvContent$Resort_PL=="N",0,1)
csvContent$Restaurant_PL <- ifelse(csvContent$Restaurant_PL=="N",0,1)
csvContent$Self.Parking_PL <- ifelse(csvContent$Self.Parking_PL=="N",0,1)
csvContent$Shuttle.Service_PL <- ifelse(csvContent$Shuttle.Service_PL=="N",0,1)
csvContent$Ski_PL <- ifelse(csvContent$Ski_PL=="N",0,1)
csvContent$Spa_PL <- ifelse(csvContent$Spa_PL=="N",0,1)
csvContent$Spa.services.in.fitness.center_PL <- ifelse(csvContent$Spa.services.in.fitness.center_PL=="N",0,1)
csvContent$Spa.online.booking_PL <- ifelse(csvContent$Spa.online.booking_PL=="N",0,1)
csvContent$Spa.F.B.offering_PL <- ifelse(csvContent$Spa.F.B.offering_PL=="N",0,1)
csvContent$Valet.Parking_PL <- ifelse(csvContent$Valet.Parking_PL=="N",0,1)
csvContent$GOLDPASSPORT_FLG_R <- ifelse(csvContent$GOLDPASSPORT_FLG_R=="N",0,1)
csvContent$ARRIVAL_FLG_R <- ifelse(csvContent$ARRIVAL_FLG_R=="N",0,1)
csvContent$OFFER_FLG_R <- ifelse(csvContent$OFFER_FLG_R=="N",0,1)


tapply(subPromoter$All.Suites_PL, subPromoter$All.Suites_PL, length)
tapply(subPromoter$WALK_IN_FLG_C, subPromoter$WALK_IN_FLG_C, length)
tapply(subPromoter$Bell.Staff_PL, subPromoter$Bell.Staff_PL, length)
tapply(subPromoter$Boutique_PL, subPromoter$Boutique_PL, length)
tapply(subPromoter$Business.Center_PL, subPromoter$Business.Center_PL, length)
tapply(subPromoter$Casino_PL, subPromoter$Casino_PL, length)
tapply(subPromoter$Conference_PL, subPromoter$Conference_PL, length)
tapply(subPromoter$Convention_PL, subPromoter$Convention_PL, length)
tapply(subPromoter$Dry.Cleaning_PL, subPromoter$Dry.Cleaning_PL, length)
tapply(subPromoter$Elevators_PL, subPromoter$Elevators_PL, length)
tapply(subPromoter$Fitness.Center_PL, subPromoter$Fitness.Center_PL, length)
tapply(subPromoter$Fitness.Trainer_PL, subPromoter$Fitness.Trainer_PL, length)
tapply(subPromoter$Golf_PL, subPromoter$Golf_PL, length)
tapply(subPromoter$Indoor.Corridors_PL, subPromoter$Indoor.Corridors_PL, length)
tapply(subPromoter$Laundry_PL, subPromoter$Laundry_PL, length)
tapply(subPromoter$Limo.Service_PL, subPromoter$Limo.Service_PL, length)
tapply(subPromoter$Mini.Bar_PL, subPromoter$Mini.Bar_PL, length)
tapply(subPromoter$Pool.Indoor_PL, subPromoter$Pool.Indoor_PL, length)
tapply(subPromoter$Pool.Outdoor_PL, subPromoter$Pool.Outdoor_PL, length)
tapply(subPromoter$Regency.Grand.Club_PL, subPromoter$Regency.Grand.Club_PL, length)
tapply(subPromoter$Restaurant_PL, subPromoter$Restaurant_PL, length)
tapply(subPromoter$Self.Parking_PL, subPromoter$Self.Parking_PL, length)
tapply(subPromoter$Shuttle.Service_PL, subPromoter$Shuttle.Service_PL, length)
tapply(subPromoter$Ski_PL, subPromoter$Ski_PL, length)
tapply(subPromoter$Spa_PL, subPromoter$Spa_PL, length)
tapply(subPromoter$Spa.services.in.fitness.center_PL, subPromoter$Spa.services.in.fitness.center_PL, length)
tapply(subPromoter$Spa.online.booking_PL, subPromoter$Spa.online.booking_PL, length)
tapply(subPromoter$Spa.F.B.offering_PL, subPromoter$Spa.F.B.offering_PL, length)
tapply(subPromoter$Valet.Parking_PL, subPromoter$Valet.Parking_PL, length)
tapply(subPromoter$GOLDPASSPORT_FLG_R, subPromoter$GOLDPASSPORT_FLG_R, length)


tapply(subDetractor$All.Suites_PL, subDetractor$All.Suites_PL, length)
tapply(subDetractor$WALK_IN_FLG_C, subDetractor$WALK_IN_FLG_C, length)
tapply(subDetractor$Bell.Staff_PL, subDetractor$Bell.Staff_PL, length)
tapply(subDetractor$Boutique_PL, subDetractor$Boutique_PL, length)
tapply(subDetractor$Business.Center_PL, subDetractor$Business.Center_PL, length)
tapply(subDetractor$Casino_PL, subDetractor$Casino_PL, length)
tapply(subDetractor$Conference_PL, subDetractor$Conference_PL, length)
tapply(subDetractor$Convention_PL, subDetractor$Convention_PL, length)
tapply(subDetractor$Dry.Cleaning_PL, subDetractor$Dry.Cleaning_PL, length)
tapply(subDetractor$Elevators_PL, subDetractor$Elevators_PL, length)
tapply(subDetractor$Fitness.Center_PL, subDetractor$Fitness.Center_PL, length)
tapply(subDetractor$Fitness.Trainer_PL, subDetractor$Fitness.Trainer_PL, length)
tapply(subDetractor$Golf_PL, subDetractor$Golf_PL, length)
tapply(subDetractor$Indoor.Corridors_PL, subDetractor$Indoor.Corridors_PL, length)
tapply(subDetractor$Laundry_PL, subDetractor$Laundry_PL, length)
tapply(subDetractor$Limo.Service_PL, subDetractor$Limo.Service_PL, length)
tapply(subDetractor$Mini.Bar_PL, subDetractor$Mini.Bar_PL, length)
tapply(subDetractor$Pool.Indoor_PL, subDetractor$Pool.Indoor_PL, length)
tapply(subDetractor$Pool.Outdoor_PL, subDetractor$Pool.Outdoor_PL, length)
tapply(subDetractor$Regency.Grand.Club_PL, subDetractor$Regency.Grand.Club_PL, length)
tapply(subDetractor$Restaurant_PL, subDetractor$Restaurant_PL, length)
tapply(subDetractor$Self.Parking_PL, subDetractor$Self.Parking_PL, length)
tapply(subDetractor$Shuttle.Service_PL, subDetractor$Shuttle.Service_PL, length)
tapply(subDetractor$Ski_PL, subDetractor$Ski_PL, length)
tapply(subDetractor$Spa_PL, subDetractor$Spa_PL, length)
tapply(subDetractor$Spa.services.in.fitness.center_PL, subDetractor$Spa.services.in.fitness.center_PL, length)
tapply(subDetractor$Spa.online.booking_PL, subDetractor$Spa.online.booking_PL, length)
tapply(subDetractor$Spa.F.B.offering_PL, subDetractor$Spa.F.B.offering_PL, length)
tapply(subDetractor$Valet.Parking_PL, subDetractor$Valet.Parking_PL, length)
tapply(subDetractor$GOLDPASSPORT_FLG_R, subDetractor$GOLDPASSPORT_FLG_R, length)


facility_type_D <- c('Suites', 'Walk-IN', 'Bell-Staff', 'Boutique', 'Business-Center', 'Casino', 'Conference', 'Convention', 'Dry-Cleaning', 'Elevators',
                   'Fitness-Center', 'Fitness-Trainer', 'Golf', 'Indoor-Corridors', 'Laundry', 'Limousine',
                   'Mini-Bar', 'Indoor-Pool', 'Outdoor-Pool', 'Grand-Club', 'Restaurant', 'Self-Parking',
                   'Shuttle-Service', 'Ski', 'Spa', 'Spa-In-Fitness', 'Spa-Online-Booking', 'Spa-Food-Beverages',
                   'Valet', 'Goldpassport')
freq_D <- c(9270,9270,7760,109,8908,104,31,4436,9204,9231,9249,3358,314,9037,7580, 5055, 3621,
          4220, 6638, 5588, 6940, 7868, 5421, 37, 2610,4630, 8362, 8323,7244,9270)
wordCloudDF_D <- data.frame(facility_type, freq)
wordCloudDetractor <- wordcloud(words = wordCloudDF_D$facility_type, freq = wordCloudDF_D$freq, rot.per = 0.5, 
          colors = brewer.pal(8,"Dark2"))
text(x=0.5, y=1, "Facilities for Detractors")



facility_type <- c('Suites', 'Walk-IN', 'Bell-Staff', 'Boutique', 'Business-Center', 'Casino', 'Conference', 'Convention', 'Dry-Cleaning', 'Elevators',
                   'Fitness-Center', 'Fitness-Trainer', 'Golf', 'Indoor-Corridors', 'Laundry', 'Limousine',
                   'Mini-Bar', 'Indoor-Pool', 'Outdoor-Pool', 'Grand-Club', 'Restaurant', 'Self-Parking',
                   'Shuttle-Service', 'Ski', 'Spa', 'Spa-In-Fitness', 'Spa-Online-Booking', 'Spa-Food-Beverages',
                   'Valet', 'Goldpassport')
freq <- c(53894,53894,42702,859,51936,719,99,21283,53401,53585,53722,22334,2012,52671,44565, 30511, 24483,
          26799, 38386, 31834, 36452, 46738, 33077, 364, 16421,29740, 47917, 48335,40244,53894)
wordCloudDF <- data.frame(facility_type, freq)

wordCloudPromoter <- wordcloud(words = wordCloudDF$facility_type, freq = wordCloudDF$freq, rot.per = 0.5, 
          colors = brewer.pal(8,"Dark2"))
text(x=0.5, y=1, "Facilities for Promoters")



grid.arrange(wordcloud(words = wordCloudDF_D$facility_type, freq = wordCloudDF_D$freq, rot.per = 0.5, 
                       colors = brewer.pal(8,"Dark2")), wordcloud(words = wordCloudDF$facility_type, freq = wordCloudDF$freq, rot.per = 0.5, 
                                                                  colors = brewer.pal(8,"Dark2")))



stateList <- unique(csvContent$State_PL)
stateList<-as.vector(stateList)
str(stateList)
stateList <- as.character(stateList)
csvContentClean <- csvContent[!(csvContent$NPS_Type == ''),]

findPercent <- function(stateList){
  statePercentPromoter <- vector(mode = "integer", length = 48)
  statePercentDetractor <- vector(mode = "integer", length = 48)
  statePercentPassive <- vector(mode = "integer", length = 48)
  for(i in 1:length(stateList)){
    statePercentPromoter[i] <- length(which(csvContentClean$State_PL == stateList[i] & csvContentClean$NPS_Type == "Promoter"))/length(which(csvContentClean$State_PL == stateList[i]))*100
    statePercentDetractor[i] <- length(which(csvContentClean$State_PL == stateList[i] & csvContentClean$NPS_Type == "Detractor"))/length(which(csvContentClean$State_PL == stateList[i]))*100
    statePercentPassive[i] <- length(which(csvContentClean$State_PL == stateList[i] & csvContentClean$NPS_Type == "Passive"))/length(which(csvContentClean$State_PL == stateList[i]))*100
    
  }
  statePercentNPSType <- data.frame(stateList, statePercentPromoter, statePercentDetractor, statePercentPassive)
  return(statePercentNPSType)
}

statePercentNPSType <- findPercent(stateList)
statePercentNPSType<- statePercentNPSType[-1,]


ggplot(statePercentNPSType,aes(x=stateList, y = statePercentPromoter)) + 
  geom_bar(stat = "identity") + ggtitle("State wise Promoter Percent (March)") + 
  labs(x = "State", y = "Percent") + coord_flip()

ggplot(statePercentNPSType,aes(x=stateList, y = statePercentDetractor)) + 
  geom_bar(stat = "identity") + ggtitle("State wise Detractor Percent (March)") + 
  labs(x = "State", y = "Percent") + coord_flip()

ggplot(statePercentNPSType,aes(x=stateList, y = statePercentPassive)) + 
  geom_bar(stat = "identity") + ggtitle("State wise Passive Percent (March)") + 
  labs(x = "State", y = "Percent") + coord_flip()

#  -------------------

unique(subPromoter$State_PL)
northDakotaPromoters <- subset(subPromoter, subPromoter$State_PL == "Louisiana")
head(northDakotaPromoters)
length(northDakotaPromoters)
unique(subPromoter$State_PL)
tapply(northDakotaPromoters$Customer_SVC_H, mean)

illinoisPromoters <- subset(subPromoter, subPromoter$State_PL == "Illinois")



tapply(illinoisPromoters$All.Suites_PL, illinoisPromoters$All.Suites_PL, length)
tapply(illinoisPromoters$WALK_IN_FLG_C, illinoisPromoters$WALK_IN_FLG_C, length)
tapply(illinoisPromoters$Bell.Staff_PL, illinoisPromoters$Bell.Staff_PL, length)
tapply(illinoisPromoters$Boutique_PL, illinoisPromoters$Boutique_PL, length)
tapply(illinoisPromoters$Business.Center_PL, illinoisPromoters$Business.Center_PL, length)
tapply(illinoisPromoters$Casino_PL, illinoisPromoters$Casino_PL, length)
tapply(illinoisPromoters$Conference_PL, illinoisPromoters$Conference_PL, length)
tapply(illinoisPromoters$Convention_PL, illinoisPromoters$Convention_PL, length)
tapply(illinoisPromoters$Dry.Cleaning_PL, illinoisPromoters$Dry.Cleaning_PL, length)
tapply(illinoisPromoters$Elevators_PL, illinoisPromoters$Elevators_PL, length)
tapply(illinoisPromoters$Fitness.Center_PL, illinoisPromoters$Fitness.Center_PL, length)
tapply(illinoisPromoters$Fitness.Trainer_PL, illinoisPromoters$Fitness.Trainer_PL, length)
tapply(illinoisPromoters$Golf_PL, illinoisPromoters$Golf_PL, length)
tapply(illinoisPromoters$Indoor.Corridors_PL, illinoisPromoters$Indoor.Corridors_PL, length)
tapply(illinoisPromoters$Laundry_PL, illinoisPromoters$Laundry_PL, length)
tapply(illinoisPromoters$Limo.Service_PL, illinoisPromoters$Limo.Service_PL, length)
tapply(illinoisPromoters$Mini.Bar_PL, illinoisPromoters$Mini.Bar_PL, length)
tapply(illinoisPromoters$Pool.Indoor_PL, illinoisPromoters$Pool.Indoor_PL, length)
tapply(illinoisPromoters$Pool.Outdoor_PL, illinoisPromoters$Pool.Outdoor_PL, length)
tapply(illinoisPromoters$Regency.Grand.Club_PL, illinoisPromoters$Regency.Grand.Club_PL, length)
tapply(illinoisPromoters$Restaurant_PL, illinoisPromoters$Restaurant_PL, length)
tapply(illinoisPromoters$Self.Parking_PL, illinoisPromoters$Self.Parking_PL, length)
tapply(illinoisPromoters$Shuttle.Service_PL, illinoisPromoters$Shuttle.Service_PL, length)
tapply(illinoisPromoters$Ski_PL, illinoisPromoters$Ski_PL, length)
tapply(illinoisPromoters$Spa_PL, illinoisPromoters$Spa_PL, length)
tapply(illinoisPromoters$Spa.services.in.fitness.center_PL, illinoisPromoters$Spa.services.in.fitness.center_PL, length)
tapply(illinoisPromoters$Spa.online.booking_PL, illinoisPromoters$Spa.online.booking_PL, length)
tapply(illinoisPromoters$Spa.F.B.offering_PL, illinoisPromoters$Spa.F.B.offering_PL, length)
tapply(illinoisPromoters$Valet.Parking_PL, illinoisPromoters$Valet.Parking_PL, length)
tapply(illinoisPromoters$GOLDPASSPORT_FLG_R, illinoisPromoters$GOLDPASSPORT_FLG_R, length)

facility_type <- c('Suites', 'Walk-IN', 'Bell-Staff', 'Boutique', 'Business-Center', 'Casino', 'Conference', 'Convention', 'Dry-Cleaning', 'Elevators',
                   'Fitness-Center', 'Fitness-Trainer', 'Golf', 'Indoor-Corridors', 'Laundry', 'Limousine',
                   'Mini-Bar', 'Indoor-Pool', 'Outdoor-Pool', 'Grand-Club', 'Restaurant', 'Self-Parking',
                   'Shuttle-Service', 'Ski', 'Spa', 'Spa-In-Fitness', 'Spa-Online-Booking', 'Spa-Food-Beverages',
                   'Valet', 'Goldpassport')
freq_Illinois <- c(2357,2357,2114,0,2357,0,0,943,2357,2357,2357,951,0,2357,2123,1492,1821,1469,838,1693,1653,1777,1718,0,0,951,2156,2284,2081,2357)
wordCloudDF <- data.frame(facility_type, freq_Loiusiana)

wordCloudIllinois <- wordcloud(words = wordCloudDF$facility_type, freq = wordCloudDF$freq_Loiusiana, rot.per = 0.5, 
                               colors = brewer.pal(8,"Dark2"))
text(x=0.5, y=1, "Facilities for Detractors in Louisiana")

freq_Loiusiana <- c(87,87,87,0,87,0,0,79,87,87,87,12,0,87,87,12,12,8,12,87,79,12,12,0,4,8,87,87,87,87)





tapply(northDakotaPromoters$All.Suites_PL, northDakotaPromoters$All.Suites_PL, length)
tapply(northDakotaPromoters$WALK_IN_FLG_C, northDakotaPromoters$WALK_IN_FLG_C, length)
tapply(northDakotaPromoters$Bell.Staff_PL, northDakotaPromoters$Bell.Staff_PL, length)
tapply(northDakotaPromoters$Boutique_PL, northDakotaPromoters$Boutique_PL, length)
tapply(northDakotaPromoters$Business.Center_PL, northDakotaPromoters$Business.Center_PL, length)
tapply(northDakotaPromoters$Casino_PL, northDakotaPromoters$Casino_PL, length)
tapply(northDakotaPromoters$Conference_PL, northDakotaPromoters$Conference_PL, length)
tapply(northDakotaPromoters$Convention_PL, northDakotaPromoters$Convention_PL, length)
tapply(northDakotaPromoters$Dry.Cleaning_PL, northDakotaPromoters$Dry.Cleaning_PL, length)
tapply(northDakotaPromoters$Elevators_PL, northDakotaPromoters$Elevators_PL, length)
tapply(northDakotaPromoters$Fitness.Center_PL, northDakotaPromoters$Fitness.Center_PL, length)
tapply(northDakotaPromoters$Fitness.Trainer_PL, northDakotaPromoters$Fitness.Trainer_PL, length)
tapply(northDakotaPromoters$Golf_PL, northDakotaPromoters$Golf_PL, length)
tapply(northDakotaPromoters$Indoor.Corridors_PL, northDakotaPromoters$Indoor.Corridors_PL, length)
tapply(northDakotaPromoters$Laundry_PL, northDakotaPromoters$Laundry_PL, length)
tapply(northDakotaPromoters$Limo.Service_PL, northDakotaPromoters$Limo.Service_PL, length)
tapply(northDakotaPromoters$Mini.Bar_PL, northDakotaPromoters$Mini.Bar_PL, length)
tapply(northDakotaPromoters$Pool.Indoor_PL, northDakotaPromoters$Pool.Indoor_PL, length)
tapply(northDakotaPromoters$Pool.Outdoor_PL, northDakotaPromoters$Pool.Outdoor_PL, length)
tapply(northDakotaPromoters$Regency.Grand.Club_PL, northDakotaPromoters$Regency.Grand.Club_PL, length)
tapply(northDakotaPromoters$Restaurant_PL, northDakotaPromoters$Restaurant_PL, length)
tapply(northDakotaPromoters$Self.Parking_PL, northDakotaPromoters$Self.Parking_PL, length)
tapply(northDakotaPromoters$Shuttle.Service_PL, northDakotaPromoters$Shuttle.Service_PL, length)
tapply(northDakotaPromoters$Ski_PL, northDakotaPromoters$Ski_PL, length)
tapply(northDakotaPromoters$Spa_PL, northDakotaPromoters$Spa_PL, length)
tapply(northDakotaPromoters$Spa.services.in.fitness.center_PL, northDakotaPromoters$Spa.services.in.fitness.center_PL, length)
tapply(northDakotaPromoters$Spa.online.booking_PL, northDakotaPromoters$Spa.online.booking_PL, length)
tapply(northDakotaPromoters$Spa.F.B.offering_PL, northDakotaPromoters$Spa.F.B.offering_PL, length)
tapply(northDakotaPromoters$Valet.Parking_PL, northDakotaPromoters$Valet.Parking_PL, length)
tapply(northDakotaPromoters$GOLDPASSPORT_FLG_R, northDakotaPromoters$GOLDPASSPORT_FLG_R, length)


louisianaPromoters <- subset(subDetractor, subPromoter$State_PL == "Louisiana")
length(louisianaPromoters)

tapply(louisianaPromoters$All.Suites_PL, louisianaPromoters$All.Suites_PL, length)
tapply(louisianaPromoters$WALK_IN_FLG_C, louisianaPromoters$WALK_IN_FLG_C, length)
tapply(louisianaPromoters$Bell.Staff_PL, louisianaPromoters$Bell.Staff_PL, length)
tapply(louisianaPromoters$Boutique_PL, louisianaPromoters$Boutique_PL, length)
tapply(louisianaPromoters$Business.Center_PL, louisianaPromoters$Business.Center_PL, length)
tapply(louisianaPromoters$Casino_PL, louisianaPromoters$Casino_PL, length)
tapply(louisianaPromoters$Conference_PL, louisianaPromoters$Conference_PL, length)
tapply(louisianaPromoters$Convention_PL, louisianaPromoters$Convention_PL, length)
tapply(louisianaPromoters$Dry.Cleaning_PL, louisianaPromoters$Dry.Cleaning_PL, length)
tapply(louisianaPromoters$Elevators_PL, louisianaPromoters$Elevators_PL, length)
tapply(louisianaPromoters$Fitness.Center_PL, louisianaPromoters$Fitness.Center_PL, length)
tapply(louisianaPromoters$Fitness.Trainer_PL, louisianaPromoters$Fitness.Trainer_PL, length)
tapply(louisianaPromoters$Golf_PL, louisianaPromoters$Golf_PL, length)
tapply(louisianaPromoters$Indoor.Corridors_PL, louisianaPromoters$Indoor.Corridors_PL, length)
tapply(louisianaPromoters$Laundry_PL, louisianaPromoters$Laundry_PL, length)
tapply(louisianaPromoters$Limo.Service_PL, louisianaPromoters$Limo.Service_PL, length)
tapply(louisianaPromoters$Mini.Bar_PL, louisianaPromoters$Mini.Bar_PL, length)
tapply(louisianaPromoters$Pool.Indoor_PL, louisianaPromoters$Pool.Indoor_PL, length)
tapply(louisianaPromoters$Pool.Outdoor_PL, louisianaPromoters$Pool.Outdoor_PL, length)
tapply(louisianaPromoters$Regency.Grand.Club_PL, louisianaPromoters$Regency.Grand.Club_PL, length)
tapply(louisianaPromoters$Restaurant_PL, louisianaPromoters$Restaurant_PL, length)
tapply(louisianaPromoters$Self.Parking_PL, louisianaPromoters$Self.Parking_PL, length)
tapply(louisianaPromoters$Shuttle.Service_PL, louisianaPromoters$Shuttle.Service_PL, length)
tapply(louisianaPromoters$Ski_PL, louisianaPromoters$Ski_PL, length)
tapply(louisianaPromoters$Spa_PL, louisianaPromoters$Spa_PL, length)
tapply(louisianaPromoters$Spa.services.in.fitness.center_PL, louisianaPromoters$Spa.services.in.fitness.center_PL, length)
tapply(louisianaPromoters$Spa.online.booking_PL, louisianaPromoters$Spa.online.booking_PL, length)
tapply(louisianaPromoters$Spa.F.B.offering_PL, louisianaPromoters$Spa.F.B.offering_PL, length)
tapply(louisianaPromoters$Valet.Parking_PL, louisianaPromoters$Valet.Parking_PL, length)
tapply(louisianaPromoters$GOLDPASSPORT_FLG_R, louisianaPromoters$GOLDPASSPORT_FLG_R, length)

subsetUSA <- subset(csvContent, csvContent$Country_PL == "United States")
head(subsetUSA)

subsetUSA$Internet_Sat_H

internetSatModel <- lm(csvContent$Likelihood_Recommend_H~csvContent$Internet_Sat_H, data = csvContent, na.rm = TRUE)
summary(internetSatModel)

subsetBusiness <- subset(csvContent, csvContent$POV_CODE_C == "BUSINESS")
tapply(subsetBusiness$Likelihood_Recommend_H, subsetBusiness$Business.Center_PL, median, na.rm = TRUE)

subsetBusinessCentre <- subset(csvContentClean, csvContentClean$Business.Center_PL == 1)
subsetBusinessCentre
subsetNoBusinessCentre <- subset(csvContentClean, csvContentClean$Business.Center_PL == 0)
subsetNoBusinessCentre
subsetElevator <- subset(csvContentClean, csvContentClean$Elevators_PL == 1)
subsetElevator
subsetNoElevator <- subset(csvContentClean, csvContentClean$Elevators_PL == 0)
subsetNoElevator

percentBSCPromoter <- sum(subsetBusinessCentre$NPS_Type == "Promoter")/(sum(subsetBusinessCentre$NPS_Type == "Promoter")+sum(subsetBusinessCentre$NPS_Type == "Detractor")+sum(subsetBusinessCentre$NPS_Type == "Passive"))*100
percentBSCDetractor <- sum(subsetBusinessCentre$NPS_Type == "Detractor")/(sum(subsetBusinessCentre$NPS_Type == "Promoter")+sum(subsetBusinessCentre$NPS_Type == "Detractor")+sum(subsetBusinessCentre$NPS_Type == "Passive"))*100
PercentBSCPassive <- sum(subsetBusinessCentre$NPS_Type == "Passive")/(sum(subsetBusinessCentre$NPS_Type == "Promoter")+sum(subsetBusinessCentre$NPS_Type == "Detractor")+sum(subsetBusinessCentre$NPS_Type == "Passive"))*100

percentNBSCPromoter <- sum(subsetNoBusinessCentre$NPS_Type == "Promoter")/(sum(subsetNoBusinessCentre$NPS_Type == "Promoter")+sum(subsetNoBusinessCentre$NPS_Type == "Detractor")+sum(subsetNoBusinessCentre$NPS_Type == "Passive"))*100
percentNBSCDetractor <- sum(subsetNoBusinessCentre$NPS_Type == "Detractor")/(sum(subsetNoBusinessCentre$NPS_Type == "Promoter")+sum(subsetNoBusinessCentre$NPS_Type == "Detractor")+sum(subsetNoBusinessCentre$NPS_Type == "Passive"))*100
PercentNBSCPassive <- sum(subsetNoBusinessCentre$NPS_Type == "Passive")/(sum(subsetNoBusinessCentre$NPS_Type == "Promoter")+sum(subsetNoBusinessCentre$NPS_Type == "Detractor")+sum(subsetNoBusinessCentre$NPS_Type == "Passive"))*100

percentElevatorPromoter <- sum(subsetElevator$NPS_Type == "Promoter")/(sum(subsetElevator$NPS_Type == "Promoter")+sum(subsetElevator$NPS_Type == "Detractor")+sum(subsetElevator$NPS_Type == "Passive"))*100
percentElevatorDetractor <- sum(subsetElevator$NPS_Type == "Detractor")/(sum(subsetElevator$NPS_Type == "Promoter")+sum(subsetElevator$NPS_Type == "Detractor")+sum(subsetElevator$NPS_Type == "Passive"))*100
percentElevatorPassive <- sum(subsetElevator$NPS_Type == "Passive")/(sum(subsetElevator$NPS_Type == "Promoter")+sum(subsetElevator$NPS_Type == "Detractor")+sum(subsetElevator$NPS_Type == "Passive"))*100

percentNoElevatorPromoter <- sum(subsetNoElevator$NPS_Type == "Promoter")/(sum(subsetNoElevator$NPS_Type == "Promoter")+sum(subsetNoElevator$NPS_Type == "Detractor")+sum(subsetNoElevator$NPS_Type == "Passive"))*100
percentNoElevatorDetractor <- sum(subsetNoElevator$NPS_Type == "Detractor")/(sum(subsetNoElevator$NPS_Type == "Promoter")+sum(subsetNoElevator$NPS_Type == "Detractor")+sum(subsetNoElevator$NPS_Type == "Passive"))*100
percentNoElevatorPassive <- sum(subsetNoElevator$NPS_Type == "Passive")/(sum(subsetNoElevator$NPS_Type == "Promoter")+sum(subsetNoElevator$NPS_Type == "Detractor")+sum(subsetNoElevator$NPS_Type == "Passive"))*100









# ------------------- SONALI

length(which(csvContent$Location_PL == "Urban" & csvContent$Country_PL == "United States"))

length(csvContent$Country_PL == "United States")

test123<-tapply(csvContent$Location_PL, csvContent$Country_PL, length)
test123
typeof(test123)
test123[1]
rownames(test123)
newDF <- data.frame(key = names(test123), value = test123)
newDF

subsetUrban <- subset(csvContent, csvContentClean$Location_PL == "Urban")
subsetAirport <- subset(csvContent, csvContentClean$Location_PL == "Airport")
subsetSuburban <- subset(csvContent, csvContentClean$Location_PL == "Suburban")
subsetResort <- subset(csvContent, csvContentClean$Location_PL == "Resort")
unique(csvContent$Location_PL)

tUrban<-tapply(subsetUrban$Location_PL, subsetUrban$Country_PL, length)
tAirport<-tapply(subsetAirport$Location_PL, subsetAirport$Country_PL, length)
tSuburban<-tapply(subsetSuburban$Location_PL, subsetSuburban$Country_PL, length)
tResort<-tapply(subsetResort$Location_PL, subsetResort$Country_PL, length)

countryLocationDf <- data.frame(country = names(tUrban), urban = tUrban, airport = tAirport, suburban = tSuburban, resort = tResort)

str(countryLocationDf)
countryLocationDf[,2]
View(countryLocationDfCopy)

countryLocationDf[1,]
countryLocationDf$suburban

countryLocationDfCopy <- countryLocationDf
rownames(countryLocationDfCopy) <- NULL

countryLocationDfCopy[is.na(countryLocationDfCopy)] <- 0

library("ggplot2")
ggplot(countryLocationDfCopyNOUSA,aes(x=countryLocationDfCopyNOUSA$country, y = countryLocationDfCopyNOUSA$airport)) + 
  geom_bar(stat = "identity") + ggtitle("Country Wise Urban Hotel Count") + 
  labs(x = "Country", y = "Detractor Count") + coord_flip()

ggplot(countryLocationDfCopy,aes(x=countryLocationDfCopy$country, y = countryLocationDfCopy$suburban)) + 
  geom_bar(stat = "identity") + ggtitle("Country Wise SubUrban Hotel Count") + 
  labs(x = "Country", y = "Count") + coord_flip()
typeof(countryLocationDfCopy$urban)

countryLocationDfCopy
countryLocationDfCopy[which(countryLocationDfCopy$country == 'United States'),]

countryLocationDfCopy$urban <- round(countryLocationDfCopy$urban)
countryLocationDfCopy$suburban <- round(countryLocationDfCopy$suburban)
countryLocationDfCopy$airport <- round(countryLocationDfCopy$airport)
countryLocationDfCopy$resort <- round(countryLocationDfCopy$resort)


countryLocationDfCopyNOUSA <- countryLocationDfCopy[-52,] 



length(which(subsetUrban$NPS_Type == 'Promoter'))/(length(which(subsetUrban$NPS_Type == 'Promoter')) + length(which(subsetUrban$NPS_Type == 'Detractor')) + length(which(subsetUrban$NPS_Type == 'Passive'))) * 100
length(which(subsetResort$NPS_Type == 'Promoter'))/(length(which(subsetResort$NPS_Type == 'Promoter')) + length(which(subsetResort$NPS_Type == 'Detractor')) + length(which(subsetResort$NPS_Type == 'Passive'))) * 100
length(which(subsetAirport$NPS_Type == 'Promoter'))/(length(which(subsetAirport$NPS_Type == 'Promoter')) + length(which(subsetAirport$NPS_Type == 'Detractor')) + length(which(subsetAirport$NPS_Type == 'Passive'))) * 100
length(which(subsetSuburban$NPS_Type == 'Promoter'))/(length(which(subsetSuburban$NPS_Type == 'Promoter')) + length(which(subsetSuburban$NPS_Type == 'Detractor')) + length(which(subsetSuburban$NPS_Type == 'Passive'))) * 100

length(which(subsetUrban$NPS_Type == 'Detractor'))/(length(which(subsetUrban$NPS_Type == 'Promoter')) + length(which(subsetUrban$NPS_Type == 'Detractor')) + length(which(subsetUrban$NPS_Type == 'Passive'))) * 100
length(which(subsetResort$NPS_Type == 'Detractor'))/(length(which(subsetResort$NPS_Type == 'Promoter')) + length(which(subsetResort$NPS_Type == 'Detractor')) + length(which(subsetResort$NPS_Type == 'Passive'))) * 100
length(which(subsetAirport$NPS_Type == 'Detractor'))/(length(which(subsetAirport$NPS_Type == 'Promoter')) + length(which(subsetAirport$NPS_Type == 'Detractor')) + length(which(subsetAirport$NPS_Type == 'Passive'))) * 100
length(which(subsetSuburban$NPS_Type == 'Detractor'))/(length(which(subsetSuburban$NPS_Type == 'Promoter')) + length(which(subsetSuburban$NPS_Type == 'Detractor')) + length(which(subsetSuburban$NPS_Type == 'Passive'))) * 100

tapply(subsetUrban$NPS_Type, subsetUrban$State_PL, length)
length(which(subsetUrban$NPS_Type == "Promoter" & subsetUrban$State_PL == "California"))
length(which(subsetUrban$NPS_Type == "Detractor" & subsetUrban$State_PL == "California"))
length(which(subsetUrban$NPS_Type == "Passive" & subsetUrban$State_PL == "California"))
length(which(subsetUrban$NPS_Type == "" & subsetUrban$State_PL == "California"))





