#############--------------------R group assignment on gambling dataset(Group 9)-------------------###############

#Read the files:
install.packages("haven")
library(haven)

Gambdemo = read_sas("RawDataIDemographics.sas7bdat")
head(Gambdemo)
dim(Gambdemo)
Gambpok = read_sas("RawDataIIIPokerChipConversions.sas7bdat")
head(Gambpok)
Gambagg = read_sas("RawDataIIUserDailyAggregation.sas7bdat")
head(Gambagg)
Gambintnt = read_sas("AnalyticDataInternetGambling.sas7bdat")
head(Gambintnt)



install.packages('dplyr')
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages('stringr')
library(stringr)

#Deriving new variables from the aggregation table

Gambagg1 = Gambagg %>% separate(Date, into = c('Year', 'Date'), sep = 4, convert = TRUE)
Gambagg1 = Gambagg1 %>% separate(Date, into = c('Month', 'Day'), sep = -2, convert = TRUE)
head(Gambagg1)

#nchar(Gambagg1$Day) == 1 

Gambagg1$Day = str_pad(Gambagg1$Day,2,pad = "0")
Gambagg1$Month = str_pad(Gambagg1$Month,2,pad = "0")


Gambagg1 = Gambagg1 %>% unite("Date",c("Day","Month","Year"),sep="/")


Gambagg1$Date = as.Date(Gambagg1$Date, format = '%d/%m/%Y')

Gambagg2 <- Gambagg1 %>% group_by(UserID,ProductID) %>% summarise(mean_Stakes = round(mean(Stakes),2), 
                                                     mean_Winnings = round(mean(Winnings),2),
                                                     mean_Bets = round(mean(Bets),2),mean_12Stakes = round(max(Stakes),2),mean_Recency_bet = as.Date("30/09/2005",'%d/%m/%Y') - max(Date))
                                                
head(Gambagg2)
tail(Gambagg1)
dim(Gambagg2)
unique(Gambagg2$ProductID)
length(unique(Gambagg$UserID))

#Creating dummy variables for the each of the products

install.packages("dummies")
library(dummies)

#Gambagg1$ProductID = as.character(Gambagg1$ProductID)
#str(Gambagg1)
#Gambagg1$Product1 <- ifelse(Gambagg1$ProductID == "1", 1, 0)
#Gambagg1$Product2 <- ifelse(Gambagg1$ProductID == "2", 1, 0)
#Gambagg1$Product3 <- ifelse(Gambagg1$ProductID == "3", 1, 0)
#Gambagg1$Product4 <- ifelse(Gambagg1$ProductID == "4", 1, 0)
#Gambagg1$Product5 <- ifelse(Gambagg1$ProductID == "5", 1, 0)
#Gambagg1$Product6 <- ifelse(Gambagg1$ProductID == "6", 1, 0)
#Gambagg1$Product7 <- ifelse(Gambagg1$ProductID == "7", 1, 0)
#Gambagg1$Product8 <- ifelse(Gambagg1$ProductID == "8", 1, 0)

#head(Gambagg1)


Gambagg3 = Gambagg2 %>%
  gather(Var, val, starts_with("mean")) %>% 
  unite(Var1,Var, ProductID) %>% 
  spread(Var1, val)

dim(Gambagg3)
head(Gambagg3)
length(unique(Gambagg3$UserID))



#Deriving new variables from the Poker chip conversion table

Gambpok$TransDateTime = as.POSIXct(Gambpok$TransDateTime,'%Y-%m-%d %H:%M:%S')

Gambpok$TransDateTime = format(as.POSIXct(Gambpok$TransDateTime,'%Y-%m-%d %H:%M:%S'), format = '%Y-%m-%d')

Gambpok$TransDateTime = as.Date(Gambpok$TransDateTime, format = '%Y-%m-%d')

Gambpok1 <- Gambpok %>% group_by(UserID,TransType) %>% summarise(mean_TransAmount = round(sum(TransAmount),2),
                                                                 mean_Recency_poker = as.Date("30/09/2005",'%d/%m/%Y') - max(TransDateTime))
                                                            
head(Gambpok)

head(Gambpok1)                                                            
length(unique(Gambpok$UserID))
tail(Gambpok)

#Spreading the columns in poker based on TransType

Gambpok2 = Gambpok1 %>%
  gather(Var, val, starts_with("mean")) %>% 
  unite(Var1,Var, TransType) %>% 
  spread(Var1, val)

#Gambpok2 = spread(Gambpok1,TransType,mean_TransAmount)

head(Gambpok2)

names(Gambpok2)[4] = "Poker_Sell"
names(Gambpok2)[5] = "Poker_Buy"
names(Gambpok2)[3] = "Recency_Poker"

Gambpok2['Profit_Poker'] = Gambpok2$Poker_Sell - Gambpok2$Poker_Buy

Gambpok2$mean_Recency_poker_124 = NULL

#Merging of the Aggregation and Poker columns:
Gambdata = merge(x=Gambagg3,y=Gambpok2,by = "UserID",all.x = TRUE)
head(Gambdata)


#Renaming the columns
colnames(Gambdata)[2:8]
colnames(Gambdata)[c(2:8)] = c("Max_Stakes_1","Max_Stakes_2","Max_Stakes_4","Max_Stakes_5","Max_Stakes_6",
                         "Max_Stakes_7","Max_Stakes_8")

install.packages('readxl')
library(readxl)

countrydata2 <- read_excel("Country_data.xls", col_names = FALSE)

#Rename column names of country data file
colnames(countrydata2)[1] <- "Country"
colnames(countrydata2)[2] <- "Names"

#Merging country data with original demographics table
Gambdemo1 = merge(x = Gambdemo, y = countrydata2, by = "Country", all.x = TRUE)

#Using this pqckqge to retrive the continent each user is from (based on country)
install.packages('countrycode')
library(countrycode)

Gambdemo1$Continent <- factor(countrycode(sourcevar = Gambdemo1[, "Names"],
                                          origin = "country.name",
                                          destination = "continent"))

#R countrycode package didn't recognize 2 countries (David Garcia ane Serbia and Montenegro), so we manually added continent
Gambdemo1[42643, 3] = "Europe"
Gambdemo1[42644, 3] = "Europe"
Gambdemo1[42645, 3] = "Europe"
Gambdemo1[42646, 3] = "Europe"
Gambdemo1[42647, 3] = "Europe"
Gambdemo1[42648, 3] = "Europe"
Gambdemo1[42649, 3] = "Asia"

#Removing uneccessary columns
Gambdemo1$Names <- Gambdemo1$Language <- Gambdemo1$RegDate <- Gambdemo1$FirstSp <- Gambdemo1$FirstCa <- Gambdemo1$FirstGa <-
  Gambdemo1$FirstPo <- Gambdemo1$ApplicationID <- Gambdemo1$Country <- Gambdemo1$FirstAct <- NULL

#Converting date columns into separate columns for year, month, day
install.packages('dplyr')
install.packages('tidyr')
library('dplyr')
library('tidyr')

Gambdemo1 <- Gambdemo1 %>% separate(FirstPay, into = c('Year', 'FirstPay'), sep = 4, convert = TRUE)
Gambdemo1 <- Gambdemo1 %>% separate(FirstPay, into = c('Month', 'FirstPay'), sep = -2, convert = TRUE)
colnames(Gambdemo1)[2] <- "Year_FirstPay"
colnames(Gambdemo1)[3] <- "Month_FirstPay"
colnames(Gambdemo1)[4] <- "Day_FirstPay"

#Find and delete users from after cut-off date (Sept 30, 2005)
which(Gambdemo1$Month_FirstPay == "10")
Gambdemo1 <- Gambdemo1[-which(Gambdemo1$Month_FirstPay == "10"), ]

head(Gambdemo1)

dim(Gambdemo1)

#Concatenate date back together
install.packages('stringr')
library(stringr)

Gambdemo1$Day_FirstPay = str_pad(Gambdemo1$Day_FirstPay,2,pad = "0")
Gambdemo1$Month_FirstPay = str_pad(Gambdemo1$Month_FirstPay,2,pad = "0")

Gambdemo1 = Gambdemo1 %>% unite("Date",c("Day_FirstPay","Month_FirstPay","Year_FirstPay"),sep="/")

Gambdemo1$Date = as.Date(Gambdemo1$Date, format = '%d/%m/%Y')

names(Gambdemo1)[2] = "First_pay_Date"
names(Gambdemo1)[1] = "UserID"

write.csv(Gambdemo1, "Gambdemo1.csv")

Gambdataoveralldata = merge(x=Gambdata,y=Gambdemo1,by = "UserID", all.x = TRUE)
head(Gambdataoveralldata)
unique(Gambdataoveralldata['Continent'])

#Data cleaning of the overall data
Gambdataoveralldata1 = Gambdataoveralldata[,-41]

dim(Gambdataoveralldata1)

#Creating dummy variables
for(v in "Continent"){
  d = dummy(v,data=Gambdataoveralldata1)
  Gambdataoveralldata1 = cbind(Gambdataoveralldata1,d)
  Gambdataoveralldata1[v] = NULL
}

#checking the NA values

summary(Gambdataoveralldata1)

Gambdataoveralldata1$Recency_Poker[which(Gambdataoveralldata1$Recency_Poker < 0)] = 0

Gambdataoveralldata1 = Gambdataoveralldata1[-which(is.na(Gambdataoveralldata1$Gender)),]

Gambdataoveralldata1 = Gambdataoveralldata1[]

is.na.dataframe <- function(x)
   do.call(cbind,lapply(x,is.na))

Gambdataoveralldata1[is.na(Gambdataoveralldata1)] <- 0

summary(Gambdataoveralldata1)

which(Gambdataoveralldata1$ContinentNA == 1)

names(Gambdataoveralldata1)[16] = 'Recency_bet_sportsFO'
names(Gambdataoveralldata1)[17] = 'Recency_bet_sportsLA'
names(Gambdataoveralldata1)[18] = 'Recency_bet_Casinoboss'
names(Gambdataoveralldata1)[19] = 'Recency_bet_supertoto'
names(Gambdataoveralldata1)[20] = 'Recency_bet_GamesVS'
names(Gambdataoveralldata1)[21] = 'Recency_bet_Gamesbwin'
names(Gambdataoveralldata1)[22] = 'Recency_bet_casinochartwell'


Gambdataoveralldata1['Casino_max_stakes'] = Gambdataoveralldata1['Max_Stakes_4'] + Gambdataoveralldata1['Max_Stakes_8']
Gambdataoveralldata1['Games_max_stakes'] = Gambdataoveralldata1['Max_Stakes_6'] + Gambdataoveralldata1['Max_Stakes_7']

Gambdataoveralldata1$Max_Stakes_4 = Gambdataoveralldata1$Max_Stakes_8 = Gambdataoveralldata1$Max_Stakes_6 = Gambdataoveralldata1$Max_Stakes_7 = NULL

Gambdataoveralldata1['Casino_mean_bets'] = Gambdataoveralldata1['mean_Bets_4'] + Gambdataoveralldata1['mean_Bets_8']
Gambdataoveralldata1['Games_mean_bets'] = Gambdataoveralldata1['mean_Bets_6'] + Gambdataoveralldata1['mean_Bets_7']

Gambdataoveralldata1$mean_Bets_4 = Gambdataoveralldata1$mean_Bets_8 = Gambdataoveralldata1$mean_Bets_6 = Gambdataoveralldata1$mean_Bets_7 = NULL

Gambdataoveralldata1['Casino_mean_Stakes'] = Gambdataoveralldata1['mean_Stakes_4'] + Gambdataoveralldata1['mean_Stakes_8']
Gambdataoveralldata1['Games_mean_Stakes'] = Gambdataoveralldata1['mean_Stakes_6'] + Gambdataoveralldata1['mean_Stakes_7']

names(Gambdataoveralldata1)[2] = 'sportsFO_max_stakes'
names(Gambdataoveralldata1)[3] = 'sportsLA_max_stakes'
names(Gambdataoveralldata1)[4] = 'Supertoto_max_stakes'
names(Gambdataoveralldata1)[5] = 'sportsFO_mean_Bets'
names(Gambdataoveralldata1)[6] = 'sportsLA_mean_Bets'
names(Gambdataoveralldata1)[7] = 'Supertoto_mean_Bets'
names(Gambdataoveralldata1)[15] = 'sportsFO_mean_Stakes'
names(Gambdataoveralldata1)[16] = 'sportsLA_mean_Stakes'
names(Gambdataoveralldata1)[17] = 'Supertoto_mean_Stakes'
names(Gambdataoveralldata1)[18] = 'sportsFO_mean_Winnings'
names(Gambdataoveralldata1)[19] = 'sportsLA_mean_Winnings'
names(Gambdataoveralldata1)[20] = 'Supertoto_mean_Winnings'
names(Gambdataoveralldata1)[26] = 'Africa'
names(Gambdataoveralldata1)[27] = 'Americas'
names(Gambdataoveralldata1)[28] = 'Asia'
names(Gambdataoveralldata1)[29] = 'Europe'
names(Gambdataoveralldata1)[30] = 'Oceania'
names(Gambdataoveralldata1)[31] = 'Others_Country'


Gambdataoveralldata1$mean_Stakes_4 = Gambdataoveralldata1$mean_Stakes_8 = Gambdataoveralldata1$mean_Stakes_6 = Gambdataoveralldata1$mean_Stakes_7 = NULL

Gambdataoveralldata1['Casino_mean_Winnings'] = Gambdataoveralldata1['mean_Winnings_4'] + Gambdataoveralldata1['mean_Winnings_8']
Gambdataoveralldata1['Games_mean_Winnings'] = Gambdataoveralldata1['mean_Winnings_6'] + Gambdataoveralldata1['mean_Winnings_7']

Gambdataoveralldata1$mean_Winnings_4 = Gambdataoveralldata1$mean_Winnings_8 = Gambdataoveralldata1$mean_Winnings_6 = Gambdataoveralldata1$mean_Winnings_7 = NULL

names(Gambdataoveralldata1)[10] = 'Recency_bet_Casinoboss'

Gambdataoveralldata2 = mutate(Gambdataoveralldata1, Casino_Recency_bet = pmin(Recency_bet_Casinoboss,Recency_bet_casinochartwell))
Gambdataoveralldata2 = mutate(Gambdataoveralldata2, Games_Recency_bet = pmin(Recency_bet_GamesVS,Recency_bet_Gamesbwin))

Gambdataoveralldata2$Recency_bet_Casinoboss = Gambdataoveralldata2$Recency_bet_casinochartwell = Gambdataoveralldata2$Recency_bet_GamesVS = Gambdataoveralldata2$Recency_bet_Gamesbwin = NULL

#Rearraging the variable names:

Gambdataoveralldata3 = Gambdataoveralldata2[,c(1,21,2,3,5,6,11,12,14,15,8,9,10,4,7,13,16,10,28,30,32,34,36,29,
                                               31,33,35,37,18,19,20,17,22,23,24,25,26,27)]
Gambdataoveralldata3$Recency_bet_supertoto = NULL

names(Gambdataoveralldata3)[17] = 'Recency_bet_supertoto'

write.csv(Gambdataoveralldata3,file='Gambling_basetable.csv')


