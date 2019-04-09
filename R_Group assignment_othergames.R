#############--------------------R group assignment on gambling dataset(Group 9)-------------------###############
####---------subsetting the other games----------####

#Install the packages:
install.packages('dplyr')
library(dplyr)
install.packages("tidyr")
library(tidyr)
install.packages('stringr')
library(stringr)
install.packages("tidyverse")
library(tidyverse)

#Reading the basetable
Overalldata = read.csv('Gambling_basetable_final.csv')
country = read.csv('country.csv')
head(Overalldata)

Overalldata$X = NULL

otherproduct = Overalldata[,c(1,2,13:27,32:37)]

str(otherproduct)

#########Casino#########

Casino = otherproduct[,c(1,2,8:12,18:23)]
which(Casino$Casino_max_stakes == 0 & Casino$Casino_mean_bets == 0 & Casino$Casino_mean_Stakes == 0 & Casino$Casino_mean_Winnings == 0)
Casino = Casino[-which(Casino$Casino_max_stakes == 0 & Casino$Casino_mean_bets == 0 & Casino$Casino_mean_Stakes == 0 & Casino$Casino_mean_Winnings == 0),]
Casino = merge(x=Casino,y=country,by = "UserID", all.x = TRUE)
Casino$X = NULL
Casino = Casino[,c(1:7,14)]
str(Casino)
Casino$Continent = as.character(Casino$Continent)

Casino$Continent[which(is.na(Casino$Continent))] = 'Others'
Casino$Gender[which(Casino$Gender == 1)] = 'Male'
Casino$Gender[which(Casino$Gender == 0)] = 'Female'

summary(Casino)

write.csv(Casino,file='Casino.csv')


#########Games#########


Games = otherproduct[,c(1,2,13:23)]
which(Games$Games_mean_Winnings == 0 & Games$Games_max_stakes == 0 & Games$Games_mean_bets == 0 & Games$Games_mean_Stakes == 0)
Games = Games[-which(Games$Games_mean_Winnings == 0 & Games$Games_max_stakes == 0 & Games$Games_mean_bets == 0 & Games$Games_mean_Stakes == 0),]
Games = merge(x=Games,y=country,by="UserID",all.x = TRUE)
Games$X = NULL
Games = Games[,c(1:7,14)]
str(Games)
Games$Continent = as.character(Games$Continent)
Games$Continent[which(is.na(Games$Continent))] = 'Others'
Games$Gender[which(Games$Gender == 1)] = 'Male'
Games$Gender[which(Games$Gender == 0)] = 'Female'

summary(Games)
write.csv(Games,file='Games.csv')

########Supertoto######

Supertoto = otherproduct[,c(1:7,18:23)]
which(Supertoto$Supertoto_max_stakes == 0 & Supertoto$Supertoto_mean_Bets == 0 & Supertoto$Supertoto_mean_Stakes == 0 & Supertoto$Supertoto_mean_Winnings == 0)
Supertoto = Supertoto[-which(Supertoto$Supertoto_max_stakes == 0 & Supertoto$Supertoto_mean_Bets == 0 & Supertoto$Supertoto_mean_Stakes == 0 & Supertoto$Supertoto_mean_Winnings == 0),]
Supertoto = merge(x=Supertoto,y=country,by="UserID",all.x = TRUE)
Supertoto$X = NULL
Supertoto = Supertoto[,c(1:7,14)]
str(Supertoto)
Supertoto$Continent = as.character(Supertoto$Continent)
Supertoto$Continent[which(is.na(Supertoto$Continent))] = 'Others'
Supertoto$Gender[which(Supertoto$Gender == 1)] = 'Male'
Supertoto$Gender[which(Supertoto$Gender == 0)] = 'Female'

summary(Supertoto)

write.csv(Supertoto,file='Supertoto.csv')

############Creating a table to include all other games except poker and sports(FO and LA)######
L = list(Casino,Games,Supertoto)

Allothergames = reduce(L, full_join, by = "UserID") %>% replace(., is.na(.), 0)
dim(Allothergames)

Allothergames = Allothergames[,-c(2,8,9,15,16,22)]

summary(Allothergames)

###Conbining the values of mean,max stakes,bets and winnings into one column
Allothergames = mutate(Allothergames, max_stakes = pmax(Supertoto_max_stakes,Casino_max_stakes,Games_max_stakes))
Allothergames = mutate(Allothergames, mean_stakes = pmax(Supertoto_mean_Stakes,Casino_mean_Stakes,Games_mean_Stakes))
Allothergames = mutate(Allothergames, mean_bets = pmax(Supertoto_mean_Bets,Casino_mean_bets,Games_mean_bets))
Allothergames = mutate(Allothergames, mean_winnings = pmax(Supertoto_mean_Winnings,Casino_mean_Winnings,Games_mean_Winnings))
Allothergames = mutate(Allothergames, LOS = pmax(Supertoto_LOS,Casino_LOS,Games_LOS))

#Delete the duplicate columns
Allothergames = Allothergames[,-c(2:16)]

#merge with the country table to remove the dummy variables for country and have a new one
Allothergames = merge(x=Allothergames,y=Demographic,by="UserID",all.x = TRUE)

str(Allothergames)

Allothergames$Continent[which(is.na(Allothergames$Continent))] = 'Others'

unique(Demographic$Continent)
summary(Allothergames)
write.csv(Allothergames,file='Allothergames.csv')

###Extracting data from the base table to see the overall users played across countries, gender etc

Demographic = Overalldata[,c(1,2,32:37)]
Demographic = merge(x=Demographic,y=country,by="UserID",all.x = TRUE)
Demographic$X = NULL
Demographic= Demographic[,c(1,2,9)]
str(Demographic)
Demographic$Continent = as.character(Demographic$Continent)
Demographic$Continent[which(is.na(Demographic$Continent))] = 'Others'
Demographic$Gender[which(Demographic$Gender == 1)] = 'Male'
Demographic$Gender[which(Demographic$Gender == 0)] = 'Female'

write.csv(Demographic,file='Demographics.csv')

hist(Casino$Casino_LOS)

barplot(table(Demographic$Continent))

