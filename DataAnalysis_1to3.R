library(SDSFoundations)

bike <- BikeData
print(BikeData)
bike$age[7]
firstten <- bike[]
table(bike$cyc_freq[bike$user_id < 11])

bike[bike$user_id < 11,]

femalebiker <- bike[bike$gender == 'F',]
femalebiker[femalebiker$cyc_freq == 'Less than once a month',]


# Import the BikeData dataset, name it "bike"

# Find the number of students in the dataset
table(bike$student)

# Pull out student data into a new dataframe
student <-bike[bike$student==1,]

# Find how often the students ride, using the new dataframe
table(student$cyc_freq)

# Create a vector for the distance variable
distance <-student$distance

# Find average distance ridden
mean(distance)

table(bike$cyc_freq)

daily <- bike[bike$cyc_freq=='Daily',]
table(daily$gender)
mean(daily$age)
age <- daily$age
femaleage <- daily$age[daily$gender=='F']
mean(femaleage)
maleage <- daily$age[daily$gender=='M']
mean(maleage)

thirtymale <- daily[daily$age >= 30 & daily$gender == 'M',]

animal10 <- head(AnimalData,10)

head(AnimalData[AnimalData$Intake.Type=='Owner Surrender',],1)

animaldata <- AnimalData
#Find the number of animals that were adopted
table(animaldata$Outcome.Type)

#Pull out only adopted animals
adopted <- animaldata[animaldata$Outcome.Type=="Adoption",]

#Pull out just the days in shelter for the adopted animals
daystoadopt <- adopted$Days.Shelter

#Visualize and describe this variable
hist(daystoadopt,break==1500)
fivenum(daystoadopt)
median(daystoadopt)
range(daystoadopt)
mean(daystoadopt)
sd(daystoadopt)
which(animaldata$Days.Shelter==max(daystoadopt))
animaldata[animaldata$Days.Shelter==max(daystoadopt),]
(max(daystoadopt)-mean(daystoadopt))/sd(daystoadopt)


head(AnimalData)

dog <- AnimalData[AnimalData$Animal.Type=='Dog',]
adultdog <- dog[dog$Age.Intake > 0,]

cat <- AnimalData[AnimalData$Animal.Type=='Cat',]
adultcat <- cat[cat$Age.Intake > 0,]

hist(adultdog$Weight)
hist(adultcat$Weight)
mean(adultcat$Weight)
sd(adultcat$Weight)
1-pnorm((13-mean(adultcat$Weight))/sd(adultcat$Weight))
fivenum(adultdog$Weight)
catwe13 <- adultcat$Weight[adultcat$Weight > 13]
dogwe13 <- adultdog$Weight[adultdog$Weight > 13]
length(dogwe13)/length(adultdog)

mean(adultcat$Weight)+(2*sd(adultcat$Weight))

adultcat[adultcat$Weight>13,]

table(AnimalData$Intake.Type)
table(AnimalData$Outcome.Type)
prop.table(AnimalData$Intake.Type)
73/129
returnowner <- AnimalData[AnimalData$Outcome.Type=='Return to Owner',]
mean(returnowner$Days.Shelter)

max(daystoadopt)

ownersur <- AnimalData[AnimalData$Intake.Type =='Owner Surrender',]
table(ownersur$Outcome.Type=='Return to Owner'& ownersur$ Animal.Type == 'Dog')
rwdays <- ownersur$Days.Shelter[ownersur$Outcome.Type=='Return to Owner' & ownersur$ Animal.Type == 'Dog']
mean(rwdays)











library(SDSFoundations)
bull <- BullRiders
first10 <- head(bull,10)
first10$YearsPro >= 10
top15 <- bull[bull$Rank15 <= 15, ]
head(top15$BuckOuts14,15)


new_bull <- bull[bull$Events13 > 0,]
plot(new_bull$Rides13,new_bull$Top10_13)
abline(lm(new_bull$Top10_13~new_bull$Rides13))
cor(new_bull$Rides13,new_bull$Top10_13)
myvar <- c("Top10_13","Rides13")
cor(new_bull[,myvar])
hist(new_bull$Rides13,breaks = 25)
fivenum(new_bull$Rides13)
fivenum(new_bull$Top10_13)
mean(new_bull$Rides13)
mean(new_bull$Top10_13)
which(new_bull$Top10_13==2 & new_bull$Rides13==22)
new_bull[which(new_bull$Top10_13==2 & new_bull$Rides13==22),]



new_bull12 <- bull[bull$Events12 > 0,]
hist(new_bull12$Earnings12)
fivenum(new_bull12$Earnings12)
myvar12 <- c("Earnings12","RidePer12","CupPoints12")
cor(new_bull12[,myvar12])
plot(new_bull12$Rides12,new_bull12$Earnings12)

plot(new_bull12$CupPoints12,new_bull12$Earnings12)
abline(lm(new_bull12$Earnings12~new_bull12$Rides12))
abline(lm(new_bull12$Earnings12~new_bull12$CupPoints12))

which(new_bull12$Earnings12 == max(new_bull12$Earnings12))
new_bull12[which(new_bull12$Earnings12 == max(new_bull12$Earnings12)),]

nooutlier <- new_bull12[new_bull12$Earnings12 < 1000000 ,] 
cor(nooutlier[,myvar12])


new_bull14 <- bull[bull$Rides14 >= 1,]
avgrides14pevent <- new_bull14$Rides14/new_bull14$Events14
hist(avgrides14pevent,breaks =25)
fivenum(avgrides14pevent)
plot(avgrides14pevent,new_bull14$Rank14)
abline(lm(new_bull14$Rank14~avgrides14pevent))
cor(new_bull14$Rank14,avgrides14pevent)