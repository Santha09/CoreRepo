library(SDSFoundations)
acl <- AustinCityLimits

View(acl)
aclfg60 <- acl[acl$Age > 60 & acl$Gender=='F',]

gender <- table(acl$Gender)
gender30 <- acl[acl$Age >= 30,]
table(gender30$Gender)
tabo30 <- table(gender30$Genre,gender30$Gender)
prop.table(tabo30,2)
genre <- table(gender30$Genre)
prop.table(genre)
tab <- table(acl$Genre,acl$Gender)
tab
prop.table(tabo30,2)

male<- acl[acl$Gender=='M',]
table(male$Grammy,male$Genre)

tabgrammy <- table(male$Grammy)
prop.table(tabgrammy)
tabgg<- table(male$Grammy,male$Genre)
prop.table(tabgg,2)
prop.table(tabgg,1)
tabgg

table(acl$Grammy,acl$Genre)
barplot(tabgg,beside=T,legend.text = T)

table(acl$Facebook.100k)
tabfb<- table(acl$Facebook.100k,acl$Age.Group)
prop.table(tabfb,2)





# linear regression#
table(WorldRecords$Event)

which(WorldRecords$Athlete=='Usain Bolt' & WorldRecords$Event == 'Mens 100m')
WorldRecords[which(WorldRecords$Athlete=='Usain Bolt' & WorldRecords$Event == 'Mens 100m'),]
which(WorldRecords$Event == 'Womens Mile' & WorldRecords$Record < 260)
WorldRecords[which(WorldRecords$Event == 'Womens Mile' & WorldRecords$Record < 260),]

#Subset the data
WR <- WorldRecords
menshot <- WR[WR$Event=='Mens Shotput',]
womenshot <- WR[WR$Event=='Womens Shotput',] 

#Create scatterplots
plot(menshot$Year,menshot$Record,main='Mens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenshot$Year,womenshot$Record,main='Womens Shotput World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

#Run linear models
linFit(menshot$Year, menshot$Record)
linFit(womenshot$Year,womenshot$Record)


menmile <- WR[WR$Event =='Mens Mile',]
womenmile <- WR[WR$Event =='Womens Mile',]
plot(menmile$Year,menmile$Record,main='Mens Mile World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
plot(womenmile$Year,womenmile$Record,main='Womens Mile World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)

linFit(menmile$Year,menmile$Record)
linFit(womenmile$Year,womenmile$Record)

menpole <-WR[WR$Event == 'Mens Polevault' & WR$Year >= 1970,]
menpole[menpole$Year == max(menpole$Year),]
menpole[menpole$Record>6,]

plot(menpole$Year,menpole$Record,main='Mens Polevault World Records',xlab='Year',ylab='World Record Distance (m)',pch=16)
linFit(menpole$Year,menpole$Record)
