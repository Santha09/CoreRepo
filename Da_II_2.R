library(SDSFoundations)
bull<- BullRiders

usa <- bull[bull$Country == 'USA',]
mean(usa$Weight)
sd(usa$Weight)
hist(usa$Weight,breaks=50)
help(hist)

t.test(usa$Weight,mu=190,assumptions = 'less')

events14 <- bull[bull$Events14 >= 5,]
mean(events14$RidePer14)
sd(events14$RidePer14)

t.test(events14$RidePer14,mu=.50,assumptions = 'greaterequal')
hist(events14$RidePer14)

earnings_per <- mean(bull$Earnings12)
hist(bull$Earnings12,breaks = 40)

earn12 <- log(bull$Earnings12)
bull$earn12 <- log(bull$Earnings12)
hist(bull$earn12)
mean(bull$earn12)
bull$earnings_per <- ifelse(!bull$Events12, 0,bull$Earnings12/bull$Events12)
hist(bull$earnings_per)
bull$logearnings_per <- ifelse(!bull$earnings_per,0,log(bull$earnings_per))
hist(bull$logearnings_per)
earn12 <- bull[bull$logearnings_per != 0,]
mean(earn12$logearnings_per)
t.test(earn12$logearnings_per,mu=8.85)

exp(8.572169)
exp(9.120605)

sum(29.4,29.0,28.4,28.8,28.9,29.3,28.5,28.2)/8
chips <- rep(NA,8)
chips[8] <- 28.2
mean(chips)
sd(chips)

t.test(chips,mu=28.5)
