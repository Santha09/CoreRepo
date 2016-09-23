library(SDSFoundations)
austin <- AustinCityLimits
austin[austin$Artist == 'Allen Toussaint',]

gendert <- table(austin$Gender)
gendert
116*.35
table(austin$Season)

genderexpt<- c(.50,.50)
genderexpt
chisq.test(gendert,p=genderexpt)$expected

top10 <- table(austin$Gender,austin$BB.wk.top10)
top10
chisq.test(top10)$expected
table(austin$BB.wk.top10)
chisq.test(top10,correct=F)
table(austin$Gender)


genre <- table(austin$Genre)
genre
genreexp <- c(.25,.25,.25,.25)
chisq.test(genre,p=genreexp)

twittergenre <- table(austin$Genre,austin$Twitter.100k)
twitter
chisq.test(twittergenre)$expected
chisq.test(twittergenre,correct=F)

austin$recent[austin$Year >= 2012] <- 1
austin$recent[austin$Year < 2012] <- 0

genderrec<-table(austin$Gender,austin$recent)
chisq.test(genderrec)$expected
chisq.test(genderrec,correct=F)
