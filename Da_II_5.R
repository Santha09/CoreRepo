library(SDSFoundations)
filmdata <- FilmData
us <- filmdata[filmdata$Studio=='Uni.',]

us[us$Rank==min(us$Rank),]
min(us$Rank)

gross <- filmdata[sort.list(filmdata$Gross,decreasing = TRUE),]
View(gross)
top10gross <- head(gross,10)
top10gross[top10gross$IMDB==min(top10gross$IMDB),]

pg <- filmdata[filmdata$Rating=='PG',]
pg13 <- filmdata[filmdata$Rating=='PG13',]
r <- filmdata[filmdata$Rating=='R',]
aggregate(Budget~Rating,filmdata,mean)
aggregate(Budget~Rating,filmdata,sd)
budget <- aov(filmdata$Budget~filmdata$Rating)
summary(budget)
TukeyHSD(budget)

aggregate(IMDB~Rating,filmdata,mean)
aggregate(IMDB~Rating,filmdata,sd)
imdb <- aov(filmdata$IMDB~filmdata$Rating)
summary(imdb)
TukeyHSD(imdb)

table(filmdata$Studio)

aggregate(Days~Studio,filmdata,mean)
aggregate(Days~Studio,filmdata,sd)
days <- aov(filmdata$Days~filmdata$Studio)
summary(days)
TukeyHSD(days)

aggregate(Pct.Dom~Studio,filmdata,mean)
aggregate(Pct.Dom~Studio,filmdata,sd)
pctd <- aov(filmdata$Pct.Dom~filmdata$Studio)
summary(pctd)
TukeyHSD(pctd)

filmdata$budcat[filmdata$Budget<100] <- 'L'
filmdata$budcat[filmdata$Budget>=100 && filmdata$Budget<150] <- 'M'
filmdata$budcat[filmdata$Budget>=150] <- 'H'
table(filmdata$budcat)

aggregate(Pct.Dom~budcat,filmdata,mean)
aggregate(Pct.Dom~budcat,filmdata,sd)
boxplot(filmdata$Pct.Dom~filmdata$budcat)
bcat <- aov(filmdata$Pct.Dom~filmdata$budcat)
summary(bcat)
TukeyHSD(bcat)


aggregate(tick~sec,ptick,mean)
aggregate(tick~sec,ptick,sd)
boxplot(ptick$tick~ptick$sec)
tick <- aov(ptick$tick~ptick$sec)
summary(tick)
TukeyHSD(tick)

