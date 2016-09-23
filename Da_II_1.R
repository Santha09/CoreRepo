library(SDSFoundations)
survey <- StudentSurvey

nameg5 <- survey[survey$name_letters > 5,]
first10<- head(survey,10)
first10[first10$name_letters > 5,]

head(survey[survey$happy < 40,])
mean(survey$name_letters)
sd(survey$name_letters)
hist(survey$name_letters)

xbar5 <- rep(NA,1000)
for (i in 1:1000){
  sample5 <- sample(survey$name_letters,size=5)
  xbar5[i]<- mean(sample5)
}
hist(xbar5,xlim=c(2,10))
mean(xbar5)
sd(xbar5)
sd(survey$name_letters)/sqrt(5)

xbar15 <- rep(NA,1000)
for (i in 1:1000){
  sample15 <- sample(survey$name_letters, size =15)
  xbar15[i]<- mean(sample15)
}
hist(xbar15,xlim=c(2,10))
mean(xbar15)
sd(xbar15)
sd(survey$name_letters)/sqrt(15)

xbar25 <- rep(NA,1000)
for (i in 1:1000){
  sample25 <- sample(survey$name_letters, size =25)
  xbar25[i]<- mean(sample25)
}
hist(xbar25,xlim=c(2,10))
mean(xbar25)
sd(xbar25)
sd(survey$name_letters)/sqrt(25)


mean(survey$happy)
sd(survey$happy)
hist(survey$happy)

xbar5 <- rep(NA,1000)
for (i in 1:1000){
  sample5 <- sample(survey$happy,size=5)
  xbar5[i]<- mean(sample5)
}
hist(xbar5,xlim=c(50,100))
mean(xbar5)
sd(xbar5)
sd(survey$happy)/sqrt(5)

xbar15 <- rep(NA,1000)
for (i in 1:1000){
  sample15 <- sample(survey$happy, size =15)
  xbar15[i]<- mean(sample15)
}
hist(xbar15,xlim=c(50,100))
mean(xbar15)
sd(xbar15)
sd(survey$happy)/sqrt(15)

xbar25 <- rep(NA,1000)
for (i in 1:1000){
  sample25 <- sample(survey$happy, size =25)
  xbar25[i]<- mean(sample25)
}
hist(xbar25,xlim=c(50,100))
mean(xbar25)
sd(xbar25)
sd(survey$happy)/sqrt(25)


mean(survey$austin)
sd(survey$austin)
hist(survey$austin)
xbar10 <- rep(NA,1000)
for (i in 1:1000){
  sample10 <- sample(survey$austin,size=10)
  xbar10[i]<- mean(sample10)
}
mean(xbar10)
sd(xbar10)
hist(xbar10)
sd(survey$austin)/sqrt(10)

qnorm(.05)