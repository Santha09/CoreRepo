library(SDSFoundations)
post <- PostSurvey
male <- post[post$gender=='Male',]
first10 <- head(post,10)
first10$live_campus=="yes"

upperhap <- post$happy[post$classification=="Junior"|post$classification=="Senior"]
lowerhap <- post$happy[post$classification=="Sophomore"|post$classification=="Freshman"]

mean(upperhap)
hist(upperhap)
hist(lowerhap)
t.test(upperhap,lowerhap)

diff_hap <- post$happy - post$post_happy
hist(diff_hap)
t.test(post$happy,post$post_happy,paired=T)

diffwork <- post$hw_hours_HS - post$hw_hours_college
hist(diffwork)
t.test(post$hw_hours_HS,post$hw_hours_college,paired=T)

greek <- post$sleep_Sat[post$greek == "yes"]
nongreek <- post$sleep_Sat[post$greek == "no"]
hist(greek)
hist(nongreek)
t.test(greek,nongreek,alternative = 'less')


diffworkn <- post$hw_hours_HS[post$major=="Nursing"] - post$hw_hours_college[post$major=="Nursing"] 
diffworkb <- post$hw_hours_HS[post$major=="Biology"] - post$hw_hours_college[post$major=="Biology"] 

hist(diffworkn)
hist(diffworkb)
t.test(diffworkn,diffworkb)

cpleft <- var(16,NA)
cpleft[1] <- 16.3
cpleft[16] <- 17.7

cpright <- var(16,NA)
cpright[16] <- 11.1
davg<- mean(cpleft-cpright)
sd(cpleft-cpright)
t.test(cpleft,cpright,paried=T,alternative ='greater')

d <- (cpleft-cpright)-3.1
sqrt(sum(d*d)/15)/sqrt(15)

