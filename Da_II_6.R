library(SDSFoundations)
res <- TempskiResilience
clin <- res[res$Group == "Clinical Sciences",]
#Intial Correlations
vars <- c("QoL", "BDI")
cor(clin[,vars])

#RQ1 Model
ov_mod <- lm(QoL ~ BDI, data=clin)
summary(ov_mod)
confint(ov_mod)

#Diagnostics
plot(ov_mod, which=1)
cutoff <- 4/(ov_mod$df) 
plot(ov_mod, which=4, cook.levels=cutoff)

#Initial correlations
vars <- c("MS.QoL", "DREEM.S.SP", "DREEM.A.SP", "Resilience", "BDI", "Age")
cor(clin[,vars], use="pairwise.complete.obs")

#Test the initial correlations
library(psych)
corr.test(clin[,vars], use="pairwise.complete.obs")

#RQ2 Model
ms_mod <- lm(MS.QoL ~ DREEM.S.SP + DREEM.A.SP + Resilience + BDI + Age, data=clin)
summary(ms_mod)
confint(ms_mod)

#Diagnostics
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)

#Put model into context
library(ggm)
lm.beta(ms_mod) 
lm.beta::lm.beta(ms_mod)
round(pCorr(ms_mod), 4) 


sci <- res[res$Group == "Basic Sciences",]
names(sci)
#Initial correlations
vars <- c("MS.QoL", "WHOQOL.PH", "WHOQOL.SOC", "WHOQOL.ENV","WHOQOL.PSY")
cor(sci[,vars], use="pairwise.complete.obs")

#Test the initial correlations
library(psych)
corr.test(sci[,vars], use="pairwise.complete.obs")

#RQ2 Model
ms_mod <- lm(MS.QoL ~ WHOQOL.PH+WHOQOL.SOC+WHOQOL.ENV+WHOQOL.PSY, data=sci)
summary(ms_mod)
confint(ms_mod)

#Diagnostics
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)

#Put model into context
library(ggm)
lm.beta(ms_mod) 
lm.beta::lm.beta(ms_mod)
round(pCorr(ms_mod), 4) 




library(SDSFoundations)
res <- TempskiResilience
clinc <- res[res$Group == "Clinical Sciences",]
names(clinc)
#Initial correlations
vars <- c("BDI", "State.Anxiety", "Trait.anxiety", "Age","Female")
cor(clinc[,vars], use="pairwise.complete.obs")

#Test the initial correlations
library(psych)
corr.test(clinc[,vars], use="pairwise.complete.obs")

#RQ2 Model
ms_mod <- lm(BDI ~ State.Anxiety+Trait.anxiety+Age+Female, data=clinc)
summary(ms_mod)
confint(ms_mod)

#Diagnostics
library(car)
vif(ms_mod)
plot(ms_mod, which=1)
cutoff <- 4/(ms_mod$df) 
plot(ms_mod, which=4, cook.levels=cutoff)

#Put model into context
library(ggm)
lm.beta(ms_mod) 
lm.beta::lm.beta(ms_mod)
round(pCorr(ms_mod), 4) 




