library(dplyr)
library(mvnormtest)
library(heplots)
library(stats)

koro <- read.csv("koro.csv",header = TRUE)
koro_preprocessed <- select(koro, therapy)
koro_preprocessed$SIKE_effect <- koro$si_post - koro$si_pre
koro_preprocessed$SFKDI_effect <- koro$sf_post - koro$sf_pre
koro_preprocessed$OAS_effect <- koro$oa_post - koro$oa_pre

#Testing MANOVA assumptions
res.shapiro <-mshapiro.test(t(koro_preprocessed[,2:4]))#tests for multivariate normality
res.boxM <- boxM(Y=koro_preprocessed[,2:4], group = koro_preprocessed[,"therapy"]) #Box's M test

#MANOVA
mod.manova <- manova(cbind(SIKE_effect,SFKDI_effect,OAS_effect) ~ therapy, data=koro_preprocessed)
summary(mod.manova, test="Wilks")
etasq(mod.manova)
summary.aov(mod.manova)

koro_preprocessed %>%
  group_by(therapy) %>%
  dplyr::summarize(Mean = mean(SFKDI_effect+SIKE_effect+OAS_effect),na.rm=TRUE)
groupmean<-c(37,34.4,25.3,8.1)
power.anova.test(groups = 4, n = 10, between.var = var(groupmean),within.var=150,sig.level = 0.05)


#multiple ANOVA
mod.anova <-aov(SFKDI_effect~therapy, data=koro_preprocessed)
etasq(mod.anova)
summary.aov(mod.anova)
TukeyHSD(mod.anova)
koro_preprocessed %>%
  group_by(therapy) %>%
  dplyr::summarize(Mean = mean(SFKDI_effect),na.rm=TRUE)

groupmean<-c(19.1, 16,12.3,1)
power.anova.test(groups = 4, n = 10, between.var = var(groupmean),within.var=120,sig.level = 0.05)