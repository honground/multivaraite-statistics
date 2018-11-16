###Multivariate analysis hw4
rm(list = ls())
install.packages("pscl")
install.packages("lmtest")
library(pscl)
library(stats)
library(lmtest)
bank <- read.table("bank.txt",sep=";",header = T)
bank$education<-as.numeric(bank$education)

#data split
smp_size <- floor(0.75 * nrow(bank))
set.seed(123)
train_ind <- sample(seq_len(nrow(bank)), size = smp_size)

train <- bank[train_ind, ]
test <- bank[-train_ind, ]

#model fitting
mod1.logit <- glm(y~ age+marital+education+default+balance+housing+campaign,
                 family = binomial,data = train)
summary(mod1.logit)

#goodness of fit
with(mod1.logit, null.deviance - deviance)
with(mod1.logit, df.null - df.residual)
with(mod1.logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#pseudo R (McFAdden)
pR2(mod1.logit)

#logit-estimate
coef(mod1.logit)
#odd-ratio
exp(coef(mod1.logit))

#likelihood test
mod2.logit <- glm(y ~ age+marital+education+default+balance+housing+campaign+previous,
                  family = binomial,data = train)
summary(mod2.logit)

anova(mod1.logit,mod2.logit, test = "LRT")
lrtest(mod1.logit,mod2.logit)
