library(lattice)
library(psych)
library(GPArotation)
library(psy)
library(dplyr)
##Multivaraite stats hw6 by suckwon hong
rm(list = ls())
getwd()
setwd("/Users/suckwonhong/Desktop/multivariate_stats")

data <- read.csv("wiscsem.csv",header = TRUE)
data$X...client<-NULL
data$agemate<-NULL

#pca varimax
pca.vari<-psych::principal(data, nfactors = 11, rotate = "varimax")
pca.vari.res<-as.data.frame(pca.vari$Vaccounted)
png(filename = "scree plot.png")
plot(c(1:11),pca.vari.res[2,], main = "Scree plot", ylab = "Proportion of variance explained",
     xlab = "Principal component")
dev.off()

#loadings
pca.vari.load<-(unclass(pca.vari$loadings))
pca.vari.load[pca.vari.load<0.15]<-NA

#pca oblique
pca.oblique<-psych::principal(data, nfactors = 11, rotate = "oblimin")
pca.oblique.res<-as.data.frame(pca.oblique$Vaccounted)
png(filename = "scree plot_obli.png")
plot(c(1:11),pca.oblique.res[2,], main = "Scree plot", ylab = "Proportion of variance explained",
     xlab = "Principal component")
dev.off()

#loadings
pca.oblique.load<-(unclass(pca.oblique$loadings))
pca.oblique.load[pca.oblique.load<0.15]<-NA

#cronbach alpha
pc1<-cronbach(select(data, info,comp,arith))
pc2<-cronbach(select(data, info,digit))
pc3<-cronbach(select(data, pictcomp,block,object))
pc8<-cronbach(select(data, info,parang,block))
pc4<-cronbach(select(data, comp,simil,pictcomp,block,object))
pc5<-cronbach(select(data, coding))
pc6<-cronbach(select(data, pictcomp,block,object))
pc7<-cronbach(select(data, info,comp,simil,vocab))



