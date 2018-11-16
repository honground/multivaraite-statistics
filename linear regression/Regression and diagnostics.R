### Multivaraite statistics - assignment 2
rm(list = ls())
## install packages
install.packages("car")
install.packages("dplyr")
install.packages("corrplot")
install.packages("fmsb")
library(car)
library(dplyr)
library(corrplot)
library(fmsb)
## load data
data <- read.csv("mpg.csv",header = TRUE)
## data manipulation 
data$origin <- as.factor(data$origin)
data[["model.year"]]<-as.factor(data[["model.year"]])
data[["cylinders"]]<-as.factor(data[["cylinders"]])
data[["horsepower"]]<-as.numeric(data[["horsepower"]])
str(data)
## handling missing data
data[data=="?"] <- NA
na_count <-sapply(data, function(x) sum(length(which(is.na(x)))))
data[is.na(data)] <- mean(data$horsepower, na.rm = TRUE) # mean implementation

## evaluate outliers
# using IQR
for(name in names(data)){
  if((name == "car.name") || (name=="origin")||(name=="cylinders")||(name=="model.year")){
  }
  else{
  x <- data[[name]]
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.3, .7), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1] 
  x[x > (qnt[2] + H)] <- caps[2]
  data[[name]]<-x
  }
}
# using cook's distance
lm.pre <- lm(X...mpg~., data = data)
cooksd<- cooks.distance(lm.pre)
png(filename = "cookd.png")
plot(lm.pre, which=4, labels.id = "")
abline(h=0.03, lty=2, col="red")
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>0.03,names(cooksd),""), col="red") 
dev.off()

data$cooksd<-cooksd
data[data=="NaN"]<- 0
data$cooky <- ifelse(data$cooksd<0.03,"keep","no")
data.new <- subset(data, cooky=="keep")
data.new <- data.new[-c(10,11)]
lm.new <- lm(X...mpg~., data = data.new)

## test regression assumptions
#normality using histogram
png(filename = "histogram.png")
hist(lm.new$residuals, main = "Distribution of residuals", freq = FALSE)
dev.off()
#normality using qqplot
png(filename = "qqplot.png")
plot(lm.new, which = 2)
dev.off()
#normality using Shaprio-Wilk W test
print(shapiro.test(lm.new$residuals))
#linearity using scatterplot
for(name in names(data.new)){
  if((name == "car.name") || (name=="origin")||(name=="cylinders")||(name=='X...mpg')||(name=="model.year")){
  }
  else{
  png(filename = paste("scatterplot",name,".png"))
  plot(data.new[[name]],data.new$X...mpg, main=paste(name,"vs X...mpg"))
  dev.off()
  }
}
data.new2 <- subset(data.new, select = -c(horsepower))
lm.new2 <- lm(X...mpg~., data = data.new2)
#Homoscedasticity using residual plot
png(filename = "residual vs fitted.png")
plot(lm.new2,which=1)
dev.off()

## evaluate multicollinearity
#correlation matrix
cor<-cor(select(data.new2, displacement, weight, acceleration)) #only continuous
cor <- as.matrix(cor)
png(filename = "correlation matrix.png")
corrplot(as.matrix(cor), method = "number")
dev.off()

fmsb::VIF(lm.new2)
ld.vars <- attributes(alias(lm.new2)$Complete)
data.new3 <- subset(data.new2,select = -c(car.name))

## run final regression
lm.new3 <- lm(X...mpg~., data = data.new3)
# test assumptions again
png(filename = "histogram.png")
hist(lm.new3$residuals, main = "Distribution of residuals", freq = FALSE)
dev.off()
png(filename = "qqplot.png")
plot(lm.new3, which = 2)
dev.off()
print(shapiro.test(lm.new3$residuals))

for(name in names(data.new3)){
  if((name == "car.name") || (name=="origin")||(name=="cylinders")||(name=='X...mpg')||(name=="model.year")){
  }
  else{
    png(filename = paste("scatterplot",name,".png"))
    plot(data.new3[[name]],data.new3$X...mpg, main=paste(name,"vs X...mpg"))
    dev.off()
  }
}
png(filename = "residual vs fitted.png")
plot(lm.new3,which=1)
dev.off()
fmsb::VIF(lm.new3)
summary(lm.new3)
