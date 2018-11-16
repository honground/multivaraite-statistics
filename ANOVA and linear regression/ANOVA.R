### Multivaraite statistics-ANOVA
rm(list = ls())
## data load
data <- read.csv("advertisement.csv",header = TRUE)
data$Day<-data$X...Day
data$X...Day<-NULL
## ANOVA without interaction
res.aov2 <- aov(Response ~ Day + Section, data = data)
summary(res.aov2)

## ANOVA with interaction
res.aov3 <- aov(Response ~ Day*Section, data=data)
summary(res.aov3)

## interaction plot
par(mar=c(3,3,3,3))
interaction.plot(data$Section, data$Day, data$Response, bty='l', main="interaction effect plot")

