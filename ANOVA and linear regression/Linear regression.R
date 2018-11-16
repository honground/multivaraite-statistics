### Multivaraite statistics-linear regression

## download package
install.packages("ggplot2") #for visualization
library(ggplot2)

## data load
data <- read.csv("demand.csv",header = TRUE)
data <- data[,2:3] #drop the first column

## make a regression model
lm.1 <- lm(demand.y ~ ., data = data) #model making
summary(lm.1) #model summary

## plotting
data$residulas <- resid(lm.1)
data$predicted <- predict(lm.1)

ggplot(data, aes(x = price.difference.x, y = demand.y)) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Plot regression slope
  geom_segment(aes(xend = price.difference.x, yend = predicted), alpha = .2) +
  geom_point() +
  geom_point(aes(y = predicted), shape = 3, color = "blue") +
  theme_bw()  # Add theme for cleaner look

## predict the value of demand for new datum (i.e. 0)
new_x <- data.frame(price.difference.x=c(0))
predict_x0 <- predict(lm.1, newdata = new_x)