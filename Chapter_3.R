library(ISLR)
library(MASS)
library(tidyverse)

Boston <- MASS::Boston
names(Boston)
?Boston
lm.fit <- lm(medv~lstat,data = Boston)
summary(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)),interval = "confidence")
predict(lm.fit, data.frame(lstat=c(5,10,15)))

#plotting linear model with R and ggplot
plot(Boston$lstat,Boston$medv)
plot(Boston[,"lstat"], Boston[,"medv"])
plot(Boston[,13], Boston[,14])
abline(lm.fit)

g <- ggplot(Boston, aes(x = lstat, y = medv))
g <- g + geom_point()
g <- g + theme_classic()
g <- g + geom_smooth()
plot(g)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

#Adding columns to data set
Bostontmp <- Boston
Bostontmp$residuals <- residuals(lm.fit)
Bostontmp$predict <- predict(lm.fit)
g <- ggplot(Bostontmp, aes(x = predict, y = residuals, color = residuals))
g <- g + geom_point()
g <- g + theme_classic()
plot(g)

Bostontmp <- Boston
Bostontmp$residuals <- residuals(lm.fit)
Bostontmp$predict <- predict(lm.fit)
g <- ggplot(Bostontmp, aes(x = lstat, y = medv, color = residuals))
g <- g + geom_point()
g <- g + theme_classic()
g <- g + scale_color_gradient2(low = "purple", mid = "white", high = "forestgreen")
plot(g)

#Interaction terms
lm.fit <- lm(medv~lstat + age, data = Boston)
summary(lm.fit)
lm.fit <- lm(medv~., data = Boston)
summary(lm.fit)
summary(lm(medv~lstat*age, data = Boston))

#Non-linear transformations of predictors
lm.fit <- lm(medv~lstat, data = Boston)
lm.fit2 <- lm(medv~poly(lstat,2), data = Boston)
summary(lm.fit)
summary(lm.fit2)
anova(lm.fit,lm.fit2)
plot(lm.fit2)
g <- ggplot(Boston, aes(x = lstat, y = medv))
g <- g + geom_point()
plot(g)
summary(lm(medv~log(Boston$rm),data=Boston))

#Qualitative predictors
Carseats <- ISLR::Carseats
names(Carseats)
lm.fit = lm(Sales~. + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)
?contrasts

#Functions

Scatterplot <- function(data, x, y, smooth = F){
  g <- ggplot(data)
  g <- g + geom_point(aes_string(x = x, y = y))
  
  if(smooth){
    g <- g + geom_smooth(method = "lm", aes_string(x = x, y = y))
  }
  
  plot(g)
}
Scatterplot(Boston, "lstat", "medv", smooth = T)
