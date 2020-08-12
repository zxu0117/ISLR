library(ISLR)
library(ggplot2)
library(tidyverse)
library(MASS)

#Stock Market Data set
Smarket <- ISLR::Smarket
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket [,-9])
plot(Smarket$Volume)

#Logistic Regression
glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
plot(glm.fits)
summary(glm.fits)
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Smarket$Direction)
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred,Smarket$Direction)
mean(glm.pred==Smarket$Direction)

gg_dat <- tibble(glm.probs, Smarket$Direction)
gg_dat$day <- seq(1:nrow(gg_dat))
g <- ggplot(gg_dat, aes(x = day, y = glm.probs, color = Smarket$Direction))
g <- g + geom_point()
g <- g + theme_classic()
plot(g)

train <- (Smarket$Year<2005)
Smarket_2005 <- Smarket[!train,]
dim(Smarket_2005)
Direction_2005 <- Smarket_2005$Direction

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket_2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred,Direction_2005)
mean(glm.pred==Direction_2005)
mean(glm.pred!=Direction_2005)

# ind <- which(Smarket$Year == 2005)
# train <- Smarket[-ind,] 
# test <- Smarket[ind,]
# glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = train, family = binomial)
# glm.probs <- predict(glm.fits, test, type = "response")

glm.fits <- glm(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fits, Smarket_2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred,Direction_2005)
mean(glm.pred==Direction_2005)
mean(glm.pred!=Direction_2005)

predict(glm.fits,newdata = data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.5,0.8)), type = "response")
