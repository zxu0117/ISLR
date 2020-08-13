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
glm_fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
plot(glm_fits)
summary(glm_fits)
coef(glm_fits)
summary(glm_fits)$coef
summary(glm_fits)$coef[,4]
glm_probs <- predict(glm_fits, type = "response")
glm_probs[1:10]
contrasts(Smarket$Direction)
glm_pred <- rep("Down", 1250)
glm_pred[glm_probs>0.5] <- "Up"
table(glm_pred,Smarket$Direction)
mean(glm_pred==Smarket$Direction)

gg_dat <- tibble(glm_probs, Smarket$Direction)
gg_dat$day <- seq(1:nrow(gg_dat))
g <- ggplot(gg_dat, aes(x = day, y = glm_probs, color = Smarket$Direction))
g <- g + geom_point()
g <- g + theme_classic()
plot(g)

train <- (Smarket$Year<2005)
Smarket_2005 <- Smarket[!train,]
dim(Smarket_2005)
Direction_2005 <- Smarket_2005$Direction

glm_fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial, subset = train)
glm_probs <- predict(glm_fits, Smarket_2005, type = "response")
glm_pred <- rep("Down", 252)
glm_pred[glm_probs>0.5] <- "Up"
table(glm_pred,Direction_2005)
mean(glm_pred==Direction_2005)
mean(glm_pred!=Direction_2005)

# ind <- which(Smarket$Year == 2005)
# train <- Smarket[-ind,] 
# test <- Smarket[ind,]
# glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = train, family = binomial)
# glm.probs <- predict(glm.fits, test, type = "response")

glm_fits <- glm(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
glm_probs <- predict(glm_fits, Smarket_2005, type = "response")
glm_pred <- rep("Down", 252)
glm_pred[glm_probs>0.5] <- "Up"
table(glm_pred,Direction_2005)
mean(glm_pred==Direction_2005)
mean(glm_pred!=Direction_2005)

predict(glm_fits,newdata = data.frame(Lag1 = c(1.2,1.5), Lag2 = c(1.5,0.8)), type = "response")

#Linear Discriminate Analysis
lda_fit <- lda(Direction~Lag1+Lag2, data = Smarket, subset = train)
plot(lda_fit)
lda_pred <- predict(lda_fit, Smarket_2005)
names(lda_pred)
lda_class <- lda_pred$class
table(lda_class, Direction_2005)
mean(lda_class==Direction_2005)
sum(lda_pred$posterior[,1]>=.5)
sum(lda_pred$posterior[,1]<.5)
lda_pred$posterior[1:20,1]
lda_class[1:20]
sum(lda_pred$posterior[,1]>.9)

#Quadratic Discriminant Analysis
qda_fit <- qda(Direction~Lag1+Lag2, data = Smarket, subset = train)
qda_pred <- predict(qda_fit, Smarket_2005)
names(qda_pred)
qda_class <- qda_pred$class
table(qda_class, Direction_2005)
mean(qda_class==Direction_2005)
