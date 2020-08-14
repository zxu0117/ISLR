library(ISLR)
library(ggplot2)
library(tidyverse)
library(MASS)
library(class)

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

#KNN
train_X <- cbind(Smarket$Lag1, Smarket$Lag2)[train,]
test_X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
train_Direction <- Smarket$Direction[train]
set.seed(1)
knn_pred <- knn(train_X, test_X, train_Direction, k = 1)
table(knn_pred,Direction_2005)
(83+43)/252
knn_pred <- knn(train_X, test_X, train_Direction, k = 3)
table(knn_pred,Direction_2005)
mean(knn_pred==Direction_2005)

# train_X <- Smarket[train,c("Lag1","Lag2")]
# test_X <- Smarket[!train,c("Lag1","Lag2")]

#Applying to a different data set
Caravan <- ISLR::Caravan
dim(Caravan)
summary(Caravan$Purchase)
348/5474
standardized_X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized_X[,1])
var(standardized_X[,2])
test <- 1:1000
train_X <- standardized_X[-test,]
test_X <- standardized_X[test,]
train_Y <- Caravan$Purchase[-test]
test_Y <- Caravan$Purchase[test]
set.seed(1)
knn_pred <- knn(train_X, test_X, train_Y, k = 1)
mean(test_Y!=knn_pred)
mean(test_Y!="No")
table(test_Y,knn_pred)
knn_pred <- knn(train_X, test_X, train_Y, k = 3)
table(knn_pred,test_Y)
5/26
knn_pred <- knn(train_X, test_X, train_Y, k = 5)
table(knn_pred,test_Y)
4/15

#Fitting new data set with logistic regression
glm_fit <-glm(Caravan$Purchase~., data = Caravan, family = binomial, subset = -test)
glm_prob <- predict(glm_fit, Caravan[test,], type = "response")
glm_pred <- rep("No",1000)
glm_pred[glm_prob >0.5] <- "Yes"
table(glm_pred,test_Y)
glm_fit <-glm(Caravan$Purchase~., data = Caravan, family = binomial, subset = -test)
glm_prob <- predict(glm_fit, Caravan[test,], type = "response")
glm_pred <- rep("No",1000)
glm_pred[glm_prob >0.25] <- "Yes"
table(glm_pred,test_Y)
