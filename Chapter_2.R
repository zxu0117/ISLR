library(ISLR)
library(tidyverse)

x <- c(1,3,8,5)
x
y <- c(4,7,6,9)
length(x)
length(y)
ls()
rm(list = ls())
?matrix
x <-matrix(data <-c(1,3,8,5), nrow = 2, ncol = 2, byrow = TRUE)
x
matrix(c(1,2,3,4), 2, 2, byrow = TRUE)
sqrt(x)
x^2
x <-rnorm(50)
hist(x)
plot(density(x))
y <-x+rnorm(50, mean = 50, sd = 0.1)
hist(y)
cor(x,y)
plot(x,y)
set.seed(2)
x <-rnorm(50)
set.seed(3)
y <-rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)
scale(y)
x <-rnorm(100)
y <-rnorm(100)
plot(x,y)
plot(x,y,xlab = "this is the x-axis",ylab = "this is the y-axis",
     main = "Plot of X vs Y")
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()


dat <-data.frame(x,y, sample(c("A", "B"),100, replace =T))
colnames(dat)[3] <- "Sample"
dat <- as_tibble(dat)
dat

g <- ggplot(dat, aes(x = x, y = y))
g <- g + geom_density2d_filled()
g <- g + geom_point(aes(col = Sample))
g <- g + facet_grid(~Sample)
plot(g)

A <-matrix(1:16,4,4,byrow=T)
A
A[2,3]
A[c(2,3),c(1,4)]
A[2:3,1:4]
A[2:3,]
A[2:3,2:3]
A[,2:3]
A[-c(2,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)
A <-matrix(1:20,5,4)
A
dim(A)

auto_data <- ISLR::Auto
auto_data
save(auto_data, file = "auto_data.Rdata")
write.csv(auto_data, file = "auto_data.csv")
auto_data <-read.csv("/Users/suzyxu/Documents/ISLR/auto_data.csv",row.names =1)
load("/Users/suzyxu/Documents/ISLR/auto_data.Rdata")
auto_data
head(auto_data)
dim(auto_data)
colnames(auto_data)
rownames(auto_data)
names(auto_data)
summary(auto_data)
str(auto_data)

dat <-auto_data
dat <- as_tibble(dat)
dat$cylinders <- as.factor(dat$cylinders)
dat

g <- ggplot(dat)
g <- g + geom_violin(aes(x = cylinders, y = horsepower))
g <- g + geom_point(aes(x = cylinders, y = horsepower, color = weight))
g <- g + theme_classic()
plot(g)

