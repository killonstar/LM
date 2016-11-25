# Main script

# Library

library(rusquant)
library(quantmod)
library(magrittr)
library(rlist)
library(lpSolve)


# Packages

source("packages/libfun.R")

# Основные параметры (время и период)
from.date <- "2013-09-26" # Параметр для изменения !!!
to.date <- "2015-09-26" # Параметр для изменения !!!
period <- "day" # Параметр для изменения !!!

# Инструменты
tickers1 <- c("ROSN", "LKOH") # Параметр для изменения !!!
list1 <- BasFun1(tickers1,from.date,to.date,period)

# Линейная регрессия

lm.r <- lm(list1[,1] ~ list1[,2]) 
summary(lm.r) 
yx <- as.data.frame(lm.r[[1]])
a <- round(yx[2,1], digits = 6)
b <- round(yx[1,1], digits = 6)
Y <- a*list1[,2]+b
SPREAD <- list1[,1] - list1[,2] * a - b # Остатки
SPREAD1 <- list1[,1] - list1[,2] * a 
Ratio <- list1[,1]/list1[,2]
a1 <- 1/a
W <- a1*list1[,1]-list1[,2]

par(mfrow=c(1,2))
plot(Ratio)
plot(W)
