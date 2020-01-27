
# Remove everything from the environment and clear out memories
gc()

# Statistic Packages
require(foreign)
require(stats)
require(lattice)
require(Matrix)
require(HH) #vif
require(quadprog) #Qudratic Programming: Constrained Regression
require(pastecs) #Summary Statistics

# Machine Learning Pacakges
require(lars)
require(e1071) #SVM
require(randomForest)
require(gbm) 
require(dismo) #Gradient Boosting Model
require(ipred) #Bagging
library(glmnet) #for better LASSO
library(parallelSVM)
library(xtable) #export to latex
require(dplyr)
library(data.table)
library(tidyverse)
library(xts)
library(reshape)
#library(Metrics)
library(lubridate)
library(dplyr)
library(h2o)

#Time Series Packages
library(TSPred)
library(forecast)
#library(prophet)
library(TSA)

#Discrete Choice Packages
library("BLPestimatoR")
library("AER")

library(foreach)
library(doParallel)

library(Matrix)

#citation("AER")


# Function 1: RMSE
rmse <- function(obs, pred) sqrt(mean((obs-pred)^2))

set.seed.parallelSWM <- function(seed, once = TRUE){
  if(missing(seed) || is.character(seed)){
    out <- function (numberCores) 
    { foreach::registerDoSEQ()
      cluster <- parallel::makeCluster(numberCores)
      doParallel::registerDoParallel(cluster)
    }
  }else{
    require("doRNG", quietly = TRUE, character.only = TRUE)
    out <- function(numberCores){
      foreach::registerDoSEQ()
      cluster <- parallel::makeCluster(numberCores)
      doParallel::registerDoParallel(cluster)
      doRNG::registerDoRNG(seed = seed, once = once)
    }
  }
  unlockBinding("registerCores", as.environment("package:parallelSVM"))
  assign("registerCores", out, "package:parallelSVM")
  lockBinding("registerCores", as.environment("package:parallelSVM"))
  unlockBinding("registerCores", getNamespace("parallelSVM"))
  assign("registerCores", out, getNamespace("parallelSVM"))
  lockBinding("registerCores", getNamespace("parallelSVM"))
  #unlockBinding("registerCores", as.environment("package:parallelSVM"))
  invisible()
}
