#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("At least one argument must be supplied (input file).n", call.=FALSE)
}

# "nudesc1" for Nu-DESC >=1
# "nudesc2" for Nu-DESC >=2
outcome=args[1]

# to be run on AWS instance

# add library path
# .libPaths("/home/rstudio/R/x86_64-pc-linux-gnu-library/3.4")

library(dplyr)
library(pROC)
library(glmnet)
library(e1071)
library(randomForest)
library(gbm)
library(caret)
library(DMwR)

# Library parallel() is a native R library, no CRAN required
library(parallel)
nCores <- detectCores(logical = FALSE)
nThreads <- detectCores(logical = TRUE)
cat("CPU with",nCores,"cores and",nThreads,"threads detected.\n")

# load the doParallel/doSNOW library for caret cluster use
library(doParallel)
cl <- makeCluster(nThreads - 1)
# set library path on each worker
# clusterEvalQ(cl, .libPaths("/home/rstudio/R/x86_64-pc-linux-gnu-library/3.4"))
registerDoParallel(cl)

# load dataset
load("rda/X.rda")

# select outcome variable
if (outcome=="nudesc1") {
  X$delirium <- X$delirium_nudesc1
  X$delirium_nudesc1 <- NULL
}
if (outcome=="nudesc2") {
  X$delirium_nudesc1 <- NULL
}

# separate into train and test datasets
train <- filter(X, dataset=="train") %>% select(-dataset)
test <- filter(X, dataset=="test") %>% select(-dataset)

# save delirium and AWOL variables and remove from dataframe
y_train <- train$delirium
awol_train <- as.numeric(as.character(train$first_awol))
train <- select(train, -first_awol)

y_test <- test$delirium
awol_test <- as.numeric(as.character(test$first_awol))
test <- select(test, -first_awol)

aucs <- numeric()
fits <- list()
preds <- data.frame(y_test=y_test)

# predictions for CSNs with AWOL scores
test_awol_idx <- which(!is.na(awol_test))
test_filtered <- test[test_awol_idx,]
awol_aucs <- numeric()
awol_preds <- data.frame(y_test=y_test[test_awol_idx],
                         awol_test=awol_test[test_awol_idx])
awol_roc <- pROC::roc(as.numeric(awol_preds$y_test), awol_preds$awol_test, plot=FALSE)
awol_auc <- awol_roc$auc

# caret resampling method
ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 3,
                     verboseIter=TRUE,
                     returnData=FALSE,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

# parameters specific to each algorithm
model_params <- list(
  glmnet     = list(method="glmnet",
                    tuneGrid=expand.grid(alpha=1,
                                         lambda=exp(seq(log(0.0001), log(0.01), length.out = 100)))),
  rf         = list(method="rf",
                    tuneGrid=expand.grid(mtry=seq(12, 27, 3)),
                    ntree=1025),
  svmLinear2 = list(method="svmLinear2",
                    tuneGrid=expand.grid(cost=exp(seq(log(0.001), log(0.05), length.out = 25))),
                    preProcess=c("center","scale")),
  gbm        = list(method="gbm",
                    tuneGrid=expand.grid(interaction.depth = 3,
                                         n.trees = (50:75)*300,
                                         shrinkage = 0.001,
                                         n.minobsinnode=1)),
  nnet       = list(method="nnet",
                    tuneGrid=expand.grid(size = c(50, 75),
                                         decay = 1),
                    MaxNWts = 1e10,
                    preProcess=c("center","scale"))
)

# common parameters across all algorithms
common_params <- list(
  form=as.formula("delirium ~ ."),
  data=train,
  metric="ROC",
  trControl=ctrl
)

# run each model
model_names <- sapply(model_params, function(x) x$method)
for (model_name in model_names) {
  print(sprintf("Training: %s", model_name))
  params <- c(common_params, model_params[[model_name]])
  fit <- do.call(caret::train, params)
  fits[[model_name]] <- fit
  # predict on entire test set
  pred <- predict(object=fit, newdata=test, type="prob")
  aucs[model_name] <- roc(response=as.numeric(y_test) - 1,
                          predictor=pred[,1],
                          plot=TRUE,
                          main=model_name,
                          xlim=c(0,1))$auc
  preds[[model_name]] <- pred
  # predict on subset of test set that has AWOL
  pred <- predict(object=fit, newdata=test_filtered, type="prob")
  awol_aucs[model_name] <- roc(response=as.numeric(test_filtered$delirium) - 1,
                               predictor=pred[,1])$auc
  awol_preds[[model_name]] <- pred
  
  model_res <- list(fit=fit,
                  aucs=aucs[model_name],
                  preds=preds[[model_name]],
                  awol_aucs=awol_aucs[model_name],
                  awol_preds=awol_preds[[model_name]],
                  awol_roc=awol_roc,
                  awol_auc=awol_auc,
                  train=train,
                  test=test)
  
  if (outcome=="nudesc1") {
    save(model_res, file=paste0("rda/", model_name, "_nudesc1.Rda"))
  } else {
    save(model_res, file=paste0("rda/", model_name, ".Rda"))  
  }
}

