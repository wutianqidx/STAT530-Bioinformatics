library("Hiiragi2013")
library(mouse4302.db)
library(tidyverse)
library(glmnet)
library(caret)

## ######################################################
## prepare dataset
## ######################################################
data("x")
embryo <- data.frame(t(exprs(x)), pData(x))

anno <- AnnotationDbi::select(mouse4302.db,
                              keys = rownames(exprs(x)),
                              columns = c("SYMBOL", "GENENAME"))

colnames(embryo)[1:45101] <- distinct(as_tibble(anno), PROBEID, .keep_all = TRUE)$SYMBOL

train_idx <- sample(1:nrow(embryo), nrow(embryo) * 0.75)
train <- embryo[train_idx, ]
test <- embryo[-train_idx, ]

## check if there is missing data or predictors with zero variance
summary(apply(train[, 1:45101], 2, sd))
sum(is.na(train))

summary(apply(test[, 1:45101], 2, sd))
sum(is.na(test))

## ######################################################
## classification
## ######################################################
## ------------------------------------------------------
## lasso
## ------------------------------------------------------
lasso <- cv.glmnet(x = data.matrix(train[, 1:45101]),
                   y = train$Embryonic.day,
                   nfolds = 5,
                   family = "multinomial")

## estimated testing error from cross-validation
min(lasso$cvm)

## predictions
pred <- predict(lasso,
                newx = data.matrix(test[, 1:45101]),
                s = "lambda.min",
                type = "class")

mean(as.character(pred) != as.character(test$Embryonic.day))
cbind(as.character(pred), as.character(test$Embryonic.day))

## ------------------------------------------------------
## random forest
## ------------------------------------------------------
train_small <- train[, c(1:100, 45103)]
test_small <- test[, c(1:100, 45103)]

rf <- train(Embryonic.day ~ .,
            data = train_small,
            method = "ranger",
            trControl = trainControl(method = "cv", 
                                     number = 5))

## estimated testing accuracy from cross-validation
rf

## predictions
pred <- predict(rf, test_small)

mean(as.character(pred) != as.character(test$Embryonic.day))
cbind(as.character(pred), as.character(test$Embryonic.day))

## ######################################################
## regression
## ######################################################
## ------------------------------------------------------
## lasso
## ------------------------------------------------------
lasso <- cv.glmnet(x = data.matrix(train[, 2:45101]),
                   y = train[,1],
                   nfolds = 5,
                   family = "gaussian")

## estimated testing error from cross-validation
min(lasso$cvm)

## predictions
pred <- predict(lasso,
                newx = data.matrix(test[, 2:45101]),
                s = "lambda.min")

mean((pred - test[, 1])^2)

## ------------------------------------------------------
## random forest
## ------------------------------------------------------
train_small <- train[, c(1:100)]
test_small <- test[, c(1:100)]

rf <- train(Copg1 ~ .,
            data = train_small,
            method = "ranger",
            trControl = trainControl(method = "cv", 
                                     number = 5))

## estimated testing error from cross-validation
rf

## predictions
pred <- predict(rf, test_small)

mean((pred - test[, 1])^2)
