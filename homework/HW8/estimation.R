library("Hiiragi2013")
library(mouse4302.db)
library(tidyverse)
library(glmnet)

## ######################################################
## prepare dataset
## ######################################################
data("x")
embryo <- data.frame(t(exprs(x)), pData(x))

anno <- AnnotationDbi::select(mouse4302.db,
                              keys = rownames(exprs(x)),
                              columns = c("SYMBOL", "GENENAME"))

colnames(embryo)[1:45101] <- distinct(as_tibble(anno), PROBEID, .keep_all = TRUE)$SYMBOL

## check if there is missing data or predictors with zero variance
summary(apply(embryo[, 1:45101], 2, sd))
sum(is.na(embryo))

## add dichotomized embryonic day for illustration
## estimation when the dependent variable has more than
## two categories has a complicated interpretation
embryo$E <- as.factor(embryo$Embryonic.day == "E3.25")

## ######################################################
## classification
## ######################################################
## ------------------------------------------------------
## standard regression
## ------------------------------------------------------
standard <- glm(embryo$E ~ data.matrix(embryo[, 1:10]),
                family = "binomial")

coef(standard)

## ------------------------------------------------------
## lasso
## ------------------------------------------------------
lasso <- cv.glmnet(x = data.matrix(embryo[, 1:45101]),
                   y = embryo$E,
                   nfolds = 5,
                   family = "binomial")

b <- coef(lasso, s = "lambda.min")
rownames(b)[which(b != 0)]
b[which(b != 0)]

## ------------------------------------------------------
## elastic net
## ------------------------------------------------------
enet <- cv.glmnet(x = data.matrix(embryo[, 1:45101]),
                  y = embryo$E,
                  alpha = 0.5,
                  nfolds = 5,
                  family = "binomial")

b <- coef(enet, s = "lambda.min")
rownames(b)[which(b != 0)]
b[which(b != 0)]

## ######################################################
## regression
## ######################################################
## ------------------------------------------------------
## standard regression
## ------------------------------------------------------
standard <- glm(embryo[,1] ~ data.matrix(embryo[,2:10]),
                family = "gaussian")

coef(standard)

## ------------------------------------------------------
## lasso
## ------------------------------------------------------
lasso <- cv.glmnet(x = data.matrix(embryo[, 2:45101]),
                   y = embryo[,1],
                   nfolds = 5,
                   family = "gaussian")

b <- coef(lasso, s = "lambda.min")
rownames(b)[which(b != 0)]
b[which(b != 0)]

## ------------------------------------------------------
## elastic net
## ------------------------------------------------------
enet <- cv.glmnet(x = data.matrix(embryo[, 2:45101]),
                  y = embryo[,1],
                  alpha = 0.5,
                  nfolds = 5,
                  family = "gaussian")

b <- coef(enet, s = "lambda.min")
rownames(b)[which(b != 0)]
b[which(b != 0)]
