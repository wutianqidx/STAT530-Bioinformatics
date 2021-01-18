library("Hiiragi2013")
library(mouse4302.db)
library(tidyverse)

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
## parametric tests based on regression
## ######################################################
## ------------------------------------------------------
## dichotomous outcome
## ------------------------------------------------------
di <- glm(embryo$E ~ data.matrix(embryo[, 1:10]),
          family = "binomial")

## reported p-values are for testing the hypothesis that
## beta_0 or beta_j equals zero
summary(di)
summary(di)$coefficients
summary(di)$coefficients[, 4]

## ------------------------------------------------------
## continuous outcome
## ------------------------------------------------------
cont <- glm(embryo[,1] ~ data.matrix(embryo[,2:10]),
            family = "gaussian")

## reported p-values are for testing the hypothesis that
## beta_0 or beta_j equals zero
summary(cont)
summary(cont)$coefficients
summary(cont)$coefficients[, 4]

## ######################################################
## nonparametric tests
## ######################################################
## ------------------------------------------------------
## wilcoxon test
## ------------------------------------------------------
w <- wilcox.test(embryo[,1] ~ embryo$E)

w
w$p.value

## ------------------------------------------------------
## fisher's exact test
## ------------------------------------------------------
table(embryo$E, embryo$genotype)

f <- fisher.test(embryo$E, embryo$genotype)

f
f$p.value

## ######################################################
## multiple tests
## ######################################################
## test for differentially expressed genes
ps <- apply(embryo[, 1:100], 2, function(y) {
    wilcox.test(y ~ embryo$E)$p.value
})

## adjusting p-values
sum(p.adjust(ps, method = "bonferroni") <= 0.05) ## bonferroni
sum(p.adjust(ps, method = "fdr") <= 0.05) ## fdr
sum(ps <= 0.05) ## per-test error
