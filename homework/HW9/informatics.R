library("Hiiragi2013")
library(mouse4302.db)
library(tidyverse)
library(biomaRt)
library(glmnet)

## ######################################################
## prepare dataset
## ######################################################
data("x")
embryo <- data.frame(t(exprs(x)), pData(x))

## check if there is missing data or predictors with zero variance
summary(apply(embryo[, 1:45101], 2, sd))
sum(is.na(embryo))

## ######################################################
## *.db
## ######################################################
anno <- AnnotationDbi::select(mouse4302.db,
                              keys = rownames(exprs(x)),
                              columns = c("SYMBOL", "GENENAME"))

colnames(embryo)[1:45101] <- distinct(as_tibble(anno), PROBEID, .keep_all = TRUE)$SYMBOL

## ######################################################
## biomart
## ######################################################
## biomart tutorial:
## https://www.bioconductor.org/packages/devel/bioc/vignettes/biomaRt/inst/doc/biomaRt.html
gene_names <- rownames(exprs(x))

## load mart you want
listMarts()
ensembl <- useMart("ENSEMBL_MART_ENSEMBL")

## look for mouse ensemble dataset
ds <- listDatasets(ensembl)
head(ds)
searchDatasets(ensembl, pattern = "ouse")
ensembl <- useDataset("mmusculus_gene_ensembl", mart = ensembl)

## filters = things you can look up by
listFilters(ensembl)
searchFilters(ensembl, pattern = "430")

## attributes = things you can request
head(listAttributes(ensembl), 30)
## get desired attributes
gene_symbols <- getBM(attributes = c("affy_mouse430_2",
                                     "ensembl_gene_id",
                                     "external_gene_name"),
                      filters = c("affy_mouse430_2"),
                      values = gene_names[1:100],
                      mart = ensembl)

## probes not in the right order and duplicated
head(gene_symbols)
head(gene_names)

## ######################################################
## DAVID
## ######################################################
embryo$E <- as.factor(embryo$Embryonic.day == "E3.25")
lasso <- cv.glmnet(x = data.matrix(embryo[, 1:45101]),
                   y = embryo$E,
                   nfolds = 5,
                   family = "binomial")

b <- coef(lasso, s = "lambda.min")

cat(paste(rownames(exprs(x))[b[-1] != 0],
          collapse = "\n"))
