library("Hiiragi2013")
library(mouse4302.db)
library(tidyverse)
library(factoextra)
library(NMF)
library(MASS)
library(umap)

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

## ######################################################
## pca
## ######################################################
pca_out <- prcomp(embryo[, 1:45101], center = TRUE, scale = TRUE)

fviz_eig(pca_out, geom = "bar", bar_width = 0.4) + ggtitle("")

fviz_pca_ind(pca_out,
             geom = "point",
             habillage = embryo$Embryonic.day,
             palette = "npg",
             mean.point = FALSE
)

## ######################################################
## nmf
## ######################################################
nmf_out <- nmf(t(embryo[, 1:1000]), 2)

U <- t(coef(nmf_out))

df = data.frame(x = U[,1], y = U[,2], cluster = as.factor(embryo$Embryonic.day))
ggplot(data = df, mapping = aes(x = x, y = y, color = cluster)) + geom_point()

## ######################################################
## mds
## ######################################################
d <- dist(embryo[, 1:45101], method = "euclidean")

fit <- isoMDS(d, k = 2)

df = data.frame(x = fit$points[,1], y = fit$points[,2], cluster = as.factor(embryo$Embryonic.day))
ggplot(data = df, mapping = aes(x = x, y = y, color = cluster)) + geom_point()

## ######################################################
## umap
## ######################################################
um = umap(embryo[, 1:45101])

df = data.frame(x = um$layout[,1], y = um$layout[,2], cluster = as.factor(embryo$Embryonic.day))
ggplot(data = df, mapping = aes(x = x, y = y, color = cluster)) + geom_point()