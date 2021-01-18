library("Hiiragi2013")
library(mouse4302.db)
library(tidyverse)
library(umap)
library(pheatmap)
library(dbscan)

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

## umap for visualizing clusters
um = umap(embryo[, 1:45101])
df = data.frame(x = um$layout[,1],
                y = um$layout[,2],
                Eday = embryo$Embryonic.day)

## ######################################################
## k-means clustering
## ######################################################
set.seed(1) ## not necessary for this class
res <- kmeans(scale(embryo[, 1:45101]), 3)

clust <- res$cluster

table(res$cluster, embryo$Embryonic.day)

df$cluster = as.factor(clust)
ggplot(data = df,
       mapping = aes(x = x,
                     y = y,
                     shape = Eday,
                     color = cluster)) +
  geom_point(size = 5)

## ######################################################
## hierarchical clustering
## ######################################################
res <- pheatmap(scale(embryo[, 1:1000]),
                cluster_rows = TRUE,
                cluster_cols = TRUE,
                cluster_distance_rows = "correlation",
                cluster_distance_cols = "correlation",
                show_rownames = FALSE,
                show_colnames = FALSE)

clust <- cutree(res$tree_row, k = 3)

table(clust, embryo$Embryonic.day)

df$cluster = as.factor(clust)
ggplot(data = df,
       mapping = aes(x = x,
                     y = y,
                     shape = Eday,
                     color = cluster)) +
    geom_point(size = 5)

## ######################################################
## snn clustering
## ######################################################
res <- sNNclust(scale(embryo[, 1:45101]),
                k = 10,
                eps = 1,
                minPts = 5)

clust <- res$cluster

table(res$cluster, embryo$Embryonic.day)

df$cluster = as.factor(clust)
ggplot(data = df,
       mapping = aes(x = x,
                     y = y,
                     shape = Eday,
                     color = cluster)) +
    geom_point(size = 5)
