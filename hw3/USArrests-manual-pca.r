# CS422 Data Mining
# Vijay K. Gurbani, Ph.D.
# Illinois Institute of Technology

# Principal Component Analysis: Manual PCA of USArrests dataset 
# using eigendecomposition.

rm(list=ls())

options(digits = 4)

data("USArrests")

X <- scale(USArrests)

head(X)

cov(X)

e <- eigen(cov(X))
row.names(e$vectors) <- c("Murder", "Assault", "UrbanPop", "Rape")
colnames(e$vectors) <- c("PC1", "PC2", "PC3", "PC4")
e

phi <- -e$vectors
phi

phi.1 <- as.matrix(phi[,1])
PC1.score <- apply(X, 1, function(x) t(phi.1) %*% x)
as.matrix(head(PC1.score))

phi.2 <- as.matrix(phi[,2])
PC2.score <- apply(X, 1, function(x) t(phi.2) %*% x)
as.matrix(head(PC2.score))

### And now using PCA
pca <- prcomp(scale(USArrests))
names(pca)
pca$sdev
pca$rotation <- -pca$rotation
pca$rotation
pca$x <- -pca$x
head(pca$x)
biplot(pca, scale=0)
