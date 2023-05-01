# AN EXAMINATION OF SOCIAL AND ECONOMIC DATA ACROSS NATIONS WORLDWIDE USING STATISTICAL ANALYSIS
# Greta Poceviciute

# Installing packages and activating required libraries
install.packages('xlsx', repos='http://cran.us.r-project.org')
install.packages('purrr', repos='http://cran.us.r-project.org')
install.packages('cluster', repos='http://cran.us.r-project.org')
install.packages('pheatmap', repos='http://cran.us.r-project.org')
install.packages('princomp', repos='http://cran.us.r-project.org')
install.packages('corrplot', repos='http://cran.us.r-project.org')
install.packages('factoextra', repos='http://cran.us.r-project.org')
install.packages('psych', repos='http://cran.us.r-project.org')
install.packages('rworldmap', repos='http://cran.us.r-project.org')
library("xlsx")
library("purrr")
library("cluster")
library("pheatmap")
library("princomp")
library("corrplot")
library("factoextra")
library("psych")
library("rworldmap")
library("pheatmap")

# Loading data
data <- read.xlsx(file.choose(), sheetName = "Data",
                  endRow = 218, header = TRUE)
rownames(data) <- data[,1]
str(data)
summary(data)

# Removing countries with missed values
data <- na.omit(round(data[,-c(1,2)],2))

# Printing the number of remaining countries 
nrow(data)

# Printing the correlation matrix
cordata <- cor(data)
corrplot(cordata, method="color", tl.col="black", tl.cex = 0.6)
corrplot(cordata, method="number", tl.col="black",
         number.cex=0.55, col="black", tl.cex = 0.6)

# Checking if the data is suitable for factor analysis
# Bartlett's test of sphericity is applied
cortest.bartlett(cor(data), n = nrow(data))

# The Kaiser-Meyer-Olkin measure is calculated
KMO(data)

# Calculating eigenvalues and eigenvectors
ev <- eigen(cor(data))

# Drawing the plot of eigenvalues
plot(
  x = seq(1:length(ev$values)), y = ev$values, type = "o",
  xlab = "Factors", ylab = "Eigenvalues",
  main = "Scree plot");
abline(1, 0, col = "red", lty=2)
# Four factors are distinguished

# Performing factor analysis
fakt <- factanal(covmat = cor(data), factors = 4,
                 method = "mle", lower = 0.01)
fakt$loadings

# Drawing heatmap of factors loadings
pheatmap(fakt$loadings, angle_col = 45, cluster_cols=F)

# Calculating factor scores
scores <- factanal(data, factors = 4, method = "mle", lower = 0.01,
                   scores = "regression")$scores

# Drawing plot of factor scores dispersion diagrams
plot(scores[,1], scores[,2], xlab = "Factor 1", 
     ylab = "Factor 2", 
     main = "The projection of world countries onto the plane of the first two factors", 
     type = "n")
text(scores[,1], scores[,2], labels = rownames(scores), cex = 0.7)
plot(scores[,1], scores[,4], xlab = "Factor 1",
     ylab = "Factor 4", type = "n")
text(scores[,1], scores[,4], labels = rownames(scores), cex = 0.7)

# Standardizing data
data_sc <- scale(data)

# Calculating cluster linkage coefficients
m <- c("single", "complete", "average", "ward")
names(m) <- c("single", "complete", "average", "ward")
ac <- function(x) {
  agnes(data_sc, method = x)$ac
}
round(map_dbl(m, ac),2)

# Hierarchical clustering
dm <- dist(data_sc)
hc <- hclust(dm, method = "ward.D")
sub_grp <- cutree(hc, k = 6)
table(sub_grp)
plot(hc, cex =0.7, col = "black")
rect.hclust(hc, k = 6, border = 2:7)

# Printing a list of countries in each cluster
clust <- as.data.frame(cbind(names(cutree(hc, k = 6)),(cutree(hc, k = 6))))
rownames(clust) <- NULL
clust1 <- clust[clust[,2]==1,]
colnames(clust1) <- c("Country", "Cluster")
clust1
clust2 <- clust[clust[,2]==2,]
colnames(clust2) <- c("Country", "Cluster")
clust2
clust3 <- clust[clust[,2]==3,]
colnames(clust3) <- c("Country", "Cluster")
clust3
clust4 <- clust[clust[,2]==4,]
colnames(clust4) <- c("Country", "Cluster")
clust4
clust5 <- clust[clust[,2]==5,]
colnames(clust5) <- c("Country", "Cluster")
clust5
clust6 <- clust[clust[,2]==6,]
colnames(clust6) <- c("Country", "Cluster")
clust6

# Adding country codes to the dataset
malDF <- data.frame(country = c(
  # Country codes for cluster 1
  "ARM", "AUT", "BGR", "CUB", "CZE", "EST", "FRA",
  "GEO", "DEU", "GRC", "HUN", "ITA", "KOR", "LVA",
  "LTU", "MUS", "MNE", "POL", "PRT", "ROU", "RUS",
  "SRB", "SVK", "SVN", "ZAF", "LCA", "THA", "USA",
  "URY",
  # Country codes for cluster 2
  "AUS", "BEL", "CAN", "CHL", "CRI", "DNK", "FIN",
  "ISL", "IRL", "LUX", "NLD", "NZL", "NOR", "ESP",
  "SWE", "CHE", "GBR",
  # Country codes for cluster 3
  "AZE", "BLZ", "BOL", "BRA", "CPV", "COL", "DOM",
  "ECU", "EGY", "SLV", "GTM", "HND", "KAZ", "KGZ",
  "MEX", "MNG", "MAR", "PER", "PHL", "UZB",
  
  # Country codes for cluster 4
  "BHR", "BRN", "CYP", "ISR", "MYS", "MDV", "MLT",
  "OMN", "SAU", "TUR", "ARE",
  # Country codes for cluster 5
  "BGD", "BFA", "BDI", "TCD", "CIV", "DJI", "GHA",
  "LAO", "MWI", "MRT", "NPL", "PAK", "RWA", "SEN",
  "TZA", "TLS",
  # Country codes for cluster 6
  "IND"),
  Clusters = c(rep("Cluster 1", nrow(clust1)), rep("Cluster 2", nrow(clust2)),
               rep("Cluster 3", nrow(clust3)), rep("Cluster 4", nrow(clust4)),
               rep("Cluster 5", nrow(clust5)), "Cluster 6"))

# Naming clusters
clusterNames <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", 
                  "Cluster 5", "Cluster 6")

# Countries are added to the map
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
# Printing the map
mapP <- mapCountryData(malMap, nameColumnToPlot = "Clusters",
               catMethod = "categorical", missingCountryCol = "lightgrey",
               colourPalette = c("red2", "seagreen", "navy",
                                 "royalblue", "mediumvioletred", 
                                 "yellow2"), addLegend=TRUE)

# Clustering using k-means method
set.seed(1)
km <- kmeans(data_sc, centers = 6)
km

# Printing a list of countries in each cluster
clustt <- cbind(data, cluster=km$cluster)
clustt1_var <- clustt[clustt$cluster==6,]
clustt1_var$cluster[clustt1_var$cluster == 6] <- 1
clustt1 <- as.data.frame(cbind(rownames(clustt1_var), clustt1_var[,25]))
colnames(clustt1) <- c("Country", "Cluster")
clustt1
clustt2_var <- clustt[clustt$cluster==2,]
clustt2 <- as.data.frame(cbind(rownames(clustt2_var), clustt2_var[,25]))
colnames(clustt2) <- c("Country", "Cluster")
clustt2
clustt3_var <- clustt[clustt$cluster==4,]
clustt3_var$cluster[clustt3_var$cluster == 4] <- 3
clustt3 <- as.data.frame(cbind(rownames(clustt3_var), clustt3_var[,25]))
colnames(clustt3) <- c("Country", "Cluster")
clustt3
clustt4_var <- clustt[clustt$cluster==1,]
clustt4_var$cluster[clustt4_var$cluster == 1] <- 4
clustt4 <- as.data.frame(cbind(rownames(clustt4_var), clustt4_var[,25]))
colnames(clustt4) <- c("Country", "Cluster")
clustt4
clustt5_var <- clustt[clustt$cluster==5,]
clustt5 <- as.data.frame(cbind(rownames(clustt5_var), clustt5_var[,25]))
colnames(clustt5) <- c("Country", "Cluster")
clustt5
clustt6_var <- clustt[clustt$cluster==3,]
clustt6_var$cluster[clustt6_var$cluster == 3] <- 6
clustt6 <- as.data.frame(cbind(rownames(clustt6_var), clustt6_var[,25]))
colnames(clustt6) <- c("Country", "Cluster")
clustt6

t(km$centers)

# Adding country codes to the dataset
malDF <- data.frame(country = c(
  # Country codes for cluster 1
  "AUS", "AUT", "BEL", "CAN", "DNK", "FIN", "FRA",
  "DEU", "ISL", "IRL", "KOR", "LUX", "NLD", "NZL",
  "NOR", "ESP", "SWE", "CHE", "GBR", "USA",
  # Country codes for cluster 2
  "ARM", "AZE", "BGD", "BLZ", "BOL", "BRA", "CPV",
  "COL", "DOM", "ECU", "EGY", "SLV", "GTM", "HND",
  "KGZ", "LAO", "MEX", "MNG", "MAR", "NPL", "PER",
  "PHL", "LCA", "THA", "UZB",
  # Country codes for cluster 3
  "BGR", "CUB", "CZE", "EST", "GEO", "GRC", "HUN",
  "ITA", "KAZ", "LVA", "LTU", "MUS", "MNE", "POL",
  "PRT", "ROU", "RUS", "SRB", "SVK", "SVN", "ZAF",
  "URY",
  # Country codes for cluster 4
  "BHR", "BDI", "TCD", "DJI", "CIV", "GHA", "MWI",
  "MRT", "PAK", "RWA", "SEN", "TZA", "TLS",
  # Country codes for cluster 5
  "BHR", "BRN", "CHL", "CRI", "CYP", "ISR", "MYS",
  "MDV", "MLT", "OMN", "SAU", "TUR", "ARE",
  # Country codes for cluster 6
  "IND"),
  Clusters = c(rep("Cluster 1", nrow(clustt1)), rep("Cluster 2", nrow(clustt2)),
               rep("Cluster 3", nrow(clustt3)), rep("Cluster 4", nrow(clustt4)),
               rep("Cluster 5", nrow(clustt5)), "Cluster 6"))

# Naming clusters
clusterNames <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", 
                  "Cluster 5", "Cluster 6")

# Countries are added to the map
malMap <- joinCountryData2Map(malDF, joinCode = "ISO3",
                              nameJoinColumn = "country")
# Printing the map
mapP <- mapCountryData(malMap, nameColumnToPlot = "Clusters",
                       catMethod = "categorical", missingCountryCol = "lightgrey",
                       colourPalette = c("red2", "seagreen", "navy",
                                         "royalblue", "mediumvioletred", 
                                         "yellow2"), addLegend=TRUE)

# Performing principal component analysis
pca <- princomp(data_sc)
summary(pca, importance = TRUE)
summary(pca, loadings = TRUE)

# Generating scatterplot of first two principal components
xlim <- range(pca$scores[,1])
plot(pca$scores,type = "n", xlim = xlim, ylim = xlim)
text(pca$scores[,1], pca$scores[,2], labels = rownames(data_sc), cex = 0.5)

# Contribution of variables to Dim-1
var <- get_pca_var(pca)
a <- fviz_contrib(pca, "var", axes = 1, xtickslab.rt = 90)
plot(a)

# Contribution of variables to Dim-2
var <- get_pca_var(pca)
a <- fviz_contrib(pca, "var", axes = 2, xtickslab.rt = 90)
plot(a)

# Contribution of variables to Dim-1-2
var <- get_pca_var(pca)
a <- fviz_contrib(pca, "var", axes = 1:2, xtickslab.rt = 90)
plot(a)

options(scipen=999)

# Hypothesis about suicides
hyp_suic <- as.data.frame(rbind(t.test(clustt1_var$SUIC_M, clustt1_var$SUIC_F, 
                                       alternative = "greater")$p.val,
                                t.test(clustt2_var$SUIC_M, clustt2_var$SUIC_F, 
                                       alternative = "greater")$p.val,
                                t.test(clustt3_var$SUIC_M, clustt3_var$SUIC_F, 
                                       alternative = "greater")$p.val,
                                t.test(clustt4_var$SUIC_M, clustt4_var$SUIC_F, 
                                       alternative = "greater")$p.val,
                                t.test(clustt5_var$SUIC_M, clustt5_var$SUIC_F, 
                                       alternative = "greater")$p.val))
rownames(hyp_suic) <- clusterNames[1:5]
colnames(hyp_suic) <- "p-value"
hyp_suic

# Hypothesis about the average employee share
hyp_wage <- as.data.frame(rbind(t.test(clustt1_var$WAGE_M, clustt1_var$WAGE_F, 
                                       alternative = "two.sided")$p.val,
                                t.test(clustt2_var$WAGE_M, clustt2_var$WAGE_F, 
                                       alternative = "two.sided")$p.val,
                                t.test(clustt3_var$WAGE_M, clustt3_var$WAGE_F, 
                                       alternative = "two.sided")$p.val,
                                t.test(clustt4_var$WAGE_M, clustt4_var$WAGE_F, 
                                       alternative = "two.sided")$p.val,
                                t.test(clustt5_var$WAGE_M, clustt5_var$WAGE_F, 
                                       alternative = "two.sided")$p.val))
rownames(hyp_wage) <- clusterNames[1:5]
colnames(hyp_wage) <- "p-value"
hyp_wage

# Hypothesis about the average life expectancy
hyp_lexp <- as.data.frame(rbind(t.test(clustt1_var$LEXP_M, clustt1_var$LEXP_F, 
                                       alternative = "two.sided")$p.val,
                                t.test(clustt2_var$LEXP_M, clustt2_var$LEXP_F, 
                                       alternative = "two.sided")$p.val,
                                t.test(clustt3_var$LEXP_M, clustt3_var$LEXP_F, 
                                       alternative = "two.sided")$p.val,
                                t.test(clustt4_var$LEXP_M, clustt4_var$LEXP_F, 
                                       alternative = "two.sided")$p.val,
                                t.test(clustt5_var$LEXP_M, clustt5_var$LEXP_F, 
                                       alternative = "two.sided")$p.val))
rownames(hyp_lexp) <- clusterNames[1:5]
colnames(hyp_lexp) <- "p-value"
hyp_lexp


# THE END