

## Libraries 

library(readxl)
library(xlsx)
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)


# Data

pcadata <- read_excel("indice_hemerobie.xlsx", sheet = "Feuil1")
# Convert as dataframe
pcadata <- as.data.frame(pcadata)
# Set "Policy initiatives" as rows name
row.names(pcadata) <- pcadata$"ID"
# Remove column "Policy initiatives"
pcadata <- pcadata[,colnames(pcadata)!= "ID"]

View(pcadata)
str(pcadata)


## PCA

df <- pcadata[, 1:6]
mypca <- PCA(df, scale.unit = TRUE, graph = FALSE)


## Eigen values

# Eigen vaues
get_eig(mypca)
#scree plot
fviz_eig(mypca, addlabels = TRUE)


## Variable contributions to the principal components

# Contributions of variables to PC1
fviz_contrib(mypca, choice = "var", axes = 1)

# Contributions of variables to PC2
fviz_contrib(mypca, choice = "var", axes = 2)

# Contributions of variables to PC3
fviz_contrib(mypca, choice = "var", axes = 3)


## Dimension description

# Axe 1 et 2
descr12 <- dimdesc(mypca, axes = c(1,2), proba = 0.05)

#Axe 1 et 3
descr13 <- dimdesc(mypca, axes = c(1,3), proba = 0.05)

# Description of dimension 1
descr12$Dim.1

# Description of dimension 1
descr12$Dim.2

# Description of dimension 3
descr13$Dim.3




#Total contribution on PC1 and PC2
fviz_contrib(mypca, choice = "ind", axes = 1)



## Biplot of individuals and variables

fviz_pca_biplot(mypca, axes = c(1, 2), repel = TRUE, geom.ind = "point", 
                col.ind = pcadata$`Indice`, addEllipses = TRUE,
                pointshape = 16, ggtheme = theme_bw(), col.var = "black",
                legend.title = "Indice d'hémérobie: ")+theme(legend.position = "top")

# Axes 1 et 3
fviz_pca_biplot(mypca, axes = c(1, 3), repel = TRUE, geom.ind = "point", 
                col.ind = pcadata$`Indice`, addEllipses = TRUE,
                pointshape = 16, ggtheme = theme_bw(), col.var = "black",
                legend.title = "Indice d'hémérobie: ")+theme(legend.position = "top")



# Clustering
## Hierarchical clustering on PCA

myhcpc <- HCPC(mypca, graph = FALSE, max = 3)


## Factor map

fviz_cluster(myhcpc,
             repel = TRUE,            
             show.clust.cent = FALSE, 
             palette = "jco",       
             ggtheme = theme_bw(),
             labelsize = 6,
             legend.title = "Groupes",
             main = "Factor map", geom = "point"
)



# Variables that describe the most each cluster
myhcpc$desc.var$quanti

# Representative individuals for each cluster
myhcpc$desc.ind$para



## Data with clusters
clusdata <- myhcpc$data.clust
clustered_data <- data.frame(select(clusdata, "crops","tree_crops",  "forest", "savannah", 
                                    "artificial", "water"), 
                             select(pcadata, "Ind_hemerob", "Indice"),
                             select(clusdata, Groupes = "clust"))
head(clustered_data, 25)

#xlsx::write.xlsx(clustered_data, "DataWithCluster.xlsx")

