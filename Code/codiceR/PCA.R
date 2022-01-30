if("FactoMineR" %in% rownames(installed.packages()) == FALSE) {install.packages("FactoMineR")}
if("factoextra" %in% rownames(installed.packages()) == FALSE) {install.packages("factoextra")}
library("FactoMineR")
library("factoextra")


# Caricamento dataset
dataset = read.csv("fetal_health.csv", header = TRUE)
#str(dataset)


# Trasformazione in factor della variabile target
dataset$fetal_health = factor(dataset$fetal_health)
#str(dataset)


dataset.active <- dataset [,1:21]
dataset.active = scale(dataset.active)
# Applicazione della pca calcolandola su 21 componenti
res_pca <- PCA(dataset.active, ncp = 21, graph = FALSE, scale = TRUE)

# Autovalori per le 21 dimensioni
eig.val <- get_eigenvalue(res_pca)
eig.val

# Istogramma percentuale varianza spiegata
fviz_eig(res_pca, ncp = 21)

# Rappresentazione delle variabili nel nuovo spazio di rappresentazione utilizzando le prime due dimensioni
fviz_pca_var(res_pca, 
             #select.var = list(cos2 = 8),
             col.var = "contrib",  # Color by contributions to the PCA
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),)

# Biplot degli individui e delle variabili nel nuovo spazio di rappresentazione utilizzando le prime due dimensioni
fviz_pca_biplot(res_pca, 
                label = "var", 
                habillage=dataset$fetal_health,
                addEllipses=TRUE, ellipse.level=0.95,
                ggtheme = theme_minimal())


ind <- get_pca_ind(res_pca)
# Cordinate individui nel nuovo spazio utilizzando le otto componenti principali
coord_ind <- ind$coord[, 1:8]

dataset_new_dim <- data.frame(coord_ind)
# Aggiungo variabile target
dataset_new_dim$fetal_health <- dataset$fetal_health

# Salvo file csv con il nuovo dataset
write.csv(dataset_new_dim, file = "fetal_health_new_dim.csv", row.names=FALSE)


