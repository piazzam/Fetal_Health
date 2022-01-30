if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")}
if("rpart" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart")}
if("rattle" %in% rownames(installed.packages()) == FALSE) {install.packages("rattle")}
if("rpart.plot" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart.plot")}
if("RColorBrewer" %in% rownames(installed.packages()) == FALSE) {install.packages("RColorBrewer")}
if("C50" %in% rownames(installed.packages()) == FALSE) {install.packages("C50")}
if("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
if("pRoc" %in% rownames(installed.packages()) == FALSE) {install.packages("pRoc")}
if("multiROC" %in% rownames(installed.packages()) == FALSE) {install.packages("multiROC")}
if("dummies" %in% rownames(installed.packages()) == FALSE) {install.packages("dummies")}

library(pRoc)
library(multiROC)
library(dummies)

# Caricamento dataset originale
dataset = read.csv("fetal_health.csv", na.strings=c("NA", ""))
# Caricamento dataset pca
dataset_pca = read.csv("fetal_health_new_dim.csv", na.strings=c("NA", ""))

dataset$fetal_health <- factor(dataset$fetal_health)
levels(dataset$fetal_health) <- c("first_class", "second_class", "third_class")
dataset_pca$fetal_health <- factor(dataset_pca$fetal_health)
levels(dataset_pca$fetal_health) <- c("first_class", "second_class", "third_class")

set.seed(1)
library(e1071)

#Suddivisione train e test
ind = sample(2, nrow(dataset), replace = TRUE, prob=c(0.7, 0.3))
testset = dataset[ind == 2,]
trainset = dataset[ind == 1,]

ind = sample(2, nrow(dataset_pca), replace = TRUE, prob=c(0.7, 0.3))
testset_pca = dataset_pca[ind == 2,]
trainset_pca = dataset_pca[ind == 1,]


# Addestramento decision tree
library(rpart)
decisionTree = rpart(fetal_health ~ ., data=trainset, method="class")
decisionTree_pca = rpart(fetal_health ~ ., data=trainset_pca, method="class")


#plot(decisionTree)
#text(decisionTree)
#str(decisionTree)

# decision tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
par(mfrow=c(1,1))
fancyRpartPlot(decisionTree, cex=0.6)
fancyRpartPlot(decisionTree_pca, cex=0.6)

# Perdizione decision tree
testset$Prediction <- predict(decisionTree, testset, type = "class")
testset_pca$Prediction <- predict(decisionTree_pca, testset_pca, type = "class")


library(C50)
library(caret)

# Matrice di confuzione
result = confusionMatrix(testset$Prediction, testset[,c("fetal_health")], mode = "prec_recall")
result
result_pca = confusionMatrix(testset_pca$Prediction, testset_pca[,c("fetal_health")], mode = "prec_recall")
result_pca


plotcp(decisionTree)
plotcp(decisionTree_pca)
printcp(decisionTree)
printcp(decisionTree_pca)



# Albero pruno
prunedDecisionTree = prune(decisionTree, cp= 0.02)
prunedDecisionTree_pca = prune(decisionTree_pca, cp= 0.015)

fancyRpartPlot(prunedDecisionTree, cex=0.6)
fancyRpartPlot(prunedDecisionTree_pca, cex=0.6)

# Predizione pruned decision tree
testset$Prediction <- predict(prunedDecisionTree, testset, type = "class")
testset_pca$Prediction <- predict(prunedDecisionTree_pca, testset_pca, type = "class")

# Matrice di confusione decision tree pruned
result_pruned = confusionMatrix(testset$Prediction, testset[,c("fetal_health")], mode = "prec_recall", positive="no")
result_pruned

# Matrice di confusione decision tree pruned pca
result_pruned_pca = confusionMatrix(testset_pca$Prediction, testset_pca[,c("fetal_health")], mode = "prec_recall", positive="no")
result_pruned_pca




# --- Information Gain ---


decisionTreeIG = rpart(fetal_health ~ ., data=trainset, method="class", parms = list(split = 'information'))
decisionTreeIG_pca = rpart(fetal_health ~ ., data=trainset_pca, method="class", parms = list(split = 'information'))

fancyRpartPlot(decisionTreeIG, cex=0.6)
fancyRpartPlot(decisionTreeIG_pca, cex=0.6)

testset$Prediction <- predict(decisionTreeIG, testset, type = "class")
testset_pca$Prediction <- predict(decisionTreeIG_pca, testset_pca, type = "class")


result_IG = confusionMatrix(testset$Prediction, testset[,c("fetal_health")], mode = "prec_recall", positive="no")
result_IG


result_IG_pca = confusionMatrix(testset_pca$Prediction, testset_pca[,c("fetal_health")], mode = "prec_recall", positive="no")
result_IG_pca


plotcp(decisionTreeIG)
plotcp(decisionTreeIG_pca)


prunedDecisionTreeIG = prune(decisionTreeIG, cp = 0.01)
prunedDecisionTreeIG_pca = prune(decisionTreeIG_pca, cp = 0.012)

fancyRpartPlot(prunedDecisionTreeIG, cex=0.6)
fancyRpartPlot(prunedDecisionTreeIG_pca, cex=0.6)


testset$Prediction <- predict(prunedDecisionTreeIG, testset, type = "class")
testset_pca$Prediction <- predict(prunedDecisionTreeIG_pca, testset_pca, type = "class")


result_IG_pruned = confusionMatrix(testset$Prediction, testset[,c("fetal_health")], mode = "prec_recall", positive="no")
result_IG_pruned


result_IG_pruned_pca = confusionMatrix(testset_pca$Prediction, testset_pca[,c("fetal_health")], mode = "prec_recall", positive="no")
result_IG_pruned_pca

#--- 10-fold-cross validation + roc curve ---

control = trainControl(method = "repeatedcv", number = 10,repeats = 3,
                       classProbs = TRUE, summaryFunction = multiClassSummary)

rpart.model= train(fetal_health ~ ., data = trainset, method = "rpart", metric = "ROC",
                   trControl = control)
rpart.prob = predict(rpart.model, testset, type = "prob")
rpart_pred <- data.frame(rpart.prob)
colnames(rpart_pred)[1] <- "fetal_healt_group1"
colnames(rpart_pred)[2] <- "fetal_healt_group2"
colnames(rpart_pred)[3] <- "fetal_healt_group3"
colnames(rpart_pred) <- paste(colnames(rpart_pred), "_pred_RF")

true_label <- dummies::dummy(testset$fetal_health, sep=".")
true_label <- data.frame(true_label)
colnames(true_label)[1] <- "fetal_healt_group1"
colnames(true_label)[2] <- "fetal_healt_group2"
colnames(true_label)[3] <- "fetal_healt_group3"
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")

final_df <- cbind(true_label, rpart_pred)
roc_res <- multi_roc(final_df)

plot_roc_df <- plot_roc_data(roc_res)
require(ggplot2)
ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))


rpart.model_pca= train(fetal_health ~ ., data = trainset_pca, method = "rpart", metric = "ROC",
                   trControl = control)


rpart.prob_pca = predict(rpart.model_pca, testset_pca, type = "prob")
rpart_pred_pca <- data.frame(rpart.prob_pca)
colnames(rpart_pred_pca)[1] <- "fetal_healt_group1"
colnames(rpart_pred_pca)[2] <- "fetal_healt_group2"
colnames(rpart_pred_pca)[3] <- "fetal_healt_group3"
colnames(rpart_pred_pca) <- paste(colnames(rpart_pred_pca), "_pred_RF")

true_label <- dummies::dummy(testset_pca$fetal_health, sep=".")
true_label <- data.frame(true_label)
colnames(true_label)[1] <- "fetal_healt_group1"
colnames(true_label)[2] <- "fetal_healt_group2"
colnames(true_label)[3] <- "fetal_healt_group3"
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")

final_df_pca <- cbind(true_label, rpart_pred_pca)
roc_res_pca <- multi_roc(final_df_pca)

plot_roc_df_pca <- plot_roc_data(roc_res_pca)
require(ggplot2)
ggplot(plot_roc_df_pca, aes(x = 1-Specificity, y=Sensitivity)) +
  geom_path(aes(color = Group, linetype=Method), size=1.5) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        legend.title=element_blank(), 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))
