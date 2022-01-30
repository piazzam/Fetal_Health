if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")}
if("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
if("multiROC" %in% rownames(installed.packages()) == FALSE) {install.packages("multiROC")}
if("C50" %in% rownames(installed.packages()) == FALSE) {install.packages("C50")}
if("dummies" %in% rownames(installed.packages()) == FALSE) {install.packages("dummies")}

library(C50)
library(caret)
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


# Tuning svm
#tuned = tune.svm(fetal_health ~ ., data = dataset, kernel=c('linear', 'polynomial', 'radial'),
#                   cost=c(0.001, 0.01, 0.1, 1, 1.5, 5,10,100, 200), probability = TRUE)
tuned = tune.svm(fetal_health ~ ., data = dataset, kernel='polynomial',
                                   cost=c(0.001, 0.01, 0.1, 1, 1.5, 5,10,100, 200), probability = TRUE)

tuned$best.parameters
tuned$best.model

svm_pred = predict(tuned$best.model, testset)
result = confusionMatrix(svm_pred, testset[,c("fetal_health")], mode = "prec_recall")
result


# Tuning svm PCA
tuned_pca = tune.svm(fetal_health ~ ., data = dataset_pca, kernel='polynomial',
                 cost=c(0.001, 0.01, 0.1, 1, 1.5, 5,10,100, 200), probability = TRUE)
tuned_pca$best.parameters
tuned_pca$best.model

svm_pred_pca = predict(tuned_pca$best.model, testset)
result_pca = confusionMatrix(svm_pred, testset[,c("fetal_health")], mode = "prec_recall")
result_pca

# --- 10-fold cross validation + roc curve---

control = trainControl(method = "repeatedcv", number = 10,repeats = 3,
                       classProbs = TRUE, summaryFunction = multiClassSummary)
svm_model_10fold = train(fetal_health ~ ., data = trainset, method = "svmPoly", metric =
                   "ROC", trControl = control, cost = 100, probability = TRUE)

svm_pred_10_fold = predict(svm_model_10fold, testset, type = "prob")
svm_pred_prob = svm_pred_10_fold
colnames(svm_pred_prob)[1] <- "fetal_healt_group1"
colnames(svm_pred_prob)[2] <- "fetal_healt_group2"
colnames(svm_pred_prob)[3] <- "fetal_healt_group3"
colnames(svm_pred_prob) <- paste(colnames(svm_pred_prob), "_pred_RF")

true_label <- dummies::dummy(testset$fetal_health, sep=".")
true_label <- data.frame(true_label)
colnames(true_label)[1] <- "fetal_healt_group1"
colnames(true_label)[2] <- "fetal_healt_group2"
colnames(true_label)[3] <- "fetal_healt_group3"
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")

result = confusionMatrix(svm_pred_prob, testset[,c("fetal_health")], mode = "prec_recall")
result

final_df <- cbind(true_label, svm_pred_prob)
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

svm_model_10fold_pca = train(fetal_health ~ ., data = trainset_pca, method = "svmPoly", metric =
                           "ROC", trControl = control, cost = 10, probability = TRUE)

svm_pred_10_fold_pca = predict(svm_model_10fold_pca, testset_pca, type = "prob")
svm_pred_prob_pca = svm_pred_10_fold_pca
colnames(svm_pred_prob_pca)[1] <- "fetal_healt_group1"
colnames(svm_pred_prob_pca)[2] <- "fetal_healt_group2"
colnames(svm_pred_prob_pca)[3] <- "fetal_healt_group3"
colnames(svm_pred_prob_pca) <- paste(colnames(svm_pred_prob_pca), "_pred_RF")

true_label <- dummies::dummy(testset_pca$fetal_health, sep=".")
true_label <- data.frame(true_label)
colnames(true_label)[1] <- "fetal_healt_group1"
colnames(true_label)[2] <- "fetal_healt_group2"
colnames(true_label)[3] <- "fetal_healt_group3"
colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
colnames(true_label) <- paste(colnames(true_label), "_true")

result = confusionMatrix(svm_pred_prob_pca, testset_pca[,c("fetal_health")], mode = "prec_recall")
result

final_df <- cbind(true_label, svm_pred_prob_pca)
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
