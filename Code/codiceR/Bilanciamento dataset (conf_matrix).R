if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")}
if("rpart" %in% rownames(installed.packages()) == FALSE) {install.packages("rpart")}
if("C50" %in% rownames(installed.packages()) == FALSE) {install.packages("C50")}
if("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}

library(rpart)
library(caret)
library(C50)


# Funzione che divide il dataset in input in trainset (70%) e testset (30%)
split_train_test <- function(dataset, prob, seed){
  set.seed(seed)
  ind = sample(2, nrow(dataset), replace = TRUE, prob=c(prob, 1-prob))
  test = dataset[ind == 2,]
  train = dataset[ind == 1,]
  return (list(train=train, test=test))
}

# Funzione allenamento e previsione decision tree
decision_tree_model <- function(trainset, testset, parms){
  if(missingArg(parms)){
    decisionTree = rpart(fetal_health ~ ., data=trainset, method="class")
  }else{
    decisionTree = rpart(fetal_health ~ ., data=trainset, method="class", parms = parms)
  }
  fancyRpartPlot(decisionTree, cex = 0.6)
  testset$Prediction <- predict(decisionTree, testset, type = "class")
  
  plotcp(decisionTree)
  result = confusionMatrix(testset$Prediction, testset[,c("fetal_health")], mode = "prec_recall")
  return (result)
}

# Funzione per oversampling di un dataset
dataset_oversampling <- function(trainset){
  train_u = upSample(trainset, trainset$fetal_health)
  table(train_u$fetal_health)
  train_u <- train_u[-dim(train_u)[2]]
  return (train_u)
}


# Funzione per undersampling di un dataset
dataset_undersampling <- function(trainset){
  train_d = downSample(trainset, trainset$fetal_health)
  table(train_d$fetal_health)
  train_d <- train_d[-dim(train_d)[2]]
  return (train_d)
}





# Caricamento dataset originale
dataset = read.csv("fetal_health.csv", na.strings=c("NA", ""))
# Caricamento dataset pca
dataset_pca = read.csv("fetal_health_new_dim.csv", na.strings=c("NA", ""))

dataset$fetal_health <- factor(dataset$fetal_health)
dataset_pca$fetal_health <- factor(dataset_pca$fetal_health)


library(e1071)
#Suddivisione train e test
allset = split_train_test(dataset, 0.7, 1)
trainset = allset$train
testset = allset$test

#Suddivisione train e test PCA
allset = split_train_test(dataset_pca, 0.7, 1)
trainset_pca = allset$train
testset_pca = allset$test



# Distribuzione variabile target
table(trainset$fetal_health)
table(trainset_pca$fetal_health)


# ********    DECISION TREE   ********* 

# --- original ---
decision_tree_model(trainset, testset)

# --- original PCA ---
decision_tree_model(trainset_pca, testset_pca)




# --- dataset up (OVERSAMPLING) ---
train_u <- dataset_oversampling(trainset)
decision_tree_model(train_u, testset)

# --- (OVERSAMPLING) PCA ---
train_u <- dataset_oversampling(trainset_pca)
decision_tree_model(train_u, testset_pca)




# --- dataset down (UNDERSAMPLING) ---
train_d <- dataset_undersampling(trainset)
decision_tree_model(train_d, testset)

# --- (UNDERSAMPLING) PCA ---
train_d <- dataset_undersampling(trainset_pca)
decision_tree_model(train_d, testset_pca)




# --- Classificazione basata sui pesi ---
decision_tree_model(trainset, testset, parms = list(prior = c(.1,.35,.55)))
# --- PCA ----
decision_tree_model(trainset_pca, testset_pca, parms = list(prior = c(.1,.35,.55)))


# Funzione tuning svm 
tune_svm_model <- function(trainset, testset, wts){
  if(missingArg(wts)){
    tuned = tune.svm(fetal_health ~ ., data = trainset, kernel='polynomial',
                     cost=c(0.001, 0.01, 0.1, 1, 1.5, 5,10,100, 200), probability = TRUE)
  }else{
    tuned = tune.svm(fetal_health ~ ., data = trainset, kernel='polynomial',
                     cost=c(0.001, 0.01, 0.1, 1, 1.5, 5,10,100, 200), probability = TRUE, class.weights = wts)
  }
  
  tuned$best.parameters
  tuned$best.model
  svm_pred = predict(tuned$best.model, testset, probability = TRUE)
  result = confusionMatrix(svm_pred, testset[,c("fetal_health")], mode = "prec_recall")
  return (result) 
}



# ***********     SVM     ************

tune_svm_model(trainset, testset)

# --- Original PCA ---
tune_svm_model(trainset_pca, testset_pca)





# --- dataset up (OVERSAMPLING) ---
train_u <- dataset_oversampling(trainset)
tune_svm_model(train_u, testset)

# --- (OVERSAMPLING) PCA---
train_u <- dataset_oversampling(trainset_pca)
tune_svm_model(train_u, testset_pca)





# --- dataset down (UNDERSAMPLING) ---
train_d <- dataset_undersampling(trainset)
tune_svm_model(train_d, testset)

# --- (UNDERSAMPLING) PCA ---
train_d <- dataset_undersampling(trainset_pca)
tune_svm_model(train_d, testset_pca)





# --- Classificazione basata sui pesi ---
wts <- 1/table(trainset$fetal_health)

#tune_svm_model(trainset, testset, wts)

# --- PCA ----
wts <- 1/table(trainset_pca$fetal_health)
#tune_svm_model(trainset_pca, testset_pca, wts)

