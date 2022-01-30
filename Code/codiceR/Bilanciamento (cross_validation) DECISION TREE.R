if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")}
if("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
if("pROC" %in% rownames(installed.packages()) == FALSE) {install.packages("pROC")}
if("multiROC" %in% rownames(installed.packages()) == FALSE) {install.packages("multiROC")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("DMwR" %in% rownames(installed.packages()) == FALSE) {install.packages("DMwR")}
if("purrr" %in% rownames(installed.packages()) == FALSE) {install.packages("purrr")}
if("MLmetrics" %in% rownames(installed.packages()) == FALSE) {install.packages("MLmetrics")}
if("dummies" %in% rownames(installed.packages()) == FALSE) {install.packages("dummies")}

library(e1071)
library(caret)
library(pROC)
library(multiROC)
library(dplyr) # for data manipulation
library(DMwR) # for smote implementation
library(purrr) # for functional programming (map)
library(MLmetrics)
library(dummies)


# Caricamento dataset PCA
dataset = read.csv("fetal_health_new_dim.csv", na.strings=c("NA", ""))

# Caricamento dataset originale
#dataset = read.csv("fetal_health.csv", na.strings=c("NA", ""))

dataset$fetal_health <- factor(dataset$fetal_health)


# Funzione che divide il dataset in input in trainset (70%) e testset (30%)
split_train_test <- function(dataset, prob, seed){
  set.seed(seed)
  ind = sample(2, nrow(dataset), replace = TRUE, prob=c(prob, 1-prob))
  test = dataset[ind == 2,]
  train = dataset[ind == 1,]
  return (list(train=train, test=test))
}

# Suddivisione train e test
allset = split_train_test(dataset, 0.7, 5627)
trainset = allset$train
testset = allset$test


table(trainset$fetal_health)




# Funzione per curva roc
roc_curve <- function(model){
  true_label <- dummies::dummy(testset$fetal_health, sep=".")
  true_label <- data.frame(true_label)
  colnames(true_label)[1] <- "fetal_healt_group1"
  colnames(true_label)[2] <- "fetal_healt_group2"
  colnames(true_label)[3] <- "fetal_healt_group3"
  colnames(true_label) <- gsub(".*?\\.", "", colnames(true_label))
  colnames(true_label) <- paste(colnames(true_label), "_true")
  
  dt_pred <- predict(model, testset, type = "prob")
  dt_pred <- data.frame(dt_pred)
  
  
  colnames(dt_pred)[1] <- "fetal_healt_group1"
  colnames(dt_pred)[2] <- "fetal_healt_group2"
  colnames(dt_pred)[3] <- "fetal_healt_group3"
  colnames(dt_pred) <- paste(colnames(dt_pred), "_pred_RF")
  
  final_df <- cbind(true_label, dt_pred)
  
  roc_res <- multi_roc(final_df)
  
  plot_roc_df <- plot_roc_data(roc_res)
  
  return (list(roc_res = roc_res, plot_roc_df = plot_roc_df))
}


plot_roc_curve <- function(plot_roc_df){
  require(ggplot2)
  ggplot(plot_roc_df, aes(x = 1-Specificity, y=Sensitivity)) +
    geom_path(aes(color = Group, linetype=Method), size=1.5) +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), 
                 colour='grey', linetype = 'dotdash') +
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0.5), 
          legend.justification=c(1, 0), legend.position=c(.95, .05),
          legend.text = element_text(size = 10),
          legend.title=element_blank(), 
          legend.background = element_rect(fill=NULL, size=1.5, 
                                           linetype="solid", colour ="black"))
}



levels(trainset$fetal_health) <- c("first_class", "second_class", "third_class")

set.seed(5627)

# Set up control function for training

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     summaryFunction = multiClassSummary,
                     classProbs = TRUE)


orig_fit <- train(fetal_health ~ .,
                  data = trainset,
                  method = "rpart",
                  metric = "ROC",
                  trControl = ctrl)
orig_fit


res = roc_curve(orig_fit)
roc_res_dectree_origin = res$roc_res
plot_roc_df = res$plot_roc_df
plot_roc_curve(plot_roc_df)



model_weights <- ifelse(trainset$fetal_health == 1,
                        (1/table(trainset$fetal_health)[1]),
                        ifelse(trainset$fetal_health == 2,
                               (1/table(trainset$fetal_health)[2]),
                               (1/table(trainset$fetal_health)[3])))



# Build weighted model
weighted_fit <- train(fetal_health ~ .,
                      data = trainset,
                      method = "rpart",
                      #verbose = FALSE,
                      weights = model_weights,
                      metric = "ROC",
                      trControl = ctrl)
weighted_fit

res = roc_curve(weighted_fit)
roc_res_dectree_weight = res$roc_res
plot_roc_df = res$plot_roc_df
plot_roc_curve(plot_roc_df)






# Build down-sampled model
ctrl$sampling <- "down"

down_fit <- train(fetal_health ~ .,
                  data = trainset,
                  method = "rpart",
                  #verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)
down_fit

res = roc_curve(down_fit)
roc_res_dectree_down = res$roc_res
plot_roc_df = res$plot_roc_df
plot_roc_curve(plot_roc_df)


# Build up-sampled model
ctrl$sampling <- "up"

up_fit <- train(fetal_health ~ .,
                  data = trainset,
                  method = "rpart",
                  #verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl)
up_fit

res = roc_curve(up_fit)
roc_res_dectree_up = res$roc_res
plot_roc_df = res$plot_roc_df
plot_roc_curve(plot_roc_df)




# Build smote model
ctrl$sampling <- "smote"

smote_fit <- train(fetal_health ~ .,
                   data = trainset,
                   method = "rpart",
                   #verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)
smote_fit

res = roc_curve(smote_fit)
roc_res_dectree_smote = res$roc_res
plot_roc_df = res$plot_roc_df
plot_roc_curve(plot_roc_df)


