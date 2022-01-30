if("e1071" %in% rownames(installed.packages()) == FALSE) {install.packages("e1071")}
if("caret" %in% rownames(installed.packages()) == FALSE) {install.packages("caret")}
if("plotly" %in% rownames(installed.packages()) == FALSE) {install.packages("plotly")}
library("e1071")

library("caret")

dataset = read.csv("fetal_health.csv", na.strings=c("NA", "")) 

dim(dataset)
str(dataset)
summary = data.frame(unclass(summary(dataset)), check.names = FALSE, stringsAsFactors = FALSE)
summary = do.call(cbind, lapply(dataset, summary))

#Controllo se ci sono valori nulli
table(is.na(dataset))


dataset$fetal_health <- factor(dataset$fetal_health)
#io metterei anche questa features come factor
dataset$histogram_tendency <- factor(dataset$histogram_tendency)

summary(da)

par(mfrow=c(1,4))
plot(dataset$fetal_health, main="Variabile target (fetal_health)", col = c(2,3,4))

table.fetal = table(dataset$fetal_health)
table.fetal

pie(table.fetal, main="Variabile target (fetal_health)", col = c(2,3,4), labels = names(table.fetal))



head(dataset)

dataset_except_target = dataset[,1:21]

par(mfrow=c(1,1))
for(i in 1:20) {
  boxplot(dataset_except_target[,i], main=names(dataset)[i]) }

par(mfrow=c(1,2))

boxplot(baseline.value ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(accelerations ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(fetal_movement ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(uterine_contractions ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5 )
boxplot(light_decelerations ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(severe_decelerations ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(prolongued_decelerations ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(abnormal_short_term_variability ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(mean_value_of_short_term_variability ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(percentage_of_time_with_abnormal_long_term_variability ~ fetal_health, dataset, cex.lab = 1.5, cex.lab = 1.5)
boxplot(mean_value_of_long_term_variability ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_width ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_min ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_max ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_number_of_peaks ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_number_of_zeroes ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_mode ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_mean ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_median ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_variance ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)
boxplot(histogram_tendency ~ fetal_health, dataset, cex.lab = 1.5, cex.axis = 1.5)

plot(histogram_tendency ~ fetal_health, dataset, cex.lab = 2, cex.axis = 2)
plot(dataset$histogram_tendency, cex.axis = 1.5)

array_name = names(dataset)


par(mfrow=c(1,1))
#creates frequency plots for all features
for (i in 1:ncol(dataset))
  hist(dataset[,i], xlab = colnames(dataset[i]), cex.axis = 1.5, main = "")


x_uno = dataset[, 1:6]
x_due = dataset[, 7:12]
x_tre = dataset[, 13:18]
x_quattro = dataset[, 19:21]
y = dataset[, 22]

par(mfrow=c(1,1))


featurePlot(x_uno, y, plot="density", scales=list(x=list(relation="free"),
                                              y=list(relation="free")), auto.key=list(columns=3))
featurePlot(x_due, y, plot="density", scales=list(x=list(relation="free"),
                                              y=list(relation="free")), auto.key=list(columns=3))
featurePlot(x_tre, y, plot="density", scales=list(x=list(relation="free"),
                                              y=list(relation="free")), auto.key=list(columns=3))
featurePlot(x_quattro, y, plot="density", scales=list(x=list(relation="free"),
                                              y=list(relation="free")), auto.key=list(columns=3))



par(mfrow=c(1,1))
plot(y,col=c(4,6,3))
#Distribuzione delle istanze rispetto alle covariate
x_decelerations = dataset[, 5:7]
x_prime_quattro = dataset[, 1:4]
x_short_long_term = dataset[, 8:11]
x_last = dataset[, 12:21]
featurePlot(x=x_esempio, y=y, plot="pairs", auto.key=list(columns=3))
featurePlot(x=x_prime_quattro, y=y, plot="pairs", auto.key=list(columns=3))
featurePlot(x=x_short_long_term, y=y, plot="pairs", auto.key=list(columns=3))
featurePlot(x=x_last, y=y, plot="pairs", auto.key=list(columns=3))


library(plotly)
m <- matrix(rnorm(9), nrow = 3, ncol = 3)
fig <- plot_ly(
  x = names(dataset[,1:20]), y = names(dataset[,1:20]),
  z = cor(dataset[,1:20]), type = "heatmap"
)
show(fig)
