#NaiveBayes
library(caret)
library(class)
library(naivebayes)

# Wczytanie zbioru danych
data <- read.csv("https://raw.githubusercontent.com/Morritz/SI-R/main/heart.csv", header = TRUE)
labels <- colnames(data)

output_label <- labels[ncol(data)]
input_labels <- labels[-ncol(data)]

n <- 100 # Iteracje
acc_list <- list()
proporcja <- 0.2

for(i in 1:n){
  
  # Podział zbioru danych na zbiór treningowy i testowy
  split <- createDataPartition(data$output, p = proporcja, list = FALSE)
  train_data <- data[split, ]
  test_data <- data[-split, ]
  
  # Dopasowanie modelu NaiveBayes do danych treningowych
  model_nb <- naive_bayes(as.logical(output) ~ ., data = train_data)
  predictions <- predict(model_nb, test_data[,input_labels])
  confusion_matrix <- table(predictions, test_data$output)
  print(confusion_matrix)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  acc_list <- append(acc_list, accuracy)
  print(paste("Accuracy #", i, accuracy))
}
print(paste("SD of accuracy", sd(as.double(acc_list))))
print(paste("Mean of accuracy", mean(as.double(acc_list))))


