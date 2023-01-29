#KNN
library(caret)
library(class)

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
  
  # Dopasowanie modelu KNN do danych treningowych
  
  model_knn <- knn(train=train_data, test=test_data, train_data$output, k=11)
  confusion_matrix <- confusionMatrix(table(test_data$output, model_knn))
  print(confusion_matrix)
  accuracy <- confusion_matrix$overall[1]
  acc_list <- append(acc_list, accuracy)
  print(paste("Accuracy #", i, accuracy))
}
print(paste("SD of accuracy", sd(as.double(acc_list))))
print(paste("Mean of accuracy", mean(as.double(acc_list))))


