library(keras)
library(reticulate)
library(caret)
library(tidyverse)

#Sys.setenv is required for Tensorflow, as doesn't support python 3.7 yet
Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3.6")
install_keras(tensorflow = "1.15.0") #only required first run, after that throws an error about the env, ignore

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#data aug ----
range01 <- function(x){(x-min(x))/(max(x)-min(x))}



channels <- "32" #can be 32, 22, 13, 5, 4rear, 4front, mid, left, right or rear
fold <- 1 #can be (1:5)


all_acc <- NULL
all_F <- NULL
all_pre <- NULL
all_r <- NULL

#import the output labels
TrainingNames <- readRDS(paste("data2/Split A/", fold, "TrainLabels.Rda", sep = "")
                         )
TestingNames <- readRDS(paste("data2/Split A/", fold, "TestLabels.Rda", sep = "")
                        )

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/Split A/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/Split A/", channels, "/", fold , "Test.Rda", sep = "")
)

for (m in 1:length(TrainingNames)) {
  TrainingSet[m,,] <- range01(TrainingSet[m,,])
}
for (m in 1:length(TestingNames)) {
  TestingSet[m,,] <- range01(TestingSet[m,,])
}

rm(m)
#dim(TrainingSet)
#dim(TestingSet)

model <- keras_model_sequential() %>% 
  
  layer_conv_1d(kernel_size = 3, filters = 5, strides = 2, activation = "relu", name = "firstConvAndInput",
                input_shape = c(dim(TrainingSet[1,,])[1], dim(TrainingSet[1,,])[2])) %>%
  layer_conv_1d(kernel_size = 3, filters = 10, strides = 2, activation = "relu") %>% 
  layer_conv_1d(kernel_size = 3, filters = 10, strides = 2, activation = "relu") %>% 
  layer_conv_1d(kernel_size = 3, filters = 20, strides = 2, activation = "relu") %>% 
  
  layer_flatten() %>% 
  layer_dropout(.6) %>% 
  layer_dense(20) %>% 
  layer_dropout(.6) %>% 
  layer_dense(10) %>% 
  layer_dense(units = 1, activation = "sigmoid") %>% #immediately compile
  compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = 0.0005, beta_1 = 0.9, beta_2 = 0.999,
                               epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                               clipvalue = NULL),
    metrics = c("acc")
  )  
history <- model %>% fit(
  TrainingSet, TrainingNames,
  validation_split = .0,
  #validation_data=list(TestingSet01, TestingNames),
  shuffle = T,
  epochs = 50, batch_size = 32, verbose = 0
)


results <- model %>% evaluate(TestingSet, TestingNames, verbose = 0)
ConfMatrix <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

  
fold <- 2
TrainingNames <- readRDS(paste("data2/Split A/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/Split A/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/Split A/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/Split A/", channels, "/", fold , "Test.Rda", sep = "")
)

for (m in 1:length(TrainingNames)) {
  TrainingSet[m,,] <- range01(TrainingSet[m,,])
}
for (m in 1:length(TestingNames)) {
  TestingSet[m,,] <- range01(TestingSet[m,,])
}

rm(m)

compile(model, 
    loss = "binary_crossentropy",
    optimizer = optimizer_adam(lr = 0.0005, beta_1 = 0.9, beta_2 = 0.999,
                               epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                               clipvalue = NULL),
    metrics = c("acc")
  )  
  history <- model %>% fit(
    TrainingSet, TrainingNames,
    validation_split = .0,
    #validation_data=list(TestingSet01, TestingNames),
    shuffle = T,
    epochs = 50, batch_size = 32, verbose = 0
  )
  
  
  results <- model %>% evaluate(TestingSet, TestingNames, verbose = 0)
  ConfMatrix2 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
  x <- as.data.frame(ConfMatrix2)
  Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
  Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
  fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100
  
  all_acc <- rbind(all_acc, results$acc)
  all_r <- rbind(all_r, Recall)
  all_pre <- rbind(all_pre, Precision)
  all_F <- rbind(all_F, fmeasure)
  

  fold <- 3
  TrainingNames <- readRDS(paste("data2/Split A/", fold, "TrainLabels.Rda", sep = "")
  )
  TestingNames <- readRDS(paste("data2/Split A/", fold, "TestLabels.Rda", sep = "")
  )
  
  #1 is HC, 0 is PD
  TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
  TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)
  
  #import the actual data
  TrainingSet <- readRDS(
    paste("data2/Split A/", channels, "/", fold , "Train.Rda", sep = "")
  )
  TestingSet <- readRDS(
    paste("data2/Split A/", channels, "/", fold , "Test.Rda", sep = "")
  )
  
  for (m in 1:length(TrainingNames)) {
    TrainingSet[m,,] <- range01(TrainingSet[m,,])
  }
  for (m in 1:length(TestingNames)) {
    TestingSet[m,,] <- range01(TestingSet[m,,])
  }
  
  rm(m)
  
  compile(model, 
          loss = "binary_crossentropy",
          optimizer = optimizer_adam(lr = 0.0005, beta_1 = 0.9, beta_2 = 0.999,
                                     epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                                     clipvalue = NULL),
          metrics = c("acc")
  )  
  history <- model %>% fit(
    TrainingSet, TrainingNames,
    validation_split = .0,
    #validation_data=list(TestingSet01, TestingNames),
    shuffle = T,
    epochs = 50, batch_size = 32, verbose = 0
  )
  
  
  results <- model %>% evaluate(TestingSet, TestingNames, verbose = 0)
  ConfMatrix3 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
  x <- as.data.frame(ConfMatrix3)
  Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
  Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
  fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100
  
  all_acc <- rbind(all_acc, results$acc)
  all_r <- rbind(all_r, Recall)
  all_pre <- rbind(all_pre, Precision)
  all_F <- rbind(all_F, fmeasure)
  
  paste("Accuracy:", round((results$acc)*100, 2))
  paste("Recall:", round(Recall, 2))
  paste("Precision:", round(Precision, 2))
  paste("F-measure:", round(fmeasure, 3))
  ConfMatrix3
  
  fold <- 4
  TrainingNames <- readRDS(paste("data2/Split A/", fold, "TrainLabels.Rda", sep = "")
  )
  TestingNames <- readRDS(paste("data2/Split A/", fold, "TestLabels.Rda", sep = "")
  )
  
  #1 is HC, 0 is PD
  TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
  TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)
  
  #import the actual data
  TrainingSet <- readRDS(
    paste("data2/Split A/", channels, "/", fold , "Train.Rda", sep = "")
  )
  TestingSet <- readRDS(
    paste("data2/Split A/", channels, "/", fold , "Test.Rda", sep = "")
  )
  
  for (m in 1:length(TrainingNames)) {
    TrainingSet[m,,] <- range01(TrainingSet[m,,])
  }
  for (m in 1:length(TestingNames)) {
    TestingSet[m,,] <- range01(TestingSet[m,,])
  }
  
  rm(m)
  
  compile(model, 
          loss = "binary_crossentropy",
          optimizer = optimizer_adam(lr = 0.0005, beta_1 = 0.9, beta_2 = 0.999,
                                     epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                                     clipvalue = NULL),
          metrics = c("acc")
  )  
  history <- model %>% fit(
    TrainingSet, TrainingNames,
    validation_split = .0,
    #validation_data=list(TestingSet01, TestingNames),
    shuffle = T,
    epochs = 50, batch_size = 32, verbose = 0
  )
  
  
  results <- model %>% evaluate(TestingSet, TestingNames, verbose = 0)
  ConfMatrix4 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
  x <- as.data.frame(ConfMatrix4)
  Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
  Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
  fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100
  
  all_acc <- rbind(all_acc, results$acc)
  all_r <- rbind(all_r, Recall)
  all_pre <- rbind(all_pre, Precision)
  all_F <- rbind(all_F, fmeasure)
  
  fold <- 5
  
  TrainingNames <- readRDS(paste("data2/Split A/", fold, "TrainLabels.Rda", sep = "")
  )
  TestingNames <- readRDS(paste("data2/Split A/", fold, "TestLabels.Rda", sep = "")
  )
  
  #1 is HC, 0 is PD
  TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
  TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)
  
  #import the actual data
  TrainingSet <- readRDS(
    paste("data2/Split A/", channels, "/", fold , "Train.Rda", sep = "")
  )
  TestingSet <- readRDS(
    paste("data2/Split A/", channels, "/", fold , "Test.Rda", sep = "")
  )
  
  for (m in 1:length(TrainingNames)) {
    TrainingSet[m,,] <- range01(TrainingSet[m,,])
  }
  for (m in 1:length(TestingNames)) {
    TestingSet[m,,] <- range01(TestingSet[m,,])
  }
  
  rm(m)
  
  compile(model, 
          loss = "binary_crossentropy",
          optimizer = optimizer_adam(lr = 0.0005, beta_1 = 0.9, beta_2 = 0.999,
                                     epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                                     clipvalue = NULL),
          metrics = c("acc")
  )  
  history <- model %>% fit(
    TrainingSet, TrainingNames,
    validation_split = .0,
    #validation_data=list(TestingSet01, TestingNames),
    shuffle = T,
    epochs = 50, batch_size = 32, verbose = 0
  )
  
  
  results <- model %>% evaluate(TestingSet, TestingNames, verbose = 0)
  ConfMatrix5 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
  x <- as.data.frame(ConfMatrix5)
  Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
  Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
  fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100
  
  all_acc <- rbind(all_acc, results$acc)
  all_r <- rbind(all_r, Recall)
  all_pre <- rbind(all_pre, Precision)
  all_F <- rbind(all_F, fmeasure)
  

round(all_acc*100, 2)  
round(all_r, 2)  
round(all_pre, 2)  
round(all_F, 3)  

ConfMatrix
ConfMatrix2
ConfMatrix3
ConfMatrix4
ConfMatrix5

  ##Back to the top and change the channels
