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



channels <- "delta" #can be 32, 22, 13, 5, 4rear, 4front, mid, left, right or rear
fold <- 1 #can be (1:5)


all_acc <- NULL
all_F <- NULL
all_pre <- NULL
all_r <- NULL

#import the output labels
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
  epochs = 50, batch_size = 32, verbose = 1
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
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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

TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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



fold <- 6
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix6 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix6)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 7
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix7 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix7)
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

fold <- 8
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix8 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix8)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 9

TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix9 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix9)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 10
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix10 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix10)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 11
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix11 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix11)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 12
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix12 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix12)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 13

TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix13 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix13)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 14
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix14 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix14)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 16
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix16 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix16)
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

fold <- 17
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix17 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix17)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 18

TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix18 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix18)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 19
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix19 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix19)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 20
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix20 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix20)
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

fold <- 21
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix21 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix21)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 22

TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix22 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix22)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 23
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix23 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix23)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 24
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix24 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix24)
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

fold <- 25
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix25 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix25)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 26

TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix26 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix26)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 28
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix28 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix28)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)


fold <- 29
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix29 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix29)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 30
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix30 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix30)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 31
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix31 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix31)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 32
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix32 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix32)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100
fmeasure <- (2*Recall*Precision)/(Recall+Precision)/100

all_acc <- rbind(all_acc, results$acc)
all_r <- rbind(all_r, Recall)
all_pre <- rbind(all_pre, Precision)
all_F <- rbind(all_F, fmeasure)

fold <- 33
TrainingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TrainLabels.Rda", sep = "")
)
TestingNames <- readRDS(paste("data2/LOPO/", channels, "/", fold, "TestLabels.Rda", sep = "")
)

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Train.Rda", sep = "")
)
TestingSet <- readRDS(
  paste("data2/LOPO/", channels, "/", fold , "Test.Rda", sep = "")
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
ConfMatrix33 <- table(TestingNames, prediction = predict_classes(model, TestingSet)) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix33)
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
ConfMatrix6
ConfMatrix7
ConfMatrix8
ConfMatrix9
ConfMatrix10
ConfMatrix11
ConfMatrix12
ConfMatrix13
ConfMatrix14
ConfMatrix16
ConfMatrix17
ConfMatrix18
ConfMatrix19
ConfMatrix20
ConfMatrix21
ConfMatrix22
ConfMatrix23
ConfMatrix24
ConfMatrix25
ConfMatrix26
ConfMatrix28
ConfMatrix29
ConfMatrix30
ConfMatrix31
ConfMatrix32
ConfMatrix33

##Back to the top and change the channels
