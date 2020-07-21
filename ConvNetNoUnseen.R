library(keras)
library(reticulate)
library(caret)
library(tidyverse)

#Sys.setenv is required for Tensorflow, as doesn't support python 3.7 yet
Sys.setenv(RETICULATE_PYTHON = "/usr/local/bin/python3.6")
install_keras(tensorflow = "1.15.0") #only required first run, after that throws an error about the env, ignore

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#data aug ----

#import the output labels
TrainingNames <- readRDS("data/TrainLabelsnopre32NoUnseen.Rda")
TestingNames <- readRDS("data/TestLabelsnopre32NoUnseen.Rda")

#1 is HC, 0 is PD
TrainingNames <- ifelse(grepl("hc", TrainingNames), 1, 0)
TestingNames <- ifelse(grepl("hc", TestingNames), 1, 0)

#import the actual data
TrainingSet <- readRDS("data/TrainFullnopre32NoUnseen.Rda")
TestingSet <- readRDS("data/TestFullnopre32NoUnseen.Rda")

#Take percentage of HCs in the complete Training Set and the Testing Set
sum(TrainingNames)*100/(length(TrainingNames)) # ~54.5%
sum(TestingNames)*100/(length(TestingNames)) # ~51.6%



#model ----
build_model <- function() {
 (model <- keras_model_sequential() %>% 
    
    layer_conv_1d(kernel_size = 20, filters = 5, strides = 2, activation = "relu", name = "firstConvAndInput",
                  input_shape = c(dim(TrainingSet[1,,])[1], dim(TrainingSet[1,,])[2])) %>%
    layer_dropout(.5) %>% 
    layer_max_pooling_1d(2) %>% 
    
    layer_conv_1d(kernel_size = 15, filters = 7, strides = 2, activation = "relu", name = "2conv") %>% 
    layer_dropout(.5) %>% 
    layer_max_pooling_1d(2) %>% 
    
    layer_conv_1d(kernel_size = 10, filters = 10, strides = 2, activation = "relu", name = "3conv") %>% 
    #layer_dropout(.5) %>% 
    layer_max_pooling_1d(2) %>% 
    
    
    layer_flatten() %>% 
    layer_dense(units = 1, activation = "sigmoid")
 ) %>% #immediately compile
   compile(
     loss = "binary_crossentropy",
     optimizer = optimizer_adam(lr = 0.001, beta_1 = 0.9, beta_2 = 0.999,
                                epsilon = NULL, decay = 0, amsgrad = FALSE, clipnorm = NULL,
                                clipvalue = NULL),
     metrics = c("acc")
   )
 
}
 model
##only for the full model as it saves the weights each epoch
model.history <- model %>% fit(
  TrainingSet, TrainingNames,
  epochs = 40, batch_size = 32, 
  validation_split = 0.2,
  view_metrics = T
)

#plots the accuracy and validation accuracy in lieu of the metrics viewer plot
#plot(1:model.history$params$epochs, model.history$metrics$acc, type = "l")
#lines(1:model.history$params$epochs, model.history$metrics$val_acc, type = "l")

results <- model %>% evaluate(TestingSet, TestingNames, verbose = 0)
(ConfMatrix <- table(TestingNames, prediction = predict_classes(model, TestingSet))) # creates a simple confusion matrix
x <- as.data.frame(ConfMatrix)
Recall <- (x[1,3]/(x[1,3]+x[2,3]))*100
Precision <- (x[1,3]/(x[1,3]+x[3,3]))*100

paste("Accuracy:", round(results$acc, 2))
paste("Recall:", round(Recall, 2))
paste("Precision:", round(Precision, 2))


model.history %>% write_rds("completeModels/Model-valacc89.11.date0707.Rda")
model %>% save_model_hdf5("completeModels/Model-valacc89.11.date0707.hdf5")
