##run fourth to put all data into useable formats for Split B

library(tidyverse)
library(data.table)

##list all files, both hc and pd, into an array
file_list <- list.files(path="~/PG-400/data2/raw/")

#Take a random sample of 20%
set.seed(32)
TestingNames <- sample(file_list)
Split.i <- TestingNames[1:504]
Split.ii <- TestingNames[505:1008]
Split.iii <- TestingNames[1009:1512]
Split.iv <- TestingNames[1513:2016]
Split.v <- TestingNames[2017:2520]
Split.vi <- TestingNames[2521:3026]

#create a not in function and then create the file names of the training set 
"%ni%" <- Negate("%in%") ##create a NOT IN function

TrainingNames <- file_list[file_list %ni% Split.i]
set.seed(32)
TrainingNames <- sample(TrainingNames)


##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,13,0))
for (i in 1:756){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,13,0))
for (i in 757:1512){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,13,0))
for (i in 1513:2268){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(),c(1024,13,0))
for (i in 2269:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet4 <- abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,13,0))
for (i in 1:length(Split.i)){
  
  temp_data <- fread(Split.i[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
#dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
#dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="~/PG-400/data2/split B/13/1Train.Rda")
saveRDS(TestingSetMod, file="~/PG-400/data2/split B/13/1Test.Rda")
saveRDS(TrainingNames, file="~/PG-400/data2/split B/13/1TrainLabels.Rda")
saveRDS(Split.i, file="~/PG-400/data2/split B/13/1TestLabels.Rda")

TrainingNames <- file_list[file_list %ni% Split.ii]
set.seed(32)
TrainingNames <- sample(TrainingNames)

##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,13,0))
for (i in 1:756){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,13,0))
for (i in 757:1512){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,13,0))
for (i in 1513:2268){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(),c(1024,13,0))
for (i in 2269:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet4 <- abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,13,0))
for (i in 1:length(Split.ii)){
  
  temp_data <- fread(Split.ii[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
#dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
#dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="~/PG-400/data2/split B/13/2Train.Rda")
saveRDS(TestingSetMod, file="~/PG-400/data2/split B/13/2Test.Rda")
saveRDS(TrainingNames, file="~/PG-400/data2/split B/13/2TrainLabels.Rda")
saveRDS(Split.ii, file="~/PG-400/data2/split B/13/2TestLabels.Rda")

TrainingNames <- file_list[file_list %ni% Split.iii]
set.seed(32)
TrainingNames <- sample(TrainingNames)

##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,13,0))
for (i in 1:756){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,13,0))
for (i in 757:1512){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,13,0))
for (i in 1513:2268){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(),c(1024,13,0))
for (i in 2269:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet4 <- abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,13,0))
for (i in 1:length(Split.iii)){
  
  temp_data <- fread(Split.iii[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
#dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
#dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="~/PG-400/data2/split B/13/3Train.Rda")
saveRDS(TestingSetMod, file="~/PG-400/data2/split B/13/3Test.Rda")
saveRDS(TrainingNames, file="~/PG-400/data2/split B/13/3TrainLabels.Rda")
saveRDS(Split.iii, file="~/PG-400/data2/split B/13/3TestLabels.Rda")

TrainingNames <- file_list[file_list %ni% Split.iv]
set.seed(32)
TrainingNames <- sample(TrainingNames)

##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,13,0))
for (i in 1:756){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,13,0))
for (i in 757:1512){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,13,0))
for (i in 1513:2268){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(),c(1024,13,0))
for (i in 2269:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet4 <- abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,13,0))
for (i in 1:length(Split.iv)){
  
  temp_data <- fread(Split.iv[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
#dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
#dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="~/PG-400/data2/split B/13/4Train.Rda")
saveRDS(TestingSetMod, file="~/PG-400/data2/split B/13/4Test.Rda")
saveRDS(TrainingNames, file="~/PG-400/data2/split B/13/4TrainLabels.Rda")
saveRDS(Split.iv, file="~/PG-400/data2/split B/13/4TestLabels.Rda")

TrainingNames <- file_list[file_list %ni% Split.v]
set.seed(32)
TrainingNames <- sample(TrainingNames)

##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,13,0))
for (i in 1:756){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,13,0))
for (i in 757:1512){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,13,0))
for (i in 1513:2268){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(),c(1024,13,0))
for (i in 2269:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet4 <- abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,13,0))
for (i in 1:length(Split.v)){
  
  temp_data <- fread(Split.v[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
#dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
#dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="~/PG-400/data2/split B/13/5Train.Rda")
saveRDS(TestingSetMod, file="~/PG-400/data2/split B/13/5Test.Rda")
saveRDS(TrainingNames, file="~/PG-400/data2/split B/13/5TrainLabels.Rda")
saveRDS(Split.v, file="~/PG-400/data2/split B/13/5TestLabels.Rda")

TrainingNames <- file_list[file_list %ni% Split.vi]
set.seed(32)
TrainingNames <- sample(TrainingNames)

##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,13,0))
for (i in 1:756){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,13,0))
for (i in 757:1512){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,13,0))
for (i in 1513:2268){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(),c(1024,13,0))
for (i in 2269:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet4 <- abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,13,0))
for (i in 1:length(Split.vi)){
  
  temp_data <- fread(Split.vi[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, T7, C3, Cz, C4, T8, FC5, FC1, FC2, FC6, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
#dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
#dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="~/PG-400/data2/split B/13/6Train.Rda")
saveRDS(TestingSetMod, file="~/PG-400/data2/split B/13/6Test.Rda")
saveRDS(TrainingNames, file="~/PG-400/data2/split B/13/6TrainLabels.Rda")
saveRDS(Split.vi, file="~/PG-400/data2/split B/13/6TestLabels.Rda")
