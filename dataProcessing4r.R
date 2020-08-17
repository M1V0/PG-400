##run second to put all data into useable formats

library(tidyverse)
library(data.table)

#set current wd to saved file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

##list all files, both hc and pd, into an array
file_list <- list.files(path="~/PG-400/data2/raw/")

#Take a  sample of six subjects (20%), three HC and three PD for testing set
HCppts <- c(1,2,4)
PDppts <- c(3,5,6)

#then list file names for the testing files
TestingNames <- vector()

for(i in HCppts){
  tempN <-   paste("hc.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}
for(i in PDppts){
  tempN <-   paste("pd.off.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}

##tidy up unneeded objects
rm(i, temp, tempN, HCppts, PDppts)

#create a not in function and then create the file names of the training set 
"%ni%" <- Negate("%in%") ##create a NOT IN function
TrainingNames <- file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()
##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,4,0))
for (i in 1:850){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,4,0))
for (i in 851:1700){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,4,0))
for (i in 1701:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,4,0))
for (i in 1:length(TestingNames)){
  
  temp_data <- fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="data2/Split A/4rear/1Train.Rda")
saveRDS(TestingSetMod, file="data2/Split A/4rear/1Test.Rda")
saveRDS(TrainingNames, file="data2/Split A/1TrainLabels.Rda")
saveRDS(TestingNames, file = "data2/Split A/1TestLabels.Rda")


file_list <- list.files(path="~/PG-400/data2/raw/")

#Take a  sample of six subjects (20%), three HC and three PD for testing set
HCppts <- c(7,8,10)
PDppts <- c(9,11,12)

#then list file names for the testing files
TestingNames <- vector()

for(i in HCppts){
  tempN <-   paste("hc.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}
for(i in PDppts){
  tempN <-   paste("pd.off.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}

##tidy up unneeded objects
rm(i, temp, tempN, HCppts, PDppts)

#create a not in function and then create the file names of the training set 
"%ni%" <- Negate("%in%") ##create a NOT IN function
TrainingNames <- file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()
##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,4,0))
for (i in 1:850){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,4,0))
for (i in 851:1700){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,4,0))
for (i in 1701:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,4,0))
for (i in 1:length(TestingNames)){
  
  temp_data <- fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="data2/Split A/4rear/2Train.Rda")
saveRDS(TestingSetMod, file="data2/Split A/4rear/2Test.Rda")
saveRDS(TrainingNames, file="data2/Split A/2TrainLabels.Rda")
saveRDS(TestingNames, file = "data2/Split A/2TestLabels.Rda")

file_list <- list.files(path="~/PG-400/data2/raw/")

#Take a  sample of six subjects (20%), three HC and three PD for testing set
HCppts <- c(18,20,21)
PDppts <- c(13,14,16)

#then list file names for the testing files
TestingNames <- vector()

for(i in HCppts){
  tempN <-   paste("hc.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}
for(i in PDppts){
  tempN <-   paste("pd.off.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}

##tidy up unneeded objects
rm(i, temp, tempN, HCppts, PDppts)

#create a not in function and then create the file names of the training set 
"%ni%" <- Negate("%in%") ##create a NOT IN function
TrainingNames <- file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()
##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,4,0))
for (i in 1:850){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,4,0))
for (i in 851:1700){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,4,0))
for (i in 1701:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,4,0))
for (i in 1:length(TestingNames)){
  
  temp_data <- fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="data2/Split A/4rear/3Train.Rda")
saveRDS(TestingSetMod, file="data2/Split A/4rear/3Test.Rda")
saveRDS(TrainingNames, file="data2/Split A/3TrainLabels.Rda")
saveRDS(TestingNames, file = "data2/Split A/3TestLabels.Rda")

file_list <- list.files(path="~/PG-400/data2/raw/")

#Take a  sample of six subjects (20%), three HC and three PD for testing set
HCppts <- c(24,25,29)
PDppts <- c(17,19,22)

#then list file names for the testing files
TestingNames <- vector()

for(i in HCppts){
  tempN <-   paste("hc.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}
for(i in PDppts){
  tempN <-   paste("pd.off.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}

##tidy up unneeded objects
rm(i, temp, tempN, HCppts, PDppts)

#create a not in function and then create the file names of the training set 
"%ni%" <- Negate("%in%") ##create a NOT IN function
TrainingNames <- file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()
##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,4,0))
for (i in 1:850){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,4,0))
for (i in 851:1700){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,4,0))
for (i in 1701:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,4,0))
for (i in 1:length(TestingNames)){
  
  temp_data <- fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="data2/Split A/4rear/4Train.Rda")
saveRDS(TestingSetMod, file="data2/Split A/4rear/4Test.Rda")
saveRDS(TrainingNames, file="data2/Split A/4TrainLabels.Rda")
saveRDS(TestingNames, file = "data2/Split A/4TestLabels.Rda")

file_list <- list.files(path="~/PG-400/data2/raw/")

#Take a  sample of six subjects (20%), three HC and three PD for testing set
HCppts <- c(30,31,32,33)
PDppts <- c(23,26,28)

#then list file names for the testing files
TestingNames <- vector()

for(i in HCppts){
  tempN <-   paste("hc.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}
for(i in PDppts){
  tempN <-   paste("pd.off.", i, ".", sep = "")
  temp <- file_list[grep(tempN,
                         file_list, fixed = T)]
  TestingNames <- append(TestingNames, temp)
}

##tidy up unneeded objects
rm(i, temp, tempN, HCppts, PDppts)

#create a not in function and then create the file names of the training set 
"%ni%" <- Negate("%in%") ##create a NOT IN function
TrainingNames <- file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()
##change the dir for the two loops
setwd("/Users/ivorym/PG-400/data2/raw")

#create the tensor of training data
TrainingSet1 <- array(numeric(),c(1024,4,0))
for (i in 1:850){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet1 <- abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(),c(1024,4,0))
for (i in 851:1700){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet2 <- abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(),c(1024,4,0))
for (i in 1701:length(TrainingNames)){
  
  temp_data <- fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TrainingSet3 <- abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}

##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <- abind::abind(TrainingSet1, TrainingSet2, TrainingSet3, along=3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(),c(1024,4,0))
for (i in 1:length(TestingNames)){
  
  temp_data <- fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
  temp_data <- select(temp_data, CP5, CP1, CP2, CP6, -time) #remove the time record as it is unnecessary
  temp_data <- temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
  TestingSet <- abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

TrainingSetMod <- aperm(TrainingSet, c(3,1,2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <- aperm(TestingSet, c(3,1,2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(TrainingSetMod, file="data2/Split A/4rear/5Train.Rda")
saveRDS(TestingSetMod, file="data2/Split A/4rear/5Test.Rda")
saveRDS(TrainingNames, file="data2/Split A/5TrainLabels.Rda")
saveRDS(TestingNames, file = "data2/Split A/5TestLabels.Rda")
