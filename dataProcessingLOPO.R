##run second to put all data into useable formats

library(tidyverse)
library(data.table)

#set current wd to saved file location
setwd("/Users/ivorym/PG-400/data2/raw")

##list all files, both hc and pd, into an array
file_list <- list.files(path = "~/PG-400/data2/raw/")

ppt <- 1

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <- Negate("%in%") ##create a NOT IN function
TrainingNames <- file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <- array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(), c(1024, 32, 0))
for (i in 1211:1815){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(TrainingSet1,
 TrainingSet2,
 TrainingSet3,
 TrainingSet4,
 TrainingSet5,
 along = 3)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 2

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 3

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

tempN <- paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <- Negate("%in%") ##create a NOT IN function
TrainingNames <- file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <- array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 4

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 5

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

tempN <- paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <- array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 6

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 7

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 8

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
 along = 3
 )
 #rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again
 
 
 #create the tensor of testing data
 TestingSet <-
 array(numeric(), c(1024, 32, 0))
 for (i in 1:length(TestingNames)){
 temp_data <-
 fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
 temp_data <-
 select(temp_data, -time) #remove the time record as it is unnecessary
 temp_data <-
 temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
 TestingSet <-
 abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
 }
 ##for ease of working, save the arrays
 TrainingSetMod <-
 aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
 dim(TrainingSetMod) #just checking it worked as intended
 TestingSetMod <-
 aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
 dim(TestingSetMod) #just checking it worked as intended
 
 ##finally save everything to load into memory at a later date
 saveRDS(
 TrainingSetMod,
 file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
 saveRDS(
 TestingSetMod,
 file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
 saveRDS(
 TrainingNames,
 file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
 saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 9

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 10

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 11

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 12

#then list file names for the testing files
TestingNames <-
vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 13

#then list file names for the testing files
TestingNames <-
vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 14

#then list file names for the testing files
TestingNames <-
vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
 file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
 saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 16

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 17

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

tempN <- paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <- Negate("%in%") ##create a NOT IN function
TrainingNames <- file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <- array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 18

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 19

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

tempN <- paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <- array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <- array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <- array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 20

#then list file names for the testing files
TestingNames <- vector()


tempN <- paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <- TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <- array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <- array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <- array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 21

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <- append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <- TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 22

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 23

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 24

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <- file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 25

#then list file names for the testing files
TestingNames <- vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again

#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 26

#then list file names for the testing files
TestingNames <-
vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 28

#then list file names for the testing files
TestingNames <-
vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

tempN <-
paste("pd.off.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 29

#then list file names for the testing files
TestingNames <-
vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 30

#then list file names for the testing files
TestingNames <-
vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <-
aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 31

#then list file names for the testing files
TestingNames <-
vector()


tempN <-
paste("hc.", ppt, ".", sep = "")
temp <-
file_list[grep(tempN,
 file_list, fixed = T)]
TestingNames <-
append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
Negate("%in%") ##create a NOT IN function
TrainingNames <-
file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
TestingNames %>% sample()
TrainingNames <-
TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
array(numeric(), c(1024, 32, 0))
for (i in 1:605){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet1 <-
abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet2 <-
abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet3 <-
abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet4 <-
abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
temp_data <-
fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TrainingSet5 <-
abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
abind::abind(
TrainingSet1,
TrainingSet2,
TrainingSet3,
TrainingSet4,
TrainingSet5,
along = 3
)
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
temp_data <-
fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
temp_data <-
select(temp_data, -time) #remove the time record as it is unnecessary
temp_data <-
temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
TestingSet <-
abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <- aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
TrainingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
TestingSetMod,
file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
TrainingNames,
file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 32

#then list file names for the testing files
TestingNames <-
        vector()


tempN <-
        paste("hc.", ppt, ".", sep = "")
temp <-
        file_list[grep(tempN,
                       file_list, fixed = T)]
TestingNames <-
        append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
        Negate("%in%") ##create a NOT IN function
TrainingNames <-
        file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
        TestingNames %>% sample()
TrainingNames <-
        TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
        array(numeric(), c(1024, 32, 0))
for (i in 1:605){
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet1 <-
                abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
        array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet2 <-
                abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
        array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet3 <-
                abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
        array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet4 <-
                abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
        array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet5 <-
                abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
        abind::abind(
                TrainingSet1,
                TrainingSet2,
                TrainingSet3,
                TrainingSet4,
                TrainingSet5,
                along = 3
        )
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
        array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
        temp_data <-
                fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TestingSet <-
                abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <- aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
        aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
        TrainingSetMod,
        file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
        TestingSetMod,
        file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
        TrainingNames,
        file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))

ppt <- 33

#then list file names for the testing files
TestingNames <-
        vector()


tempN <-
        paste("hc.", ppt, ".", sep = "")
temp <-
        file_list[grep(tempN,
                       file_list, fixed = T)]
TestingNames <-
        append(TestingNames, temp)

#create a not in function and then create the file names of the training set
"%ni%" <-
        Negate("%in%") ##create a NOT IN function
TrainingNames <-
        file_list[file_list %ni% TestingNames]

## shuffle the order of both Testing and Training
set.seed(32)
TestingNames <-
        TestingNames %>% sample()
TrainingNames <-
        TrainingNames %>% sample()

#create the tensor of training data
TrainingSet1 <-
        array(numeric(), c(1024, 32, 0))
for (i in 1:605){
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet1 <-
                abind::abind(TrainingSet1, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet2 <-
        array(numeric(), c(1024, 32, 0))
for (i in 606:1210){
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet2 <-
                abind::abind(TrainingSet2, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet3 <-
        array(numeric(), c(1024, 32, 0))
for (i in 1211:1815)
{
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet3 <-
                abind::abind(TrainingSet3, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet4 <-
        array(numeric(), c(1024, 32, 0))
for (i in 1816:2420){
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet4 <-
                abind::abind(TrainingSet4, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
TrainingSet5 <-
        array(numeric(), c(1024, 32, 0))
for (i in 2421:length(TrainingNames)){
        temp_data <-
                fread(TrainingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TrainingSet5 <-
                abind::abind(TrainingSet5, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##computationally way less expensive to do the above four sets (~9 minutes each loop on 2 cores)
TrainingSet <-
        abind::abind(
                TrainingSet1,
                TrainingSet2,
                TrainingSet3,
                TrainingSet4,
                TrainingSet5,
                along = 3
        )
#rm(TrainingSet1, TrainingSet2, TrainingSet3, TrainingSet4) #if you wanna clean up again


#create the tensor of testing data
TestingSet <-
        array(numeric(), c(1024, 32, 0))
for (i in 1:length(TestingNames)){
        temp_data <-
                fread(TestingNames[i], stringsAsFactors = F) #read in files using the fread function
        temp_data <-
                select(temp_data, -time) #remove the time record as it is unnecessary
        temp_data <-
                temp_data[1:1024,] #ensure all matrices are 1024 rows, as some end ones are one time point longer
        TestingSet <-
                abind::abind(TestingSet, temp_data, along = 3) #bind the new matrix to the tensor as a new slice
}
##for ease of working, save the arrays
TrainingSetMod <- aperm(TrainingSet, c(3, 1, 2)) #force into samples, time, features
dim(TrainingSetMod) #just checking it worked as intended
TestingSetMod <-
        aperm(TestingSet, c(3, 1, 2)) #force into samples, time, features
dim(TestingSetMod) #just checking it worked as intended

##finally save everything to load into memory at a later date
saveRDS(
        TrainingSetMod,
        file = paste("~/PG-400/data2/LOPO/", ppt, "Train.Rda", sep = ""))
saveRDS(
        TestingSetMod,
        file = paste("~/PG-400/data2/LOPO/", ppt, "Test.Rda", sep = ""))
saveRDS(
        TrainingNames,
        file = paste("~/PG-400/data2/LOPO/", ppt, "TrainLabels.Rda", sep = ""))
saveRDS(TestingNames, paste(file = "~/PG-400/data2/LOPO/", ppt, "TestLabels.Rda", sep = ""))