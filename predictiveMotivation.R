library(corrplot)
library(caret)
library(randomForest)

if (!file.exists("Training.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "Training.csv")
}

if (!file.exists("Testing.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "Testing.csv")
}

Training <- read.csv("Training.csv", header = TRUE, na.strings = c("NA",""))
Testing <- read.csv("Testing.csv", header = TRUE, na.strings = c("NA",""))

dim(Training)
dim(Testing)

Training_clean <- Training[,(colSums(is.na(Training)) == 0)]
Testing_clean <- Testing[,(colSums(is.na(Testing)) == 0)]

deleteCol <- c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window")
Trainig_clean <- Training_clean[,!(names(Training_clean) %in% deleteCol)]
Testing_clean <- Testing_clean[,!(names(Testing_clean) %in% deleteCol)]

inTrain = createDataPartition(y = Trainig_clean$classe, p = 0.7, list = FALSE)
Training_partition <- Trainig_clean[inTrain,]
Training_Test_partition <- Trainig_clean[-inTrain,]


set.seed(123123)
modelFit <- train(classe ~ ., method="rf", data=Training_partition,
                  ntree=500, tuneGrid=data.frame(.mtry = 3))
modelFit$finalModel

prediction <- predict(modelFit, Testing)
Testing$classe <- prediction
prediction

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#pml_write_files(as.character(prediction))