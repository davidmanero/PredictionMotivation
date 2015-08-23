library(corrplot)
library(caret)

if (!file.exists("pmlTraining.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "pmlTraining.csv")
}

if (!file.exists("pmlTesting.csv")) {
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "pmlTesting.csv")
}

pmlTraining <- read.csv("pmlTraining.csv", header = TRUE, na.strings = c("NA",""))
pmlTesting <- read.csv("pmlTesting.csv", header = TRUE, na.strings = c("NA",""))

dim(pmlTraining)

pmlTraining_filter_col <- pmlTraining[,(colSums(is.na(pmlTraining)) == 0)]
pmlTesting_filter_col <- pmlTesting[,(colSums(is.na(pmlTesting)) == 0)]

removeCol <- c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window")
pmlTrainig_filter_col <- pmlTraining_filter_col[,!(names(pmlTraining_filter_col) %in% removeCol)]
pmlTesting_filter_col <- pmlTesting_filter_col[,!(names(pmlTesting_filter_col) %in% removeCol)]

inTrain = createDataPartition(y = pmlTrainig_filter_col$classe, p = 0.7, list = FALSE)
pmlTraining_sub_data <- pmlTrainig_filter_col[inTrain,]
pmlValid_sub_data <- pmlTrainig_filter_col[-inTrain,]

corMatrix<- cor(pmlTraining_sub_data[, -54])
corrplot(corMatrix, order = "FPC", method = "color", type = "lower", tl.cex = 0.8, tl.col = rgb(0, 0, 0))

preProc <- preProcess(pmlTraining_sub_data[, -54], method = "pca", thresh = 0.99)
trainPC <- predict(preProc, pmlTraining_sub_data[, -54])
valid_testPC <- predict(preProc, pmlValid_sub_data[, -54])

modFit <- train(pmlTraining_sub_data$classe ~ ., method = "rf", data = trainPC, trControl = trainControl(method = "cv", number = 4), importance = TRUE)

varImpPlot(modFit$finalModel, sort = TRUE, type = 1, pch = 19, col = 1, cex = 1, main = "Importance of the Individual Principal Components")

predValidRF <- predict(modFit, valid_testPC)
confus <- confusionMatrix(pmlValid_sub_data$classe, predValidRF)
confus$table

accur <- postResample(pmlValid_sub_data$classe, predValidRF)
modAccuracy <- accur[[1]]
modAccuracy

out_of_sample_error <- 1 - modAccuracy
out_of_sample_error

testPC <- predict(preProc, pmlTesting_filter_col[, -54])
pred_final <- predict(modFit, testPC)
pred_final

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#pml_write_files(as.character(pred_final))