library(pROC)

library(plyr)

library(Boruta)

kFilePath <- "C:/Users/Ivan/Documents/GitHub/titanic-kaggle-competition/"

#kFilePath <- "C:/Users/user/Documents/GitHub/titanic-kaggle-competition/"

kTrainFile <- "train.csv"

kTestFile <- "test.csv"

kFeatureEngineeringFile <- "titanic_feature_engineering.R"

kOutputFile <- "predictions.csv"

source(paste(kFilePath,kFeatureEngineeringFile,sep=""))

kInnecesaryColumns <- c(1)

kColumnsToBeCastedAsFactors <- c(2,3)

df.trainData <- read.csv(file = paste(kFilePath,kTrainFile, sep = ""), header = T)

df.testData <- read.csv(file = paste(kFilePath,kTestFile, sep = ""), header = T)

vec.survived <- as.factor(df.trainData$Survived)

vec.testPassengerIds <- df.testData$PassengerId

int.survivedPosition <- which(names(df.trainData) == "Survived")

df.allData <- PerformFeatureEngineering(df.trainData[,-int.survivedPosition], df.testData, vec.survived)

#Split Clean Data Into Training and Testing

vec.trainRows <- 1:nrow(df.trainData)

df.trainData <- df.allData[vec.trainRows,]
  
df.testData <- df.allData[-vec.trainRows,]

#Train Models

rf.survivalModel <- randomForest(vec.survived ~ ., data = df.trainData, ntree = 750, importance = T)

#Measure Performance in Training

vec.predictions <- predict(rf.survivalModel, newdata = df.trainData)

vec.trues <- vec.survived==vec.predictions

vec.trues <- vec.trues[vec.trues==T]

num.accuracy <- length(vec.trues) / nrow(df.trainData)

print(paste("Accuracy",num.accuracy))
    
#Make Predictions for Testing

vec.predictions <- predict(rf.survivalModel, newdata = df.testData)

df.resultSet <- data.frame(PassengerId = vec.testPassengerIds,
                           Survived = vec.predictions)

write.csv(df.resultSet, file = paste(kFilePath, kOutputFile, sep = ""), row.names=FALSE)
