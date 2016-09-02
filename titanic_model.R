library(pROC)

library(plyr)

library(randomForest)

#kFilePath <- "C:/Users/Ivan/Documents/Kaggle/Titanic/"

kFilePath <- "C:/Users/user/Documents/GitHub/titanic-kaggle-competition/"

kTrainFile <- "train.csv"

kTestFile <- "test.csv"

kOutputFile <- "predictions.csv"

kInnecesaryColumns <- c(1)

kColumnsToBeCastedAsFactors <- c(2,3)

df.trainData <- read.csv(file = paste(kFilePath,kTrainFile, sep = ""), header = T)

df.testData <- read.csv(file = paste(kFilePath,kTestFile, sep = ""), header = T)

df.trainData[,kColumnsToBeCastedAsFactors] <- lapply(df.trainData[,kColumnsToBeCastedAsFactors],
                                                     function(x) as.factor(x))

df.trainData <- df.trainData[, -kInnecesaryColumns]

#NA analysis

num.totalRows <- nrow(df.trainData)

vec.naByColumn <- unlist(lapply(df.trainData, function(x) length(is.na(x)[is.na(x)==T])))

barplot(vec.naByColumn, main = "NA Values by Column")

#Sex Analysis

vec.Sex <- unlist(summary(df.trainData$Sex))

pie(vec.Sex, main = "Sex Distribution")

df.testData[,kColumnsToBeCastedAsFactors] <- lapply(df.testData[,kColumnsToBeCastedAsFactors],
                                                    function(x) as.factor(x))
num.index <- 1

num.cutoff <- NULL

lst.datasets <- list(df.trainData, df.testData)

for(oneDataSet in lst.datasets){
  
  #Replace NA Values with Column Mean for Fare
  
  oneDataSet[is.na(oneDataSet[,"Fare"]), "Fare"] <- mean(oneDataSet[,"Fare"], na.rm = TRUE)
  
  #Creation of Ticket Category Vector
  
  vec.ticket <- as.character(oneDataSet$Ticket)
  
  vec.ticketCategory <- as.factor(unlist(lapply(vec.ticket,
                                                function(x) ifelse(!is.na(as.numeric(substr(x,1,1))),
                                                                   ifelse(as.numeric(substr(x,1,1))>3 || as.numeric(substr(x,1,1)) ==0,
                                                                          "Other Number",substr(x,1,1)),substr(x,1,1)))))
  
  #Create Dummy Variables for PClass, Embarked, Cabin and Ticket Category
  
  mat.Pclass <- model.matrix( ~ Pclass - 1, data = oneDataSet)
  
  #Create Dummy Variables for Name
  
  vec.partialNames <- unlist(lapply(oneDataSet$Name, function(x) trimws(as.character(unlist(strsplit(as.character(x),split=","))[2]))))
  
  vec.titles <- unlist(lapply(vec.partialNames, function(x) as.character(unlist(strsplit(as.character(x),split=" "))[1])))
  
  vec.titlesFiltered <- ifelse(!vec.titles %in% c("Master.","Miss.","Mr.","Mrs."),"Other",vec.titles)
  
  #Create Dummy Variable for Cabin
  
  vec.cabin <- ifelse(!substring(oneDataSet$Cabin,1,1) %in% c("A","B","C","D","E","F"),"Other",substring(oneDataSet$Cabin,1,1))
  
  #Include Only Q and S. Otherwise it would fail since the other levels aren't included in the testset
  
  mat.Embarked <- model.matrix( ~ oneDataSet$Embarked - 1)[,c("oneDataSet$EmbarkedQ","oneDataSet$EmbarkedS")]
  
  mat.cabin <- model.matrix( ~ vec.cabin - 1)
  
  mat.ticketCategory <- model.matrix( ~ vec.ticketCategory - 1)
  
  mat.titles <- model.matrix( ~ vec.titlesFiltered - 1)
  
  df.dummyDataset <- data.frame(mat.Pclass, mat.Embarked, mat.cabin, mat.ticketCategory, mat.titles)
  
  df.dummyDataset[,1:length(df.dummyDataset)] <- lapply(df.dummyDataset[,1:length(df.dummyDataset)],
                                                        function(x) as.factor(x))
  
  vec.columnsWithTwoOrMoreLevels <- unlist(lapply(df.dummyDataset,function(x) length(levels(x))!=1)) 
  
  df.dummyDataset <- df.dummyDataset[,vec.columnsWithTwoOrMoreLevels]
  
  #Add Dummy Variables to a Consolidated Dataset
  
  if(num.index == 1){
    
    kColumnsNamesToKeep <- c("Sex","Age","SibSp","Parch","Fare","Survived")
    
    oneDataSet <- data.frame(oneDataSet[,kColumnsNamesToKeep],df.dummyDataset)
    
    kColumnToRemoveForAgeModel <- which(names(oneDataSet)=="Survived")
    
    df.ageModel <- subset(oneDataSet, subset = is.na(oneDataSet$Age)!=T)[,-kColumnToRemoveForAgeModel]
    
    lm.ageModel <- lm(Age ~ . ,df.ageModel)
    
    lm.ageModelWithStepwise <- step(lm.ageModel)
    
    vec.agePredictionsForNA <- round(predict(lm.ageModelWithStepwise,
                                             subset(oneDataSet,
                                                    subset = is.na(oneDataSet$Age)==T)))
    
    oneDataSet$Age <- replace(oneDataSet$Age,is.na(oneDataSet$Age),vec.agePredictionsForNA)
    
    #Create Dummy Variable for Mother Indicator
    
    vec.motherIndicator <- factor(as.numeric((oneDataSet$Age>=18 & oneDataSet$vec.titlesFilteredMiss.==1 & 
                                                oneDataSet$Sex=="female" & oneDataSet$Parch>0)))
    
    #Create Dummy Variable for Child Indicator
    
    vec.childIndicator <- factor(as.numeric(oneDataSet$Age<18))
    
    oneDataSet <- data.frame(oneDataSet,vec.motherIndicator,vec.childIndicator)
    
    glm.survivalModel <- glm(Survived ~ ., data = oneDataSet, family = binomial)
    
    glm.survivalModelWithStepwise <- step(glm.survivalModel)    
    
    vec.predictions <- predict.glm(glm.survivalModelWithStepwise, newdata = oneDataSet)
    
    roc.curve <- roc(oneDataSet$Survived, vec.predictions)
    
    num.cutoff <- coords(roc.curve, x="best", input="threshold", best.method="youden")[[1]]
    
    
  }
  
  kColumnsNamesToKeep <- c("Sex","Age","SibSp","Parch","Fare")
  
  oneDataSet <- data.frame(oneDataSet[,kColumnsNamesToKeep],df.dummyDataset)
  
  vec.agePredictionsForNA <- round(predict(lm.ageModelWithStepwise,
                                           subset(oneDataSet,
                                                  subset = is.na(oneDataSet$Age)==T)))
  
  oneDataSet$Age <- replace(oneDataSet$Age,is.na(oneDataSet$Age),vec.agePredictionsForNA)
  
  #Create Dummy Variable for Mother Indicator
  
  vec.motherIndicator <- factor(as.numeric((oneDataSet$Age>=18 & oneDataSet$vec.titlesFilteredMiss.==1 & 
                                              oneDataSet$Sex=="female" & oneDataSet$Parch>0)))
  
  #Create Dummy Variable for Child Indicator
  
  vec.childIndicator <- factor(as.numeric(oneDataSet$Age<18))
  
  oneDataSet <- data.frame(oneDataSet,vec.motherIndicator,vec.motherIndicator)
  
  vec.predictions <- predict.glm(glm.survivalModelWithStepwise, newdata = oneDataSet)
  
  vec.predictions <- ifelse(vec.predictions>=num.cutoff,1,0)
  
  num.index <- num.index + 1
  
}

df.resultSet <- data.frame(PassengerId = df.testData$PassengerId,
                           Survived = vec.predictions)

write.csv(df.resultSet, file = paste(kFilePath, kOutputFile, sep = ""), row.names=FALSE)
