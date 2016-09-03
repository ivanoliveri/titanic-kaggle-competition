library(pROC)

library(plyr)

kFilePath <- "C:/Users/Ivan/Documents/GitHub/titanic-kaggle-competition/"

#kFilePath <- "C:/Users/user/Documents/GitHub/titanic-kaggle-competition/"

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
  
  oneDataSet[is.na(oneDataSet[,"Fare"]), "Fare"] <- median(oneDataSet[,"Fare"], na.rm = TRUE)
  
  oneDataSet$Fare <- ifelse(oneDataSet$Fare==0,0,log(oneDataSet$Fare))
  
  #Creation of Ticket Category Vector
  
  vec.ticket <- as.character(oneDataSet$Ticket)
  
  vec.ticketCategory <- unlist(lapply(vec.ticket, function(x) ifelse(is.na(as.numeric(unlist(strsplit(x," "))[1]))==T,
                                                  unlist(strsplit(x," "))[1],"No City Defined")))
  
  vec.ticketCategory <-gsub("\\/","",toupper(gsub("\\.","",vec.ticketCategory)))
  
  vec.ticketCategory <- ifelse(vec.ticketCategory=="NO CITY DEFINED","NO CITY DEFINED",
                               substr(vec.ticketCategory,1,1))
  
  #Create Dummy Variables for PClass, Embarked, Cabin and Ticket Category
  
  mat.Pclass <- model.matrix( ~ Pclass - 1, data = oneDataSet)
  
  #Create Dummy Variables for Name
  
  vec.partialNames <- unlist(lapply(oneDataSet$Name, function(x) trimws(as.character(unlist(strsplit(as.character(x),split=","))[2]))))
  
  vec.titles <- gsub("\\.","",unlist(lapply(vec.partialNames, function(x) as.character(unlist(strsplit(as.character(x),split=" "))[1]))))
  
  vec.titles[vec.titles %in% c("Capt", "Col", "Dr", "Major", "Rev", "Sir")] <- "Noble"
  
  vec.titles[vec.titles %in% c("Dona", "Ms", "the","Lady")] <- "Mrs"
  
  vec.titles[vec.titles %in% c("Mme", "Mlle")] <- "Miss"
  
  vec.titles[vec.titles %in% c("Jonkheer","Don")] <- "Mr"
  
  #Create Dummy Variable for Cabin
  
  vec.cabin <- ifelse(!substring(oneDataSet$Cabin,1,1) %in% c("A","B","C","D","E","F"),"Other",substring(oneDataSet$Cabin,1,1))
  
  #Create Dummy Variable for Fare
  
  vec.fareIsZero <- as.factor(as.numeric(oneDataSet$Fare==0))
  
  #Include Only Q and S. Otherwise it would fail since the other levels aren"t included in the testset
  
  mat.Embarked <- model.matrix( ~ oneDataSet$Embarked - 1)[,c("oneDataSet$EmbarkedQ","oneDataSet$EmbarkedS")]
  
  mat.cabin <- model.matrix( ~ vec.cabin - 1)
  
  mat.ticketCategory <- model.matrix( ~ vec.ticketCategory - 1)
  
  mat.titles <- model.matrix( ~ vec.titles - 1)
  
  df.dummyDataset <- data.frame(mat.Pclass, mat.Embarked, mat.cabin, 
                                mat.ticketCategory, mat.titles, vec.fareIsZero)
  
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
    
    vec.motherIndicator <- factor(as.numeric((oneDataSet$Age>=18 & oneDataSet$vec.titlesMiss==1 & 
                                                oneDataSet$Sex=="female" & oneDataSet$Parch>0)))
    
    #Create Dummy Variable for Child Indicator
    
    vec.childIndicator <- factor(as.numeric(oneDataSet$Age<18))
    
    oneDataSet <- data.frame(oneDataSet,vec.motherIndicator,vec.childIndicator)
    
    rf.survivalModel <- randomForest(Survived ~ ., data = oneDataSet, ntree = 1000, importance = T)
    
    vec.predictions <- predict(rf.survivalModel, newdata = oneDataSet)
    
  }
  
  kColumnsNamesToKeep <- c("Sex","Age","SibSp","Parch","Fare")
  
  oneDataSet <- data.frame(oneDataSet[,kColumnsNamesToKeep],df.dummyDataset)
  
  vec.agePredictionsForNA <- round(predict(lm.ageModelWithStepwise,
                                           subset(oneDataSet,
                                                  subset = is.na(oneDataSet$Age)==T)))
  
  oneDataSet$Age <- replace(oneDataSet$Age,is.na(oneDataSet$Age),vec.agePredictionsForNA)
  
  #Create Dummy Variable for Mother Indicator
  
  vec.motherIndicator <- factor(as.numeric((oneDataSet$Age>=18 & oneDataSet$vec.titlesMiss==1 & 
                                              oneDataSet$Sex=="female" & oneDataSet$Parch>0)))
  
  #Create Dummy Variable for Child Indicator
  
  vec.childIndicator <- factor(as.numeric(oneDataSet$Age<18))
  
  oneDataSet <- data.frame(oneDataSet,vec.motherIndicator,vec.motherIndicator)
  
  vec.predictions <- predict(rf.survivalModel, newdata = oneDataSet)
  
  num.index <- num.index + 1
  
}

df.resultSet <- data.frame(PassengerId = df.testData$PassengerId,
                           Survived = vec.predictions)

write.csv(df.resultSet, file = paste(kFilePath, kOutputFile, sep = ""), row.names=FALSE)
