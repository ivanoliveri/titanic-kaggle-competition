CategorizeAge <- function (vec.ages){
  
  ifelse((vec.ages>=0 & vec.ages<=15) ,"0-15",
         (ifelse((vec.ages>15 & vec.ages<=30), "16-30",
                 (ifelse((vec.ages>30 & vec.ages<=50),"31-50","+51")
                 )
         )
         )
  )
  
}

PerformFeatureEngineering <- function(df.trainData, df.testData, vec.salePrice){
  
  df.allData <- rbind(df.trainData, df.testData)
  
  #Replace NA Values with Column Mean for Fare
  
  df.allData[is.na(df.allData[,"Fare"]), "Fare"] <- median(df.allData[,"Fare"], na.rm = TRUE)
  
  df.allData$Fare <- ifelse(df.allData$Fare==0,0,log(df.allData$Fare))
  
  #Creation of Ticket Category Vector
  
  vec.ticket <- as.character(df.allData$Ticket)
  
  vec.ticketCategory <- unlist(lapply(vec.ticket, function(x) ifelse(is.na(as.numeric(unlist(strsplit(x," "))[1]))==T,
                                                                     unlist(strsplit(x," "))[1],"No City Defined")))
  
  vec.ticketCategory <-gsub("\\/","",toupper(gsub("\\.","",vec.ticketCategory)))
  
  vec.ticketCategory <- ifelse(vec.ticketCategory=="NO CITY DEFINED","NO CITY DEFINED",
                               substr(vec.ticketCategory,1,1))
  
  #Create Dummy Variables for PClass, Embarked, Cabin and Ticket Category
  
  mat.Pclass <- model.matrix( ~ Pclass - 1, data = df.allData)
  
  #Create Dummy Variables for Name
  
  vec.partialNames <- unlist(lapply(df.allData$Name, function(x) trimws(as.character(unlist(strsplit(as.character(x),split=","))[2]))))
  
  vec.titles <- gsub("\\.","",unlist(lapply(vec.partialNames, function(x) as.character(unlist(strsplit(as.character(x),split=" "))[1]))))
  
  vec.titles[vec.titles %in% c("Capt", "Col", "Dr", "Major", "Rev", "Sir")] <- "Noble"
  
  vec.titles[vec.titles %in% c("Dona", "Ms", "the","Lady")] <- "Mrs"
  
  vec.titles[vec.titles %in% c("Mme", "Mlle")] <- "Miss"
  
  vec.titles[vec.titles %in% c("Jonkheer","Don")] <- "Mr"
  
  #Create Dummy Variable for Cabin
  
  vec.cabin <- ifelse(!substring(df.allData$Cabin,1,1) %in% c("A","B","C","D","E","F"),"Other",substring(df.allData$Cabin,1,1))
  
  #Create Dummy Variable for Fare
  
  vec.fareIsZero <- as.factor(as.numeric(df.allData$Fare==0))
  
  #Include Only Q and S. Otherwise it would fail since the other levels aren"t included in the testset
  
  mat.Embarked <- model.matrix( ~ df.allData$Embarked - 1)[,c("df.allData$EmbarkedQ","df.allData$EmbarkedS")]
  
  mat.cabin <- model.matrix( ~ vec.cabin - 1)
  
  mat.ticketCategory <- model.matrix( ~ vec.ticketCategory - 1)
  
  mat.titles <- model.matrix( ~ vec.titles - 1)
  
  df.dummyDataset <- data.frame(mat.Pclass, mat.Embarked, mat.cabin, 
                                mat.ticketCategory, mat.titles, vec.fareIsZero)
  
  df.dummyDataset[,1:length(df.dummyDataset)] <- lapply(df.dummyDataset[,1:length(df.dummyDataset)],
                                                        function(x) as.factor(x))
  
  vec.columnsWithTwoOrMoreLevels <- unlist(lapply(df.dummyDataset,function(x) length(levels(x))!=1)) 
  
  df.dummyDataset <- df.dummyDataset[,vec.columnsWithTwoOrMoreLevels]
  
  kColumnsNamesToKeep <- c("Sex","Age","SibSp","Parch","Fare")
  
  df.allData <- data.frame(df.allData[,kColumnsNamesToKeep],df.dummyDataset)
  
  df.ageModel <- subset(df.allData, subset = is.na(df.allData$Age)!=T)
  
  rf.ageModel <- randomForest(Age ~ . ,df.ageModel, ntree=1000)
  
  vec.agePredictionsForNA <- round(predict(rf.ageModel,
                                           subset(df.allData,
                                                  subset = is.na(df.allData$Age)==T)))
  
  df.allData$Age <- replace(df.allData$Age,is.na(df.allData$Age),vec.agePredictionsForNA)
  
  #Create Dummy Variable for Mother Indicator
  
  vec.motherIndicator <- factor(as.numeric((df.allData$Age>=18 & df.allData$vec.titlesMiss==1 & 
                                              df.allData$Sex=="female" & df.allData$Parch>0)))
  
  #Create Dummy Variable for Child Indicator
  
  vec.childIndicator <- factor(as.numeric(df.allData$Age<18))
  
  #Create Dummy Variable for Age
  
  vec.ageCategory <- as.factor(CategorizeAge(df.allData$Age))
  
  mat.ages <- model.matrix( ~ vec.ageCategory -1)
  
  df.allData <- data.frame(df.allData,vec.motherIndicator,vec.childIndicator,mat.ages)
  
  df.trainData <- df.allData[1:nrow(df.trainData),]
  
  bor.results <- Boruta(df.trainData, vec.survived, maxRuns=100, doTrace=0)
  
  vec.nonRejectedColumns <- 
    names(bor.results$finalDecision[bor.results$finalDecision == "Confirmed"])
  
  vec.nonRejectedColumnsPositions <-
    unlist(lapply(vec.nonRejectedColumns,
                  function(x) which(x==names(df.allData))))
  
  df.allData <- df.allData[,vec.nonRejectedColumnsPositions]
  
  return(df.allData)
  
}