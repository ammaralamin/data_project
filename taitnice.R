setwd("C:/Users/pc/Desktop/project/taitnic pro")

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE,header =TRUE )
titanic.test  <- read.csv(file = "test.csv",stringsAsFactors = FALSE,header = TRUE)                          


titanic.test$Survived <- NA
##Compain to table
titanic.full <- rbind(titanic.train,titanic.test)
View(titanic.full)


#في قمتين من عندهن معنا هحولن الي s

titanic.full[titanic.full$Embarked=='',"Embarked"] <- 'S'

table(is.na(titanic.full$Age))
table(is.na(titanic.full$Cabin))

age.median <- median(titanic.full$Age,na.rm = TRUE)

titanic.full[is.na(titanic.full$Age),"Age"] <- age.median
#is.na(titanic.full$Age)ده عباره عن فلتر لقيمه
table(is.na(titanic.full$Age))
table(titanic.full$Age)
table(-age)
#table of fare have missing value this clean procces
fare.median <- median(titanic.full$Fare,na.rm = TRUE)
fare.median
titanic.full[is.na(titanic.full$Fare),"Fare"] <- fare.median

#categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex    <- as.factor(titanic.full$Sex)
titanic.full$Embarked<- as.factor(titanic.full$Embarked)


#spilt data set  back out into train and test 

titanic.train <- titanic.full[titanic.full$IsTrainSet==TRUE,]

titanic.test <- titanic.full[titanic.full$IsTrainSet==FALSE,]
#cast surv in train table
titanic.train$Survived <- as.factor(titanic.train$Survived)

#predication 
survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survied.equation)
install.packages("randomForest")
library(randomForest)

titnac.predection<- randomForest(formula= survived.formula, data =titanic.train, ntree = 500 ,mtry = 3, nodesize = 0.01 * nrow(titanic.test))


features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Prediction <-predict(titnac.predection, newdata = titanic.test)
Prediction
View(Prediction)
PassengerId <- titanic.test$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Prediction
tail(output.df)

write.csv(output.df,file = "kaggle_submission.csv", row.names =FALSE )
