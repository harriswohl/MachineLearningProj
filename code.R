download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", "train.csv")
download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", "test.csv")
train <- read.csv("./train.csv")
val <- read.csv("./test.csv")

#getting rid of columns with NAs
nas <- sapply(train, function(x) sum(is.na(x)))
train <- train[, nas<1900]

remove <- nearZeroVar(train)
train <- train[, -remove]
train <- train[, -1]
train <- subset(train, select = -c(user_name, cvtd_timestamp))


library(caret)
library(randomForest)

#create partition in our training set
set.seed(8675309)
inTrain <- createDataPartition(train$classe, p = .7, list = FALSE)
training <- train[inTrain,]
testing <- train[-inTrain,]

rf <- randomForest(classe ~ ., data = training)
confusionMatrix(predict(rf, testing), testing$classe)

predict(rf, val)