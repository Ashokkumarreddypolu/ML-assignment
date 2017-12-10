
z<-tempfile()
z1<-tempfile()

download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv',z)


download.file('https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv',z1)
training<-read.csv(z,stringsAsFactors = F)
testing<-read.csv(z1,stringsAsFactors = F)
z=apply(training, 2, function(col)mean(is.na(col)))
training1<-training[,z<0.8]
trainR <- grepl("^X|timestamp|window", names(training1))
training2 <- training1[, !trainR]
classe <- training1$classe
training_F <- training2[, sapply(training2, is.numeric)]
training_F<-cbind(training_F,classe)
library(caret)
traingF<-createDataPartition(training_F$classe,p=0.7,list=F)
traing1_F<-training_F[traingF,]
testing1_F<-training_F[-traingF,]
train_control<- trainControl(method="cv", number=5)
model<- train(classe~., data=traing1_F, trControl=train_control,method="rf",ntree=5)
confusionMatrix(model)
confusionMatrix(testing1_F$classe,predict(model,testing1_F))
testing_F=testing[,names(training_F)]












