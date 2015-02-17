#First of all, you need to set working directory.
getwd()
setwd("GitHub/Titanic/")

#Import train and test data
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#Let's see basic stats
summary(train)
summary(test)

#Within data, it might be better to change data type from integer to factor.
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)

#In order to understand data, visualize it !
library(ggplot2)
library(reshape2)
ggplot(train)+geom_density(aes(x=Age, fill = Survived, col = Survived), alpha = 0.1)
ggplot(train)+geom_density(aes(x=Fare, fill = Survived, col = Survived), alpha = 0.1)
ggplot(train)+geom_density(aes(x=SibSp, fill = Survived, col = Survived), alpha = 0.1)
ggplot(train)+geom_density(aes(x=Parch, fill = Survived, col = Survived), alpha = 0.1)
table(train$Survived, train$Sex)
table(train$Survived, train$Pclass)


#Split training set and validation set
set.seed(1234)
idx<-sample(dim(train)[1], dim(train)[1] * 0.8)
train_train<-train[idx,]
train_valid<-train[-idx,]


#Logistic Regression
train_lr<-glm(Survived ~Age + Sex, data = train_train, family = "binomial")
summary(train_lr)
names(train_lr)

#Here is odds ratio
exp(train_lr$coefficients)


#Predict
y_pred<-predict(train_lr,newdata = train_valid)
y_pred[1:10]
y_pred<-predict(train_lr,newdata = train_valid,type="response")
y_pred[1:10]

acc_tab<-table(train_valid$Survived, ifelse(y_pred > 0.5,1,0))
acc_tab
(acc_tab[1] + acc_tab[4])/length(y_pred)


#This is a simple function (Let's see how to create funciton in R)
determine_cutoff<-function(cutoff, train_valid, y_pred){
  
  # res is the empty object
  res<-NULL

  #during for-loop, this function keeps accuracy scores with different cutoff
  for( i in 1:length(cutoff)){
    acc_tab<-table(train_valid$Survived, ifelse(y_pred > cutoff[i],1,0))
    acc_tab
    tmp<-(acc_tab[1] + acc_tab[4])/length(y_pred)
    res<-c(res,tmp)
  }
  return(res)
}

cutoffs<-seq(0,1,by = 0.01)
accuracy_scores<-determine_cutoff(cutoff = cutoffs, train_valid, y_pred)
plot(cutoffs, accuracy_scores, type="l")



#Random Forest
library(randomForest)

#Here is a secret function (original source code developed by Jeremy)
data_conversion<-function(samp){
  for (n in names(samp)){
    col<-samp[,n]
    na<-is.na(col)
    if (any(na)){
      if (is.numeric(col)){
        print(paste("*",n))
        samp[,paste(n,"NA",sep="_")]<-as.numeric(na)+1
        if(median(col,na.rm=T)<1){
          samp[na,n]<-1
        }else{
          samp[na,n]<-median(col,na.rm=T)          
        }
      }else{
        print(paste(".",n))
        samp[na,n] <-'*#NA,#'
      }
    }
    if (nlevels(col)>32){
      print(paste("32",n))
      samp[,n]<-unclass(col)
    }
    dt<-strptime(col,"%m/%d/%Y")
    if(!any(is.na(dt)))
    {
      print(paste("!",n))
      samp[,n] = as.double(dt)
      samp[,paste(n,"wday",sep="_")]=dt$wday
    }
  }
  return(samp)  
}

#Apply the secret function
train2<-data_conversion(train)

#Split data
train_train2<-train2[idx,]
train_valid2<-train2[-idx,]

#Random Forest!
train_rf<-randomForest(Survived~.-PassengerId, data = train_train2, ntree = 1000, importance = TRUE, nodesize = 20)
plot(train_rf)
train_rf

#Prediction
y_pred_rf<-predict(train_rf, newdata= train_valid2, type="prob")
acc_scores_rf<-determine_cutoff(cutoffs, train_valid = train_valid2, y_pred = y_pred_rf[,2])
plot(cutoffs, acc_scores_rf, type="l")

#Check the importance 
varImpPlot(train_rf)
importance(train_rf)

#partial Plot
partialPlot(train_rf, x.var = Sex, pred.data = train_train2)
partialPlot(train_rf, x.var = Fare, pred.data = train_train2)
partialPlot(train_rf, x.var = Pclass, pred.data = train_train2)

#Random Forest again! 
train_rf<-randomForest(Survived~ Sex + Pclass + Age, data = train_train2, ntree = 10000, importance = TRUE, nodesize = 40)
train_rf
varImpPlot(train_rf)





#Feature engneering
train[1,]
train$Name
tmp<-strsplit(as.character(train$Name),"[,]")
res<-NULL
for(i in 1:length(tmp)){
  tmp2<-strsplit(tmp[[i]][2], "[.]", perl =TRUE)[[1]][1]
  res<-c(res,tmp2)
}

train$title<-as.factor(res)
