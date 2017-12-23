retail_train=read.csv("D:/Retail Dataset/retail_trainingset.csv",na.strings = c(""," ","NA"))
retail_test=read.csv("D:/Retail Dataset/retail_testset.csv")
colSums(is.na(retail_train))
colSums(is.na(retail_test))
dim(retail_train)
dim(retail_test)
str(retail_train)
str(retail_test)
Sys.Date()


table(retail_train$Total_visits)
table(retail_test$Total_visits)

levels(retail_train$Gender)
levels(retail_train$Region)
levels(retail_train$Channel)
retail_train$Churn=factor(retail_train$Churn,levels=c("No","Yes"),labels=c(0,1))
retail_train$Gender=factor(retail_train$Gender,levels=c("Female","Male"),labels=c(0,1))
retail_train$Region=factor(retail_train$Region,levels=c("A","B"),labels=c(0,1))
retail_train$Channel=factor(retail_train$Channel,levels=c("Both","Offline","Online"),
                            labels=c(0,1,2))

#
retail_test$Gender=factor(retail_test$Gender,levels=c("Female","Male"),labels=c(0,1))
retail_test$Region=factor(retail_test$Region,levels=c("A","B"),labels=c(0,1))
retail_test$Channel=factor(retail_test$Channel,levels=c("Both","Offline","Online"),
                            labels=c(0,1,2))

table(retail_train$Churn)
prop.table(table(retail_train$Churn))


library("DMwR")
retail_train<-SMOTE(Churn~., data=retail_train, perc.over = 200, k = 5, perc.under = 200,
      learner = NULL)
table(retail_train$Churn)

library("lubridate")
retail_train$DOB=ymd(retail_train$DOB)
retail_train$Card_registration_date=ymd(retail_train$Card_registration_date)
retail_train$Last_visit=ymd(retail_train$Last_visit)
retail_train$No_of_days=retail_train$Last_visit-retail_train$Card_registration_date
retail_train$Avg_visits=retail_train$No_of_days/retail_train$Total_visits
retail_train$Days_since_last_visit=Sys.Date()-retail_train$Last_visit


retail_test$DOB=ymd(retail_test$DOB)
retail_test$Card_registration_date=ymd(retail_test$Card_registration_date)
retail_test$Last_visit=ymd(retail_test$Last_visit)

#

retail_test$No_of_days=retail_test$Last_visit-retail_test$Card_registration_date
retail_test$Avg_visits=retail_test$No_of_days/retail_test$Total_visits
retail_test$Days_since_last_visit=Sys.Date()-retail_test$Last_visit


retail_test$DOB=ymd(retail_test$DOB)
retail_test$Card_registration_date=ymd(retail_test$Card_registration_date)
retail_test$Last_visit=ymd(retail_test$Last_visit)

Churn=retail_train[,10]
retail_train=retail_train[,-10]
retail_train=data.frame(retail_train,Churn)
retail_train$No_of_days=as.numeric(retail_train$No_of_days)
retail_train$Avg_visits=as.numeric(floor(retail_train$Avg_visits))
retail_train$Days_since_last_visit=as.numeric(retail_train$Days_since_last_visit)
str(retail_train)

#
retail_test$No_of_days=as.numeric(retail_test$No_of_days)
retail_test$Avg_visits=as.numeric(floor(retail_test$Avg_visits))
retail_test$Days_since_last_visit=as.numeric(retail_test$Days_since_last_visit)


ID=retail_test[,1]
retail_test=retail_test[,-1]

split<-createDataPartition(y =retail_train$Churn, p = 0.7, list = FALSE)

retail_sample_train<-retail_train[split,]

retail_sample_test<-retail_train[-split,]

#library("caTools")
#retail_sample<-sample.split(retail_train$Churn,SplitRatio = 0.8)
#retail_sample_train<-subset(retail_train,retail_sample==T)
#retail_sample_test<-subset(retail_train,retail_sample==F)
set.seed(123)
library("caret")
cntrl<-trainControl(method="cv",number=5)
model_rpart<-train(x=retail_sample_train[,-13],y=retail_sample_train[,13],method = "rpart",trControl = cntrl)
model_rf<-train(x=retail_sample_train[,-13],y=retail_sample_train[,13],method = "rf",trControl = cntrl)
varImp(model_rf)


model_c5.0<-train(x=retail_sample_train[,-13],y=retail_sample_train[,13],method = "C5.0",trControl = cntrl)

pred_rf<-predict(model_rf,newdata=retail_sample_test1,type="prob")
confusionMatrix(pred_rf,retail_sample_test$Churn)

pred_rpart<-predict(model_rpart,newdata=retail_sample_test,type="raw")
confusionMatrix(pred_rf,retail_sample_test$Churn)

library("pROC")
pred_rf=as.numeric(as.character(pred_rf))
auc <- roc(retail_sample_test$Churn, pred_rf)
print(auc)


library("dummies")
retail_train1=dummy.data.frame(retail_train,names=c("Gender","Region","Channel"),sep="_")
names(retail_train1)
retail_train1=retail_train1[-c(8,9,10)]

retail_test1=dummy.data.frame(retail_test,names=c("Gender","Region","Channel"),sep="_")
names(retail_test1)
retail_test1=retail_test1[-c(8,9,10)]

str(retail_train1)

split1<-createDataPartition(y =retail_train1$Churn, p = 0.7, list = FALSE)

retail_sample_train1<-retail_train1[split1,]

retail_sample_test1<-retail_train1[-split1,]

model_rf<-train(x=retail_sample_train1[,-14],y=retail_sample_train1[,14],method = "rf",trControl = cntrl)
pred_rf<-predict(model_rf,newdata=retail_sample_test1,type="prob")
confusionMatrix(pred_rf,retail_sample_test$Churn)

grid=expand.grid(C=c(0.1,0.5,1),sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                          0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9))
grid=expand.grid(C=1,sigma=1)
model_svm<-train(x=retail_sample_train1[,-14],y=retail_sample_train1[,14],method="svmRadial",trControl=ctrl
                 ,preProcess = c("center", "scale"),tuneGrid=grid)
pred_svm=predict(model_svm,newdata = retail_sample_test1)
confusionMatrix(retail_sample_test1[,14],pred_svm)

model_gbm<- train(x=retail_sample_train1[,-14],y=retail_sample_train1[,14],method='gbm',
                  trControl=cntrl,tuneLength=3)
pred_gbm<-predict(model_gbm,newdata=retail_sample_test1,type="prob")
confusionMatrix(pred_gbm,retail_sample_test1[,14])
length(pred_gbm)
length(retail_sample_train1[,14])

k=train(x=retail_sample_train1[,-14],y=retail_sample_train1[,14],method='knn',trControl=cntrl,tuneGrid=expand.grid(k=1:31))
pred_k=predict(k,newdata = retail_sample_test1,type="prob")
confusionMatrix(pred_k,retail_sample_test1[,14])


library(h2o)
localH2O <- h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(retail_sample_train1)
test.h2o <- as.h2o(retail_sample_test1)
colnames(train.h2o)
y.dep <- 14
x.indep <- 1:13
model_ann <- h2o.deeplearning(x = x.indep, 
                   y = y.dep,
                   training_frame = train.h2o,
                   activation = "Rectifier",
                   hidden = c(5,5),
                   epochs = 100)
h2o.performance(model_ann)
predict.reg <- as.data.frame(h2o.predict(model_ann, test.h2o))
predict.reg$predict
h2o.confusionMatrix(predict.reg[,1],test.h2o["Churn"])
confusionMatrix(predict.reg$predict,retail_sample_test1$Churn)
h2o.exit()


model_lr<-train(retail_sample_train1[,-14],retail_sample_train1[,14],method='glm',trControl=ctrl,tuneLength=3)
pred_lr<-predict(model_lr,newdata=retail_sample_test1)
confusionMatrix(pred_lr,retail_sample_test1[,14])
nrow(retail_sample_train1)
nrow(retail_sample_test1)
length(pred_xgb)
pred_rpart*0.5
pred_majority<-as.factor(ifelse(pred_rf==1 & pred_xgb==1,1,ifelse(pred_rf==1 & pred_gbm==1,1,ifelse(pred_xgb==1 & pred_gbm==1,1,0))))
confusionMatrix(pred_majority,retail_sample_test1)
class(pred_rpart)
pred_weighted_avg<-(as.numeric(as.character(pred_rf))*0.5)+(as.numeric(as.character(pred_xgb))*0.25)+(as.numeric(as.character(pred_gbm))*0.25)
pred_weighted_avg<-(pred_rf*0.5)+(pred_gbm*0.25)+(pred_xgb*0.25)
pred_weighted_avg<-as.factor(ifelse(testSet$pred_weighted_avg>0.5,'Y','N'))
avg=pred_weighted_avg

library("xgboost")
xgbGrid <- expand.grid(nrounds = 300,
                       max_depth = 1,
                       eta = 0.3,
                       gamma = 0.01,
                       colsample_bytree = .7,
                       min_child_weight = 1,
                       subsample = 0.9)

set.seed(12)
fit_xgb <- train(Churn ~.,retail_sample_train1,method = 'xgbTree',tuneGrid = xgbGrid,trControl = trainControl(method = 'repeatedcv',number = 5))
pred_xgb<-predict(fit_xgb,newdata=retail_sample_test1,type="prob")


pred_rf<-predict(model_rf,newdata=retail_test,type="prob")
pred_rf[,2]

confusionMatrix(pred_rf,retail_test$Churn)
class(pred_rf[,2])
pred_rf=as.numeric(as.character(pred_rf))
names(retail_train)
names(retail_test)

pred_rf<-predict(model_rf,newdata=retail_test1,type="prob")
pred_xgb<-predict(fit_xgb,newdata=retail_test1,type="prob")
pred_gbm<-predict(model_gbm,newdata=retail_test1,type="prob")
pred_weighted_avg<-(as.numeric(as.character(pred_rf))*0.5)+(as.numeric(as.character(pred_xgb))*0.25)+(as.numeric(as.character(pred_gbm))*0.25)
pred_weighted_avg<-(pred_rf*0.5)+(pred_gbm*0.25)+(pred_xgb*0.25)
submission=read.csv("D:/Retail Dataset/submission.csv")
ID
bs<-data.frame(ID=ID,Churn=pred_weighted_avg[,2])
View(bs)
write.csv(bs,file = "retail_output1.csv")

