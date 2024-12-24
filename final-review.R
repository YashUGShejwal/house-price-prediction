setwd("P:/VIT/SY SEM 4/DS/Course Project")
#Property.Price.Prediction.by.Group.2

f1<-read.csv("kc_house_data.csv")
f1<-subset(f1, select = -c(id,date))

#importing libraries
library(ggplot2)
library(dplyr)
library(rpart) 
library(tidyverse)
library(randomForest)
library(modelr)
library(e1071)
library(caret)
library(xgboost)

#attach(f1)
str(f1)
summary(f1)
mlr=lm(price~.,f1)
summary(mlr)

f1$yr_renovated[f1$yr_renovated == 0] = FALSE
f1$yr_renovated[f1$yr_renovated != 0] = TRUE
f1$yr_renovated = as.factor(f1$yr_renovated)

f1<-subset(f1, select = -c(sqft_basement,floors))
f1$price=f1$price/100000


hist(f1$price,main='Distribution of Price',xlab='Price in Lakhs',ylab='Frequency',col=blues9)
hist(f1$bedrooms,main='Distribution of BedRooms',xlab = 'No. of Bedrooms',ylab = 'Frequency',col=blues9)
hist(f1$condition,main='Distribution w.r.t. Condition',xlab='Condition',ylab='Frequency',col = blues9,)

plot(f1$sqft_living,f1$price,main = 'Price By Living area',ylab='Price in Lakhs',xlab='Living Area',col='blue', pch=20)
plot(f1$sqft_lot,f1$price,main='Price By Lot Size',ylab='Price in Lakh',xlab='Lot Size',col='blue',pch=20)
plot(f1$price~condition,data=f1,col='blue',main='Price By Condition',xlab='Condition (poor to excellent)',ylab='Price in Lakh',pch=20)

set.seed(123)

trainIndex = createDataPartition(f1$price, p = 0.7, list = FALSE, times = 1)
train <- f1[trainIndex, ]
test <- f1[-trainIndex, ]


# Simple Linear Regression

slr = lm(price~sqft_living,train)
summary(slr)
rsquareforslr<-summary(slr)$r.squared
p=ggplot(data=train,aes(x=price,y=sqft_living))+geom_point()+geom_smooth(method = "lm", formula = y~x, color="red2",se=F)
print(p)
result<-predict(slr)
print(result)
train1=cbind(train,result)

pred_result=predict(slr,test)
comp_result<-as.data.frame(cbind(actual = test$price, predicted = pred_result))
error1 = comp_result$actual - comp_result$predicted
comp_result<-cbind(comp_result,error1)
rmse1 = sqrt(mean(comp_result$error1^2))
print(rmse1)
predictbySLR<-comp_result$predicted

# Mulitiple Linear Regression:

mlr=lm(price~.,train)
summary(mlr)
rsquareformlr<-summary(mlr)$r.squared
result2=predict(mlr)
print(result2)
train2=cbind(train,result2)

pred_result2=predict(mlr,test)
comp_result2<-as.data.frame(cbind(actual = test$price, predicted = pred_result2))
error2 = comp_result2$actual - comp_result2$predicted
comp_result2<-cbind(comp_result2,error2)
rmse2 = sqrt(mean(comp_result2$error2^2))
print(rmse2)

predictbyMLR=comp_result2$predicted
# f1[sapply(f1,is.factor)]<-data.matrix(f1[sapply(df,is.factor)])
# f1
# str(f1)
#
# char_columns<-sapply(f1,is.character)
# f2<-f1
# f2[,char_columns]<-as.data.frame(apply(f2[,char_columns],4,as.numeric))
# sapply(f2,class)
#
# newWaterfront=as.numeric(f1$waterfront)
# cbind(f1,newWaterfront)

#DecisionTree

fit<- rpart(price~.,data=train)
plot(fit,uniform=TRUE)
text(fit,cex=0.7)

head(f1)
predict(fit,head(f1))
mae(model = fit, data = head(f1))

pred_result3=predict(fit,test)
comp_result3<-as.data.frame(cbind(actual = test$price, predicted = pred_result3))
error3 = comp_result3$actual - comp_result3$predicted
comp_result3<-cbind(comp_result3,error3)
rmse3 = sqrt(mean(comp_result3$error3^2))
rsq.rpart(fit)
print(rmse3)

predictbyDTree=comp_result3$predicted

splitData=resample_partition(f1,c(test=0.3,train=0.7))
sapply(splitData,dim)

fit2<- rpart(price~sqft_living+sqft_lot+bedrooms+bathrooms+condition+grade+waterfront+view,data=splitData$train)
mae(model = fit2, data = splitData$test)
plot(fit2,uniform = TRUE)
text(fit2,cex=0.7)
tail(f1)
predict(fit2,tail(f1))

get_mae<-function(maxdepth, target, predictors, training_data, testing_data)
{
  predictors<- paste(predictors, collapse="+")
  formula<- as.formula(paste(target,"~",predictors,sep=""))
  model<-rpart(formula,data=training_data,control = rpart.control(maxdepth = maxdepth))
  mae<-mae(model,testing_data)
  return(mae)
}

target<-"price"
predictors<- c("sqft_living","yr_built","zipcode","sqft_above","bedrooms","bathrooms","condition","grade","waterfront","view")

for(i in 1:10)
{
  mae<-get_mae(maxdepth = i, target = target, predictors = predictors, training_data = splitData$train, testing_data = splitData$test)
  print(glue::glue("Maxdepth: ",i,"\tMAE: ",mae))
}

#RandomForestAlgorithm
set.seed(1)
fit3<- randomForest(price~., data= train, na.action=na.exclude)
pred_result5=predict(fit3,test)
comp_result5<-as.data.frame(cbind(actual = test$price, predicted = pred_result5))
error5 = comp_result5$actual - comp_result5$predicted
comp_result5<-cbind(comp_result5,error5)
rmse5 = sqrt(mean(comp_result5$error5^2))
print(rmse5)

predictbyRF=comp_result5$predicted

predict(fit3,tail(f1))
tail(f1)


#SVR
set.seed(1)
svm_model = svm(price~.,train)
summary(svm_model)

result = predict(svm_model)
print(result)
train5 = cbind(train,result)

pred_result = predict(svm_model,test)
comp_results = as.data.frame(cbind(actual = test$price, predicted = pred_result))
error = comp_results$actual - comp_results$predicted
comp_result6<-cbind(comp_results,error)

rmse6 = sqrt(mean(comp_result6$error^2))  
print(rmse6)

predictbySVM<-comp_result6$predicted

#KNNRegression

set.seed(1)
model<-train(price~.,data=train,method='knn',preProcess=c("center","scale"))
model
pred_result9<-predict(model,test)
comp_result9<-as.data.frame(cbind(actual = test$price, predicted = pred_result9))
error9 = comp_result9$actual - comp_result9$predicted
comp_result9<-cbind(comp_result9,error9)
rmse9 = sqrt(mean(comp_result9$error9^2))
print(rmse9)

predictByKNN=comp_result9$predicted

#XGBoost

train7=data.matrix(train)
test7=data.matrix(test)

xgb<- xgboost(data=train7,label=train$price,max.depth=2,nrounds=1000)
pred_result7<-predict(xgb,test7)
comp_result7<-as.data.frame(cbind(actual = test$price, predicted = pred_result7))
error7 = comp_result7$actual - comp_result7$predicted
comp_result7<-cbind(comp_result7,error7)
rmse7 = sqrt(mean(comp_result7$error7^2))
print(rmse7)


xgb_1 = xgboost(data = as.matrix(train %>%
                                   select(-price)),
                label=train$price,
                nrounds = 100,                                                 # max number of trees to build
                verbose = TRUE,                                        
                print_every_n = 1,
                early_stop_round = 10                                          # stop if no improvement within 10 trees
)

xgb_cv_1 = xgb.cv(data = as.matrix(train %>%
                                     select(-price)),
                  label = train$price,
                  nrounds = 100,
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print_every_n = 1,
                  early_stop_round = 10
)

# plot the AUC for the training and testing samples
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) +
  geom_line() +
  theme_bw()


 predictByXGB=comp_result7$predicted

mae1<-mae(model=slr,data=test)
cat("\nThe Mean Absolute Error with SLR:",mae1)
cat("\nThe Root Mean Square Error with SLR:",rmse1)

mae2<-mae(model=mlr,data=test)
cat("\nThe Mean Absolute Error with MLR:",mae2)
cat("\nThe Root Mean Square Error with MLR:",rmse2)

mae3<-mae(model=fit,data=test)
cat("\nThe Mean Absolute Error with DTree:",mae3)
cat("\nThe Root Mean Square Error with DTree:",rmse3)
mae4<-mae(model=fit2,data=splitData$test)
cat("\nThe Mean Absolute Error with DT2:",mae4)

mae5<-mae(model=fit3,data=test)
cat("\nThe Mean Absolute Error with RF:",mae5)
cat("\nThe Root Mean Square Error with RF:",rmse5)

mae6<-mae(model=svm_model,data=test)
cat("\nThe Mean Absolute Error with SVM:",mae6)
cat("\nThe Root Mean Square Error with SVM:",rmse6)

mae9<-caret::MAE(test$price,predictByKNN)
cat("\nThe Mean Absolute Error with KNN:",mae9)
cat("\nThe Root Mean Square Error with KNN:",rmse9)

mae8<-caret::MAE(test$price,predictByXGB)
cat("\nThe Mean Absolute Error with XGB:",mae8)
cat("\nThe Root Mean Square Error with XGB:",rmse7)

cat("\nThus as of now XGBoost is more Accurate than other Algorithms.")


comparision<-data.frame(test$price,predictbySLR,predictbyMLR,predictbyDTree,predictbyRF,predictbySVM,predictByKNN,predictByXGB)
print(comparision)

actual <- comparision$test.price
predictedRF <- comparision$predictbyRF
rsquareforRF <- 1 - (sum((actual-predictedRF)^2)/sum((actual-mean(actual))^2))

predictedbyDTree <- comparision$predictbyDTree
rsquareforDTree <- 1 - (sum((actual-predictedbyDTree)^2)/sum((actual-mean(actual))^2))

predictedbySVM <- comparision$predictbySVM
rsquareforSVM <- 1 - (sum((actual-predictedbySVM)^2)/sum((actual-mean(actual))^2))

predictedbyKNN <- comparision$predictByKNN
rsquareforKNN <- 1 - (sum((actual-predictedbyKNN)^2)/sum((actual-mean(actual))^2))

predictedbyXGB <- comparision$predictByXGB
rsquareforXGB <- 1 - (sum((actual-predictedbyXGB)^2)/sum((actual-mean(actual))^2))

Algorithms<-c("SLR","MLR","DTree","RF","SVM","KNN","XGB")
RSquare<- c(rsquareforslr,rsquareformlr,rsquareforDTree,rsquareforRF,rsquareforSVM,rsquareforKNN,rsquareforXGB)
RMSE<- c(rmse1,rmse2,rmse3,rmse5,rmse6,rmse9,rmse7)
MAE<- c(mae1,mae2,mae3,mae5,mae6,mae9,mae8)

performance<- data.frame(Algorithms,RSquare,RMSE,MAE)

x=0:length(comparision)
plot(x,comparision,col="red",type="1",lwd=2,main="House Price Prediction by SLR")
lines(comparision,predictbySLR,col="blue",lwd=2)
legend("topright",legend = c("Actual","Predicted"),fill = c("red","blue"),col = 2:3, adj = c(0,0.6))
grid()

ggplot(data=test, aes(x=predict(xgb,test7), y= price)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with XGBoost')

ggplot(data=test, aes(x=predict(model,test), y= price)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with KNN Regression')

ggplot(data=test, aes(x=predict(fit3,test), y= price)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with Random Forest')

ggplot(data=test, aes(x=predict(svm_model,test), y= price)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with SVM')

ggplot(data=test, aes(x=predict(fit,test), y= price)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with Decision Tree')

ggplot(data=test, aes(x=predict(mlr,test), y= price)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with MLR')

ggplot(data=test, aes(x=predict(slr,test), y= price)) +geom_point(color="blue") +
  geom_abline(intercept=0, slope=1,color="red")+labs(x='Predicted Values', y='Actual Values', title='Predicted vs. Actual Values with SLR')


#plot(predict(slr,test), type= "o", col= "red", ylab= "", ylim= c(0, 60),
#     + main= "Comparison amongst actual n predicted")
#lines(predict(slr,test), type = "o", col = "blue")
#legend(1, 60, legend = c("Actual", "Predicted"),
#      + col = c("red", "blue", "green"), lty = 1:1, cex = 0.9)
compforPlot<-comparision[1:100,]
actual<-compforPlot$test.price
predictedbySLR<-comparision$predictbySLR
newTest<-cbind(test,predictedbySLR)
testing<-newTest[1:50,]
ggplot()+ geom_line(data=testing, mapping = aes(x=sqft_living,y=price), color="blue")+ geom_line(data=testing, mapping = aes(x=sqft_living,y=predictedbySLR), color="red")

