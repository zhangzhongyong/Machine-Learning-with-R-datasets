
## identify risky bank loans using c5.0 decisions trees

data_credit<-read.csv(file="credit.csv")
data_credit$default<-factor(data_credit$default,levels=c('1',"2"),labels=c("No","Yes"))
str(data_credit)

## set random selection of train data
set.seed(12345)
rand_index<-order(runif(1000))
data_credit<-data_credit[rand_index,]
credit_train<-data_credit[1:900,]
credit_test<-data_credit[901:1000,]

#### building a decisions trees model
library("C50")
trees_model<-C5.0(credit_train[-21],y=credit_train$default)
#summary(trees_model)

## try to improve its performacne by changing values of trials
trees_model10<-C5.0(credit_train[-21],y=credit_train$default,trials=10)
#summary(trees_model10)

## show the prediction error of initial model
test_predict<-predict(trees_model,credit_test[-21])
library("gmodels")
CrossTable(test_predict,credit_test$default,prop.r=F,prop.c=F,expected=F,prop.chisq=F)

## show the prediction error of improved model 
test_predict10<-predict(trees_model10,credit_test[-21])
CrossTable(test_predict10,credit_test$default,prop.r=F,prop.c=F,expected=F,prop.chisq=F)

## building a model  adding cost penalty
cost_matrix<-matrix(c(0,1,4,0),nrow=2)
trees_costs<-C5.0(credit_train[-21],y=credit_train$default,costs=cost_matrix)

## show the results of model 
test_predict_cost<-predict(trees_costs,credit_test[-21])
CrossTable(test_predict_cost,credit_test$default,prop.r=F,prop.c=F,expected=F,prop.chisq=F)

