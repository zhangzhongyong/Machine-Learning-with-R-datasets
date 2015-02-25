
## chapter4 Probablistic learning

raw_data<- read.csv("sms_spam.csv",stringsAsFactor=F)
raw_data$type<- factor(raw_data$type)

library("tm")
data_corpus<-Corpus(VectorSource(raw_data$text))

inspect(data_corpus)
library("tm")
## process the raw data
data_c_clean<-tm_map(data_corpus,content_transformer(tolower))
data_c_clean<-tm_map(data_c_clean,removeNumbers)
data_c_clean<-tm_map(data_c_clean,removePunctuation)
data_c_clean<-tm_map(data_c_clean,removeWords,stopwords())
data_c_clean<-tm_map(data_c_clean,stripWhitespace)
## creat a document term matrix
data_dtm<-DocumentTermMatrix(data_c_clean)

## prepare the train data as well as the test data
data_dtm_train<-data_dtm[1:4169,]
data_dtm_test<-data_dtm[4170:5559,]

data_raw_train<-raw_data[1:4169,]
data_raw_test<-raw_data[4170:5559,]

data_corpus_train<-data_corpus[1:4169]
data_corpus_test<-data_corpus[4170:5559]

prop_train_T<-prop.table(table(data_raw_train$type))
prop_test_T<-prop.table(table(data_raw_test$type))

spam<- subset(data_raw_train,type=="spam")
ham<- subset(data_raw_train,type=="ham")
library('RColorBrewer')
library("wordcloud")
png("wc_spam.png")
wordcloud(spam$text,min.freq=40,random.order=F, scale=c(5,0.1),colors=brewer.pal(8, “Dark2″))
dev.off()
png("wc_ham.png")
wordcloud(ham$text, min.freq=40,random.order=F, scale=c(5,0.1),colors=brewer.pal(8, “Dark2″))
dev.off()

library("e1071")

freq_train<-sort(apply(data_dtm_train,2,sum))
freq_train<-freq_train[freq_train>=5]
data_dtm_train<-data_dtm_train[,names(freq_train)]

## convert frequency to dummy value
convert_counts<-function(x){
  x<-ifelse(x>0,1,0)
  x<-factor(x,levels=c(0,1),labels=c("NO","YES"))
  x
}

final_train<-apply(data_dtm_train,2,FUN=convert_counts)
final_test<-apply(X=data_dtm_test,2,FUN=convert_counts)

## build the naiveBayes model
m<-naiveBayes(final_train,data_raw_train$type,laplace=0)
##  make predictions
test_predict<-predict(m,final_test)

library(gmodels)
CrossTable(test_predict,data_raw_test$type)
  
