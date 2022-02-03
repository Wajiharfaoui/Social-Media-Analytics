# Building model to predict engagement category of a tweet

library(xgboost)
library(caret)
library(nnet)
library(pROC)
library(multiROC)
library(vader)

#functions ------

getLength <- function(data, remove_extras=FALSE){
  # ---------------------------------------------------------------------------------------------------------------------------
  #
  # REFERENCE TO URL PATTERN: https://stackoverflow.com/questions/26496538/extract-urls-with-regex-into-a-new-data-frame-column
  # REFRENCE TO HASHTAG PATTERN: https://stackoverflow.com/questions/13762868/how-do-i-extract-hashtags-from-tweets-in-r
  #
  # ---------------------------------------------------------------------------------------------------------------------------
  
  # url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  # ht_pattern <- "#\\S+"
  
  if(remove_extras){
    data<- gsub("[0-9]+|[[:punct:]]|http\\S+\\s*|#\\S+|@\\S+|[[:cntrl:]]|\\d|[[:punct:]]|","",data)
    data<- gsub("^[[:space:]]*|[[:space:]]*$|","",data)
  }
  return(nchar(data))
}

getTimeOfDay <- function(data){
  hour_of_tweet <- format(strptime(data, "%Y-%m-%d %H:%M:%S"), "%H")
  time_of_day <- ifelse(as.numeric(hour_of_tweet)<6, "Late Night",
                        ifelse(as.numeric(hour_of_tweet) >=6 & as.numeric(hour_of_tweet) <12, "Morning", ifelse(as.numeric(hour_of_tweet) >=12 & as.numeric(hour_of_tweet) <18, "Afternoon", "Evening")))
  return(time_of_day)
}

isWeekend <- function(data){
  day_of_week <- as.POSIXlt(data)$wday
  return(ifelse(day_of_week==0 | day_of_week==6, 1,0))
}

getRegexCount<- function(data, regex){
  return (str_count(data, regex))
}

getEmojiCount <- function(index){
  return(length(ji_extract_all(alltweets$text[index])[[1]]))
}

sentiment_estimate<-function(tweet_text){
  if (nchar(tweet_text)>280) stop("Maximum length is 280 characters!")
  
  vader_sentiment <- sign(vader_df(tweet_text, incl_nt = T, neu_set = T, rm_qm = F)$compound)
  final_sentiment <- ifelse(vader_sentiment>0,"Positive",
                            ifelse(vader_sentiment<0,"Negative","Neutral"))
  list("vader_sent"=vader_sentiment,"final_sent"=final_sentiment)
}


#modeling -------
# setwd('C:/Users/mserrano/OneDrive - IESEG/MSc/2ND SEMESTER/SOCIAL MEDIA ANALYTICS/Group Project/App')

base_table <- read.csv("final_base_table.csv", row.names = 1)
target <- base_table$engagement_categ
label <- as.integer(target) - 1 

set.seed(123)

base_table$engagement_categ <- as.factor(base_table$engagement_categ)

train_index<- createDataPartition(base_table$engagement_categ, p = .8, list = FALSE)

training_set <- base_table[ train_index,]
test_set  <- base_table[-train_index,]

train_data <- as.matrix(training_set[, -(1)])
test_data <- as.matrix(test_set[,-(1)])

train_label <- label[train_index]
test_label <- label[-(train_index)]

###############################################################
#
# CODE REFERENCE: https://www.rdocumentation.org/packages/nnet/versions/7.3-12/topics/multinom?
#
###############################################################



mn.net <- nnet::multinom(engagement_categ ~ ., training_set)




engagement_predictor<-function(tweet_text,topic,ph_c=0,vid_c=0,gif_c=0,ot_med_c=0,app="iphone"){

  base_names<-names(base_table)[2:ncol(base_table)]
  
  base_pred = data.frame(matrix(
    vector(), 1, length(base_names), dimnames=list(c(), base_names)),
    stringsAsFactors=F)
  
  #prediction base filling
  base_pred$tweet_length<-getLength(tweet_text)
  base_pred$clean_tweet_length<-getLength(tweet_text,TRUE)
  base_pred$is_weekend<-isWeekend(now())
  base_pred$upper_count <- getRegexCount(tweet_text, "[A-Z]")
  base_pred$exclamation_count <- getRegexCount(tweet_text, "[?!]")
  base_pred$hashtag_ct<-sum(str_count(tweet_text,"#(\\d|\\w)+"))
  base_pred$mentions_ct<-sum(str_count(tweet_text,"@(\\d|\\w)+"))
  base_pred$photos_count<-ph_c
  base_pred$videos_count<-vid_c
  base_pred$gifs_count<-gif_c
  base_pred$other_media_count<-ot_med_c
  base_pred$emojis_count<-sum(str_count(tweet_text,"[^\x01-\x7F]"))
  base_pred$source_Sprinklr<-ifelse(app=="Sprinklr",1,0)
  base_pred$source_Twitter.for.iPhone<-ifelse(app=="iPhone App",1,0)
  base_pred$source_Twitter.Web.App<-ifelse(app=="Web App",1,0)
  base_pred$time_of_day_Evening<-ifelse(getTimeOfDay(now())=="Evening",1,0)
  base_pred$time_of_day_Late.Night<-ifelse(getTimeOfDay(now())=="Late Night",1,0)
  base_pred$time_of_day_Morning<-ifelse(getTimeOfDay(now())=="Morning",1,0)
  base_pred$month_2<-ifelse(month(now())==2,1,0)
  base_pred$month_3<-ifelse(month(now())==3,1,0)
  base_pred$month_4<-ifelse(month(now())==4,1,0)
  base_pred$month_5<-ifelse(month(now())==5,1,0)
  base_pred$month_6<-ifelse(month(now())==6,1,0)
  base_pred$month_7<-ifelse(month(now())==7,1,0)
  base_pred$month_8<-ifelse(month(now())==8,1,0)
  base_pred$month_9<-ifelse(month(now())==9,1,0)
  base_pred$month_10<-ifelse(month(now())==10,1,0)
  base_pred$month_11<-ifelse(month(now())==11,1,0)
  base_pred$month_12<-ifelse(month(now())==12,1,0)
  base_pred$vader_sentiment_.1<-ifelse(sentiment_estimate(tweet_text)$vader_sent>0,1,0)
  base_pred$vader_sentiment_.1.1<-ifelse(sentiment_estimate(tweet_text)$vader_sent<0,1,0)
  base_pred$final_topic_2<-ifelse(topic==2,1,0)
  base_pred$final_topic_3<-ifelse(topic==3,1,0)
  base_pred$final_topic_4<-ifelse(topic==4,1,0)
  base_pred$final_topic_5<-ifelse(topic==5,1,0)
  base_pred$final_topic_6<-ifelse(topic==6,1,0)
  base_pred$final_topic_7<-ifelse(topic==7,1,0)
  
  pred <- predict(mn.net, newdata=base_pred, type="prob")
  maxprob<-names(pred[which(pred==max(pred))])
  maxprob_nbr<-round(as.numeric(pred[which(pred==max(pred))])*100,2)
  

  pred_engagement<-ifelse(maxprob=="1","Not Engaging",
                          ifelse(maxprob=="2","Quite Engaging",
                                 ifelse(maxprob=="3","Engaging","Highly Engaging")))
  list("pred"=pred,"pred_engagement"=pred_engagement,"maxprob_nbr"=maxprob_nbr)
}





