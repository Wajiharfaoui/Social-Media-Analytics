setwd("C:/Users/promaninfante/OneDrive - IESEG/SM Analytics")
source("tokens.R")
if(!require("httr")) install.packages("httr"); library("httr")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("stringr")) install.packages("stringr"); library("stringr")
devtools::install_github("hadley/emo")
library(emo)
number_of_tweets <- 1000
AllTweets <- list()
meta <- list()
for(i in 1:(number_of_tweets/100)){
  print(i)
  if(i ==1){
    url_complete <- modify_url(
      url = "https://api.twitter.com",
      path = c("2", "users","8771022","tweets"),
      query= list(
        exclude="replies,retweets",
        expansions="attachments.poll_ids,attachments.media_keys,author_id,entities.mentions.username,geo.place_id,in_reply_to_user_id,referenced_tweets.id,referenced_tweets.id.author_id",
        tweet.fields="attachments,author_id,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld",
        user.fields="created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld",
        place.fields="contained_within,country,country_code,full_name,geo,id,name,place_type",
        poll.fields="duration_minutes,end_datetime,id,options,voting_status",
        media.fields="duration_ms,height,media_key,preview_image_url,type,url,width,public_metrics,non_public_metrics,organic_metrics,promoted_metrics",
        max_results=100
      )
    )
    resTweets <- GET(url = url_complete,add_headers(authorization = paste0("Bearer ",tw_bearer_token)))
    AllTweets[[i]] <- fromJSON(httr::content(resTweets, "text"),flatten=T)
    meta[[i]] <- fromJSON(httr::content(resTweets, "text"))$meta
  } else {
    if(sum(grepl("next_token",names(meta[[i-1]])))){
      url_complete <- modify_url(
        url = "https://api.twitter.com",
        path = c("2", "users","8771022","tweets"),
        query= list(
          exclude="replies,retweets",
          expansions="attachments.poll_ids,attachments.media_keys,author_id,entities.mentions.username,geo.place_id,in_reply_to_user_id,referenced_tweets.id,referenced_tweets.id.author_id",
          tweet.fields="attachments,author_id,conversation_id,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,reply_settings,source,text,withheld",
          user.fields="created_at,description,entities,id,location,name,pinned_tweet_id,profile_image_url,protected,public_metrics,url,username,verified,withheld",
          place.fields="contained_within,country,country_code,full_name,geo,id,name,place_type",
          poll.fields="duration_minutes,end_datetime,id,options,voting_status",
          media.fields="duration_ms,height,media_key,preview_image_url,type,url,width,public_metrics,non_public_metrics,organic_metrics,promoted_metrics",
          max_results=100,
          pagination_token = meta[[i-1]]$next_token
        )
      )
      resTweets <- GET(url = url_complete,add_headers(authorization = paste0("Bearer ",tw_bearer_token)))
      AllTweets[[i]] <- fromJSON(httr::content(resTweets, "text"),flatten=T)
      meta[[i]] <- fromJSON(httr::content(resTweets, "text"))$meta
    } else {
      break
    }
  }
}
# Now we will bind all the data elements together using the rbindlist function from data.table
library(data.table)
alltweets <- rbindlist(lapply(AllTweets,function(x) x$data),use.names=T,fill=TRUE)
users <- rbindlist(lapply(AllTweets,function(x) x$includes$users),use.names=T,fill=TRUE)
media <- rbindlist(lapply(AllTweets,function(x) x$includes$media),use.names=T,fill=TRUE)

# Creating engagement variable based on weighted average of likes, replies, quotes and retweets
alltweets$engagement <- alltweets$public_metrics.quote_count*0.4 + alltweets$public_metrics.retweet_count *0.3
+  alltweets$public_metrics.reply_count*0.2 + alltweets$public_metrics.like_count*0.1

# Engagement categories based on quantiles [1,4]
alltweets$engagement_categ <-  cut(
  alltweets$engagement,
  breaks = quantile(alltweets$engagement, c(0, 0.25, 0.5, 0.75, 1)),
  labels = c(1, 2, 3, 4),
  right  = FALSE,
  include.lowest = TRUE
)

getLength <- function(data, remove_extras=FALSE){
  # ---------------------------------------------------------------------------------------------------------------------------
  #
  # REFERENCE TO URL PATTERN: https://stackoverflow.com/questions/26496538/extract-urls-with-regex-into-a-new-data-frame-column
  # REFRENCE TO HASHTAG PATTERN: https://stackoverflow.com/questions/13762868/how-do-i-extract-hashtags-from-tweets-in-r
  #
  # ---------------------------------------------------------------------------------------------------------------------------
  
  url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
  ht_pattern <- "#\\S+"
  
  if(remove_extras){
    data<- str_trim(str_replace_all(str_replace_all(data, url_pattern,""), ht_pattern,""))
  }
  return(str_length(data))
}

# Length of original tweet with all hashtags and urls
alltweets$tweet_length <- getLength(alltweets$text)

# Length of tweet after removing all hashtags and urls
alltweets$clean_tweet_length <- getLength(alltweets$text, TRUE)

# Extract in which part of day the tweet was posted
getTimeOfDay <- function(data){
  hour_of_tweet <- format(strptime(data, "%Y-%m-%dT%H:%M:%S.000Z"), "%H")
  time_of_day <- ifelse(as.numeric(hour_of_tweet)<6, "Late Night",
         ifelse(as.numeric(hour_of_tweet) >=6 & as.numeric(hour_of_tweet) <12, "Morning", ifelse(as.numeric(hour_of_tweet) >=12 & as.numeric(hour_of_tweet) <18, "Afternoon", "Evening")))
  return(time_of_day)
}

alltweets$time_of_day <- getTimeOfDay(alltweets$created_at)

# Extract weekday or weekend 
isWeekend <- function(data){
  day_of_week <- as.POSIXlt(data)$wday
  return(ifelse(day_of_week==0 | day_of_week==6, 1,0))
}

alltweets$is_weekend <- isWeekend(alltweets$created_at)

# Extract upper case count and ? ! count

getRegexCount<- function(data, regex){
  return (str_count(data, regex))
}

alltweets$upper_count <- getRegexCount(alltweets$text, "[A-Z]")
alltweets$exclamation_count <- getRegexCount(alltweets$text, "[?!]")

# Get number of hashtags and mentions in each tweet


alltweets <- alltweets %>%
    mutate(hashtag_ct = lapply(row_number(), 
                               function(x) ifelse(is.null(nrow(entities.hashtags[[x]]["tag"])), 
                                                  0, 
                                                  nrow(entities.hashtags[[x]]["tag"]) )),
           mentions_ct = lapply(row_number(), 
                                function(x) ifelse(is.null(nrow(entities.mentions[[x]]["username"])), 
                                                   0, 
                                                   nrow(entities.mentions[[x]]["username"]) )))

# Get count of photos, videos, gifs and others per tweet
getMediaCount <- function(index){
  media_list<- alltweets$attachments.media_keys[[index]]
  if(!is.null(media_list)){
    # Loop through every media key of a specific tweet
    count_photo <- 0
    count_video <- 0
    count_gif <- 0
    count_other <-0
    for(i in 1:length(media_list)){
      key <- media_list[i]
      type <- media[media$media_key == key, ][["type"]]
      if (length(type)>0){
        if(type == "photo"){
          count_photo<- count_photo+1
        } else if (type == "video"){
          count_video = count_video +1
        } else if(type == "animated_gif"){
          count_gif = count_gif + 1
        } else{
          count_other = count_other+1
        }
      }
      else{
        count_other = count_other+1
      }
    }
    return(c(photos= count_photo, videos= count_video, gifs= count_gif, others = count_other))
  } else {
    return(c(photos= 0, videos= 0, gifs= 0, others = 0))
  }
}

# Count emojis in tweet
getEmojiCount <- function(index){
  return(length(ji_extract_all(alltweets$text[index])[[1]]))
}

alltweets <- alltweets %>% mutate(
  photos_count = lapply(row_number(), function(x){ return(getMediaCount(x)[["photos"]])}),
  videos_count =lapply(row_number(), function(x){ return(getMediaCount(x)[["videos"]])}),
  gifs_count =lapply(row_number(), function(x){ return(getMediaCount(x)[["gifs"]])}),
  other_media_count = lapply(row_number(), function(x){ return(getMediaCount(x)[["others"]])}),
  emojis_count = lapply(row_number(), function(x){return(getEmojiCount(x))})
  
  )

# Removing unuseful columns 

alltweets <- alltweets[, -c("reply_settings","referenced_tweets","conversation_id","in_reply_to_user_id","lang","author_id", "entities.mentions", "entities.hashtags", "entities.annotations", "entities.urls","public_metrics.retweet_count", "public_metrics.reply_count", "public_metrics.quote_count", "public_metrics.like_count", "attachments.media_keys", "attachments.poll_ids")]
