setwd("C:/Users/promaninfante/OneDrive - IESEG/SM Analytics")
source("tokens.R")
if(!require("httr")) install.packages("httr"); library("httr")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

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

length(alltweets$attachments.media_keys[[7]]) 
test <- c("26","232")
length(test)
