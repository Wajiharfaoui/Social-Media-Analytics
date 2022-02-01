#################################################################
# Sentiment Analysis
#################################################################
install.packages('sentimentr')
library(sentimentr)
library(stringr)
install.packages("Unicode")
library(Unicode)
library(rvest)
library(lexicon)
library(syuzhet)
library(tm)
library(SnowballC)

# this function applies count_matches on a vector of texts and outputs a data.frame
emojis_matching <- function(texts, matchto, sentiment = NA) {
  
  texts %>% 
    map_df(count_matches, 
           matchto = matchto, 
           sentiment = sentiment)
  
}


# this function outputs the emojis found in a string as well as their occurences
count_matches <- function(string, matchto, sentiment = NA) {
  
  vec <- str_count(string, matchto)
  matches <- which(vec != 0)
  
  descr <- NA
  cnt <- NA
  
  if (length(matches) != 0) {
    
    descr <- description[matches]
    cnt <- vec[matches]
    
  } 
  
  df <- data.frame(text = string, count = cnt, sentiment = NA)
  
  if (!is.na(sentiment) && length(sentiment[matches]) != 0) {
    
    df$sentiment <- sentiment[matches]
    
  }
  
  return(df)
  
}


# Call the tweets dataset
tweets <- read.csv("./data/tweets.csv")
tweets_text <- tweets[,c("id","text")]

# Remove numbers,punctuation, URLs, hashtags, mentions, controls, special characters  
tweets_text <- mutate(tweets_text, text = gsub("[0-9]+|[[:punct:]]|http\\S+\\s*|#\\S+|@\\S+|[[:cntrl:]]|\\d|[[:punct:]]|","",text))


# lowercase 
tweets_text$text <- tolower(tweets_text$text)


# Removeleading and trailing whitespaces
tweets_clean <- mutate(tweets_text, text = gsub("^[[:space:]]*|[[:space:]]*$|","",text))

tweets_clean$text <- iconv(tweets_clean$text, from = "latin1", to = "ascii", sub = "byte")
# according to this study (https://aclanthology.org/L14-1265/), removing stopwords when doing sentiment analysis degrades classification performance.


tweets_sentiment <- tweets_clean %>%
  mutate(SentimentR = sentiment_by(text)$ave_sentiment)

emDict_raw <- read.csv2("./data/Emoji.csv") %>% 
  select(description = EN, r_encoding = ftu8, unicode)

# plain skin tones
skin_tones <- c("light skin tone", 
                "medium-light skin tone", 
                "medium skin tone",
                "medium-dark skin tone", 
                "dark skin tone")

# remove plain skin tones and remove skin tone info in description
emDict <- emDict_raw %>%
  # remove plain skin tones emojis
  filter(!description %in% skin_tones) %>%
  # remove emojis with skin tones info, e.g. remove woman: light skin tone and only
  # keep woman
  filter(!grepl(":", description)) %>%
  mutate(description = tolower(description)) %>%
  mutate(unicode = as.u_char(unicode))
# all emojis with more than one unicode codepoint become NA 

matchto <- emDict$r_encoding
description <- emDict$description

url <- "http://kt.ijs.si/data/Emoji_sentiment_ranking/index.html"

# get emoticons
emojis_raw <- url %>%
  read_html() %>%
  html_table() %>%
  data.frame() %>%
  select(-Image.twemoji., -Sentiment.bar.c.i..95..)
names(emojis_raw) <- c("char", "unicode", "occurrences", "position", "negative", 
                       "neutral", "positive", "sentiment_score", "description", 
                       "block")
# change numeric unicode to character unicode to be able to match with emDict 
emojis <- emojis_raw %>%
  mutate(unicode = as.u_char(unicode)) %>%
  mutate(description = tolower(description)) 


# merge with emDict to get encoding
emojis_merged <- emojis %>%
  merge(emDict, by = "unicode")

new_matchto <- emojis_merged$r_encoding
new_description <- emojis_merged$description.x
sentiment <- emojis_merged$sentiment_score

# Calculate emojis sentiment 

EmojisSentiment <- emojis_matching(tweets_sentiment$text, new_matchto, sentiment) %>%
  mutate(EmojisSentiment = count * as.numeric(sentiment)) %>%
  group_by(text) %>% 
  summarise(EmojisSentiment = sum(sentiment, na.rm = TRUE))

SentimentTweets <- left_join(tweets_sentiment,EmojisSentiment, by="text")

# Calculate sentiment based on different methods 

SentimentTweets <- SentimentTweets%>%
  mutate(syuzhetSentiment = get_sentiment(text, method="syuzhet"), BingSentiment=get_sentiment(text, method="bing"), 
         AfinSentiment=get_sentiment(text, method="afinn"), NrcSentiment=get_sentiment(text, method="nrc", lang = "english"))

# Standardize scores 

SentimentTweets$SentimentR <- sign(SentimentTweets$SentimentR)
SentimentTweets$EmojisSentiment <- sign(SentimentTweets$EmojisSentiment)
SentimentTweets$syuzhetSentiment <- sign(SentimentTweets$syuzhetSentiment)
SentimentTweets$BingSentiment <- sign(SentimentTweets$BingSentiment)
SentimentTweets$AfinSentiment <- sign(SentimentTweets$AfinSentiment)
SentimentTweets$NrcSentiment <- sign(SentimentTweets$NrcSentiment)

# Calculate final score 
SentimentTweets$FinalSentiment <- sign(rowMeans(SentimentTweets[,3:8], na.rm=TRUE))


SentimentData <- SentimentTweets %>%
  select(id,text,FinalSentiment)%>%
  mutate(text=gsub("<.*?>", "",text))

SentimentTweets <- apply(SentimentTweets,2,as.character)
SentimentTweets <- SentimentTweets[,c(1,2,9)]
write(SentimentTweets,"./data/Sentiment.csv")
