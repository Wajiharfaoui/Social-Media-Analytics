setwd("C:/Users/promaninfante/OneDrive - IESEG/SM Analytics/Social-Media-Analytics/data")
options(scipen = 999)

if(!require("fastDummies")) install.packages("fastDummies"); library("fastDummies")

base_table <- readRDS("./base_table.rds")
final_tweets <-  readRDS("./final_tweets_all_cols.rds")
topic_categs <- readRDS("./TopicData.rds")

sentiment <- readRDS("./SentimentData.rds")


sent_dummies <- dummy_cols(sentiment, select_columns= c("FinalSentiment"), remove_first_dummy = TRUE, remove_selected_columns= TRUE)
sent_dummies <- sent_dummies[, c("id","FinalSentiment_0","FinalSentiment_1")]

# BASE TABLE 
final_base_table <- merge(base_table, sent_dummies, by="id", all.x=TRUE)

final_base_table <- merge(final_base_table, topic_categs, by.x="id", by.y="document", all.x=TRUE)

final_base_table$final_topic <- ifelse(is.na(final_base_table$final_topic),7, final_base_table$final_topic )

final_base_table <- dummy_cols(final_base_table, select_columns= c("final_topic"), remove_first_dummy = TRUE, remove_selected_columns= TRUE)
final_base_table <- final_base_table[, -c("id")]

bt_csv <- apply(final_base_table,2,as.character)

write.csv(bt_csv, "./final_base_table.csv")

# DATASET ALL COLUMNS

sentiment_merge <- sentiment[,c("id", "FinalSentiment")]
final_tweets_all_cols <- merge(final_tweets, sentiment_merge , by="id", all.x=TRUE)
final_tweets_all_cols <- merge(final_tweets_all_cols, topic_categs, by.x="id", by.y="document", all.x=TRUE)

final_tweets_all_cols$final_topic <- ifelse(is.na(final_tweets_all_cols$final_topic),7, final_tweets_all_cols$final_topic )
all_cols_csv  <- apply(final_tweets_all_cols,2,as.character)

write.csv(all_cols_csv, "./final_all_cols.csv")
