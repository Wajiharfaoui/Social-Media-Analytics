if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("tidytext")) install.packages("tidytext"); library("tidytext")
if(!require("udpipe")) install.packages("udpipe"); library("udpipe")
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
if (!require("ldatuning")) install.packages("ldatuning", quiet=TRUE) ; require("ldatuning")
if (!require("topicmodels")) install.packages("topicmodels", quiet=TRUE) ; require("topicmodels")
if (!require("ggplot2")) install.packages("ggplot2", quiet=TRUE) ; require("ggplot2")
if (!require("stringr")) install.packages("stringr", quiet=TRUE) ; require("stringr")

setwd("C:/Users/jsanchezramirez/OneDrive - IESEG/Documents/Second semester/2. Social media analytics/Project/data")
alltweets <- readRDS("alltweets.rds")



#####################################
#TOPIC FEATURES
#####################################
alltweets_text <- alltweets[, c('id', 'text', 'entities.urls')]

#Removing urls and emojis
alltweets_text$text_no_url <- gsub("[^\x01-\x7F]", "", alltweets_text$text)
alltweets_text$text_no_url <- str_trim(gsub('http\\S+\\s*',"", alltweets_text$text_no_url)) #removes trailing/leading spaces
alltweets_text$text_no_url <- tolower(gsub('http\\S+\\s*',"", alltweets_text$text_no_url))



#POStagging NA
udmodel <- udpipe_download_model(language = "english")
udmodel <- udpipe_load_model(file = udmodel$file_model)

alltweets_POS <- udpipe_annotate(udmodel, 
                                 doc_id = alltweets_text$id ,
                                 x=alltweets_text$text_no_url)
alltweets_POS <- as.data.frame(alltweets_POS)

#Select tokens according to UPOS
alltweets_POS_f <- alltweets_POS[alltweets_POS$upos %in% c("ADJ","NOUN","PROPN","INTJ"),c("doc_id","sentence","token","lemma","upos")]
alltweets_POS_f <- alltweets_POS_f[!alltweets_POS_f$token %in% c("dunkin'","dunkin","donut","donuts","amp","dd"),]

#Convert to dtm to determine optimal number of topics
tweets_dtm <- alltweets_POS_f %>% 
  count(doc_id, lemma, sort = TRUE) %>%
  cast_dtm(document = doc_id, term = lemma,
           value = n, weighting = tm::weightTf)

# Number of topics optimal
Ntopics <- FindTopicsNumber(
  tweets_dtm,
  topics = seq(from = 2, to = 50, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(Ntopics)

#Selecting no. of topics according to the previous results
no_topic <- 6

#LDA Topics
set.seed(1234)
tweets_lda <- LDA(tweets_dtm, k = no_topic,method="gibbs",control = list(iter = 500, verbose = 25) )
tweets_lda


#terms per topic Beta
tweet_topics <- tidy(tweets_lda, matrix = "beta")


#top terms per topic
top_tweet_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_tweet_terms


top_tweet_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

#topics per tweet
tweet_documents <- tidy(tweets_lda, matrix = "gamma")

# Choose, per tweet, the most important topic 
tweet_doc_topic <- tweet_documents %>%
  group_by(document) %>%
  arrange(desc(gamma)) %>%
  slice(1) 

#For tweets with a gamma less than 0.19 we created a category of "Others/Random topics"
tweet_doc_topic$final_topic <- ifelse(tweet_doc_topic$gamma < 0.19,7,tweet_doc_topic$topic)

tweet_doc_topic %>%
  group_by(final_topic) %>% 
  summarise(nbr_documents = n())

final_topic_tweet <- tweet_doc_topic[,c("document","final_topic")]
