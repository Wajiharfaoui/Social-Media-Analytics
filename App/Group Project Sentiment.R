#libraries
library(shiny)
library(data.table)
library(shinythemes)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(rtweet)
library(tidytext)
library(stringr)
library(forcats)
library(tidyr)
library(wordcloud2)
library(reshape2)
library(udpipe)
library(vader)
library(packcircles)

################ PREPARATION ------------------------------
dat<-read.csv("final_all_cols.csv")

data <- readRDS("final_all_cols.RDS")

dat$categ_name <- ifelse(dat$engagement_categ==1,"4. Not Engaging",
                         ifelse(dat$engagement_categ==2,"3. Quite engaging",
                                ifelse(dat$engagement_categ==3, "2. Engaging",
                                       "1. Highly engaging")))
#general data preparation ----

dat$created_at<-as.Date(dat$created_at)
dat$year<-year(dat$created_at)
topics_names<-c("Donations/Supporting Foundations","Special Celebrations","Special Offers/Discounts",
                "Order by App/Drive Thru","Limited Edition Products","Delivery Order","Others")
topics<-data.frame("final_topic"=c(1:7),"Topic"=topics_names)

dat<-merge(dat,topics,by="final_topic")
dat$Topic<-relevel(as.factor(dat$Topic),ref="Others")

summary_sentiment <- dat %>%
  group_by(vader_sentiment)%>%
  summarise(n=n())%>%
  mutate(Perc_freq = paste0(round(100 * n/sum(n), 0), "%"))

text <- c('Negative', 'Neutral', 'Positive')
data_sentiment <- data.frame(summary_sentiment, text)

years <- c("2020","2021","2022")
weekend <- c(length(dat$is_weekend[dat$is_weekend==1 & year(dat$created_at) == 2020]),
             length(dat$is_weekend[dat$is_weekend==1 & year(dat$created_at) == 2021]),
             length(dat$is_weekend[dat$is_weekend==1 & year(dat$created_at) == 2022]))
weekdays <- c(length(dat$is_weekend[dat$is_weekend==0 & year(dat$created_at) == 2020]),
              length(dat$is_weekend[dat$is_weekend==0 & year(dat$created_at) == 2021]),
              length(dat$is_weekend[dat$is_weekend==0 & year(dat$created_at) == 2022]))
data_weeks <- data.frame(years, weekend, weekdays)


# data preparation for wordcloud ----

# Wordclouds 
wc_data <- data[,c("id",'text', 'final_topic')]
wc_data$text <- gsub("[^\x01-\x7F]", "", wc_data$text)
wc_data$text <- str_trim(gsub('http\\S+\\s*',"", wc_data$text))
wc_data$text <- tolower(gsub('http\\S+\\s*',"", wc_data$text))

#POStagging NA
# udmodel <- udpipe_download_model(language = "english")
# udmodel <- udpipe_load_model(file = udmodel$file_model)

udmodel <- udpipe_load_model("english-ewt-ud-2.5-191206.udpipe")

alltweets_POS <- udpipe_annotate(udmodel, 
                                 doc_id = wc_data$id ,
                                 x=wc_data$text)
alltweets_POS <- as.data.frame(alltweets_POS)

#Select tokens according to UPOS
alltweets_POS_f <- alltweets_POS[alltweets_POS$upos %in% c("ADJ","NOUN","PROPN","INTJ"),c("doc_id","sentence","token","lemma","upos")]
alltweets_POS_f <- alltweets_POS_f[!alltweets_POS_f$token %in% c("dunkin'","dunkin","donut","donuts","amp","dd"),]

tweets_topics <- merge(x = alltweets_POS_f, y = wc_data, by.x = "doc_id", by.y="id", all.x = TRUE)
tweets_topics <- tweets_topics[tweets_topics$token != "&gt",]

#data preparation for engagement by topic----

data_freq <- data %>% group_by(engagement_categ, final_topic) %>%
  summarize(count=n())

engagement_not <- data_freq[data_freq$engagement_categ==1, ]$count
engagement_quite <- data_freq[data_freq$engagement_categ==2,]$count
engagement_eng <- data_freq[data_freq$engagement_categ==3,]$count
engagement_high <- data_freq[data_freq$engagement_categ==4,]$count

engagement_not_pc <- round(100*engagement_not/(engagement_not+engagement_quite+engagement_eng+engagement_high),2)
engagement_quite_pc <- round(100*engagement_quite/(engagement_not+engagement_quite+engagement_eng+engagement_high),2)
engagement_eng_pc <- round(100*engagement_eng /(engagement_not+engagement_quite+engagement_eng+engagement_high),2)
engagement_high_pc <- round(100*engagement_high /(engagement_not+engagement_quite+engagement_eng+engagement_high),2)

engagement_titles <- c("Not Engaging", "Quite Engaging", "Engaging", "High Engaging")

topic_titles <- c("Donations/Supporting Foundations","Special Celebrations","Special Offers/Discounts",
                  "Order by App/Drive Thru","Limited Edition Products","Delivery Order","Others")

data_graph_engagement <- data.frame(topic_titles, engagement_not_pc, engagement_quite_pc, engagement_eng_pc, engagement_high_pc)

#data preparation for engagement by media----

data_media <- data[,c("id","engagement_categ","photos_count","videos_count","gifs_count")]
data_media$has_media <- ifelse((data_media$photos_count>0 | data_media$videos_count >0 | data_media$gifs_count >0), 1,0)
data_media_freq <- data_media %>% group_by(engagement_categ, has_media) %>%
  summarize(count=n())

engagement_categ <- c("Not Engaging", "Quite Engaging", "Engaging", "Highly Engaging")
media <- data_media_freq[data_media_freq$has_media==1,]$count
no_media <- data_media_freq[data_media_freq$has_media==0,]$count

data_graph_media <- data.frame(engagement_categ, media, no_media)

data_graph_media$engagement_categ <- factor(data_graph_media$engagement_categ, levels = data_graph_media[["engagement_categ"]])

#source functions for the prediction

source("EngagementModelPred.R")
# Upper case words vs engagement
dat$upper_categ <-ifelse(dat$upper_count <=4 , "1. Less than 4",
                         ifelse(dat$upper_count <= 10, "2. Upper case [5 - 10]",
                                ifelse(dat$upper_count <= 15, "3. Upper case [11 - 15]",
                                       "4. More than 15")))


upper_gr <- dat %>% 
  group_by(upper_categ,categ_name) %>% 
  summarize(n = n())%>%
  mutate(Perc_freq = paste0(round(100 * n/sum(n), 0), "%"))

# Engagement per length

length_gr <- dat %>% 
  group_by(categ_name) %>% 
  summarize(avg_length = mean(tweet_length))

packing <- circleProgressiveLayout(length_gr$avg_length, sizetype='area')
data <- cbind(length_gr, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Sentiment and engagement over time 
dat$date_month <- substr(dat$created_at,start =1,stop=7)
dat$name_sent  <- ifelse(dat$vader_sentiment == 0, "Neutral",
                         ifelse(dat$vader_sentiment == 1, "Positive",
                                "Negative"))

sentiment_gr <- dat %>% 
  group_by(date_month,name_sent) %>%
  summarize(avg_eng = mean(engagement))

sentiment_gr <- sentiment_gr  %>% pivot_wider(id_cols=date_month,names_from =name_sent, values_from=avg_eng)
sentiment_gr$Negative[is.na(sentiment_gr$Negative)] <- 0

# Engagement per hashtag 
hashtags_grouped <-dat %>%
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#")) %>%
  filter(categ_name %in% c("1. Highly engaging","4. Not Engaging"))%>%
  group_by(hashtag,categ_name ) %>%
  count(hashtag, sort = TRUE)

sorted_topic <- hashtags_grouped %>%
  group_by(categ_name) %>%
  top_n(1, n) %>%
  ungroup() %>%
  arrange(categ_name, -n)
################ FUNCTIONS  ------------------------------

#sentiment

topicPie<-function(y){
  dat %>% filter(year==y) %>% count(Topic) %>%
    plot_ly(labels = ~Topic,values = ~n, textinfo = "label+percent",
            showlegend = F,marker = list(colors = c("#F5821F","#E11388","#673710","#E113CC","#673766","#F582AA","#fd2f00"))) %>% 
    layout(autosize=T) %>% add_pie(hole = 0.5)
}

topicBars<-function(y){
  dat<-data.table(dat)
  colors<-c("#F5821F","#E11388","#673710","#673710","#673766","#F582AA","#fd2f00")
  counts_topics2<-dat[year==y][,.(others=sum(Topic=="Others"),del=sum(Topic=="Delivery Order"),
                                  don=sum(Topic=="Donations/Supporting Foundations"),lim=sum(Topic=="Limited Edition Products"),
                                  order=sum(Topic=="Order by App/Drive Thru"),celeb=sum(Topic=="Special Celebrations"),
                                  disc=sum(Topic=="Special Offers/Discounts")),by=month]
  
  fig<-plot_ly(counts_topics2,x=~month,y=~others, type="bar",name="Others", marker = list(color = colors[1])) %>%
    add_trace(y = ~del, name = 'Delivery Order', marker = list(color = colors[2])) %>% 
    add_trace(y = ~don, name = 'Donations/Supporting Foundations', marker = list(color = colors[3])) %>% 
    add_trace(y = ~lim, name = 'Limited Edition Products', marker = list(color = colors[4])) %>% 
    add_trace(y = ~order, name = 'Order by App/Drive Thru', marker = list(color = colors[5])) %>% 
    add_trace(y = ~celeb, name = 'Special Celebrations', marker = list(color = colors[6])) %>% 
    add_trace(y = ~disc, name = 'Special Offers/Discounts', marker = list(color = colors[7])) %>% 
    layout(yaxis = list(title = 'Count'),xaxis = list(title = '') , barmode = 'stack',legend = list(orientation="h"))
  fig
}

dat_eng <- dat %>% mutate(YearMonth = format(dat$created_at, "%Y-%m")) %>%
  group_by(YearMonth,engagement_categ)%>% 
  summarise(count=n())%>%
  pivot_wider(names_from = engagement_categ, values_from = count) %>%
  replace(is.na(.), 0)%>% 
  setNames(.,c("date","Not_Engaging","Quite_Engaging","Engaging","Highly_Engaging"))

dat_eng_weekend <- dat %>% mutate(YearMonth = format(dat$created_at, "%Y-%m"))%>%
  filter(is_weekend == 1)%>%
  group_by(YearMonth)%>% 
  summarise(engagement=sum(engagement))

dat_eng_weekdays <- dat %>% mutate(YearMonth = format(dat$created_at, "%Y-%m"))%>%
  filter(is_weekend == 0)%>%
  group_by(YearMonth)%>% 
  summarise(engagement=sum(engagement))
dat_eng_days <- merge(x = dat_eng_weekend, y = dat_eng_weekdays, by = "YearMonth", all = TRUE)
dat_eng_days[is.na(dat_eng_days)] <- 0

hello<-function(x){
  if (nchar(x)>280) stop("Maximum length is 280 characters!")
  if (agrepl("hola",x)){hi="Hola amigos :)"} else {hi =""}
  hi
}


generateWordCloud <- function(topic){
  
  topic1<-topics$final_topic[which(topics$Topic==topic)]
  # Group by word and count frequency
  wordsFreq <- tweets_topics %>% filter(final_topic==topic1) %>% group_by(token) %>% #
    summarize(freq = n()) %>%
    arrange(-freq) %>% top_n(25)
  
  colors_dk<-  c('#E11383', '#F5821F','#F5823F','#673710',"F5825F", "#E1135F", "#E11388", "#F5823G", "#673710", "#673716")
  wordcloud2(data=wordsFreq, size=1.4, color=colors_dk, shape="triangle")
}


generateSentimentGraph <- function(from, to){
  sentiment_data <- dat[, c('created_at', 'vader_sentiment')]
  sentiment_data$created_at <- as.Date(sentiment_data$created_at)
  sentiment_data <- sentiment_data %>% filter(created_at >= as.Date(from), created_at <= as.Date(to))
  
  sentiment_data$created_at <- format(as.Date(sentiment_data$created_at), "%Y-%m") 
  sentiment_data$vader_sentiment <- ifelse(sentiment_data$vader_sentiment == "-1", "Negative", ifelse(sentiment_data$vader_sentiment=="0", "Neutral", "Positive"))
  sentiment_data <- sentiment_data %>%
    group_by(created_at, vader_sentiment) %>% summarize(count = n()) %>%
    pivot_wider(names_from = vader_sentiment, values_from = count) %>%
    replace(is.na(.), 0)%>%
    mutate(Negative_pct = 100*Negative/(Negative+Neutral+Positive),
           Positive_pct = 100*Positive/(Negative+Neutral+Positive),
           Neutral_pct = 100*Neutral/(Negative+Neutral+Positive) )
  
  fig <- sentiment_data %>%
    
    ungroup() %>% plot_ly(type = 'scatter', mode = 'lines')%>%
    add_trace(x = ~created_at, y = ~Negative_pct, name = 'Negative', line=list(color='#673716'))%>%
    add_trace(x = ~created_at, y = ~Neutral_pct, name = 'Neutral', line=list(color='#F5821F'))%>%
    add_trace(x = ~created_at, y = ~Positive_pct, name = 'Positive', line=list(color='#E11383'))%>%
    layout(title = '',legend=list(x=0.90,y=1,title=list(text='')),
           xaxis = list(dtick = "M1", tickformat="%b<br>%Y",zerolinecolor = '#ffff',
                        zerolinewidth = 2,gridcolor = 'ffff', title= "" ),
           yaxis = list(zerolinecolor = '#ffff',
                        zerolinewidth = 2,
                        gridcolor = 'ffff', title= "Tweets percentage")) #plot_bgcolor='#e5ecf6',
  
  fig
  
}


generateMediaCount <- function(from, to){
  media_count <- dat[, c('id','created_at', 'photos_count', "videos_count", "gifs_count")]
  media_count$created_at <- as.Date(media_count$created_at)
  media_count <- media_count %>% filter(created_at >= as.Date(from), created_at <= as.Date(to))
  
  media_count <- media_count %>% mutate(photos_count = as.numeric(photos_count),
                                        videos_count = as.numeric(videos_count),
                                        gifs_count = as.numeric(gifs_count) )
  media_count$has_media<-ifelse((media_count$photos_count>0 | media_count$videos_count >0 | media_count$gifs_count >0), 1,0)
  # photos_pct = 100*photos_count/(gifs_count+videos_count+photos_count),
  # videos_pct = 100*videos_count/(gifs_count+videos_count+photos_count)
  media_ct <- sum(media_count$has_media)
  total_ct <- nrow(media_count)
  no_media_ct<- total_ct - media_ct
  
  melted_data<- melt(media_count, id.vars = "id", measure.vars = c("photos_count", "videos_count", "gifs_count"))
  melted_data <- melted_data[melted_data$value>0,]
  photos_ct <- nrow(melted_data[melted_data$variable == "photos_count",])
  videos_ct <- nrow(melted_data[melted_data$variable == "videos_count",])
  gif_ct <- nrow(melted_data[melted_data$variable == "gifs_count",])
  
  fig <- plot_ly(
    labels = c(" ", "Media", "No Media", "Photos", "Videos", "GIFs"),
    parents = c("", " ", " ", "Media", "Media", "Media"),
    values = c(total_ct, media_ct, no_media_ct, photos_ct, videos_ct, gif_ct),
    marker = list(colors = c(
      "#FFFFF","#F5821F","#E11388","#E11383","#673710",
      "#F5823F"
    )),
    type = 'sunburst',
    branchvalues = 'total') %>%
    layout(title = '')
  fig
}


################ UI ------------------------------

ui <- fluidPage(theme = shinytheme("united"),
                tags$img(src="Dunkin'_Donuts_Logo.png", align="right", height = 100, width=260),
                
                h2(strong("Dunkin Donuts Sentiment Analysis")),
                navlistPanel( widths = c(2,10),
                              tabPanel(h4(strong("Overview")),
                                       
                                       mainPanel(
                                         tabsetPanel(type="tabs",
                                                     tabPanel('Descriptive Pt.1',br(),br(),
                                                              
                                                              fluidRow(align="center", h3(strong("Tweets' Timeline"))
                                                              ),br(),br(),         
                                                              fluidRow(align="left",br(),br(),
                                                                       dateRangeInput("date_range", strong("Select the range of dates"),start=min(dat$created_at),end=max(dat$created_at),min=min(dat$created_at),max=max(dat$created_at),format = "yyyy-mm-dd"),br(),br()
                                                              ),
                                                              
                                                              fluidRow(align="center", 
                                                                       plotOutput("plot1", click = "plot_click"),br(),br(),
                                                                       verbatimTextOutput("info"),br(),br()
                                                              ),
                                                              
                                                              fluidRow(align="center",h3(strong("Top hashtags")),
                                                                       plotOutput("plot2", click = "plot_click2"),br(),br(),
                                                                       verbatimTextOutput("info2"),br(),br()
                                                              ),
                                                              fluidRow(align="left",selectInput("year1","Select Year",c(2020,2021,2022)),br(),br()
                                                              ),
                                                              fluidRow(align="center",
                                                                       h3(strong("Topics by Month")),br(),br(),
                                                                       plotlyOutput("plot3"),br(),br()
                                                              ),
                                                              fluidRow(align="center",
                                                                       column(6,
                                                                              h3(strong("Topic Distribution")),br(),br(),
                                                                              plotlyOutput("plot4"),br(),br()
                                                                       ),
                                                                       column(6,
                                                                              h3(strong("Time Posting Distribution")),br(),br(),
                                                                              plotlyOutput("plot7"),br(),br()
                                                                       )
                                                              )
                                                     ),
                                                     
                                                     tabPanel("Descriptive Pt.2",br(),br(),
                                                              fluidRow(align="center",
                                                                       h3("Length of Tweets Histogram"),br(),br(),
                                                                       plotlyOutput("plot5"),br(),br()
                                                              ),
                                                              fluidRow(align="center",
                                                                       h3("Length of Tweets by Date"),br(),br(),
                                                                       plotlyOutput("plot6"),br(),br()
                                                              ),
                                                              fluidRow(align="center",
                                                                       h3("Sentiment distribution in tweets"),br(),br(),
                                                                       plotlyOutput("plot8"),br(),br()
                                                              ),
                                                              fluidRow(align="center",
                                                                       h3("Weekdays/weekends share of tweets"),br(),br(),
                                                                       plotlyOutput("plot9"),br(),br()
                                                              ),
                                                              fluidRow(align="left",
                                                                       dateRangeInput("date_range2", strong("Select the range of dates"),start=min(dat$created_at),end=max(dat$created_at),min=min(dat$created_at),max=max(dat$created_at),format = "yyyy-mm-dd"),br(),br()
                                                              ),
                                                              fluidRow(align="center",
                                                                       h3("Tweets sentiment over months"),br(),br(),
                                                                       plotlyOutput("plot12"),br(),br()
                                                              ),
                                                              fluidRow(align="left",
                                                                       dateRangeInput("date_range3", strong("Select the range of dates"),start=min(dat$created_at),end=max(dat$created_at),min=min(dat$created_at),max=max(dat$created_at),format = "yyyy-mm-dd"),br(),br()
                                                              ),
                                                              fluidRow(align="center",
                                                                       h3("Media Distribution"),br(),br(),
                                                                       plotlyOutput("plot13")
                                                              )
                                                     ),
                                                     tabPanel("Wordcloud",br(),br(),
                                                              fluidRow(align="left",
                                                                       selectInput("sel_topic","Select Topic",topics$Topic),br(),br()
                                                              ),
                                                              fluidRow(align="center",
                                                                       wordcloud2Output("wordcloud1"))
                                                     )
                                         )
                                         
                                       )
                              ),
                              tabPanel(h4(strong("Engagement")),
                                       
                                       mainPanel(
                                         tabsetPanel(type="tabs",
                                                     tabPanel('Descriptive Pt.1',br(),br(),
                                                              fluidRow(align="center", h3(strong("Engagement Over Time")),br(),br(),
                                                                       plotlyOutput("plot10"),br(),br()
                                                              ), 
                                                              fluidRow(align="center", h3(strong("Engagement vs. Day Over Time")),br(),br(),
                                                                       plotlyOutput("plot11"),br(),br()
                                                              ),
                                                              fluidRow(align="center", h3(strong("Engagement vs. Topics")),br(),br(),
                                                                       plotlyOutput("plot14"),br(),br()
                                                              ),
                                                              fluidRow(align="center", h3(strong("Engagement vs. Media")),br(),br(),
                                                                       plotlyOutput("plot15"),br(),br()
                                                              )
                                                     ),
                                                     tabPanel('Descriptive Pt.2',br(),br(),
                                                              fluidRow(align="center", h3(strong("Uppercase words frequency vs. Engagement")),br(),br(),
                                                                       plotOutput("plot16"),br(),br(),br(),br(),
                                                              ), 
                                                              fluidRow(align="center",
                                                                       column(6,
                                                                              h3("Most Engaging Hashtag"),br(),br(),
                                                                              h4(strong(textOutput("text4")))
                                                                       ),
                                                                       column(6,
                                                                              h3("Least Engaging Hashtag"),br(),br(),
                                                                              h4(strong(textOutput("text5")))
                                                                       )),br(),br(),br(),br(),
                                                              fluidRow(align="center", h3(strong("Engagement vs. Tweet's length")),br(),br(),
                                                                       plotlyOutput("plot17"),br(),br()
                                                              ),
                                                              fluidRow(align="center", h3(strong("Engagement per sentiment overtime")),br(),br(),
                                                                       plotlyOutput("plot18"),br(),br()
                                                              ),
                                                              
                                                     )
                                                     
                                                     
                                         )
                                         
                                       )
                              ),
                              tabPanel(h4(strong("Engagement Predictor")),br(),
                                       mainPanel(
                                         fluidRow(align="left",
                                                  h4("Please input a desired Tweet"),br(),
                                                  textAreaInput("new_tweet", "", value = "Write tweet...", width = "1000px"),br()
                                         ),
                                         fluidRow(align="left",
                                                  column(6,
                                                         selectInput("sel_topic2","Select Topic",topics$Topic),br(),br()
                                                  ),
                                                  column(6,
                                                         selectInput("tw_app","Select App",c("Sprinklr","iPhone App","Web App")),br(),br()
                                                  )
                                         ),
                                         fluidRow(align="center",
                                                  column(3,
                                                         numericInput("ph_c","Photos Count",0,0,10)
                                                  ),
                                                  column(3,
                                                         numericInput("vid_c","Videos Count",0,0,10)
                                                  ),
                                                  column(3,
                                                         numericInput("gif_c","Gifs Count",0,0,10)
                                                  ),
                                                  column(3,
                                                         numericInput("ot_med_c","Other Media Count",0,0,10),br(),br()
                                                  )
                                         ),
                                         fluidRow(align="center",
                                                  actionButton("submit","Submit", icon("twitter")),br(),br()
                                         ),
                                         fluidRow(align="center",
                                                  column(4,
                                                         h4("Estimated Sentiment"),
                                                         h3(strong(textOutput("text1")))
                                                  ),
                                                  column(4,
                                                         h4("Estimated Engagement"),
                                                         h3(strong(textOutput("text2")))
                                                  ),
                                                  column(4,
                                                         h4("Engagement Probability"),
                                                         h3(strong(textOutput("text3")))
                                                  )
                                                  
                                         )
                                         
                                         
                                       )
                                       
                              )
                )
                
)



################ SERVER ------------------------------


server <- function(input, output){
  
  # ############### first page users 
  # 
  # #plots
  
  output$plot1 <- renderPlot({
    dat[dat$created_at>= input$date_range[1]& dat$created_at<= input$date_range[2],] %>% ts_plot("months") +
      labs(x = NULL, y = NULL,
           title = "Frequency of Dunkin Donuts official account tweets") + 
      theme_minimal()
  })
  
  output$info <- renderText({
    y_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Number of tweets=", round(e$y, 0), "\n Date=", format(as.POSIXct(e$x,origin="1970-01-01"),"%Y-%m-%d"))
    }
    paste0(y_str(input$plot_click))
  })
  
  output$plot2 <- renderPlot({
    dat %>% 
      unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
      filter(str_detect(hashtag, "^#")) %>%
      count(hashtag, sort = TRUE) %>%
      top_n(10)%>% mutate(name = fct_reorder(hashtag, n)) %>% 
      ggplot( aes(x=name, y=n)) + geom_bar(stat="identity", fill="#f5821f", alpha=.6, width=.4) + 
      coord_flip() + xlab("") + theme_bw()
  })
  
  output$info2 <- renderText({
    y_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Number of tweets=", round(e$y, 0))
    }
    paste0(y_str(input$plot_click2))
  })
  
  plottopicbars<-reactive({topicBars(input$year1)}) 
  output$plot3 <- renderPlotly({
    plottopicbars()
  })
  
  plottopicpie<-reactive({topicPie(input$year1)}) 
  output$plot4 <- renderPlotly({
    plottopicpie()
  })
  
  output$plot5 <- renderPlotly({
    plot_ly(dat,x=~tweet_length,type="histogram", marker=list(color="#F5821F")) %>%
      layout(bargap=0.01,yaxis = list(title = 'Count'),xaxis = list(title = 'Tweet Length'))
    
  })
  
  output$plot6 <- renderPlotly({
    plot_ly(dat, x = ~created_at, y = ~tweet_length, color = ~Topic,colors=c("#F5821F","#E11388","#673710","#E113CC","#673766","#F582AA")) %>%
      layout(yaxis = list(title = 'Tweet Length (characters)'),xaxis = list(title = ''),legend = list(orientation="h"))
    
  })
  
  output$plot7 <- renderPlotly({
    
    dat %>% 
      group_by(time_of_day) %>% 
      summarize(count = n()) %>% plot_ly(labels = ~time_of_day, values = ~count, textinfo = "label+percent",
                                         marker = list(colors = c("#F5821F","#E11388","#673710"))) %>% 
      add_pie(hole = 0.5) %>% layout(title = "",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$plot8 <- renderPlotly({
    
    plot_ly(data_sentiment, x = ~text, y = ~n, type = 'bar',text=~Perc_freq,
            marker = list(color = '#E11388',
                          line = list(color = 'E113CC',
                                      width = 1.5))) %>% 
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$plot9 <- renderPlotly({
    
    fig <- plot_ly(data_weeks, x = ~years, y = ~weekend, type = 'bar', name = 'Weekend',marker = list(color ="E11383"))
    fig <- fig %>% add_trace(y = ~weekdays, name = 'Weekdays',marker = list(color ="F5821F"))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    fig
  })
  
  ########### twitter sentiment and engagement ---
  
  tw_sentiment<-reactive({ 
    sentiment_estimate(input$new_tweet)
  }) 
  output$text1<-renderText({
    req(input$submit)
    return(isolate(tw_sentiment()$final_sent))
  })
  
  tw_eng_pred<-reactive({
    
    topic_number<-topics$final_topic[topics$Topic==input$sel_topic2]
    engagement_predictor(tweet_text=input$new_tweet,topic=topic_number,
                         ph_c=input$ph_c,vid_c=input$vid_c,gif_c=input$gif_c,
                         ot_med_c=input$ot_med_c,app=input$tw_app)
  }) 
  output$text2<-renderText({
    req(input$submit)
    return(isolate(tw_eng_pred()$pred_engagement))
  })
  output$text3<-renderText({
    req(input$submit)
    return(isolate(paste0(tw_eng_pred()$maxprob_nbr,"%")))
  })
  
  output$plot10 <- renderPlotly({
    
    fig <- dat_eng %>% ungroup() %>% 
      plot_ly(., x = ~date, y = ~Not_Engaging, type = 'bar', name = 'Not Engaging', marker = list(color = '#679EAD'))
    
    
    fig <- fig %>% add_trace(y = ~Quite_Engaging, name = 'Quite Engaging', marker = list(color = '#683817'))
    fig <- fig %>% add_trace(y = ~Engaging, name = 'Engaging', marker = list(color = '#f5821f'))
    fig <- fig %>% add_trace(y = ~Highly_Engaging, name = 'Highly Engaging', marker = list(color = '#e11383'))
    
    fig <- fig %>% layout(xaxis = list(title = "", tickangle = -45),
                          yaxis = list(title = ""),
                          margin = list(b = 100),
                          barmode = 'group')
    fig
    
  })
  
  output$plot11 <- renderPlotly({ 
    
    fig <- plot_ly(dat_eng_days, x = ~YearMonth, y = ~engagement.y, type = 'bar', name = 'Weekdays', marker = list(color = '#f5821f'))
    fig <- fig %>% add_trace(y = ~engagement.x, name = 'Weekend', marker = list(color = '#e11383'))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    
    fig
  })
  
  wc<-reactive({ 
    generateWordCloud(input$sel_topic)
  }) 
  output$wordcloud1 <- renderWordcloud2({
    wc()
  })
  
  sentGraph<-reactive({ 
    generateSentimentGraph(input$date_range2[1],input$date_range2[2])
  }) 
  output$plot12 <- renderPlotly({
    sentGraph()
  })
  
  
  mediaGraph<-reactive({ 
    generateMediaCount(input$date_range2[1],input$date_range2[2])
  }) 
  output$plot13 <- renderPlotly({
    mediaGraph()
  })
  
  output$plot14 <- renderPlotly({
    fig <- plot_ly(data_graph_engagement, x = ~engagement_not_pc, y = ~topic_titles, type = 'bar', orientation = 'h', name="Not Engaging",
                   marker = list(color = '#ee7aa8',
                                 line = list(color = 'rgb(248, 248, 249)', width = 1))) 
    fig <- fig %>% add_trace(x = ~engagement_quite_pc, marker = list(color = '#FA9D99'), name="Quite Engaging") 
    fig <- fig %>% add_trace(x = ~engagement_eng_pc, marker = list(color = '#F8746E'), name="Engaging") 
    fig <- fig %>% add_trace(x = ~engagement_high_pc, marker = list(color = '#F5281F'), name="Highly Engaging") 
    
    fig <- fig %>% layout(xaxis = list(title = "",
                                       showgrid = FALSE,
                                       showline = FALSE,
                                       showticklabels = FALSE,
                                       zeroline = FALSE,
                                       domain = c(0.15, 1)),
                          yaxis = list(title = "",
                                       showgrid = FALSE,
                                       showline = FALSE,
                                       showticklabels = FALSE,
                                       zeroline = FALSE),
                          barmode = 'relative',
                          paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)') 
    
    fig <- fig %>% add_annotations(xref = 'paper', yref = 'y', x = 0.14, y = topic_titles,
                                   xanchor = 'right',
                                   text = topic_titles,
                                   font = list(family = 'Arial', size = 12,
                                               color = 'rgb(67, 67, 67)'),
                                   showarrow = FALSE, align = 'right')
    # # labeling the percentages of each bar (x_axis)
    fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                                   x = engagement_not_pc / 2, y = topic_titles,
                                   text = paste(data_graph_engagement[,"engagement_not_pc"], '%'),
                                   font = list(family = 'Arial', size = 12,
                                               color = 'rgb(248, 248, 255)'),
                                   showarrow = FALSE)
    fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                                   x = engagement_not_pc + engagement_quite_pc / 2, y = topic_titles,
                                   text = paste(data_graph_engagement[,"engagement_quite_pc"], '%'),
                                   font = list(family = 'Arial', size = 12,
                                               color = 'rgb(248, 248, 255)'),
                                   showarrow = FALSE)
    fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                                   x = engagement_not_pc + engagement_quite_pc + engagement_eng_pc / 2, y = topic_titles,
                                   text = paste(data_graph_engagement[,"engagement_eng_pc"], '%'),
                                   font = list(family = 'Arial', size = 12,
                                               color = 'rgb(248, 248, 255)'),
                                   showarrow = FALSE)
    fig <- fig %>% add_annotations(xref = 'x', yref = 'y',
                                   x = engagement_not_pc + engagement_quite_pc + engagement_eng_pc + engagement_high_pc / 2, y = topic_titles,
                                   text = paste(data_graph_engagement[,"engagement_high_pc"], '%'),
                                   font = list(family = 'Arial', size = 12,
                                               color = 'rgb(248, 248, 255)'),
                                   showarrow = FALSE)
    #labeling the first Likert scale (on the top)
    fig <- fig %>% add_annotations(xref = 'x', yref = 'paper',
                                   x = c(32 / 2, 27 + 32 / 2, 27 + 32 + 31 / 2, 27 + 32 + 31 + 9 / 2),
                                   y = 1.25,
                                   text = engagement_titles,
                                   font = list(family = 'Arial', size = 12,
                                               color = 'rgb(67, 67, 67)'),
                                   showarrow = FALSE)
    fig
  })
  
  
  output$plot15 <- renderPlotly({
    fig_media <- plot_ly(data_graph_media, x = ~engagement_categ, y = ~no_media, type = 'bar', name = 'No Media', marker = list(color = '#F5B4CE'))
    fig_media <- fig_media %>% add_trace(y = ~media, name = 'Media', marker = list(color = '#F7524B'))
    fig_media <- fig_media %>% layout(xaxis = list(title = "", tickangle = -45),
                                      yaxis = list(title = ""),
                                      margin = list(b = 100),
                                      barmode = 'group')
    fig_media
  })
  
  output$plot16 <- renderPlot({
    upper_plot <- ggplot(upper_gr, aes(fill=categ_name, y=n, x=upper_categ)) + 
      geom_bar(position="fill", stat="identity") +
      scale_fill_manual(values = c('#679EAD', '#683817','#f5821f','#e11383'))+
      theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
      ggtitle ("Engagement vs. number of upper case")
    upper_plot
  })
  
  output$plot17 <- renderPlotly({
    length_eng_plot <-ggplot() + 
      geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
      scale_color_manual(values = c('#679EAD', '#683817','#f5821f','#e11383')) +
      geom_text(data = data, aes(x, y,  label = categ_name), color="black") +
      theme_void() + 
      theme(legend.position="none",plot.title = element_text(hjust = 0.5))+ 
      coord_equal()
    
    ggplotly(length_eng_plot)
  })
  
  output$plot18 <- renderPlotly({
    fig <- ungroup(sentiment_gr)%>%
      plot_ly( x = ~date_month,y = ~Positive, name ='Positive',type='scatter',mode ='lines')%>%
      add_trace(y = ~Negative, name ='Negative',type='scatter',mode ='lines')%>%
      add_trace(y = ~Neutral, name = 'Neutral',  type='scatter',mode ='lines') %>%
      layout(yaxis = list (title = "Engagement"),xaxis = list (title = "Month"))
    
    fig
  }) 
  output$text4<-renderText({
    return(sorted_topic$hashtag[1])
  })
  output$text5<-renderText({
    return(sorted_topic$hashtag[2])
  })
  
  
  
} 

shinyApp(ui = ui, server = server)
