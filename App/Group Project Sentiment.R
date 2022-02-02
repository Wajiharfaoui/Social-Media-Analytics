#create small functions for easier access and load libraries

h <- function(x){head(x,1)}

#to get number of unique values easier
uni <- function(x){length(unique(x))}

#to get the mode of character values
mode <- function(x){names(tail(sort(table(x)),1))}

colors <- c('#D0CBCA', '#E5562A', '#336EF9','#151F47')

#libraries
library(shiny)
library(data.table)
library(shinythemes)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(rtweet)
library(tidytext)
library(stringr)
library(forcats)
library(tidyr)

################ PREPARATION ------------------------------
setwd('C:/Users/mserrano/OneDrive - IESEG/MSc/2ND SEMESTER/SOCIAL MEDIA ANALYTICS/Group Project/App')
dat<-read.csv("final_all_cols.csv")

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
data <- data.frame(years, weekend, weekdays)


topicPie<-function(y){
  dat %>% filter(year==y) %>% count(Topic) %>%
    plot_ly(labels = ~Topic,values = ~n, textinfo = "label+percent",
            showlegend = F) %>% layout(autosize=T) %>% add_pie(hole = 0.5)
}

topicBars<-function(y){
  dat<-data.table(dat)
  colors<-colorspace::terrain_hcl(12,c=c(65,50),l=c(30,120),power=c(1/3,1.5))
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



################ UI ------------------------------

ui <- fluidPage(theme = shinytheme("united"),
                tags$img(src="dd_logo.png", align="right", height = 100, width=250),
                
                h2(strong("Dunkin Donuts Sentiment Analysis")),
                navlistPanel( widths = c(2,10),
                              tabPanel(h4(strong("Overview")),
                                       
                                       mainPanel(
                                         tabsetPanel(type="tabs",
                                                     tabPanel('Descriptive',
                                                              fluidRow(align="left", h4(strong("Tweets' Timeline")),
                                                                       dateRangeInput("date_range", strong("Select the range of dates"),start=min(dat$created_at),end=max(dat$created_at),min=min(dat$created_at),max=max(dat$created_at),format = "yyyy-mm-dd")
                                                              ),
                                                              
                                                              fluidRow(align="center", 
                                                                       plotOutput("plot1", click = "plot_click"),
                                                                       verbatimTextOutput("info")
                                                              ),
                                                              
                                                              fluidRow(align="center",h4(strong("Top hashtags")),
                                                                       plotOutput("plot2", click = "plot_click2"),
                                                                       verbatimTextOutput("info2")
                                                              ),
                                                              fluidRow(align="left",selectInput("year1","Select Year",c(2020,2021,2022))
                                                              ),
                                                              fluidRow(align="center",
                                                                       h4(strong("Topics by Month")),
                                                                       plotlyOutput("plot3")
                                                              ),
                                                              fluidRow(align="center",
                                                                       column(6,
                                                                              h4(strong("Topic Distribution")),
                                                                              plotlyOutput("plot4")
                                                                       ),
                                                                       column(6,
                                                                              h4(strong("Time Posting Distribution")),
                                                                              plotlyOutput("plot7")
                                                                       )
                                                              )
                                                     ),

                                                     tabPanel("Descriptive 2",
                                                              fluidRow(align="center",
                                                                       h4("Length of Tweets Histogram"),
                                                                       plotlyOutput("plot5")
                                                              ),
                                                              fluidRow(align="center",
                                                                       h4("Length of Tweets by Date"),
                                                                       plotlyOutput("plot6")
                                                              ),
                                                              fluidRow(align="center",
                                                                       h4("Sentiment distribution in tweets"),
                                                                       plotlyOutput("plot8")
                                                              ),
                                                              fluidRow(align="center",
                                                                       h4("Weekdays/weekends share of tweets"),
                                                                       plotlyOutput("plot9")
                                                              )
                                                     ),
                                                     tabPanel("Wordcloud"
                                                     )
                                         )
                                         
                                       )
                              ),
                              tabPanel(h4(strong("Engagement")),
                                       
                                       mainPanel(
                                         tabsetPanel(type="tabs",
                                                     tabPanel('Main',
                                                              fluidRow(align="center", h4(strong("Engagement overtime")),
                                                                       plotlyOutput("plot10")
                                                              ), 
                                                              fluidRow(align="center", h4(strong("Engagement vs. Day overtime")),
                                                                       plotlyOutput("plot11")
                                                              )
                                                     ),
                                                     tabPanel('Products',
                                                              
                                                     )
                                                     
                                                     
                                         )
                                         
                                       )
                              ),
                              tabPanel(h4(strong("Engagement Predictor")),
                                       mainPanel(
                                         fluidRow(align="left",
                                                  h4("Please input a desired Tweet"),
                                                  textAreaInput("new_tweet", "", value = "Write tweet...", width = "1000px"),
                                                  actionButton("submit","Submit", icon("twitter"))
                                         ),
                                         fluidRow(align="left",
                                                  textOutput("text1")
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
      paste0("Number of tweets=", round(e$y, 0))
    }
    paste0(y_str(input$plot_click))
  })
  
  output$plot2 <- renderPlot({
    dat %>% 
      unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
      filter(str_detect(hashtag, "^#")) %>%
      count(hashtag, sort = TRUE) %>%
      top_n(10)%>% mutate(name = fct_reorder(hashtag, n)) %>% 
      ggplot( aes(x=name, y=n)) + geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) + 
      coord_flip() + xlab("") + theme_bw()
  })
  
  output$info2 <- renderText({
    y_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("Number of tweets=", round(e$y, 0))
    }
    paste0(y_str(input$plot_click2))
  })
  
  ########################## Wajih Part ends here ####################
  plottopicbars<-reactive({topicBars(input$year1)}) 
  output$plot3 <- renderPlotly({
    plottopicbars()
  })
  
  plottopicpie<-reactive({topicPie(input$year1)}) 
  output$plot4 <- renderPlotly({
    plottopicpie()
  })
  
  output$plot5 <- renderPlotly({
    plot_ly(dat,x=~tweet_length,type="histogram", marker=list(color="#151F47")) %>%
      layout(bargap=0.01,yaxis = list(title = 'Count'),xaxis = list(title = 'Tweet Length'))
    
  })
  
  output$plot6 <- renderPlotly({
    plot_ly(dat, x = ~created_at, y = ~tweet_length, color = ~Topic,colors=colorspace::terrain_hcl(7,c=c(65,50),l=c(30,80),power=c(1/3,1.5))) %>%
      layout(yaxis = list(title = 'Tweet Length (characters)'),xaxis = list(title = ''),legend = list(orientation="h"))
    
  })
  
  output$plot7 <- renderPlotly({
    
    dat %>% 
      group_by(time_of_day) %>% 
      summarize(count = n()) %>% plot_ly(labels = ~time_of_day, values = ~count, textinfo = "label+percent",
                                         marker = list(colors = c('rgb(227,35,139)', 'rgb(104,56,23)','rgb(0,104,87)'))) %>% 
      add_pie(hole = 0.5) %>% layout(title = "",  showlegend = F,
                                     xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$plot8 <- renderPlotly({
    
    plot_ly(data_sentiment, x = ~text, y = ~n, type = 'bar',text=~Perc_freq,
            marker = list(color = 'rgb(158,202,225)',
                          line = list(color = 'rgb(8,48,107)',
                                      width = 1.5))) %>% 
      layout(title = "",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
  })
  
  output$plot9 <- renderPlotly({
    
    fig <- plot_ly(data, x = ~years, y = ~weekend, type = 'bar', name = 'Weekend',marker = list(color ="E11383"))
    fig <- fig %>% add_trace(y = ~weekdays, name = 'Weekdays',marker = list(color ="F5821F"))
    fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')
    fig
  })
  
  hello1<-reactive({ 
    hello(input$new_tweet)
  }) 
  observeEvent(input$submit, {
    output$text1<-renderText({hello1()})
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
  # output$plot3 <- renderPlotly({
  #   firstAct<- dtmart[FirstAct>=input$date_start&FirstAct<=input$date_end][,.N,by=FirstAct][order(FirstAct)]
  #   plot_ly(firstAct, type = 'scatter', mode = 'lines')%>%
  #     add_trace(x = ~FirstAct, y = ~N,marker = list(color = "rgb(21, 31, 71)"),line = list(color ='rgb(21,31,71')) %>%
  #     layout(showlegend = F,yaxis=list(title="Number of Users"),xaxis=list(title="")) 
  #   })
  # 
  # output$plot4 <- renderPlotly({
  #   plot_ly(alpha=0.4) %>% add_histogram(dtmart[FirstPay>=input$date_start&FirstPay<=input$date_end]$FirstPay, 
  #                                        name="First Pay", marker = list(color = "rgba(21, 31, 71,0.8)")) %>%
  #     add_histogram(dtmart[FirstAct>=input$date_start&FirstAct<=input$date_end]$FirstAct, 
  #                   name="First Activity",marker = list(color = "rgba(254, 95, 48,0.8)")) %>% 
  #     layout(barmode="stack",legend = list(x=0.5,y=0.9))
  #   
  # })
  # output$plot5 <- renderPlotly({
  #   plot_ly(alpha=0.4) %>% 
  #     add_histogram(dtmart[interval_firstPay<quantile(interval_firstPay,0.95)]$interval_firstPay, 
  #                   name="Interval First Pay", marker = list(color = "rgb(21, 31, 71)")) %>%
  #     layout(xaxis=list(title="Days"),yaxis=list(title="Number of Users"))
  # })
  # 
  # output$mapPlot <- renderPlotly({
  #   
  #   plot_ly(dfmap,type='choropleth', locations=dfmap$CODE, z=dfmap$n, text=dfmap$Country_Name, colorscale="Greens")
  # })
  # 
  # output$plot6 <- renderPlotly({
  #   if (input$removeGerman==TRUE){
  #     dtmart %>%filter(Language_Name!="German")%>% count(Language_Name) %>% 
  #       plot_ly(x=~Language_Name,y=~n) %>% add_bars(marker = list(color = c(colors,colors,colors,colors))) %>%
  #       layout(xaxis = list(title="Language",categoryorder = "total descending"),yaxis=list(title="Users"))
  #   }
  #   else {
  #     dtmart %>% count(Language_Name) %>% 
  #       plot_ly(x=~Language_Name,y=~n) %>% add_bars(marker = list(color = c(colors,colors,colors,colors))) %>%
  #       layout(xaxis = list(title="Language",categoryorder = "total descending"),yaxis=list(title="Users"))
  #   }
  #   
  # })
  # 
  # output$plot7 <- renderPlotly({
  #   if (input$removeGermany==TRUE){
  #     topCountries<-names(tail(sort(table(dtmart[Country_Name!="Germany"]$Country_Name)),10))
  #     
  #     dtmart %>% filter(Country_Name %in%topCountries)%>% count(Country_Name) %>% arrange(n)  %>% 
  #       plot_ly(x = ~Country_Name,y = ~n) %>% add_bars(marker = list(color = c("rgb(21, 31, 71)"))) %>%
  #       layout(xaxis = list(title="Country",categoryorder = "total descending"),yaxis=list(title="Users"))
  #     
  #   }
  #   else {
  #     topCountries<-names(tail(sort(table(dtmart$Country_Name)),10))
  #     
  #     dtmart %>% filter(Country_Name %in%topCountries)%>% count(Country_Name) %>% arrange(n)  %>% 
  #       plot_ly(x = ~Country_Name,y = ~n) %>% add_bars(marker = list(color = c("rgb(21, 31, 71)"))) %>%
  #       layout(xaxis = list(title="Country",categoryorder = "total descending"),yaxis=list(title="Users"))
  #   }
  #   
  # })
  # output$plotApps <- renderPlotly({
  #   topApps<-names(tail(sort(table(dtmart$Application_Name)),10))
  #   
  #   dtmart %>% filter(Application_Name %in%topApps) %>%count(Application_Name) %>%
  #     plot_ly(labels = ~Application_Name,values = ~n, textinfo = "label+percent",
  #             showlegend = F) %>% add_pie(hole = 0.4)
  # })
  # 
  # #text outputs
  # 
  # output$UsersDailyGen<-renderText({format(round(mean(users_daily$Users),0),big.mark=',')})
  # output$UsersDailyGenP<-renderText({format(round(mean(users_daily_p$Poker_Users),0),big.mark=',')})
  # 
  # ############### second page bets 
  # 
  # #plots
  # 
  # output$plot8 <- renderPlotly({
  #   if (input$removeSport==TRUE){
  #     plot_ly(mean_daily[!agrepl("Sports",Product)],x = ~Product,y = ~Average) %>% 
  #       add_bars(marker = list(color = c("rgb(21, 31, 71)"))) %>%
  #       layout(yaxis=list(title="Daily Users"))
  #   }
  #   else {
  #     plot_ly(mean_daily,x = ~Product,y = ~Average) %>% 
  #       add_bars(marker = list(color = c("rgb(254, 95, 48)","rgb(21, 31, 71)"))) %>%
  #       layout(yaxis=list(title="Daily Users"))
  #   }
  #   
  # })
  # 
  # output$plot9 <- renderPlotly({
  #   
  #   plot_ly(users_daily, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
  #     add_trace(x = ~Date, y = ~Users,line = list(color ='rgb(21, 31, 71)'),
  #               fillcolor='rgba(50, 100, 250, 0.4)') %>%
  #     layout(showlegend = F)
  # })
  # 
  # output$plot10 <- renderPlotly({
  #   
  #   if (input$removeSport==TRUE) {
  #     byprod<- dtmart[,.(CasinoBossMedia=sum(CasinoBossMedia),CasinoChartwell=sum(CasinoChartwell),
  #                        GamesBwin=sum(GamesBwin),GamesVS=sum(GamesVS),
  #                        Supertoto=sum(Supertoto)),by=MostActiveDay][!is.na(MostActiveDay)]
  #     plot_ly(byprod, x=~MostActiveDay,y=~GamesBwin, name="Games Bwin", type="bar") %>%
  #       add_trace(y=~GamesVS, name="Games VS" ) %>%
  #       add_trace(y=~CasinoBossMedia, name="Casino Boss Media" ) %>%
  #       add_trace(y=~CasinoChartwell, name="Casino Chartwell" ) %>%
  #       add_trace(y=~Supertoto, name= "Supertoto"  )  %>% 
  #       layout(yaxis = list(title = 'Count'),xaxis = list(title = "",
  #                                                         categoryorder = "total descending"), barmode = 'stack')
  #   }
  #   else {
  #     byprod<- dtmart[,.(CasinoBossMedia=sum(CasinoBossMedia),CasinoChartwell=sum(CasinoChartwell),
  #                        GamesBwin=sum(GamesBwin),GamesVS=sum(GamesVS),SportsBookFixed=sum(SportsBookFixed),
  #                        SportsBookLive=sum(SportsBookLive),Supertoto=sum(Supertoto)),by=MostActiveDay][!is.na(MostActiveDay)]
  #     plot_ly(byprod, x=~MostActiveDay,y=~SportsBookFixed, name="Sports Book Fixed", type="bar") %>%
  #       add_trace(y=~SportsBookLive, name="Sports Book Live") %>%
  #       add_trace(y=~GamesBwin, name="Games Bwin") %>%
  #       add_trace(y=~GamesVS, name="Games VS" ) %>%
  #       add_trace(y=~CasinoBossMedia, name="Casino Boss Media" ) %>%
  #       add_trace(y=~CasinoChartwell, name="Casino Chartwell" ) %>%
  #       add_trace(y=~Supertoto, name= "Supertoto"  )  %>% 
  #       layout(yaxis = list(title = 'Count'),xaxis = list(title = "",
  #                                                         categoryorder = "total descending"), barmode = 'stack')
  #   }
  #  
  #   
  # })
  # 
  # daily2<-reactive({summary_daily(prod=input$selProd,start=input$date_start2,end=input$date_end2)}) 
  # 
  # 
  # output$plot11 <- renderPlotly({
  #   
  #   plot_ly(
  #     domain = list(x = c(0, 100), y = c(0, 100)),
  #     value = mean(daily2()$casino_profitability,na.rm=T)*100,
  #     title = list(text = ""), type = "indicator",
  #     mode = "gauge+number+delta", number = list(suffix = "%"),
  #     gauge = list(bar = list(color = "rgb(21,31,71)"),
  #                  bordercolor = "gray"),
  #     delta = list(reference = 15, increasing = list(color = "darkgreen"),decreasing=list(color = "darkred"))) %>%
  #     layout(margin = list(l=20,r=30))
  # })
  # 
  # output$plot12 <- renderPlotly({
  #   daily2() %>% count(one_time_player) %>%
  #     plot_ly(x = ~one_time_player,y = ~n) %>% 
  #     add_bars(marker = list(color = c("rgb(254, 95, 48)","rgb(21, 31, 71)"))) %>%
  #     layout(xaxis=list(title=""),yaxis=list(title="Count"))
  #   
  # 
  # })
  # 
  # output$plot13 <- renderPlotly({
  #   
  #   daily2() %>% count(MostActiveDay) %>%
  #     plot_ly(labels = ~MostActiveDay,values = ~n, type = "pie", textinfo = "label+percent",
  #             marker = list(color = c("#151F47", "#AA3344")),
  #             showlegend = F)
  # })
  # 
  # output$plot14 <- renderPlotly({
  #   
  #   plot_ly(alpha=0.4) %>% add_histogram(daily2()$Recency, marker = list(color = "rgba(27, 63, 10,0.8)")) %>%
  #     layout(barmode="stack",legend = list(orientation = 'v'),xaxis=list(title="Days"),yaxis=list(title="Number of Users"))
  #   
  # })
  # 
  # 
  # 
  # #text outputs
  # users_daily_prod<-reactive({
  #   daily[Product_Name==input$selProd][Date>=input$date_start2&Date<=input$date_end2][,.(Users=uni(UserID)),by=Date]
  #   }) 
  # 
  # output$DailyUsers<-renderText({format(round(mean(users_daily_prod()$Users,na.rm=T),0),big.mark=',')})
  # output$TotStakes<-renderText({format(round(sum(daily2()$Stakes,na.rm=T),2),big.mark=',')})
  # output$TotWins<-renderText({format(round(sum(daily2()$Winnings,na.rm=T),2),big.mark=',')})
  # output$TotBal<-renderText({format(round(sum(daily2()$Stakes,na.rm=T)-sum(daily2()$Winnings,na.rm=T),2),big.mark=',')})
  # 
  # output$MeanBets<-renderText({format(round(mean(daily2()$meanBets,na.rm=T),0),big.mark=',')})
  # output$MeanStakes<-renderText({format(round(mean(daily2()$StakesBet,na.rm=T),2),big.mark=',')})
  # output$MeanWins<-renderText({format(round(mean(daily2()$WinningsBet,na.rm=T),2),big.mark=',')})
  # output$MeanBal<-renderText({format(round(mean(daily2()$BalanceBet,na.rm=T),2),big.mark=',')})
  # 
  # ############### third page poker 
  # 
  # output$plot15 <- renderPlotly({
  #   if (input$removeOutliers==TRUE){
  #     plot_ly(poker[MeanBuy<quantile(MeanBuy,0.98,na.rm=T)], y=~MeanBuy, color= ~PrefTimeOfDay, type="box")  %>% 
  #       layout(showlegend = F)
  #   }
  #   else {
  #     plot_ly(poker, y=~MeanBuy, color= ~PrefTimeOfDay, type="box")  %>% 
  #       layout(showlegend = F)
  #   }  
  #   
  # })
  # 
  # output$plot16 <- renderPlotly({
  #   if (input$removeOutliers==TRUE){
  #     plot_ly(poker[MeanSell<quantile(MeanSell,0.98,na.rm=T)], y=~MeanSell, color= ~PrefTimeOfDay, type="box") %>%
  #       layout(showlegend=F)
  #   }
  #   else {
  #     plot_ly(poker, y=~MeanSell, color= ~PrefTimeOfDay, type="box") %>%
  #       layout(showlegend=F)
  #   }
  #   
  # })
  # 
  # chipss2<-reactive({
  #   summary_chips2(start=input$date_start3,end=input$date_end3)
  # }) 
  # 
  # output$plot17 <- renderPlotly({
  #  
  #   plot_ly(chipss2(), x=~Sells, y=~Buys, color=~time_of_day, size=~Buys, marker = list(opacity = 0.8),
  #           sizes=c(10,400))
  #   
  # })
  # 
  # output$plot18 <- renderPlotly({
  #   if (input$removeOutliers==TRUE){
  #     plot_ly(data = poker[MeanBuy<quantile(MeanBuy,0.98,na.rm=T)],
  #             x = ~MostActiveDayPoker, y = ~round(BalancePoker,2),
  #             color = ~PrefTimeOfDay, colors=colors) %>% 
  #       layout(yaxis=list(title="Balance"),xaxis=list(title=""),legend = list(orientation="h"))
  #   }
  #   else {
  #     plot_ly(data = poker,
  #             x = ~MostActiveDayPoker, y = ~round(BalancePoker,2),
  #             color = ~PrefTimeOfDay, colors=colors) %>% 
  #       layout(yaxis=list(title="Balance"),xaxis=list(title=""),legend = list(orientation="h"))
  #   }
  #   
  #   
  # })
  # 
  # output$plot19 <- renderPlotly({
  #   plot_ly(users_daily_p, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
  #     add_trace(x = ~Date, y = ~Poker_Users,line = list(color ='rgb(254, 95, 48)'),
  #               fillcolor='rgba(246, 143, 112, 0.4)') %>%
  #     layout(showlegend = F, yaxis=list(title="Poker Users"))
  #   
  # })
  # 
  # output$plot20 <- renderPlotly({
  #   poker %>% count(Player_Category) %>%
  #     plot_ly(labels = ~Player_Category,values = ~n, type = "pie", textinfo = "label+percent",
  #             showlegend = F)
  #   
  # })
  # 
  # output$plot21 <- renderPlotly({
  #   if (input$removeOutliers2==TRUE){
  #     poker[sell_ratio<quantile(sell_ratio,0.98,na.rm=T)] %>% 
  #       plot_ly(x=~Player_Category,y=~sell_ratio,type = "box",color= ~Player_Category) %>%
  #       layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))
  #     
  #   }
  #     else {
  #      poker %>% 
  #         plot_ly(x=~Player_Category,y=~sell_ratio,type = "box",color= ~Player_Category) %>%
  #         layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))
  #       
  #     }
  #   
  # })
  # 
  # output$plot22 <- renderPlotly({
  #   poker2$NewCat<-categories(poker2[,input$var1],poker2[,input$var2])
  #   poker2 %>% count(NewCat) %>%
  #     plot_ly(labels = ~NewCat,values = ~n, type = "pie", textinfo = "label+percent",
  #             showlegend = F)
  #   
  # })
  # 
  # output$plot23 <- renderPlotly({
  #   poker2$NewCat<-categories(poker2[,input$var1],poker2[,input$var2])
  #   
  #   if (input$removeOutliers2==TRUE){
  #     poker2 %>% filter(sell_ratio<quantile(sell_ratio,0.98,na.rm=T)) %>% 
  #       plot_ly(x=~NewCat,y=~sell_ratio,type = "box",color= ~NewCat) %>%
  #       layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))
  #     
  #   }
  #   else {
  #     poker2 %>% 
  #       plot_ly(x=~NewCat,y=~sell_ratio,type = "box",color= ~NewCat) %>%
  #       layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))
  #     
  #   }
  #   
  # })
  # 
  # #text outputs
  # users_daily_pok<-reactive({
  #   summary_chips(start=input$date_start3,end=input$date_end3)
  # }) 
  # 
  # output$DailyUsersP<-renderText({format(round(mean(users_daily_pok()$Users,na.rm=T),0),big.mark=',')})
  # output$TotBuys<-renderText({format(round(sum(users_daily_pok()$Buys,na.rm=T),2),big.mark=',')})
  # output$TotSells<-renderText({format(round(sum(users_daily_pok()$Sells,na.rm=T),2),big.mark=',')})
  # 
  # output$MeanBuys<-renderText({format(round(mean(users_daily_pok()$MeanBuys,na.rm=T),2),big.mark=',')})
  # output$MeanSells<-renderText({format(round(mean(users_daily_pok()$MeanSell,na.rm=T),2),big.mark=',')})
  
} 

shinyApp(ui = ui, server = server)
