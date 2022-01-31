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

################ PREPARATION ------------------------------
setwd("C:/Users/warfaoui/OneDrive - IESEG/Desktop/Social Media Analytics/Group Project/Social-Media-Analytics/Social-Media-Analytics-main/App")
dat<-read.csv("final_all_cols.csv")


################ UI ------------------------------

########################## Wajih Part ####################
ui <- fluidPage(theme = shinytheme("united"),
                tags$img(src="dd_logo.png", align="right", height = 80, width=200),

  h2(strong("Dunkin Donuts Sentiment Analysis")),
  navlistPanel( widths = c(2,10),
    tabPanel(h4(strong("Overview")),
          
               mainPanel(
                 tabsetPanel(type="tabs",
                             tabPanel('Descriptive',
                                      fluidRow(align="center", h4(strong("Tweets' Timeline")),
                                        dateRangeInput("date_range", strong("Select the range of dates"),start=min(dat$created_at),end=max(dat$created_at),min=min(dat$created_at),max=max(dat$created_at),format = "yyyy-mm-dd")
                                      ),
                                      
                                      fluidRow(align="center", 
                                        plotOutput("plot1", click = "plot_click"),
                                        verbatimTextOutput("info")
                                      ),
                                      
                                      fluidRow(align="center",h4(strong("Top hashtags")),
                                         plotOutput("plot2", click = "plot_click2"),
                                         verbatimTextOutput("info2")
                                      )),
########################## Wajih Part ends here #################### 

                             tabPanel("Wordcloud",
                                      fluidRow(align="center",
                                               h4(strong("Location of Users")),
                                               plotlyOutput("mapPlot")
                                               ),
                                      fluidRow(align="center", 
                                               splitLayout(cellWidths = c("50%","50%"), h4(strong("Users' Language")),h4(strong("Top 10 Countries"))),
                                               splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot6"),plotlyOutput("plot7")),
                                               h4(strong("Most Used Application")),
                                               plotlyOutput("plotApps")
                                               )
                             )
                             
                 )
                 
               )
               
      
    )),
    tabPanel(h4(strong("Engagement")),
             
             mainPanel(
               tabsetPanel(type="tabs",
                           tabPanel('Main',
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("50%","50%"), h4(strong("Average Daily Users")),h4(strong("Daily Users by Date"))),
                                             splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot8"),plotlyOutput("plot9")),
                                    ),
                                    fluidRow(align="center",
                                      h4("Most Active Days by Product"),
                                      plotlyOutput("plot10")
                                    )
                                    ),
                           tabPanel('Products',
                                    fluidRow(align="center",
                                             column(4,
                                                    selectInput("selProd",strong("Select Product"),unique("Topic 1"))
                                                    ),
                                             column(4,
                                                    dateInput("date_start2", strong("Start Date"),min(dat$created_at),min=min(dat$created_at),max=max(dat$created_at))
                                             ),
                                             column(4,
                                                    dateInput("date_end2", strong("End Date"),"2005-09-30",min=min(dat$created_at),max=max(dat$created_at))                                             
                                                    )
                                             ),
                                    fluidRow(align="center",
                                             column(3,
                                                    h4("Daily Users"),
                                                    h3(strong(textOutput("DailyUsers")))
                                             ),
                                             column(3,
                                                    h4("Total Stakes"),
                                                    h3(strong(textOutput("TotStakes")))
                                             ),
                                             column(3,
                                                    h4("Total Winnings"),
                                                    h3(strong(textOutput("TotWins")))
                                             ),
                                             column(3,
                                                    h4("Total Profit"),
                                                    h3(strong(textOutput("TotBal")))
                                             ),
                                    ),
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("50%","50%"), h4(strong("Profitability (%)")),h4(strong("One Time Players"))),
                                             splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot11"),plotlyOutput("plot12"))
                                    ),
                                    fluidRow(align="center",
                                             column(3,
                                                    h4("Mean Bets per Day"),
                                                    h3(strong(textOutput("MeanBets")))
                                             ),
                                             column(3,
                                                    h4("Mean Stakes per Bet"),
                                                    h3(strong(textOutput("MeanStakes")))
                                             ),
                                             column(3,
                                                    h4("Mean Wins per Bet"),
                                                    h3(strong(textOutput("MeanWins")))
                                             ),
                                             column(3,
                                                    h4("Mean Balance per Bet"),
                                                    h3(strong(textOutput("MeanBal")))
                                             ),
                                    ),
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("40%","60%"), h4(strong("Most Active Day")),h4(strong("Recency"))),
                                             splitLayout(cellWidths = c("40%","60%"), plotlyOutput("plot13"),plotlyOutput("plot14"))
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
  
########################## Wajih Part ####################  
output$plot1 <- renderPlot({
  dat[dat$created_at>= input$date_range[1]& dat$created_at<= input$date_range[2],] %>% ts_plot("months") +
    labs(x = NULL, y = NULL,
         title = "Frequency of Dunckin Donuts official account tweets") + 
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
  # 
  # output$plot2 <- renderPlotly({
  #   dtmart<-data.table(dtmart)
  #   regs<- dtmart[,.N,by=RegDate]
  #   plot_ly(regs, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
  #     add_trace(x = ~RegDate, y = ~N,marker = list(color = "rgb(21, 31, 71)"),
  #               line = list(color ='rgb(21,31,71'),
  #               fillcolor='rgba(50, 100, 250, 0.4)') %>%
  #     layout(showlegend = F, xaxis=list(title=""),yaxis=list(title="Number of registered users"))
  #   })
  # 
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
  # output$totUsers<-renderText({format(nrow(dtmart),big.mark=',')})
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
