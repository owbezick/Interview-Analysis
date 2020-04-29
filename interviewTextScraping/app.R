#
# Leader Interview Shiny Application
#
# Author: Owen Bezick
#


# Source Libraries
source("dataIntake.R", local = TRUE)
source("bushnellData.R", local = TRUE)
#libraries
source("libraries.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Rwandan Genocide Interviews", titleWidth = "350px"
  )
  , dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "welcome", text = "Welcome", icon = icon("info")) 
      , menuItem(tabName = "leader", text = "Leader Interview", icon = icon("search"))
      , menuItem(tabName = "bushnell", text = "Bushnell Interview", icon = icon("search"))
    )
  )
  , dashboardBody( 
    tabItems(
      tabItem(
        tabName = "welcome"
        , fluidRow(
           box(width = 12, status = "primary", title = "The Sentiment Analysis of US Diplomats Around the Rwandan Genocide"
               , textOutput("info")
          )
          , box(width = 12, status = "primary", title = "Acknowledgements"
                , textOutput("thank")
          )
        )
        , fluidRow(
          box(width = 12, status = "primary", title = "Interview PDF"
              , column(width = 6
                       , uiOutput("leaderPDF")
              )
              , column(width = 6
                       , uiOutput("bushnellPDF")
              )
          )
        )
      )
      , tabItem(
        tabName = "leader"
        , fluidRow(
          box(width = 12, status ="primary"
              , textOutput("joyce"))
          , tabBox(width = 12, title = "Overall", side = c("right")
                 , tabPanel("Entire Interview"
                            , echarts4rOutput("overallSentimentLeader")
                 )
                 , tabPanel("Key Words"
                            , echarts4rOutput("keyTermsSentimentLeader")
                 )
          )
        )
        , fluidRow(
          tabBox(width = 12, title = "High Sentiment Answers", side = c("right")
                 , tabPanel("Entire Interview"
                            , DTOutput("entireInterviewAnswersLeader"))
                 , tabPanel("Key Words"
                            , DTOutput("keyWordAnswersLeader"))
          )
        )
      )
      , tabItem(
        tabName = "bushnell"
        , fluidRow(
          box(width = 12, status ="primary"
              , textOutput("bushnell"))
          , tabBox(width = 12, title = "Overall", side = c("right")
                 , tabPanel("Entire Interview"
                            , echarts4rOutput("overallSentimentBushnell")
                 )
                 , tabPanel("Key Words"
                            , echarts4rOutput("keyTermsSentimentBushnell")
                 )
          )
        )
        , fluidRow(
          tabBox(width = 12, title = "High Sentiment Answers", side = c("right")
                 , tabPanel("Entire Interview"
                            , DTOutput("entireInterviewAnswersBushnell"))
                 , tabPanel("Key Words"
                            , DTOutput("keyWordAnswersBushnell"))
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$info <- renderText("Hello! My name is Cadie B. McNaboe, and I am a Political Science and Anthropology major at Davidson College.
                            This sentiment (thematic) analysis focuses on oral histories 
                            (interviews done by the Association for Diplomatic Studies & Training) 
                            about Ambassador Joyce Leader, a prominent American diplomat who served 
                            in Rwanda as Deputy Chief of Mission. She served from 1991-1994, working 
                            on the attempts at peace via the Arusha Accords, and was in Rwanda leading 
                            up to the genocide. This analysis is part of a longer project exploring the 
                            American presence relating to the Rwandan Genocide. ")
  
  output$thank <- renderText("This project would not have been possible without the assistance of three individuals.
                             Thank you to my professor, Jane Zimmerman, for her mentorship and wealth of information 
                             on the Rwandan Genocide and recommendations for research resources. I'd also like to thank 
                             Dr, Che Smith for her guidance and brainstorming throughout this project, and for being an
                             incredible resource to help me understand and imagine an output such as this one. Lastly, 
                             I'd like to thank Owen Bezick for his creation of this website, which is significantly better
                             than the word documents I was going to put it in. This website, and many of its components, 
                             would not be possible without him.")
  output$leaderPDF <- renderUI({
    tags$iframe(style="height:600px; width:100%", src = "https://www.adst.org/OH%20TOCs/Leader,%20Joyce%20E.toc.pdf")
  })
  
  output$joyce <- renderText("As you can see below, this bar chart exists in two different formats. The default chart 
                             for this Joyce Leader interview is the sentiment of the entire interview. After running 
                             the sentiment for this interview, I began to reflect on why the positive sentiment more 
                             than doubled the negative sentiment. I realized then that the interview (displayed on the 
                             main page) covers the plan of her life until 2003, when the interview occurred. To have a 
                             better understanding of the sentiment specifically related to the Rwandan Genocide, I ran
                             the sentiment only with answers that included the key words of 'Rwanda', 'Rwandan', or 'Genocide'. 
                             Any answer that included at least one of these terms was included in the Key Words sentiment analysis,
                             which can be access via the Key Words tab. As you can see below, this presented a more balanced 
                             sentiment, with fear becoming a more present sentiment for the reduced interview." )
  
  output$bushnell <- renderText("As you can see below, this bar chart exists in two different formats. The default chart 
                                for this Prudence Bushnell interview is the sentiment of the entire interview. After running
                                the sentiment for this interview, I began to reflect on why the positive sentiment was 
                                substantially more the negative sentiment. I realized then that the interview (displayed 
                                on the main page) covers the plan of her life until 2005, when the interview occurred. To 
                                have a better understanding of the sentiment specifically related to the Rwandan Genocide, 
                                I ran the sentiment only with answers that included the key words of 'Rwanda', 'Rwandan', or
                                'Genocide'. Any answer that included at least one of these terms was included in the Key Words
                                sentiment analysis, which can be access via the Key Words tab. As you can see below, this presented 
                                a more balanced sentiment, with fear becoming a much more present sentiment for the reduced interview. ")
  output$bushnellPDF <- renderUI({
    tags$iframe(style="height:600px; width:100%", src = "https://www.adst.org/OH%20TOCs/Bushnell,%20Prudence.toc.pdf")
  })
  #LEADER
  output$overallSentimentLeader <- renderEcharts4r({
    sentimentBar(overall_sentiment, "Entire Interview Sentiment")
  })
  
  output$keyTermsSentimentLeader <- renderEcharts4r({
    sentimentBar(key_words_chart_df, "Answers Mentioning Rwanda & Genocide")
  })
  
  output$entireInterviewAnswersLeader <- renderDT({
    all_answers_sorted %>%
      select(`Summed Sentiment` = overall_sentiment, Answer = answer) %>%
      datatable(rownames = F)
  })
  
  output$keyWordAnswersLeader <- renderDT({
    key_words_sorted %>%
      select(`Summed Sentiment` = overall_sentiment, Answer = answer) %>%
      datatable(rownames = F)
  })
  #BUSHNELL
  output$overallSentimentBushnell <- renderEcharts4r({
    sentimentBar(overall_sentiment_b, "Entire Interview Sentiment")
  })
  
  output$keyTermsSentimentBushnell <- renderEcharts4r({
    sentimentBar(key_words_chart_df_b, "Answers Mentioning Rwanda & Genocide")
  })
  
  output$entireInterviewAnswersBushnell <- renderDT({
    all_answers_sorted_b %>%
      select(`Summed Sentiment` = overall_sentiment, Answer = answer) %>%
      datatable(rownames = F)
  })
  
  output$keyWordAnswersBushnell <- renderDT({
    key_words_sorted_b %>%
      select(`Summed Sentiment` = overall_sentiment, Answer = answer) %>%
      datatable(rownames = F)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)