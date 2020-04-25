#
# Leader Interview Shiny Application
#
# Author: Owen Bezick
#


# Source Libraries
source("dataIntake.R", local = TRUE)

#libraries
source("libraries.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Leader Interview" 
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
          box(width = 12, status = "primary", title = "Welcome"
              , textOutput("info")
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
          tabBox(width = 12, title = "Overall", side = c("right")
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
          tabBox(width = 12, title = "Overall", side = c("right")
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
  output$info <- renderText("This sentiment (thematic) analysis focuses on oral histories 
                            (interviews done by the Association for Diplomatic Studies & Training) 
                            about Ambassador Joyce Leader, a prominent American diplomat who served 
                            in Rwanda as Deputy Chief of Mission. She served from 1991-1994, working 
                            on the attempts at peace via the Arusha Accords, and was in Rwanda leading 
                            up to the genocide. This analysis is part of a longer project exploring the 
                            American presence relating to the Rwandan Genocide. ")
  
  output$leaderPDF <- renderUI({
    tags$iframe(style="height:600px; width:100%", src = "https://www.adst.org/OH%20TOCs/Leader,%20Joyce%20E.toc.pdf")
  })
  
  output$bushnellPDF <- renderUI({
    tags$iframe(style="height:600px; width:100%", src = "https://www.adst.org/OH%20TOCs/Bushnell,%20Prudence.toc.pdf")
  })
  
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)