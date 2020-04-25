#
# Leader Interview Shiny Application
#
# Author: Owen Bezick
#


# Source Libraries
source("dataIntake.R", local = TRUE)
source("libraries.R", local = TRUE)

ui <- dashboardPage(
  dashboardHeader(title = "Leader Interview" 
  )
  , dashboardSidebar(
    sidebarMenu(
      menuItem(tabName = "welcome", text = "Welcome", icon = icon("info")) 
      , menuItem(tabName = "sentimentAnalysis", text = "Sentiment Analysis", icon = icon("search"))
    )
  )
  , dashboardBody( 
    tabItems(
      tabItem(
        tabName = "welcome"
        , fluidRow(
          box(width = 12, status = "primary", title = "Info"
              , textOutput("info")
          )
        )
        , fluidRow(
          box(width = 12, status = "primary", title = "Full Interview PDF"
              # , tags$iframe(style="height:500px; width = 100%; scrolling = yes"
              #               , src="")
              )
        )
      )
      , tabItem(
        tabName = "sentimentAnalysis"
        , fluidRow(
          tabBox(width = 12, title = "Overall", side = c("right")
                 , tabPanel("Entire Interview"
                            , echarts4rOutput("overallSentiment")
                 )
                 , tabPanel("Key Words"
                            , echarts4rOutput("keyTermsSentiment")
                 )
          )
        )
        , fluidRow(
          tabBox(width = 12, title = "High Sentiment Answers", side = c("right")
                 , tabPanel("Entire Interview"
                            , DTOutput("entireInterviewAnswers"))
                 , tabPanel("Key Words"
                            , DTOutput("keyWordAnswers"))
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  output$info <- renderText("Some information text")
  output$overallSentiment <- renderEcharts4r({
    sentimentBar(overall_sentiment, "Entire Interview Sentiment")
  })
  
  output$keyTermsSentiment <- renderEcharts4r({
    sentimentBar(key_words_chart_df, "Answers Mentioning Rwanda & Genocide")
  })
  
  output$entireInterviewAnswers <- renderDT({
    all_answers_sorted %>%
      select(`Summed Sentiment` = overall_sentiment, Answer = answer) %>%
      datatable(rownames = F)
  })
  
  output$keyWordAnswers <- renderDT({
    key_words_sorted %>%
      select(`Summed Sentiment` = overall_sentiment, Answer = answer) %>%
      datatable(rownames = F)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)