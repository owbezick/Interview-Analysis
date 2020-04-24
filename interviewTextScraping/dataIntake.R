library(tidyverse)
library(dplyr)
library(pdftools)
library(tidytext)
library(tm)
library(qdap)
library(qdapRegex)
library(rlist)
library(tokenizers)
library(stopwords)
library(syuzhet)
library(echarts4r)
# Import Leader pdf file ----
raw_lines <- pdf_text("leader_interview.pdf") %>%
  read_lines()

# Text Preprocessing ----
# Extract interview Lines
raw_interview <- raw_lines[149:3429]

# Append into single string
str_interview = ""
for(line in raw_interview) {
  str_interview = paste(str_interview, line)
}

# Trim/ Strip white space
interview_trimmed_white_space <- trimws(stripWhitespace(str_interview))

# Extract Answers
answers <- rm_between_multiple(interview_trimmed_white_space, left = 'LEADER:', right = 'Q:', extract = T)

# Make corpus of answers
answers_source <- VectorSource(answers)
answers_corpus <- VCorpus(answers_source)

# Clean Corpus Function
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "the", "but"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(replace_number))
  return(corpus)
}

# Clean Answers
clean_answers <- clean_corpus(answers_corpus)

# Create term-doc matrix
answer_tdm <- TermDocumentMatrix(clean_answers)
answer_m <- as.matrix(answer_tdm)

# Term frequency
q_term_freq <- sort(rowSums(answer_m), decreasing = T)
# Show top 100
q_term_freq[1:100]

# Key Words: Genocide, rwanda/n

# Sentiment Analysis ----
# Create dataframe
answers_df <- data.frame(text=unlist(sapply(clean_answers, `[`, "content")), 
                         stringsAsFactors=F)

# Get lists of questions
answers_ls <- answers_df %>%
  select(text) %>%
  pull()

# Sentiment for each question
answer_sentiment <- get_nrc_sentiment(answers_ls)

# Add answer back into sentiment
answer_sentiment$answer = answers_ls

# Overall Sentiment Bar Chart 
sentimentBar <- function(df, title){
  df %>%
    e_chart(chart) %>%
    e_bar("negative", name = "Negative", color = "#543005") %>%
    e_bar("anger", name = "Anger", color = "#8c510a") %>%
    e_bar("disgust", name = "Disgust", color = "#bf812d") %>%
    e_bar("fear", name = "Fear", color = "#dfc27d") %>%
    e_bar("sadness", name = "Sadness", color = "#f6e8c3") %>%
    e_bar("anticipation", name= "Anticipation", color = "#c7eae5") %>%
    e_bar("joy", name = "Joy", color = "#80cdc1") %>%
    e_bar("surprise", name = "Surprise", color = "#35978f") %>%
    e_bar("trust", name = "Trust", color = "#01665e") %>%
    e_bar("positive", name = "Positive", color = "#003c30") %>%
    e_axis_labels(x = "Emotion", y = "Number of Tags") %>%
    e_title(title) %>%
    e_legend(bottom = 0) %>%
    e_tooltip()
}

# Data
overall_sentiment <- answer_sentiment %>%
  summarise(anger = sum(anger)
            , disgust = sum(disgust)
            , fear = sum(fear)
            , joy = sum(joy)
            , sadness = sum(sadness)
            , surprise = sum(surprise)
            , trust = sum(trust)
            , anticipation = sum(anticipation)
            , negative = sum(negative)
            , positive = sum(positive)) %>%
  mutate(chart = "")

overall_sentiment_bar <- sentimentBar(overall_sentiment, "Overall Interview Sentiment")

# Key Word Sentiment
# Find answers with keywords
key_words_df <-answer_sentiment %>%
  mutate(rwanda = ifelse(str_detect(answer, "rwanda"), 1, 0)) %>%
  mutate(genocide = ifelse(str_detect(answer, "genocide"), 1, 0)) %>%
  mutate(rwandan = ifelse(str_detect(answer, "rwandan"), 1, 0)) %>%
  filter(rwanda > 0 | genocide >0 | rwandan >0) %>%
  summarise(anger = sum(anger)
            , disgust = sum(disgust)
            , fear = sum(fear)
            , joy = sum(joy)
            , sadness = sum(sadness)
            , surprise = sum(surprise)
            , trust = sum(trust)
            , anticipation = sum(anticipation)
            , negative = sum(negative)
            , positive = sum(positive)) %>%
  mutate(chart = "")

key_word_sentiment_bar <- sentimentBar(key_words_df, "Answers Mentioning Rwanda & Genocide")
