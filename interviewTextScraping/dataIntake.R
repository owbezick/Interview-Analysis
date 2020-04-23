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

