library(tidyverse)
library(pdftools)
library(tidytext)
library(tm)
library(qdap)
library(qdapRegex)
library(rlist)
library(tokenizers)
library(stopwords)

# Import Leader pdf file
raw_lines <- pdf_text("leader_interview.pdf") %>%
  read_lines()

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

# BUGS: replace contraditions, remove stopwards
# Clean Corpus Function
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removeWords, c(stopwords("en")))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, content_transformer(replace_contraction))
  corpus <- tm_map(corpus, content_transformer(replace_number))
  return(corpus)
}

# Clean Questions
clean_answers <- clean_corpus(answers_corpus)

# Check differnce of first question
clean_answers[[1]]$content
clean_answers[[100]]$content

# Create term-doc matrix
answer_tdm <- TermDocumentMatrix(clean_answers)
answer_m <- as.matrix(answer_tdm)

# Term frequency
q_term_freq <- sort(rowSums(answer_m), decreasing = T)
# Show top 10
q_term_freq[1:100]

# Key Words: Genocide, rwanda/n
# Tokenization 
# Questions
question_tokens <- tokenize_words(questions, stopwords = stopwords::stopwords("en"))
# Answers
answer_tokens <- tokenize_words(answers, stopwords = stopwords::stopwords("en"))

