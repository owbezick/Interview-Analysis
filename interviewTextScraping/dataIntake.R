library(tidyverse)
library(pdftools)
library(tidytext)
#install.packages('tm')
library(tm)
install.packages("qdapRegex")
library(qdapRegex)

# Import Leader pdf file
raw_lines <- pdf_text("Leader, Joyce E.toc.pdf") %>%
  read_lines()

raw_interview <- raw_lines[149:3429]

# Append into single
str_interview = ""
for(line in raw_interview) {
  str_interview = paste(str_interview, line)
}

# Trim/ Strip white space
interview_trimmed_white_space <- trimws(stripWhitespace(str_interview))

# Extract Questions
questions <- rm_between_multiple(interview_trimmed_white_space, left = 'Q:', right = 'LEADER:', extract = T)

# Extract Answers
answers <- rm_between_multiple(interview_trimmed_white_space, left = 'LEADER:', right = 'Q:', extract = T)


