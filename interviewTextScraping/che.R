library(tidyverse)
library(pdftools)
library(tidytext)
#install.packages("textdata")
library(textdata)
#install.packages("wordcloud")
library(wordcloud)

# Import Leader pdf file
Leader <- pdf_text("leader_interview.pdf") %>%
  read_lines()

# Keep lines 149 to the end
Leader <- Leader[149:3429]

# Transform into a data frame with numbered line and text
Leader <- tibble(line = 1:3281, text=Leader)

# Find indices where interviewer begins question
Qs <- which(str_detect(Leader$text, "Q:"))
# Find indices where Leader begins speaking
Ls <- which(str_detect(Leader$text, "LEADER:"))


# If row i is in Qs, then make the text of that line under "question"
# else, make it under "answer"
Leader <- Leader %>%
  mutate(speaker = case_when(
    text %in% text[Qs] ~  "question",
    text %in% text[Ls] ~ "answer"))

# If speaker is NA, then grab the lag value of speaker,
# all the way down the dataset
for (i in 2:nrow(Leader))
{
  ifelse(is.na(Leader$speaker[i]),
         Leader$speaker[i] <- Leader$speaker[i-1],
         Leader$speaker[i] <- Leader$speaker[i])
  
}

# Slice data to keep only important text
Leader_text <- Leader %>%
  filter(speaker == "answer") %>%
  select(-speaker) %>%
  mutate(text = str_replace(text, "LEADER: ",""))


# Tokenize text
df <- Leader_text %>%
  unnest_tokens(word, text)
# Now each line is a single word of text from the interview

# Remove common words (stop words)
df <- df %>%
  anti_join(stop_words)

### end of text processing ###

# Count the most frequent words
df %>%
  count(word, sort=TRUE) %>%
  head(20)

# Make a plot of most frequent words
df %>%
  count(word, sort=TRUE) %>%
  head(20) %>%
  ggplot(aes(x = reorder(word,n), y = n)) +
  geom_col(fill="darkblue") +
  coord_flip()

# How many times is Rwanda mentioned?
nrow(df[which(str_detect(df$word, "rwanda")),])
# Rwanda or Rwandan mentioned 64 times

# Edit dataset so that Rwanda and Rwandan count as the same word
df <- df %>%
  mutate(word = case_when(str_detect(word, "rwanda") == TRUE ~ "rwanda",
                          TRUE ~ word))

# Set up plot so that rwanda is colored differently than other bars
df$is_rwanda <- str_detect(df$word, "rwanda")

# Redo word frequency plot
df %>%
  count(word, sort=TRUE) %>%
  head(20) %>%
  mutate(is_rwanda = str_detect(word, "rwanda")) %>%
  ggplot(aes(x = reorder(word,n), y = n)) +
  geom_col(aes(fill=is_rwanda)) +
  scale_fill_manual(values = c("lightblue","darkblue")) +
  coord_flip() +
  theme_minimal()


# Sentiment analysis
nrc_sentiments <- df %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(sentiment) %>%
  count(sentiment, word, sort=TRUE)

nrc_sentiments %>%
  head(50) %>%
  View()

# Let's look at the "nrc" sentiments
get_sentiments("nrc") %>%
  View()

# Create a word cloud
pal <- brewer.pal(15, "PuOr")

df %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100,
                 colors = pal))

# Lexical dispersion plot
# Plots the occurrence of specific words across the text
df %>%
  filter(word %in% c("genocide", "peace", "murder", "rwanda")) %>%
  ggplot(aes(x = line, y=word)) + geom_point(alpha=0.4)

# Write out the df dataset as a csv
# write_csv(df, "Leader_word_by_line.csv")

# Cut down the size of the dataset to only the last half of the interview
df_trim <- df %>%
  filter(line > 1400)

# Only pull lines 1400 to 2800
df_trim <- df %>%
  filter(line >= 1400 & line <= 2800)