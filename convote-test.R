library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(dbplyr)
library(tidyr)
library(ggplot2)

filePath <- "./convote_v1.1/data_stage_one/training_set/"
files <- list.files(filePath)

analyzeSpeech <- function(file) {
  # Read the file in and split its tokens
  text <- readLines(paste0(filePath, file))
  congressText <- tibble(line = 1:NROW(text), text = text)
  tidy_congress <- congressText %>%
    unnest_tokens(word, text)
  
  # Get the sentiment using bing
  congressSentiment <- tidy_congress %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, index = line, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative)
  return(congressSentiment)
}

runAnalyzation <- function() {
  netSentiments <- structure(list(File = character(),
                          Sentiment = integer()),
                     class = "data.frame")
  # Run analyzeSpeech for each statement
  for(f in files) {
    try(sen <- analyzeSpeech(f), TRUE)
    netSentiments[NROW(netSentiments) + 1,] = list(f, sum(sen$sentiment))
    #try(ggplot(sen, aes(index, sentiment, fill = word)) +
    #  geom_col(show.legend = FALSE), TRUE)
  }
  
  return(netSentiments)
}

# Get the analysis
sentiments <- runAnalyzation()

# Normalize the results
maxSentiment <- max(sentiments$Sentiment)
for(rownum in 1:NROW(sentiments)) {
  sentiments[rownum, "Sentiment"] = sentiments[rownum, "Sentiment"] /
    maxSentiment
}

# Plot sentiments
ggplot(sentiments, aes(File, Sentiment)) +
  geom_col(show.legend = FALSE)
print(paste("Net Sentiment Total:", sum(sentiments$Sentiment), " "))

#ggplot(test_sentiment, aes(index, sentiment, fill = word)) +
#  geom_col(show.legend = FALSE) +
#  facet_wrap(~word, ncol = 2, scales = "free_x")