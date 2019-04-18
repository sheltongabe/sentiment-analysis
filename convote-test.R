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

# Normalize the results and get a prediction of yes or no
results <- structure(list(Prediction = character(),
                          Actual = character(),
                          Correct = integer()),
                     class = "data.frame")
maxSentiment <- max(sentiments$Sentiment)
for(rownum in 1:NROW(sentiments)) {
  sentiments[rownum, "Sentiment"] = sentiments[rownum, "Sentiment"] /
    maxSentiment
  
  # Get Prediction
  prediction <- "N"
  if(sentiments[rownum, "Sentiment"] > 0) {
    prediction <- "Y"
  }
  else {
    prediction <- "N"
  }
  
  # Get actual and correctness
  actual <- substring(sentiments[rownum, "File"], 22, 22)
  correct <- 0
  if(actual == prediction) {
    correct <- 1
  }
  else {
    correct <- 0
  }
  
  results[NROW(results) + 1,] = list(prediction, actual, correct)
}

# Plot sentiments
ggplot(sentiments, aes(File, Sentiment)) +
  geom_col(show.legend = FALSE)
print(paste("Net Sentiment Total:", sum(sentiments$Sentiment), " "))

write.csv(sentiments, file="sentiments.csv")

# Calculate overall precision
precision <- length(which(results$Correct == 1)) / NROW(results) * 100

print(paste("Precision", precision))

#ggplot(test_sentiment, aes(index, sentiment, fill = word)) +
#  geom_col(show.legend = FALSE) +
#  facet_wrap(~word, ncol = 2, scales = "free_x")