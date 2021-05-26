#loading the required packages####
library(tidytext)
library(rtweet)
library(SnowballC)
library(dplyr)
library(tidy)
library(ggplot2)
library(tidyverse)
library(twitteR)
library(wordcloud)
library(tm)

#Establishing the connection to Twitter####
consumerKey = 'xxxx'
consumerSecret = 'xxxx'

accessToken = 'xxxx'
accessTokenSecret = 'xxxx'

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#Extracting tweets with hashtag "covid"####
wordExtract = searchTwitter("#covid",n=3000,lang="en")

#Performing Data transformation and cleaning####
ConvertToText = sapply(wordExtract, function(x) x$getText())

corpus_data = Corpus(VectorSource(ConvertToText))

corpus_data = tm_map(corpus_data, removePunctuation)

corpus_data = tm_map(corpus_data, content_transformer(tolower))

corpus_data = tm_map(corpus_data, function(x)removeWords(x,stopwords()))

corpus_data = tm_map(corpus_data, removeWords, c("RT","are","that","verified"))

removeURL = function(x) gsub("http[[:alnum:]]*", "", x)
corpus_data = tm_map(corpus_data, content_transformer(removeURL))

#Preparing Data for WordCloud Plot####
covid_2 = TermDocumentMatrix(corpus_data)
covid_2 = as.matrix(covid_2)
covid_2 = sort(rowSums(covid_2),decreasing = TRUE)

covid_2 = data.frame(word = names(covid_2), freq = covid_2)
head(covid_2,5)

set.seed(1234)
dev.new(width = 1000, height = 1000, unit = "px")
par(bg = "black")
wordcloud(corpus_data, min.freq = 5, max.words = 80,
          scale = c(3.5,.5),colors = brewer.pal(8,"Dark2"), 
random.color = T ,random.order = F, rot.per = 0.35)

#Plotting a Time Series plot of the Twitter Data
zz = twListToDF(wordExtract)
ts_plot(zz, "hours",col = "white",lwd = 1.5)+
  labs(x = NULL, y = NULL,
       title = "Frequency of tweets with a #COVID hashtag",
       subtitle = paste0(format(min(zz$created), "%Y %B %d"), " to ", format(max(zz$created),"%Y %B %d")),
       caption = "Data collected from Twitter's REST API via rtweet") + 
  theme(panel.background = element_rect(colour = "black", fill = "black"),
        panel.grid.major = element_line(color = "grey"),  
        panel.grid.minor = element_line(color = "grey"),
        )

