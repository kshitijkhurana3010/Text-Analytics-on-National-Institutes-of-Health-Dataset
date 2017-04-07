setwd("E:/Rise Spring 2017/Advance Business Intelligence/Assignment # 3")
library(data.table)

data <- fread("psychcentral_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 



library(dplyr)
library(tidytext)

colnames(data)

data %>% unnest_tokens(word,q_content)

data[1:5]

tidy_data<-data %>% unnest_tokens(word,q_content)
tidy_data[1:20]

data("stop_words")

tidy_data<- tidy_data %>%
  anti_join(stop_words)

tidy_data %>% count(word,sort=TRUE)

library(ggplot2)
tidy_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()




library(SnowballC)
tidy_data <- data %>%
  unnest_tokens(word, q_content) %>%
  mutate(word = wordStem(word)) 

tidy_data<- tidy_data %>%
  anti_join(stop_words)

tidy_data %>% count(word,sort=TRUE)

library(ggplot2)
tidy_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

library(wordcloud)

tidy_data %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))


install.packages("reshape2", repos = "https://cran.r-project.org")
library(reshape2)

tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

##answers


tidy_data<-data %>% unnest_tokens(word,answers)
tidy_data[1:20]

data("stop_words")

tidy_data<- tidy_data %>%
  anti_join(stop_words)

tidy_data %>% count(word,sort=TRUE)

library(ggplot2)
tidy_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()



library(SnowballC)
tidy_data <- data %>%
  unnest_tokens(word, answers) %>%
  mutate(word = wordStem(word)) 

tidy_data<- tidy_data %>%
  anti_join(stop_words)

tidy_data %>% count(word,sort=TRUE)

library(ggplot2)
tidy_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 6000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

library(wordcloud)

tidy_data %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))


install.packages("reshape2", repos = "https://cran.r-project.org")
library(reshape2)


tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)


##Topic Modellng
library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$q_content), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 5) # k is the number of topics to be found.
library(tidytext)
lda_td <- tidy(lda)
lda_td


top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#K=2
lda_2 <- LDA(dtm.new, k = 2) # k is the number of topics to be found.
lda_td_2 <- tidy(lda_2)
lda_td_2


top_terms_2 <- lda_td_2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_2 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#K=3
lda_3 <- LDA(dtm.new, k = 3) # k is the number of topics to be found.
lda_td_3 <- tidy(lda_3)
lda_td_3


top_terms_3 <- lda_td_3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_3 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#K=4
lda_4 <- LDA(dtm.new, k = 4) # k is the number of topics to be found.
lda_td_4 <- tidy(lda_4)
lda_td_4


top_terms_4 <- lda_td_4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_4 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#K=10
lda_10 <- LDA(dtm.new, k = 10) # k is the number of topics to be found.
lda_td_10 <- tidy(lda_10)
lda_td_10


top_terms_10 <- lda_td_10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_10 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


##Topic-modelling

library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 10) # k is the number of topics to be found.
lda_td_1_10 <- tidy(lda)
lda_td_1_10


top_terms_1_10 <- lda_td_1_10 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_1_10 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k=2

lda_2 <- LDA(dtm.new, k = 2) # k is the number of topics to be found.
lda_td_1_2 <- tidy(lda_2)
lda_td_1_2


top_terms_1_2 <- lda_td_1_2 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_1_2 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k=8

lda_8 <- LDA(dtm.new, k = 8) # k is the number of topics to be found.
lda_td_1_8 <- tidy(lda_8)
lda_td_1_8


top_terms_1_8 <- lda_td_1_8 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_1_8 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

#k=11

lda_11 <- LDA(dtm.new, k = 11) # k is the number of topics to be found.
lda_td_1_11 <- tidy(lda_11)
lda_td_1_11


top_terms_1_11 <- lda_td_1_11 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_1_11 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


#k=14

lda_14 <- LDA(dtm.new, k = 14) # k is the number of topics to be found.
lda_td_1_14 <- tidy(lda_14)
lda_td_1_14


top_terms_1_14 <- lda_td_1_14 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms_1_14 %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
