#library
library(dplyr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(DataExplorer)
library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
library(ggplot2)
library(dplyr)
library(tidytext)

data<- read.csv("Consumer_Complaints.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","N/A","NaN","","?","Not Available")) 


#filter the Credit Reporting data
filtered_credit<-data %>% 
  filter(Product %in% c("Credit reporting"))

#filter the Mortgage data
filtered_mortgage<-data %>% 
  filter(Product %in% c("Mortgage"))


#WORK  ON CREDIT DATA

filtered_credit$Consumer.complaint.narrative <- as.character(filtered_credit$Consumer.complaint.narrative)


#Tokenization
tidy_text <- filtered_credit %>%
  unnest_tokens(word, Consumer.complaint.narrative)


str(filtered_credit)

#Stop words removal
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)

#Remove the words that doesn't make any sense

  tidy_text<-tidy_text %>% 
  filter(!word %in% c("xxxx",NA,"xx"))
#count
 tidy_text %>%
    count(word, sort = TRUE) 


#plotting the most frequent words
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 10000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Sentiment Analysis

sentiment <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)



tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100, scale=c(1,0.75))

#Topic Modelling






library(dplyr)
filtered_data_2<-data %>% 
  filter(Product %in% c("Mortgage"))
str(filtered_data)
library(tidytext)

filtered_data_2$Consumer.complaint.narrative <- as.character(filtered_data_2$Consumer.complaint.narrative)
tidy_text <- filtered_data_2 %>%
  unnest_tokens(word, Consumer.complaint.narrative)
tidy_text[1:20]
str(filtered_data_2)

data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words)

tidy_text %>%
  count(word, sort = TRUE) 

tidy_text<-tidy_text %>% 
  filter(!word %in% c("xxxx",NA,"xx"))


library(ggplot2)

tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 5000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

sentiment <- tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)
library(wordcloud)
library(reshape2)
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100,rot.per = 0.35, scale = c(3,.5))


#Topic Modelling

filtered_tp_credit<-filtered_credit%>%
  filter(!is.na(Consumer.complaint.narrative))
filtered_tp_credit<-filtered_tp_credit %>% 
  filter(!Consumer.complaint.narrative %in% c("xxxx","can","xx","get","paid"))


corpus <- Corpus(VectorSource(filtered_tp_credit$Consumer.complaint.narrative), readerControl=list(language="en"))
tweet_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE))
tweet_dtm


lda_credit <- LDA(tweet_dtm, k = 3, control = list(seed = 1234))
lda_credit


lda_td <- tidy(lda_credit)
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





#WORK  ON MORTGAGE DATA

filtered_mortgage$Consumer.complaint.narrative <- as.character(filtered_mortgage$Consumer.complaint.narrative)


#Tokenization
tidy_data <- filtered_mortgage %>%
  unnest_tokens(word, Consumer.complaint.narrative)


str(filtered_credit)

#Stop words removal
data(stop_words)
tidy_data <- tidy_data %>%
  anti_join(stop_words)

#Remove the words that doesn't make any sense

tidy_data<-tidy_data %>% 
  filter(!word %in% c("xxxx",NA,"xx"))
#count
tidy_data %>%
  count(word, sort = TRUE) 


#plotting the most frequent words
tidy_data %>%
  count(word, sort = TRUE) %>%
  filter(n > 30000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#Sentiment Analysis

sentiment <- tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)



tidy_data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100, scale=c(1,0.75))

#Topic Modelling











#Topic Modelling

filtered_tp_mortgage<-filtered_mortgage%>%
  filter(!is.na(Consumer.complaint.narrative))
filtered_tp_mortgage <- filtered_tp_mortgage[1:10000,]
filtered_tp_mortgage<-filtered_tp_mortgage %>% 
  filter(!Consumer.complaint.narrative %in% c("xxxx","can","xx","get","paid"))


corpus <- Corpus(VectorSource(filtered_tp_mortgage$Consumer.complaint.narrative), readerControl=list(language="en"))
tweet_dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 3, removeNumbers = TRUE, removePunctuation = TRUE))
tweet_dtm


lda_mortgage <- LDA(tweet_dtm, k = 3, control = list(seed = 1234))
lda_mortgage


lda_tp <- tidy(lda_mortgage)
lda_tp




top_terms <- lda_tp %>%
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


