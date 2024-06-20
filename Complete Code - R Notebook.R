### Contents
# Basic Data Preprocessing and Cleaning
# Extracting Sentiments
# Building Word Cloud
# Answering Few  Questions about the data
# Visualisations on data
# Prepare Data for model building / Building a Document Term Matrix

#install.packages("tidytext")
#install.packages("tm")   #This package is used for NLP in R
#install.packages("SnowballC")  #Package for stopwords
#install.packages("tidytext")  #Tidy package for text analytics
#install.packages("sentimentr") #package for extracting sentiments
#install.packages("wordcloud") #For making Word Clouds
#install.packages("RColorBrewer") # package for the colours for word cloud
#install.packages("wordcloud2")
#install.packages("topicmodels")#For Topic Modelling

setwd("G:/LILTHOMA/Rise_Wpu/Mentor_Mind/Improve customer experience by analyzing product reviews of e-commerce website/Component 3/data")

library(tidytext)
library(stringr)
library(dplyr)
library(sentimentr)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(ggplot2)
library(topicmodels)

tweets <-read.csv("flipkart_product.csv",fileEncoding = "latin1")

#Data Cleaning
starting_rows <- nrow(tweets)
tweets$ProductName <- str_replace(tweets$ProductName, "\\?ÿ\\?ÿ", " ")
tweets$Price=str_extract(tweets$Price, "\\b(\\d{1,4}(,\\d{3})*)\\b", group = 1)
tweets$Summary <- str_replace_all(tweets$Summary,  "\\u0083|\\u009d|\\u008b|\\u008f", "")
tweets$Summary <- str_replace_all(tweets$Summary,  "ð", "")

tweets$Rate<-if_else(tweets$Rate=="Pigeon Favourite Electric Kettle?ÿ?ÿ(1.5 L, Silver, Black)",NA,
                     (if_else(tweets$Rate=="Bajaj DX 2 L/W Dry Iron",NA,
                              if_else(tweets$Rate=="Nova Plus Amaze NI 10 1100 W Dry Iron?ÿ?ÿ(Grey & Turquoise)",NA,
                                      if_else(tweets$Rate=="s","5",tweets$Rate)))))

tweets <- na.omit(tweets)

############################      Extracting Sentiments from Text using sentimentr

tweets=tweets%>%mutate(row_num=row_number())  #Unique Identification of each review
sentiment_value <- with(tweets,sentiment_by(get_sentences(Summary),list(row_num)))  #Getting the Sentiments
sentiment_value[,c("row_num","ave_sentiment")]
new_tweet <- inner_join(tweets,sentiment_value[,c("row_num","ave_sentiment")],by="row_num") #Created new df with sentiment scores

############################     Building World Cloud


all_text <- tweets$Summary
docs <- VCorpus(VectorSource(all_text))
docs <- docs %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
#Removed Numbers Puctuations Whitespaces stopwords and lower cased the text values

#Building Word Cloud
tweets_words <-  tweets %>%
  select(Summary) %>%
  unnest_tokens(word, Summary)
words <- tweets_words %>% count(word, sort=TRUE)
set.seed(1234) # for reproducibility 
#word_cloud <- wordcloud(words = words$word, freq = words$n,min.freq = 3,max.words=200, random.order=FALSE,rot.per=0.35,colors=brewer.pal(8, "Dark2"))
library(wordcloud2)
word_cloud2=wordcloud2(data=words, size=1.6, color='random-dark')

########################### Answering Few  Questions about the data
#1)No of Products in the list and average rating of each product
#2)Average price of each product
#3)Average Sentiment of the Product
#4)Correlation between number of words and sentiment
#5)Correlation between price and sentiment

df <- new_tweet
df$Price <- str_replace_all(df$Price,",","")
df$Rate <- as.numeric(df$Rate,errors="coerce")
df$Price <- as.numeric(df$Price,errors="coerce")
avg_ratings <- df%>%group_by(ProductName)%>%
  summarise(count=length(Rate),avg_rating=mean(Rate))%>%
  arrange(-count)
avg_price <- df%>%group_by(ProductName)%>%
  summarise(count=length(Price),avg_pricee=mean(Price))%>%
  arrange(-count)
avg_sentiment <- df%>%group_by(ProductName)%>%
  summarise(count=length(Price),avg_senti=mean(ave_sentiment))%>%
  arrange(-count)
df <- mutate(df,word_count=nchar(Summary))
df$Sentiment <- if_else(df$ave_sentiment >=0.0000000000,"Positive","Negative")
#Scatter plots
ggplot(df,aes(word_count,ave_sentiment))+geom_point()+geom_smooth(method=lm)+
  labs(title="Scatter plot of number of words vs avg sentiment")
ggplot(df,aes(Price,ave_sentiment))+geom_point()+geom_smooth(method=lm)+
  labs(title="Scatter plot of Price vs avg sentiment")
ggplot(df,aes(word_count,Rate))+geom_point()+geom_smooth(method=lm)+
  labs(title="Scatter plot of number of words vs Rate")
ggplot(df,aes(Price,Rate))+geom_point()+geom_smooth(method=lm)+
  labs(title="Scatter plot of Price vs Rate")
ggplot(df,aes(word_count))+geom_histogram(aes(fill=Sentiment),bins=10)+
  labs(title="Histogram of word_count segregated by sentimet")
df%>%group_by(Sentiment)%>%count()%>%ggplot(aes(x=Sentiment,y=n))+
  geom_bar(stat="identity",aes(fill=Sentiment))+
  labs(title = "Positive and Negative Revies count")

###############Building DTM for document Term Matrix
#### for building DTM you can uncomment
#dtm < -DocumentTermMatrix(docs)
#You can reduce the sparsity so that you dont have to handle large size
#This will create a dtm of 4GB+ so we are not creating one here


###Conclusion
#We have cleaned the data
#Build word cloud
#Extract Sentiments expressed in reviews
#Basic Exploratory Data Analysis
#Few Visualizations


###           THANK YOU


#Component 4 
#Task One

