library(shiny)
library(shinythemes)
library(shinydashboard)
library(leaflet)
library(data.table)
library(lubridate)
library(reshape2)
library(textclean)
library(dplyr)
library(tidytext)
library(stringr) 
library(SnowballC)
library(shinyWidgets)

getwd()
setwd("D:/Purdue Onedrive/OneDrive - purdue.edu/BAIM/Fall 1/R for analytics/R-project/Final")

#Read files
business_raw=read.csv("business.csv",stringsAsFactors = FALSE)
user_raw=read.csv("user.csv",stringsAsFactors = FALSE)
review_raw=read.csv("review.csv",stringsAsFactors = FALSE)

#Filter for city of Madison
business_filter=business_raw[business_raw$city=="Madison",]
review_filter=review_raw[review_raw$business_id %in% business_filter$business_id,]
user_filter=user_raw[user_raw$user_id %in% review_filter$user_id,]

user_filter$yelping_since=ymd_hms(user_filter$yelping_since)


#Text mining... Reference: http://rpubs.com/LuizFelipeBrito/NLP_Text_Mining_001

#Clean the text and remove all numerals, punctuations and unknown characters
rev <- review_filter %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text !="") 
rev$text=gsub("[_]", "", rev$text)
rev$text=gsub("<br />", "", rev$text)
rev$text=gsub("\\w*[0-9]+\\w*\\s*", "", rev$text)
rev$text=gsub("['.?:(){}!@#$%^&*]", "", rev$text)

#Tokenize the text and convert each word to a row
rev=rev %>% unnest_tokens(word,text,to_lower = TRUE)

#Stemming all word (Using SnowballC package)
rev$word <- wordStem(rev$word,  language = "english")

#Remove stop words
data(stop_words)
rev <- rev %>% 
  anti_join(stop_words, "word")

#Get word frequency. Noticed that data is highly right skewed
wcount=as.data.frame(table(rev$word))
rev=merge(rev,wcount,by.x='word',by.y='Var1')
#Keep only those words that occur more than 40 and less than 25000 time to account for some of the skew
rev=rev[rev$Freq>40 & rev$Freq<25000,]


#Building a Review vector for every restaurant
rev$value=1
vector_matrix = dcast(rev[,c('value','business_id','word')],business_id ~ word, value.var='value')
vector_matrix[,2:ncol(vector_matrix)]= log(vector_matrix[,2:ncol(vector_matrix)]+1)


#Function to calculate Cosine similarity between restaurants
cos_sim = data.frame()
cosine_func <- function(a){
  for (i in 1:nrow(a)){
    X = as.numeric(a[i,c(2:ncol(a))])
    print(c("garbage value to make sure it works blah I is ",i))
    for (j in 1:nrow(a)){
      Y = as.numeric(a[j,c(2:ncol(a))])
      print(c("J be ",j))
      cos_sim[i,j]= sum(X*Y)/sqrt(sum(Y^2)*sum(X^2))
    }
  }
  return(cos_sim)
}

madison=cosine_func(vector_matrix)
write.csv(madison,"madison_cosine1.csv")
