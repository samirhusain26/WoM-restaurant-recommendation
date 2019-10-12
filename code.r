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
library(rsconnect)


getwd()
setwd("/home/husain2/UR4A stuff/Project/Final")

# #Read files
# business_raw=read.csv("business.csv",stringsAsFactors = FALSE)
# user_raw=read.csv("user.csv",stringsAsFactors = FALSE)
# review_raw=read.csv("review.csv",stringsAsFactors = FALSE)
# 
# #Take just madison
# business_filter=business_raw[business_raw$city=="Madison",]
# review_filter=review_raw[review_raw$business_id %in% business_filter$business_id,]
# user_filter=user_raw[user_raw$user_id %in% review_filter$user_id,]
# 
# #Write sample to new file (internal)
# write.csv(business_filter,'business_filter.csv')
# write.csv(review_filter,'review_filter.csv')
# write.csv(user_filter,'user_filter.csv')

#Read files
business_filter=read.csv("business_filter.csv",stringsAsFactors = FALSE)
user_filter=read.csv("user_filter.csv",stringsAsFactors = FALSE)
review_filter=read.csv("review_filter.csv",stringsAsFactors = FALSE)
user_filter$X.1=NULL
business_filter$X.1=NULL
review_filter$X.1=NULL
user_filter$yelping_since=ymd_hms(user_filter$yelping_since)


#Text mining
#Clean - http://rpubs.com/LuizFelipeBrito/NLP_Text_Mining_001
rev <- review_filter %>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text !="") 
rev$text=gsub("[_]", "", rev$text)
rev$text=gsub("<br />", "", rev$text)
rev$text=gsub("\\w*[0-9]+\\w*\\s*", "", rev$text)
rev$text=gsub("['.?:(){}!@#$%^&*]", "", rev$text)

#Tokenize
rev=rev %>% unnest_tokens(word,text,to_lower = TRUE)

#Stemming the word
rev$word <- wordStem(rev$word,  language = "english")

#Remove stop words
data(stop_words)
rev <- rev %>% 
  anti_join(stop_words, "word")

#Get word frequency
wcount=as.data.frame(table(rev$word))
rev=merge(rev,wcount,by.x='word',by.y='Var1')
rev=rev[rev$Freq>40 & rev$Freq<25000,]

rev$value=1
#Review vector for every restaurant
vector_matrix = dcast(rev[,c('value','business_id','word')],business_id ~ word, value.var='value')
vector_matrix[,2:ncol(vector_matrix)]= log(vector_matrix[,2:ncol(vector_matrix)]+1)

#Cosine similarity
cos_sim = data.frame(business_id = vector_matrix$business_id, sim =0 )
cosine_func <- function(a,bus_id){
  X = as.numeric(a[a$business_id == bus_id,c(2:ncol(a))])
  for (i in a$business_id){
    print(i)
    Y = as.numeric(a[a$business_id == i,c(2:ncol(a))])
    cos_sim[cos_sim$business_id==i,]$sim = sum(X*Y)/sqrt(sum(Y^2)*sum(X^2))
  }
  cos_sim$rank <- NA
  cos_sim[order(-cos_sim$sim),]$rank <- 1:nrow(cos_sim)
  return(cos_sim)
}

#given a user id, get their stats
user_select=user_filter[5,]
count_review=user_select$review_count
user_since=round(difftime(Sys.time(),user_select$yelping_since,units = "days"),0)
#reviews by given user
review_by=rev[rev$user_id %in% user_select$user_id,]
review_count=length(unique(review_by[,'business_id']))
freq_list=business_filter[business_filter$business_id %in% review_by$business_id,]
freq_list$rating=strrep("\U2605", freq_list$stars*2)
freq_list_render=freq_list[,c('name','address','rating','stars')]
s=list()
for(i in 1:3){
  s[[i]]=cosine_func(vector_matrix,freq_list[i,'business_id'])
}

final=merge(merge(s[[1]],s[[2]],by='business_id'),s[[3]],by='business_id')
final$final=((final$rank.x)+(final$rank.y)+(final$rank))/3
final=merge(final,business_filter,by='business_id')
final=final[order(final$final),]
final=final[1:10,]
final$rating=strrep("\U2605", final$stars*2)

final_list_render=final[,c('name','address','rating','stars')]

ui <- navbarPage (
  tags$style(HTML("
      
       .pic {
          margin: auto;
  width: 65%;
  border: 2px solid grey;
  padding: 4px;
      }
      .log {
      margin: auto;
  width: 70%;
  border: 4px solid green;
  padding: 10px;
      
      }

    ")),
  theme = shinytheme("paper"),
  title = div(img(src ="https://www.purdue.edu/discoverypark/resources/toolkit/files/logo/purdue/Purdue-Sig-Black-Gold-rgb.png", width = "150px", height = "48px", style="margin-top: -14px; margin-right:-14px;margin-left:-14px", height = 50)),
  tabPanel(
    
    'Home',
    textOutput('intro'),
    tags$head(tags$style("#intro{color: black;
                                 font-size: 15px;
                                 text-align: left
                                 }"
    )
    ),
    #setBackgroundImage(src = "https://getjar.co.uk/wp-content/uploads/2018/07/4855789-white-background-images-1.jpg"),
    div(id = "login",
    textInput(inputId = 'user',label = 'Username'),
    textInput(inputId = 'user',label = 'Password'),
    br(),
    actionButton("Login", "Log in")
    ),
    tags$style(
      type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 40%;left: 50%;margin-top: -100px;margin-left: -150px;}"
    )),
  tabPanel(
    'App',
    sidebarLayout(
      sidebarPanel(
        width = 3,
        helpText(h5("User details")),
        div(class="pic",
          tags$img(src = "https://media.licdn.com/dms/image/C5603AQF14ovUK3e5JA/profile-displayphoto-shrink_200_200/0?e=1576108800&v=beta&t=opwY2sQDksrCMlKIGRe6oRttSNFqNZ8rvnjMGauwC44")
        ),
        h6('Name:'),
        textOutput(outputId = 'name',inline = TRUE),
        tags$head(tags$style("#name{color: black;
                                 font-size: 20px;
                                 }"
        )
        ),
        h6('User Since:'),
        textOutput(outputId = 'yelpsince'),
        tags$head(tags$style("#yelpsince{color: black;
                                 font-size: 20px;
                                 }"
        )
        ),
        h6('Total number of Reviews:'),
        textOutput(outputId = 'totalreview'),
        tags$head(tags$style("#totalreview{color: black;
                                 font-size: 20px;
                                 }"
        )
        ),
        actionButton("reco","Recommend")

      ),
      mainPanel(
        h4("Restaurants reviewed by user"),
        dataTableOutput("already_out"),
        br()
      )
    )
  ),
  tabPanel(
    "About us",
    tags$hr(),
    h5('This project has been designed and developed by students of MS-BAIM ,Class of 2020 at Krannert School of Management, Purdue University.',style="text-align: center"),
    div(
      div(tags$img(src='https://media.licdn.com/dms/image/C5103AQE4lG3rKRB8Ag/profile-displayphoto-shrink_200_200/0?e=1576108800&v=beta&t=dZMjf2L9tpJ7LU0A0v-0cDor2gc36PdOfdoUjdhidkI', width=180, height=180),style="width: 20%;text-align: center;display: inline-block;float: left;"),
      div(tags$img(src='https://media.licdn.com/dms/image/C4D03AQFUayH2jpLWHw/profile-displayphoto-shrink_200_200/0?e=1576108800&v=beta&t=6JoPIbJt7AZPgLOdA7XvGhBYcEsiboRCQnmwDozozzg', width=180, height=180),style="width: 20%;text-align: center;display: inline-block;float: left;"),
      div(tags$img(src='https://media.licdn.com/dms/image/C5603AQHaW5WdcxXGTg/profile-displayphoto-shrink_200_200/0?e=1576108800&v=beta&t=Ta3e9QUZnlI3FS3UCqdydIpnxkSLa1QsagxmtaLZZNU', width=180, height=180),style="width: 20%;text-align: center;display: inline-block;float: left;"),
      div(tags$img(src='https://media.licdn.com/dms/image/C5603AQEU03MJn7JFfw/profile-displayphoto-shrink_200_200/0?e=1576108800&v=beta&t=RSvGtBIeD6QZy0sxghoTULQv5ozydAStcroJj3ZuKR0', width=180, height=180),style="width: 20%;text-align: center;display: inline-block;float: left;"),
      div(tags$img(src='https://media.licdn.com/dms/image/C4D03AQHlhGpaCUmsnA/profile-displayphoto-shrink_200_200/0?e=1576108800&v=beta&t=XAnCCaPrx75XVOhRZ9nJ0ytLa2QY6x8k4mdX1jqA_Jc', width=180, height=180),style="width: 20%;text-align: center;display: inline-block;float: left;"),
      style="text-align: center;width: 100%; height: 30px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;"),
    div(
      div(h5("Arun K"),style="width: 15%; margin-left:2.5%; margin-right: 2.5%; text-align: center;display: inline-block;float:left;border-radius: 40px "),
      div(h5("Juily V"),style="width: 15%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%"),
      div(h5("Yizhu L"),style="width: 15%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
      div(h5("Samir H"),style="width: 15%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
      div(h5("Maharshi D"),style="width: 15%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
      style="text-align: center;width: 100%; height: 80px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;"),
    div(  
      div(tags$a(href="https://www.linkedin.com/in/arun-ramakrishnan7/","Linkedin"),style="width: 15%; margin-left:2.5%; margin-right: 2.5%; text-align: center;display: inline-block;float:left;border-radius: 40px"),
      div(tags$a(href="https://www.linkedin.com/in/juilyvasandani/","Linkedin"),style="width: 15%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%"),
      div(tags$a(href="https://www.linkedin.com/in/yizhu-l-807531a4/","Linkedin"),style="width: 15%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
      div(tags$a(href="https://www.linkedin.com/in/samirhusain26/","Linkedin"),style="width: 15%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
      div(tags$a(href="https://www.linkedin.com/in/maharshi-dutta-7a6430b0/","Linkedin"),style="width: 15%;text-align: center;display: inline-block;float: left;border-radius: 40px;margin-left:2.5%; margin-right: 2.5%;"),
      style="text-align: center;width: 100%; height: 80px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;")
  )
)
server <- function(input, output) {
  
  output$name <- renderText({
    #user_select$name
    "Matthew"
  })
  output$yelpsince <- renderText({
    paste(round(user_since/365,0)," Years")
  })
  output$totalreview <- renderText({
    review_count
  })
  observeEvent(input$reco, {
    showModal(modalDialog(
      renderDataTable({
        final_list_render
      }), 
      title = "List of recommendations", 
      easyClose = TRUE, fade = TRUE,
      actionButton("map","Show Map") )
    )})
  
  
  output$already_out<- renderDataTable({
    freq_list_render
  })
  
  output$intro=renderText({
    "This app is a highly personalized restaurant recommendation system that revolutionizes the logic behind recommender systems by digitizing and integrating WOM (Word of Mouth) marketing into our model."
  })
  
  observeEvent(input$map, {
    showModal(modalDialog(renderLeaflet({
      leaflet(width = 500,height = 300) %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=final$longitude, lat=final$latitude, popup=paste(final$name,", ",final$address))
    }), title ="Map of restaurants", easyClose = TRUE, fade = TRUE)
    
    )
    
  }
  )
  observeEvent(input$Login, {
    showModal(modalDialog("Succesful", 
                          title ="Login", easyClose = TRUE, fade = TRUE))
    
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

