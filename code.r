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
library(dqshiny)
library(rsconnect)

#Read files
business_filter=read.csv("business_filter.csv",stringsAsFactors = FALSE)
user_filter=read.csv("user_filter.csv",stringsAsFactors = FALSE)
review_filter=read.csv("review_filter.csv",stringsAsFactors = FALSE)
distance_matrix=read.csv("madison_cos_final1.csv",stringsAsFactors = FALSE)
business_filter$uniID=paste0("Business",business_filter$X)
user_filter$username=paste0(user_filter$name,"-",user_filter$X)

user_filter$X.1=NULL
business_filter$X.1=NULL
review_filter$X.1=NULL
user_filter$yelping_since=ymd_hms(user_filter$yelping_since)
distance_matrix$rank=1:nrow(distance_matrix)

ui <- navbarPage (id="inTabset",
                  theme = shinytheme("paper"),
                  title = div(img(src ="https://www.purdue.edu/discoverypark/resources/toolkit/files/logo/purdue/Purdue-Sig-Black-Gold-rgb.png", width = "150px", height = "48px", style="margin-top: -14px; margin-right:-14px;margin-left:-14px", height = 50)),
                  tabPanel(
                    'Home',
                    tags$style(HTML("
                  #login {
                  margin: auto;
                  width: 20%;
                  border: 2px solid grey;
                  padding: 20px;
                  }
                  
                  .log {
                  margin: auto;
                  width: 70%;
                  border: 4px solid green;
                  padding: 10px;
                  }"
                    )
                    ),
                    tags$head(tags$style("#intro{color: black;
                                 font-size: 15px;
                                 text-align: left
                                 }"
                    )
                    ),
                    div(id = "login",
                        autocomplete_input("user", h5('Username'), user_filter$username, max_options = 1000),
                        textInput(inputId = 'pass',label = h5('Password'),placeholder = "Not required for demo"),
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
                            div(tags$img(src = "https://cdn.pixabay.com/photo/2015/10/05/22/37/blank-profile-picture-973460_960_720.png",width=240, height=180),style="border:2px solid black;")
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
server <- function(input, output,session) {
  
  output$name <- renderText({
    user_select=user_filter[user_filter$username==input$user,]
    user_select$name
  })
  output$yelpsince <- renderText({
    user_select=user_filter[user_filter$username==input$user,]
    user_since=round(difftime(Sys.time(),user_select$yelping_since,units = "days"),0)
    paste(round(user_since/365,0)," Years")
  })
  output$totalreview <- renderText({
    user_select=user_filter[user_filter$username==input$user,]
    count_review=user_select$review_count
    count_review
  })
  observeEvent(input$reco, {
    showModal(modalDialog(
      renderDataTable({
        user_select=user_filter[user_filter$username==input$user,]
        count_review=user_select$review_count
        user_since=round(difftime(Sys.time(),user_select$yelping_since,units = "days"),0)
        review_by=review_filter[review_filter$user_id %in% user_select$user_id,]
        review_count=length(unique(review_by[,'business_id']))
        freq_list=business_filter[business_filter$business_id %in% review_by$business_id,]
        freq_list_business=unique(freq_list$uniID)
        freq_list$rating=strrep("\U2605", freq_list$stars*2)
        freq_list_render=freq_list[,c('name','address','rating','stars')]
        j=1
        rankframe=data.frame(uniID=business_filter$uniID)
        for (i in freq_list_business){
          distance_matrix=distance_matrix[order(-distance_matrix[,i]),]
          varname=distance_matrix$rank-1
          rankframe=cbind(rankframe,varname)
          j=j+1
        }
        rankframe$avgrank=(rowSums(rankframe[2:ncol(rankframe)]))/(ncol(rankframe)-2)
        rankframe=rankframe[order(rankframe$avgrank),]
        rankframe=rankframe[1:10,]
        
        final_list=business_filter[business_filter$uniID %in% rankframe$uniID,]
        final_list$rating=strrep("\U2605", final_list$stars*2)
        final_list_render=final_list[,c('name','address','rating','stars')]
        final_list_render
      }), 
      title = "List of recommendations", 
      easyClose = TRUE, fade = TRUE,
      actionButton("map","Show Map") )
    )})
  
  output$already_out<- renderDataTable({
    user_select=user_filter[user_filter$username==input$user,]
    review_by=review_filter[review_filter$user_id %in% user_select$user_id,]
    review_count=length(unique(review_by[,'business_id']))
    freq_list=business_filter[business_filter$business_id %in% review_by$business_id,]
    freq_list_business=unique(freq_list$uniID)
    freq_list$rating=strrep("\U2605", freq_list$stars*2)
    freq_list_render=freq_list[,c('name','address','rating','stars')]
    freq_list_render
  })
  
  observeEvent(input$map, {
    showModal(modalDialog(renderLeaflet({
      user_select=user_filter[user_filter$username==input$user,]
      count_review=user_select$review_count
      user_since=round(difftime(Sys.time(),user_select$yelping_since,units = "days"),0)
      review_by=review_filter[review_filter$user_id %in% user_select$user_id,]
      review_count=length(unique(review_by[,'business_id']))
      freq_list=business_filter[business_filter$business_id %in% review_by$business_id,]
      freq_list_business=unique(freq_list$uniID)
      freq_list$rating=strrep("\U2605", freq_list$stars*2)
      freq_list_render=freq_list[,c('name','address','rating','stars')]
      j=1
      rankframe=data.frame(uniID=business_filter$uniID)
      for (i in freq_list_business){
        distance_matrix=distance_matrix[order(-distance_matrix[,i]),]
        varname=distance_matrix$rank-1
        rankframe=cbind(rankframe,varname)
        j=j+1
      }
      rankframe$avgrank=(rowSums(rankframe[2:ncol(rankframe)]))/(ncol(rankframe)-2)
      rankframe=rankframe[order(rankframe$avgrank),]
      rankframe=rankframe[1:10,]
      
      final_list=business_filter[business_filter$uniID %in% rankframe$uniID,]
      final_list$rating=strrep("\U2605", final_list$stars*2)
      final_list_render=final_list[,c('name','address','rating','stars')]
      
      leaflet(width = 500,height = 300) %>%
        addTiles() %>%
        addMarkers(lng=final_list$longitude, lat=final_list$latitude, popup=paste(final_list$name,", ",final_list$address))
    }), title ="Map of restaurants", easyClose = TRUE, fade = TRUE)
    
    )
    
  }
  )
  observeEvent(input$Login, {
    showModal(modalDialog("Succesful", 
                          title ="Login", easyClose = TRUE, fade = FALSE),
              updateTabsetPanel(session, "inTabset",
                                selected = "App")
              
    )
    
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

