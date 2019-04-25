#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(httr)
library(jsonlite)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel(title = tags$h2("Movie Crawler",style = "font-family: 'Lobster', cursive;
        color: green;"),windowTitle = "Movie Crawler"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        helpText("Enter the name of the movie you want to search for and click",br(),
                 "Get Movie Info:"),
           textInput(inputId = "movie_name",label = "Movie Name:",value = "Batman",width = "250px"),
           hr(),
           textInput(inputId = "year",label = "Year Movie was Released",value = "1989"),
           actionButton(inputId = "get_data",label = "Get Movie Info:"),
           hr(),
           wellPanel(tags$b("Movie Poster:"),wellPanel(uiOutput(outputId = "image")))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         wellPanel(fluidRow(column(3,textInput(inputId = "title",label = "Movie Title:")),
                  column(3,textInput(inputId = "rating",label = "Rating")),
                  column(3,textInput(inputId = "released",label = "Year Released"))),
         fluidRow(column(3,textInput(inputId = "time",label = "Run Time")),
                  column(3,textInput(inputId = "genre",label = "Genre:")),
                  column(3,textInput(inputId = "director",label = "Director:")))),
         wellPanel(fluidRow(column(3,textAreaInput(inputId = "actor",label = "Starring:",height = "150px")),
                  column(3,textAreaInput(inputId = "writer",label = "Written By:",height = "150px")),
                  column(3,textInput(inputId = "dvd",label = "DVD Release:"))),
         fluidRow(column(3,textAreaInput(inputId = "plot",label = "Movie Plot:",height = "250px")),
                  column(3,textAreaInput(inputId = "awards",label = "Awards & Nominations",height = "150px")),
                  column(3,textInput(inputId = "money",label = "Box Office:")),
                  column(3,wellPanel(tableOutput('movie_table')))))
         # wellPanel(tableOutput('movie_table'))

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
  
  observeEvent(input$get_data,{
    movie<- GET(paste0("http://www.omdbapi.com/?t='",input$movie_name,"'&y=",input$year,"&plot=''full'&apikey=672de4d"))
    info<- content(x = movie,as = "text")
    info <- fromJSON(info, flatten = TRUE)
    typeof(info)
    if(info$Response == "False"){
      showModal( modalDialog( title=paste0("Movie Not Found !!! "),
                              br(),
                              div(tags$b(paste0("Please Try your search again!!!" ), style = "color: red;"))
                              ))
                 updateTextInput(session = session,inputId = "title",label = "Movie Title:",value = "")
                 updateTextInput(session = session,inputId = "rating",label = "Rating",value = "")
                 updateTextInput(session = session,inputId = "released",label = "Year Released",value = "")
                 updateTextInput(session = session,inputId = "time",label = "Run Time:",value = "")
                 updateTextInput(session = session,inputId = "genre",label = "Genre:",value = "")
                 updateTextInput(session = session,inputId = "director",label = "Director:",value = "")
                 updateTextInput(session = session,inputId = "writer",label = "Written By:",value = "")
                 updateTextInput(session = session,inputId = "actor",label = "Starring:",value = "")
                 updateTextInput(session = session,inputId = "dvd",label = "DVD Release:",value = "")
                 updateTextAreaInput(session = session,inputId = "plot",label = "Movie Plot:",value = "")
                 updateTextAreaInput(session = session,inputId = "awards",label = "Awards & Nominations",value = "")
                 updateTextAreaInput(session = session,inputId = "money",label = "Box Office:",value = "")
                              
                              
      
    }else{
    updateTextInput(session = session,inputId = "title",label = "Movie Title:",value = info$Title)
    updateTextInput(session = session,inputId = "rating",label = "Rating",value = info$Rated)
    updateTextInput(session = session,inputId = "released",label = "Year Released",value = info$Released)
    updateTextInput(session = session,inputId = "time",label = "Run Time:",value = info$Runtime)
    updateTextInput(session = session,inputId = "genre",label = "Genre:",value = info$Genre)
    updateTextInput(session = session,inputId = "director",label = "Director:",value = info$Director)
    updateTextInput(session = session,inputId = "writer",label = "Written By:",value = info$Writer)
    updateTextInput(session = session,inputId = "actor",label = "Starring:",value = info$Actors)
    updateTextInput(session = session,inputId = "dvd",label = "DVD Release:",value = info$DVD)
    updateTextAreaInput(session = session,inputId = "plot",label = "Movie Plot:",value = info$Plot)
    updateTextAreaInput(session = session,inputId = "awards",label = "Awards & Nominations",value = info$Awards)
    updateTextAreaInput(session = session,inputId = "money",label = "Box Office:",value = info$BoxOffice)
    
    }
  })
  
 observeEvent(input$get_data,{
   
   movie<- GET(paste0("http://www.omdbapi.com/?t='",input$movie_name,"'&y=",input$year,"&apikey=672de4d"))
   info<- content(x = movie,as = "text")
   info <- fromJSON(info, flatten = TRUE)
   info
   # output$image <- renderText({
   #   return(paste0("'<iframe src=>'",info$Poster,"'</iframe>'"))
  # })
   })
 
 poster<- eventReactive(input$get_data,{
   
   movie<- GET(paste0("http://www.omdbapi.com/?t='",input$movie_name,"'&y=",input$year,"&apikey=672de4d"))
   info<- content(x = movie,as = "text")
   info <- fromJSON(info, flatten = TRUE)
   info <- info$Poster
   print(info)
   return(info)
   
 })
 
 mov_table<- eventReactive(input$get_data,{
   
   movie<- GET(paste0("http://www.omdbapi.com/?t='",input$movie_name,"'&y=",input$year,"&apikey=672de4d"))
   info<- content(x = movie,as = "text")
   info <- fromJSON(info, flatten = TRUE)
   info <- info$Ratings
   info <- as.data.frame(info)
   colnames(info) <- c("Site:","Score:")
   print(info)
   return(info)
   
 })

 
 output$movie_table <- renderTable(mov_table())
 
 output$image <- renderUI({
   tags$img(src=poster())
   
 })
 
}

# Run the application 
shinyApp(ui = ui, server = server)

