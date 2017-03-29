# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",function(message) {eval(message.value);});'))),
  # Application title
  titlePanel("Cyberthreat Detection"),

  fluidRow(
    # Sidebar with a slider input for number of bins
    
    sidebarLayout(
      sidebarPanel(
        helpText("Choose the packets to be scanned"),
        
        
        fileInput(inputId="file", label = h3("File input")),
        
        helpText("Choose random packets from selected file input")
        
        
      ),
      #sidebarPanel(
      # helpText("Choose single packet")
      
      
      # fileInput(inputId="file", label = h3("File input"))
      # ),
      # Show a plot of the generated distribution
      mainPanel(
        radioButtons(inputId="cl", label = h3("Choose Classifier",style="color:red"),
                     choices = list("kNN" = "KNN Classifier","Naive Bayes"="Naive Bayes Classifier","SVM" = "SVM Classifier",
                                    "Decision Tree" = "Decision Tree Classifier", "Random Forest" = "Random Forest Classifier"),selected = 1),
        #plotOutput("distPlot")
        actionButton(inputId = "go",
                     label = "Update")
        
        
        ,tags$hr(),textOutput("text1"),tags$hr(),
        tags$h3(textOutput("text2"),style="color:green"),tags$br(),
        tags$h4(textOutput("text3"),style="color:green")
        #textOutput("acc")
      )
    )
  ),
 fluidRow(
   sidebarPanel(
     
     helpText("Click below to choose a random packet from selected file input"),
     
     actionButton(inputId="random", "An action button")
   )
   
   
   
 )
))
